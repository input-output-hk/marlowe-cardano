-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Safety analysis for Plutus transactions in Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Transaction (
  -- * Plutus Transactions
  executeTransaction,
  findTransactions,
  findTransactions',
  foldTransactionsM,
) where

import Control.Monad.Except (MonadError (throwError), MonadIO (..), foldM, liftEither, liftIO)
import Data.Bifunctor (first, second)
import Data.List (nub, nubBy)
import Data.String (IsString (..))
import Language.Marlowe.Analysis.Safety.Ledger (worstMinimumUtxo')
import Language.Marlowe.Analysis.Safety.Types (Transaction (..))
import Language.Marlowe.Core.V1.Merkle (
  Continuations,
  MerkleizedContract (..),
  deepDemerkleize,
  demerkleizeContract,
  merkleizeInput,
 )
import Language.Marlowe.Core.V1.Semantics (
  MarloweData (MarloweData, marloweContract, marloweParams, marloweState),
  MarloweParams (..),
  Payment (Payment),
  TransactionInput (..),
  TransactionOutput (Error, TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings),
  computeTransaction,
  totalBalance,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Deposit),
  Case (Case),
  ChoiceId (ChoiceId),
  Contract (..),
  Input (MerkleizedInput),
  InputContent (IChoice, IDeposit),
  Party (Address, Role),
  Payee (Party),
  State (..),
  Token (..),
  Value (Constant),
  getInputContent,
 )
import Language.Marlowe.FindInputs (getAllInputs)
import Language.Marlowe.Scripts (marloweTxInputsFromInputs)

import qualified Data.ByteString.Short as SBS (ShortByteString)
import qualified Plutus.ApiCommon as P (LedgerPlutusVersion (PlutusV2), evaluateScriptCounting)
import qualified Plutus.Script.Utils.Scripts as P (datumHash)
import qualified Plutus.V2.Ledger.Api as P hiding (evaluateScriptCounting)
import qualified PlutusTx.AssocMap as AM

-- | Execute a Marlowe transaction.
executeTransaction
  :: (IsString e)
  => (MonadError e m)
  => P.EvaluationContext
  -- ^ The Plutus evaluation context.
  -> SBS.ShortByteString
  -- ^ The validator.
  -> P.Address
  -- ^ The semantics validator address.
  -> P.Address
  -- ^ The payout validator address.
  -> [P.TxInInfo]
  -- ^ The reference-script input, if any.
  -> P.Address
  -- ^ The public-key address executing the transaction.
  -> MarloweParams
  -- ^ The parameters for the Marlowe contract instance.
  -> Transaction
  -- ^ The transaction to be executed.
  -> m P.ExBudget
  -- ^ Action to execute the transaction and to return the new state and contract along with the execution cost.
executeTransaction evaluationContext semanticsValidator semanticsAddress payoutAddress referenceInputs creatorAddress marloweParams@MarloweParams{..} (Transaction marloweState marloweContract TransactionInput{..} output) =
  do
    -- We only attend to details that make affect the *execution cost* of the script context,
    -- so the corresponding transaction does not actually need to be valid.
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e -> throwError . fromString $ show e
    let oneLovelace = P.singleton P.adaSymbol P.adaToken 1
        inDatum = P.Datum $ P.toBuiltinData MarloweData{..}
        inDatumHash = P.datumHash inDatum
        inValue = totalBalance $ accounts marloweState
        inScriptTxRef = P.TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 0
        inScriptTx = P.TxInInfo inScriptTxRef $ P.TxOut semanticsAddress inValue (P.OutputDatumHash inDatumHash) Nothing
        outDatum =
          if txOutContract == Close
            then mempty
            else pure . P.Datum . P.toBuiltinData $ MarloweData marloweParams txOutState txOutContract
        outDatumHash = P.datumHash <$> outDatum
        outValue = totalBalance $ accounts txOutState
        outScriptTx = flip (P.TxOut semanticsAddress outValue) Nothing . P.OutputDatumHash <$> outDatumHash
        redeemer = P.Redeemer . P.toBuiltinData $ marloweTxInputsFromInputs txInputs
        makePayment (Payment _ (Party (Address _ address)) (Token currency name) amount) =
          pure $ P.TxOut address (P.singleton currency name amount) P.NoOutputDatum Nothing
        makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
          pure $
            P.TxOut
              payoutAddress
              (P.singleton currency name amount)
              (P.OutputDatumHash . P.datumHash . P.Datum $ P.toBuiltinData (rolesCurrency, role))
              Nothing
        makePayment _ = mempty
        makePaymentDatum (Payment _ (Party (Role role)) _ _) =
          pure . P.Datum $ P.toBuiltinData (rolesCurrency, role)
        makePaymentDatum _ = mempty
        consolidatePayments ps =
          let extractAddressHash (P.TxOut address _ hash _) = (address, hash)
              extractValue (P.TxOut _ value _ _) = value
           in [ P.TxOut address value hash Nothing
              | ah@(address, hash) <- nub $ extractAddressHash <$> ps
              , let value = foldMap extractValue $ filter ((== ah) . extractAddressHash) ps
              ]
        outPayments = consolidatePayments $ concatMap makePayment txOutPayments
        outPaymentDatums = concatMap makePaymentDatum txOutPayments
        findRole :: InputContent -> [P.TokenName]
        findRole (IDeposit _ (Role role) _ _) = pure role
        findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
        findRole _ = mempty
        roles = fmap (flip (P.singleton rolesCurrency) 1) . foldMap findRole $ getInputContent <$> txInputs
        inRoles =
          uncurry (P.TxInInfo . P.TxOutRef "1111111111111111111111111111111111111111111111111111111111111111")
            <$> zip [1 ..] outRoles
        outRoles = [P.TxOut creatorAddress role P.NoOutputDatum Nothing | role <- roles]
        merkleDatums =
          concat
            [ case input of
              MerkleizedInput _ _ contract -> pure . P.Datum $ P.toBuiltinData contract
              _ -> mempty
            | input <- txInputs
            ]
        outMerkle =
          [ P.TxOut creatorAddress oneLovelace (P.OutputDatumHash $ P.datumHash datum) Nothing
          | datum <- merkleDatums
          ]
        inFunds =
          P.TxInInfo
            (P.TxOutRef "1111111111111111111111111111111111111111111111111111111111111111" 0)
            (P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing)
        outChange = P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing
        txInfoInputs =
          inFunds
            : inScriptTx
            : inRoles
        txInfoReferenceInputs = referenceInputs
        txInfoOutputs =
          outChange
            : outScriptTx
              <> outPayments
              <> outRoles
              <> outMerkle
        txInfoFee = oneLovelace
        txInfoMint = mempty
        txInfoDCert = mempty
        txInfoWdrl = AM.empty
        txInfoValidRange = P.Interval (P.LowerBound (P.Finite $ fst txInterval) True) (P.UpperBound (P.Finite $ snd txInterval) True)
        findSignatory :: InputContent -> [P.PubKeyHash]
        findSignatory (IDeposit _ (Address _ (P.Address (P.PubKeyCredential pkh) _)) _ _) = pure pkh
        findSignatory (IChoice (ChoiceId _ (Address _ (P.Address (P.PubKeyCredential pkh) _))) _) = pure pkh
        findSignatory _ = mempty
        txInfoSignatories =
          (foldMap findSignatory $ getInputContent <$> txInputs)
            <> case creatorAddress of
              P.Address (P.PubKeyCredential pkh) _ -> pure pkh
              _ -> mempty
        txInfoRedeemers = AM.singleton scriptContextPurpose redeemer
        txInfoData = AM.fromList $ ((,) =<< P.datumHash) <$> inDatum : outDatum <> outPaymentDatums <> merkleDatums
        txInfoId = "2222222222222222222222222222222222222222222222222222222222222222"
        scriptContextTxInfo = P.TxInfo{..}
        scriptContextPurpose = P.Spending inScriptTxRef
        scriptContext = P.ScriptContext{..}
    case evaluateSemantics evaluationContext semanticsValidator (P.toData inDatum) (P.toData redeemer) (P.toData scriptContext) of
      (_, Right budget) -> pure budget
      (msg, Left err) -> throwError . fromString $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."

-- | Visit transactions along all execution paths of a a contract.
foldTransactionsM
  :: (Monad m)
  => (Monoid a)
  => (Transaction -> m a)
  -- ^ Function to collect results.
  -> [Transaction]
  -- ^ The transactions.
  -> m a
  -- ^ The collected results.
foldTransactionsM f =
  (foldM $ (. f) . fmap . (<>)) mempty

-- | Find transactions along all execution paths of a contract.
findTransactions
  :: (IsString e)
  => (MonadError e m)
  => (MonadIO m)
  => Party
  -- ^ The contract creator.
  -> Integer
  -- ^ The initial lovelace for the creator's account.
  -> MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [Transaction]
  -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions creatorAddress minAda mc@MerkleizedContract{..} =
  do
    let ada = Token P.adaSymbol P.adaToken
        prune (Transaction s c i _) (Transaction s' c' i' _) = s == s' && c == c' && i == i'
    paths <- findPaths creatorAddress minAda mc
    nubBy prune
      . concat
      <$> sequence
        [ findTransactionPath mcContinuations state mcContract inputs
        | (minTime, inputs) <- paths
        , let state = State (AM.singleton (creatorAddress, ada) minAda) AM.empty AM.empty minTime
        ]

-- | Find transactions along all execution paths of a contract.
findTransactions'
  :: (IsString e)
  => (MonadError e m)
  => (MonadIO m)
  => MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [Transaction]
  -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions' mc@MerkleizedContract{..} =
  let utxoCostPerByte = 4310
      creatorAddress =
        Address True $
          P.Address
            (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
            (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      minAda = worstMinimumUtxo' utxoCostPerByte mcContract mcContinuations
   in findTransactions creatorAddress minAda mc

-- | Find the transactions corresponding to a path of demerkleized inputs.
findTransactionPath
  :: (IsString e)
  => (MonadError e m)
  => Continuations
  -- ^ The continuations of the contact.
  -> State
  -- ^ The initial contract.
  -> Contract
  -- ^ The initial state.
  -> [TransactionInput]
  -- ^ The path of demerkleized inputs.
  -> m [Transaction]
  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransactionPath continuations state contract =
  let go ((state', contract'), previous) input =
        do
          (input', output) <- findTransaction continuations state' contract' input
          case output of
            TransactionOutput{..} -> pure ((txOutState, txOutContract), Transaction state' contract' input' output : previous)
            Error e -> throwError . fromString $ show e
   in fmap snd . foldM go ((state, contract), [])

-- | Find the transaction corresponding to demerkleized input.
findTransaction
  :: (IsString e)
  => (MonadError e m)
  => Continuations
  -- ^ The continuations of the contact.
  -> State
  -- ^ The initial contract.
  -> Contract
  -- ^ The initial state.
  -> TransactionInput
  -- ^ The demerkleized input to the contract.
  -> m (TransactionInput, TransactionOutput)
  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransaction continuations state contract TransactionInput{..} =
  do
    input <-
      TransactionInput txInterval . snd <$> foldM (merkleizeInput txInterval continuations) ((state, contract), []) txInputs
    pure (input, computeTransaction input state contract)

-- | Find all of the paths through a Marlowe contract.
findPaths
  :: (IsString e)
  => (MonadError e m)
  => (MonadIO m)
  => Party
  -- ^ The creator.
  -> Integer
  -- ^ The initial lovelace in the creator's account.
  -> MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [(P.POSIXTime, [TransactionInput])]
  -- ^ The paths throught the Marlowe contract.
findPaths creatorAddress minAda MerkleizedContract{..} =
  do
    let ada = Token P.adaSymbol P.adaToken
        forever = 4_102_444_800_000 {- 1 Jan 2100 -}
        -- Add an initial deposit so that `getAllInputs` accounts for the initial state in its analysis.
        contract = When [Case (Deposit creatorAddress creatorAddress ada $ Constant minAda) mcContract] forever Close
    paths <-
      liftEither . first (fromString . show)
        =<< liftIO . getAllInputs
        =<< demerkleizeContract mcContinuations (deepDemerkleize contract)
    pure
      . filter (not . null . snd) -- Discard the input that is only the initial deposit.
      $ second tail <$> paths -- Discard the initial deposit from each path.

-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics
  :: P.EvaluationContext
  -- ^ The evaluation context.
  -> SBS.ShortByteString
  -- ^ The validator script.
  -> P.Data
  -- ^ The datum.
  -> P.Data
  -- ^ The redeemer.
  -> P.Data
  -- ^ The script context.
  -> (P.LogOutput, Either P.EvaluationError P.ExBudget)
  -- ^ The result.
evaluateSemantics evaluationContext validator datum redeemer context =
  P.evaluateScriptCounting
    P.PlutusV2
    (P.ProtocolVersion 8 0)
    P.Verbose
    evaluationContext
    validator
    [datum, redeemer, context]
