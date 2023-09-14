{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Safety analysis for Plutus transactions in Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Transaction (
  -- * Plutus Transactions
  calcMarloweTxExBudget,
  executeTransaction,
  findTransactions,
  findTransactions',
  foldTransactionsM,
  inputsRequiredRoles,
  LockedRoles (..),
  MarloweExBudget (..),
  TxInSpec (..),
  TxOutSpec (..),
  UseReferenceInput (..),
  ValidatorEvalContext (..),
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
  InputContent (IChoice, IDeposit),
  Party (Address, Role),
  Payee (Party),
  State (..),
  Token (..),
  Value (Constant),
  getInputContent,
 )
import Language.Marlowe.FindInputs (getAllInputs)
import Language.Marlowe.Scripts.Types (marloweTxInputsFromInputs)
import Language.Marlowe.Util (dataHash)

import Control.Monad (guard)
import Control.Monad.State (MonadState (get), evalStateT)
import Control.Monad.State.Class (MonadState (put))
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Short as SBS (ShortByteString)
import Data.Functor ((<&>))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Language.Marlowe (Input (..))
import qualified Plutus.ApiCommon as P (LedgerPlutusVersion (PlutusV2), evaluateScriptCounting)
import qualified Plutus.V2.Ledger.Api as P hiding (evaluateScriptCounting)
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Builtins.Class as P
import Text.Printf (printf)

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
        inDatumHash = P.DatumHash $ dataHash inDatum
        inValue = totalBalance $ accounts marloweState
        inScriptTxRef = P.TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 0
        inScriptTx = P.TxInInfo inScriptTxRef $ P.TxOut semanticsAddress inValue (P.OutputDatumHash inDatumHash) Nothing
        outDatum =
          if txOutContract == Close
            then mempty
            else pure . P.Datum . P.toBuiltinData $ MarloweData marloweParams txOutState txOutContract
        outDatumHash = P.DatumHash . dataHash <$> outDatum
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
              (P.OutputDatumHash . P.DatumHash . dataHash . P.Datum $ P.toBuiltinData (rolesCurrency, role))
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
          [ P.TxOut creatorAddress oneLovelace (P.OutputDatumHash $ P.DatumHash $ dataHash datum) Nothing
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
        txInfoData = AM.fromList $ ((,) =<< P.DatumHash . dataHash) <$> nub (inDatum : outDatum <> outPaymentDatums <> merkleDatums)
        txInfoId = "2222222222222222222222222222222222222222222222222222222222222222"
        scriptContextTxInfo = P.TxInfo{..}
        scriptContextPurpose = P.Spending inScriptTxRef
        scriptContext = P.ScriptContext{..}
    case evaluateSemantics evaluationContext semanticsValidator (P.toData inDatum) (P.toData redeemer) (P.toData scriptContext) of
      (_, Right budget) -> pure budget
      (msg, Left err) -> throwError . fromString $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."

newtype UseReferenceInput = UseReferenceInput Bool
  deriving (Eq, Show)

newtype LockedRoles = LockedRoles [P.TokenName]
  deriving (Eq, Show)

data MarloweExBudget = MarloweExBudget
  { mvbSemanticsValidator :: Maybe P.ExBudget
  -- ^ The execution cost of the semantics validator.
  , mvbOpenRoleValidator :: [P.ExBudget]
  -- ^ The execution cost of the open role validator and the remaining locked role tokens.
  }
  deriving (Eq, Show)

inputsRequiredRoles :: [Input] -> [P.TokenName]
inputsRequiredRoles = foldMap (findRole . getInputContent)
  where
    findRole :: InputContent -> [P.TokenName]
    findRole (IDeposit _ (Role role) _ _) = pure role
    findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
    findRole _ = mempty

-- | Execute a Marlowe transaction.
calcMarloweTxExBudget
  :: (IsString e)
  => (MonadError e m)
  => P.EvaluationContext
  -- ^ The Plutus evaluation context.
  -> (SBS.ShortByteString, P.Address, UseReferenceInput)
  -- ^ The semantics validator and its address.
  -> (SBS.ShortByteString, P.Address, UseReferenceInput, LockedRoles)
  -- ^ The open role validator and its address.
  -> P.Address
  -- ^ The payout validator address.
  -> MarloweParams
  -- ^ The parameters for the Marlowe contract instance.
  -> Transaction
  -- ^ The transaction to be executed.
  -> m MarloweExBudget
calcMarloweTxExBudget
  evaluationContext
  (semanticsValidator, semanticsAddress, semanticsUseReferenceInput)
  (openRoleValidator, openRoleAddress, openRoleUseReferenceInput, LockedRoles openRoleLockedTokens)
  payoutAddress
  marloweParams@MarloweParams{..}
  (Transaction marloweState marloweContract TransactionInput{..} output) = flip evalStateT initSequence $ do
    creatorAddress <- freshAddress
    -- We only attend to details that make affect the *execution cost* of the script context,
    -- so the corresponding transaction does not actually need to be valid.
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e -> throwError . fromString $ show e
    let semanticsName = ValidatorName "MarloweSemantics"
        oneLovelace = P.singleton P.adaSymbol P.adaToken 1
        inMarlowe = do
          let tiAddress = semanticsAddress
              tiDatum = Just $ P.Datum $ P.toBuiltinData MarloweData{..}
              tiValue = totalBalance $ accounts marloweState
              redeemer = P.Redeemer . P.toBuiltinData $ marloweTxInputsFromInputs txInputs
              tiScriptEvaluationContext =
                Just
                  (semanticsName, semanticsValidator, redeemer, semanticsUseReferenceInput)
          TxInSpec{..}

        possibleOutMarlowe = do
          datum <-
            if txOutContract == Close
              then Nothing
              else pure . P.Datum . P.toBuiltinData $ MarloweData marloweParams txOutState txOutContract
          let value = totalBalance $ accounts txOutState
          pure $ TxOutSpec semanticsAddress value (Just datum)

        makePayment (Payment _ (Party (Address _ address)) (Token currency name) amount) =
          pure $ TxOutSpec address (P.singleton currency name amount) Nothing
        makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
          pure $
            TxOutSpec payoutAddress (P.singleton currency name amount) (Just $ P.Datum $ P.toBuiltinData (rolesCurrency, role))
        makePayment _ = mempty
        outPayments = foldMap makePayment txOutPayments

        requiredRoles = inputsRequiredRoles txInputs
        (requiredUnlockedRoles, requiredLockedRoles) = L.partition (`elem` openRoleLockedTokens) requiredRoles

        roleNFT = flip (P.singleton rolesCurrency) 1
        inRoles = do
          let tiAddress = openRoleAddress
              tiDatum = Just $ P.Datum $ P.toBuiltinData P.emptyByteString
              redeemer = P.Redeemer . P.toBuiltinData $ P.emptyByteString
              ctx idx =
                Just
                  (ValidatorName ("OpenRole-" <> show idx), openRoleValidator, redeemer, openRoleUseReferenceInput)
          [TxInSpec creatorAddress (roleNFT role <> oneLovelace) Nothing Nothing | role <- requiredUnlockedRoles]
            <> [ TxInSpec{tiValue = tiValue, tiScriptEvaluationContext = ctx idx, ..}
               | (role, idx) <- zip requiredLockedRoles [(1 :: Int) ..]
               , let tiValue = roleNFT role <> oneLovelace
               ]
        outRoles = [TxOutSpec creatorAddress (roleNFT role) Nothing | role <- requiredRoles]

        merkleDatums =
          concat
            [ case input of
              MerkleizedInput _ _ contract -> pure . P.Datum $ P.toBuiltinData contract
              NormalInput{} -> mempty
            | input <- txInputs
            ]
        outMerkles =
          [ TxOutSpec creatorAddress oneLovelace (Just datum)
          | datum <- merkleDatums
          ]

    resultMap <-
      calcValidatorsExBudget
        evaluationContext
        creatorAddress
        (inMarlowe : inRoles)
        (foldMap pure possibleOutMarlowe <> outRoles <> outPayments <> outMerkles)
        txInterval
        []
        (ConsolidateTxOuts True)
        (ProvideFunding True)

    case (M.lookup semanticsName resultMap, M.elems $ M.delete semanticsName resultMap) of
      (Just semanticsExBudget, openRoleBudgets) ->
        pure $
          MarloweExBudget
            { mvbSemanticsValidator = Just semanticsExBudget
            , mvbOpenRoleValidator = openRoleBudgets
            }
      _ -> throwError "Marlowe semantics validator failed to return execution cost"

data TxInSpec = TxInSpec
  { tiAddress :: P.Address
  , tiValue :: P.Value
  , tiDatum :: Maybe P.Datum
  , tiScriptEvaluationContext :: Maybe (ValidatorName, SBS.ShortByteString, P.Redeemer, UseReferenceInput)
  }

data TxOutSpec = TxOutSpec
  { toAddress :: P.Address
  , toValue :: P.Value
  , toDatum :: Maybe P.Datum
  }

-- | Used for detailed reporting
newtype ValidatorName = ValidatorName String
  deriving (Eq, Ord, Show, IsString)

-- | Whether to merge together outputs with the same address.
newtype ConsolidateTxOuts = ConsolidateTxOuts Bool

-- | Whether to attach some funding txin and change output.
newtype ProvideFunding = ProvideFunding Bool

-- | Simple context for generating fresh addresses and txids.
newtype Sequence = Sequence Int

initSequence :: Sequence
initSequence = Sequence 0

freshInt :: (MonadState Sequence m) => m Int
freshInt = do
  Sequence i <- get
  put $ Sequence $ i + 1
  pure i

freshPubKeyHash :: (MonadState Sequence m) => m P.PubKeyHash
freshPubKeyHash = do
  i <- freshInt
  pure $ P.PubKeyHash $ P.stringToBuiltinByteString $ printf "%056d" i

freshAddress :: (MonadState Sequence m) => m P.Address
freshAddress = do
  i <- freshPubKeyHash
  j <- freshPubKeyHash
  pure $ P.Address (P.PubKeyCredential i) $ Just $ P.StakingHash $ P.PubKeyCredential j

freshTxId :: (MonadState Sequence m) => m P.TxId
freshTxId = do
  i <- freshInt
  pure $ P.TxId $ P.stringToBuiltinByteString $ printf "%064d" i

freshTxOutRef :: (MonadState Sequence m) => m P.TxOutRef
freshTxOutRef = do
  txId <- freshTxId
  pure $ P.TxOutRef txId 0

data ValidatorEvalContext = ValidatorEvalContext
  { vecValidator :: SBS.ShortByteString
  , vecInDatum :: P.Datum
  , vecRedeemer :: P.Redeemer
  , vecTxOutRef :: P.TxOutRef
  -- ^ The reference is needed to set the spending script purpose correctly.
  }

calcValidatorExBudget
  :: (IsString e)
  => (MonadError e m)
  => P.EvaluationContext
  -- ^ The Plutus evaluation context.
  -> P.ScriptContext
  -- ^ The Plutus script context.
  -> ValidatorEvalContext
  -- ^ The validator to evaluate.
  -> m P.ExBudget
  -- ^ The execution cost.
calcValidatorExBudget evaluationContext scriptContext (ValidatorEvalContext validator datum redeemer txOutRef) = do
  let scriptPurpose = P.Spending txOutRef
      scriptContext' = scriptContext{P.scriptContextPurpose = scriptPurpose}
  case evaluateSemantics evaluationContext validator (P.toData datum) (P.toData redeemer) (P.toData scriptContext') of
    (_, Right budget) -> pure budget
    (msg, Left err) -> throwError . fromString $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."

datumHash :: P.Datum -> P.DatumHash
datumHash = P.DatumHash . dataHash

calcValidatorsExBudget
  :: (IsString e)
  => (MonadError e m)
  => (MonadState Sequence m)
  => P.EvaluationContext
  -- ^ The Plutus evaluation context.
  -> P.Address
  -- ^ The address of the creator of the transaction.
  -> [TxInSpec]
  -- ^ Inputs to the transaction. They possibly contain named spending validators which we want to evaluate.
  -> [TxOutSpec]
  -- ^ Scripts which we want to evaluate
  -> (P.POSIXTime, P.POSIXTime)
  -- ^ The validity interval of the transaction.
  -> [P.PubKeyHash]
  -- ^ Extra signatories to the transaction.
  -> ConsolidateTxOuts
  -- ^ Whether to merge together outputs with the same address.
  -> ProvideFunding
  -- ^ Whether to attach some funding txin and change output.
  -> m (M.Map ValidatorName P.ExBudget)
  -- ^ Action to execute the transaction and to return the new state and contract along with the execution cost.
calcValidatorsExBudget evaluationContext creatorAddress txInSpecs txOutSpecs (invalidBefore, invalidHereafter) extraSignatories (ConsolidateTxOuts consolidate) (ProvideFunding provideFunding) = do
  let oneLovelace = P.singleton P.adaSymbol P.adaToken 1
      txInfoOutputs = do
        let extraTxOuts = [P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing | provideFunding]
            parts' = do
              let parts = txOutSpecs <&> \(TxOutSpec address value datum) -> ((address, datum), value)
              if consolidate
                then M.toList $ M.fromListWith (<>) parts
                else parts
            parts'' =
              parts' <&> \((address, datum), value) -> do
                let datum' = case datum of
                      Nothing -> P.NoOutputDatum
                      Just d -> P.OutputDatumHash $ datumHash d
                P.TxOut address value datum' Nothing
        extraTxOuts <> parts''

  (txInfoInputs, validatorsEvalContext) <- do
    extraTxIns <-
      if provideFunding
        then do
          fundTxOutRef <- freshTxOutRef
          let fundTxIn = P.TxInInfo fundTxOutRef (P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing)
          pure [fundTxIn]
        else pure []
    (txIns, validatorsEvalContext) <- do
      unzip <$> for txInSpecs \(TxInSpec address value possibleDatum possibleScriptEvalContext) -> do
        txOutRef <- freshTxOutRef
        let datum' = case possibleDatum of
              Nothing -> P.Datum $ P.toBuiltinData P.emptyByteString
              Just d -> d
            txIn = P.TxInInfo txOutRef (P.TxOut address value (P.OutputDatumHash $ datumHash datum') Nothing)
            possibleValidatorContext = do
              (name, validator, redeemer, _) <- possibleScriptEvalContext
              let validatorEvalContext = ValidatorEvalContext validator datum' redeemer txOutRef
              pure (name, validatorEvalContext)
        pure (txIn, possibleValidatorContext)
    pure (extraTxIns <> txIns, M.fromList $ catMaybes validatorsEvalContext)

  txInfoReferenceInputs <-
    catMaybes <$> for txInSpecs \(TxInSpec addr _ _ possibleScriptEvalContext) -> runMaybeT $ do
      (_, _, _, UseReferenceInput useReferenceInput) <- MaybeT $ pure possibleScriptEvalContext
      guard useReferenceInput
      let toValidatorHash :: P.Address -> Maybe P.ValidatorHash
          toValidatorHash (P.Address (P.ScriptCredential k) _) = Just k
          toValidatorHash _ = Nothing
          toScriptHash (P.ValidatorHash vh) = P.ScriptHash vh
      MaybeT $ for (toScriptHash <$> toValidatorHash addr) \validatorHash -> do
        referenceTxOutRef <- freshTxOutRef
        pure $ P.TxInInfo referenceTxOutRef (P.TxOut creatorAddress oneLovelace P.NoOutputDatum (Just validatorHash))

  txInfoId <- freshTxId
  let txInfoRedeemers =
        AM.fromList $
          M.elems validatorsEvalContext <&> \(ValidatorEvalContext _ _ redeemer txOutRef) -> do
            let purpose = P.Spending txOutRef
            (purpose, redeemer)

      txInfoValidRange =
        P.Interval
          (P.LowerBound (P.Finite invalidBefore) True)
          (P.UpperBound (P.Finite invalidHereafter) True)
      txInfoFee = oneLovelace
      txInfoMint = mempty
      txInfoDCert = mempty
      txInfoWdrl = AM.empty
      txInfoSignatories =
        extraSignatories
          <> case creatorAddress of
            P.Address (P.PubKeyCredential pkh) _ -> pure pkh
            _ -> mempty
      txInfoData = do
        let inDatums = catMaybes $ txInSpecs <&> \(TxInSpec _ _ datum _) -> datum
            outDatums = catMaybes $ txOutSpecs <&> \(TxOutSpec _ _ datum) -> datum
        AM.fromList $ ((,) =<< datumHash) <$> (nub $ inDatums <> outDatums)

  scriptPurposePlaceholder <- P.Spending <$> freshTxOutRef
  let scriptContextTxInfo = P.TxInfo{..}
      scriptContext = P.ScriptContext{scriptContextPurpose = scriptPurposePlaceholder, ..}

  pairs <- for (M.toList validatorsEvalContext) \(name, validatorEvalContext) -> do
    exBudget <- calcValidatorExBudget evaluationContext scriptContext validatorEvalContext
    pure (name, exBudget)
  pure $ M.fromList pairs

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
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> Party
  -- ^ The contract creator.
  -> Integer
  -- ^ The initial lovelace for the creator's account.
  -> MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [Transaction]
  -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions requireContinuations creatorAddress minAda mc@MerkleizedContract{..} =
  do
    let ada = Token P.adaSymbol P.adaToken
        prune (Transaction s c i _) (Transaction s' c' i' _) = s == s' && c == c' && i == i'
    paths <- findPaths requireContinuations creatorAddress minAda mc
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
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [Transaction]
  -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions' requireContinuations mc@MerkleizedContract{..} =
  let utxoCostPerByte = 4310
      creatorAddress =
        Address True $
          P.Address
            (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
            (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      minAda = worstMinimumUtxo' utxoCostPerByte mcContract mcContinuations
   in findTransactions requireContinuations creatorAddress minAda mc

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
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> Party
  -- ^ The creator.
  -> Integer
  -- ^ The initial lovelace in the creator's account.
  -> MerkleizedContract
  -- ^ The bundle of contract information.
  -> m [(P.POSIXTime, [TransactionInput])]
  -- ^ The paths throught the Marlowe contract.
findPaths requireContinuations creatorAddress minAda MerkleizedContract{..} =
  do
    let ada = Token P.adaSymbol P.adaToken
        forever = 4_102_444_800_000 {- 1 Jan 2100 -}
        -- Add an initial deposit so that `getAllInputs` accounts for the initial state in its analysis.
        contract = When [Case (Deposit creatorAddress creatorAddress ada $ Constant minAda) mcContract] forever Close
    paths <-
      liftEither . first (fromString . show)
        =<< liftIO . getAllInputs
        =<< demerkleizeContract mcContinuations (deepDemerkleize requireContinuations contract)
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
