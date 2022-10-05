-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Analyze Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Analyze
  ( -- * Types
    ContractInstance(..)
    -- * Analysis
  , analyze
  ) where


import Codec.Serialise (serialise)
import Control.Monad (guard)
import Control.Monad.Except (MonadError(throwError), MonadIO(..), foldM, unless, when)
import Control.Monad.Reader (MonadReader, ReaderT(runReaderT))
import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Foldable (toList)
import Data.Generics.Multiplate (Multiplate(..), foldFor, preorderFold, purePlate)
import Data.List (nub, nubBy, (\\))
import Data.String (IsString(..))
import Data.Yaml (encode)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliIO, liftCliMaybe, queryInEra)
import Language.Marlowe.CLI.Merkle (deepDemerkleize, merkleizeInput)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError(CliError)
  , Continuations
  , MarloweTransaction(MarloweTransaction, mtContinuations, mtContract, mtInputs, mtPayments, mtRange, mtRoleValidator, mtRolesCurrency, mtSlotConfig, mtState, mtValidator)
  , SomeMarloweTransaction(SomeMarloweTransaction)
  , askEra
  , toMultiAssetSupportedInEra
  , withShelleyBasedEra
  )
import Language.Marlowe.Core.V1.Semantics
  ( MarloweData(MarloweData, marloweContract, marloweParams, marloweState)
  , MarloweParams(..)
  , Payment(Payment)
  , TransactionInput(..)
  , TransactionOutput(Error, TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings)
  , computeTransaction
  , totalBalance
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Deposit, Notify)
  , Case(..)
  , ChoiceId(ChoiceId)
  , Contract(..)
  , Input(MerkleizedInput)
  , InputContent(IChoice, IDeposit)
  , Observation(AndObs, ChoseSomething, NotObs, OrObs, ValueEQ, ValueGE, ValueGT, ValueLE, ValueLT)
  , Party(Address, Role)
  , Payee(Account, Party)
  , State(..)
  , Token(..)
  , Value(AddValue, AvailableMoney, ChoiceValue, Cond, DivValue, MulValue, NegValue, SubValue)
  , getInputContent
  )
import Language.Marlowe.FindInputs (getAllInputs)
import Language.Marlowe.Scripts
  (marloweTxInputsFromInputs, marloweValidator, marloweValidatorHash, rolePayoutValidatorHash)
import Numeric.Natural (Natural)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Data.ByteString.Char8 as BS8 (putStr)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.Functor.Constant as F (Constant(..))
import qualified Data.Map.Strict as M (foldr, lookup, null)
import qualified Data.Set as S (Set, empty, filter, map, member, singleton, size, union)
import qualified Ledger.Tx.CardanoAPI as P (toCardanoValue)
import qualified Ledger.Typed.Scripts as P (validatorScript)
import qualified Plutus.ApiCommon as P (LedgerPlutusVersion(PlutusV2), evaluateScriptCounting)
import qualified Plutus.Script.Utils.Scripts as P (datumHash)
import qualified Plutus.V1.Ledger.Ada as P (lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address as P (scriptHashAddress)
import qualified Plutus.V2.Ledger.Api as P hiding (evaluateScriptCounting)
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Prelude as P


-- | A bundle of information describing a contract instance.
data ContractInstance =
  ContractInstance
  {
    ciRolesCurrency :: P.CurrencySymbol  -- ^ The roles currency.
  , ciState         :: State             -- ^ The initial state of the contract.
  , ciContract      :: Contract          -- ^ The initial contract.
  , ciContinuations :: Continuations     -- ^ The merkleized continuations for the contract.
  }
    deriving Show


-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze :: MonadError CliError m
        => MonadIO m
        => MonadReader (CliEnv era) m
        => Api.LocalNodeConnectInfo Api.CardanoMode  -- ^ The connection info for the local node.
        -> FilePath                                  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
        -> Bool                                      -- ^ Whether to check preconditions for Marlowe state.
        -> Bool                                      -- ^ Whether to check lengths of role names.
        -> Bool                                      -- ^ Whether to check lengths of token names.
        -> Bool                                      -- ^ Whether to check the `maxValueSize` protocol limit.
        -> Bool                                      -- ^ Whether to check the `utxoCostPerWord` protocol limit.
        -> Bool                                      -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
        -> Bool                                      -- ^ Whether to compute tight estimates of worst-case bounds.
        -> m ()                                      -- ^ Print estimates of worst-case bounds.
analyze connection marloweFile preconditions roles tokens maximumValue minimumUtxo executionCost best =
  do
    SomeMarloweTransaction _language _era MarloweTransaction{..} <- decodeFileStrict marloweFile
    era <- askEra
    protocol@Api.ProtocolParameters{protocolParamMaxValueSize} <- queryInEra connection Api.QueryProtocolParameters
    let
      checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost
      ci = ContractInstance mtRolesCurrency mtState mtContract mtContinuations
    transactions <-
      if checkAll || executionCost || best && (maximumValue || minimumUtxo)
        then findTransactions ci
        else pure mempty
    let
      maybeTransactions = guard best >> pure transactions
    when (preconditions || checkAll)
      $ checkPreconditions ci
    when (roles || checkAll)
      $ checkRoles ci
    when (tokens || checkAll)
      $ checkTokens ci
    when (maximumValue || checkAll)
      $ checkMaximumValue protocolParamMaxValueSize maybeTransactions ci
    when (minimumUtxo || checkAll)
      $ withShelleyBasedEra era
      $ checkMinimumUtxo era protocol maybeTransactions ci
    when (executionCost || checkAll)
      $ checkExecutionCost protocol ci transactions


-- | Report invalid properties in the Marlowe state:
--
--   * Invalid roles currency.
--   * Invalid currency symbol or token name in an account.
--   * Account balances that are not positive.
--   * Duplicate accounts, choices, or bound values.
checkPreconditions :: MonadIO m
                   => ContractInstance  -- ^ The bundle of contract information.
                   -> m ()              -- ^ Action to print a report on preconditions.
checkPreconditions ContractInstance{ciRolesCurrency, ciState=State{..}} =
  let
    nonPositiveBalances = filter ((<= 0) . snd) . AM.toList
    duplicates (AM.keys -> x) = x \\ nub x
  in
    putYaml "Preconditions"
      [
        "Invalid roles currency"        .= invalidCurrency ciRolesCurrency
      , "Invalid account tokens"        .= filter invalidToken (snd <$> AM.keys accounts)
      , "Non-positive account balances" .= nonPositiveBalances accounts
      , "Duplicate accounts"            .= duplicates accounts
      , "Duplicate choices"             .= duplicates choices
      , "Duplicate bound values"        .= duplicates boundValues
      ]


-- | Detect an invalid currency symbol.
invalidCurrency :: P.CurrencySymbol -> Bool
invalidCurrency currency@P.CurrencySymbol{..} = currency /= P.adaSymbol && P.lengthOfByteString unCurrencySymbol /= 28


-- | Detect an invalid token.
invalidToken :: Token -> Bool
invalidToken (Token currency@P.CurrencySymbol{..} token@P.TokenName{..}) =
  not $  currency == P.adaSymbol
         && token == P.adaToken
      || P.lengthOfByteString unCurrencySymbol == 28
         && P.lengthOfByteString unTokenName <= 32


-- | Detect an invalid role name.
invalidRole :: P.TokenName -> Bool
invalidRole P.TokenName{..} = P.lengthOfByteString unTokenName > 32


-- | Check that all tokens are valid.
checkTokens :: MonadIO m
            => ContractInstance  -- ^ The bundle of contract information.
            -> m ()              -- ^ Action to print a report on token validity.
checkTokens ci =
  putYaml "Tokens"
    [
      "Invalid tokens" .= invalidToken `S.filter` extractFromContract ci
    ]


-- | Check that all roles are valid.
checkRoles :: MonadIO m
           => ContractInstance  -- ^ The bundle of contract information.
           -> m ()              -- ^ Action to print a report on role-name validity.
checkRoles ci =
  let
    roles = extractFromContract ci
  in
    putYaml "Role names"
      [
        "Invalid role names" .= invalidRole `S.filter` roles
      , "Blank role names" .= P.adaToken `S.member` roles
      ]


-- | Check that the protocol limit on maximum value in a UTxO is not violated.
checkMaximumValue :: MonadError CliError m
                  => MonadIO m
                  => Maybe Natural        -- ^ The `maxValue` protocol parameter.
                  -> Maybe [Transaction]  -- ^ The transactions to traverse.
                  -> ContractInstance     -- ^ The bundle of contract information.
                  -> m ()                 -- ^ Action to print a report on `maxValue` validity.
checkMaximumValue (Just maxValue) maybeTransactions ci =
  let
    size =
      case maybeTransactions of
        Just transactions -> let
                               measure (_, contract, _, _) = [computeValueSize $ extractAll contract]
                             in
                               maximum $ foldMap measure transactions
        Nothing           -> computeValueSize $ extractFromContract ci
  in
    putYaml "Maximum value"
      [
        "Actual" .= size
      , "Maximum" .= maxValue
      , "Unit" .= ("byte" :: String)
      , "Invalid" .= (size > fromEnum maxValue)
      , "Percentage" .= (100 * fromIntegral size / fromIntegral maxValue :: Double)
      ]
checkMaximumValue _ _ _ = throwError $ CliError "Missing `maxValue` protocol parameter."


-- | Check that the protocol limit on minimum UTxO is not violated.
checkMinimumUtxo :: forall era m
                 .  Api.IsShelleyBasedEra era
                 => MonadError CliError m
                 => MonadIO m
                 => Api.ScriptDataSupportedInEra era  -- ^ The era.
                 -> Api.ProtocolParameters            -- ^ The protocol parameters.
                 -> Maybe [Transaction]               -- ^ The transactions to traverse.
                 -> ContractInstance                  -- ^ The bundle of contract information.
                 -> m ()                              -- ^ Action to print a report on the `minimumUTxO` validity.
checkMinimumUtxo era protocol maybeTransactions ci =
  do
    let
      compute tokens =
        do
          value <- liftCli (P.toCardanoValue $ mconcat [P.singleton cs tn 1 | Token cs tn <- toList tokens])
          let
            address =
              Api.makeShelleyAddressInEra
                  Api.Mainnet
                  (Api.PaymentCredentialByScript "88888888888888888888888888888888888888888888888888888888")
                  (Api.StakeAddressByValue $ Api.StakeCredentialByKey "99999999999999999999999999999999999999999999999999999999")
            out =
              Api.TxOut
                address
                (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                (Api.TxOutDatumHash era "5555555555555555555555555555555555555555555555555555555555555555")
                Api.ReferenceScriptNone
          liftCli $ Api.calculateMinimumUTxO Api.shelleyBasedEra out protocol
    value <-
      case maybeTransactions of
        Just transactions -> let
                               measure (_, contract, _, _) = (: []) <$> compute (extractAll contract)
                             in
                               Api.lovelaceToValue . maximum . fmap Api.selectLovelace <$> foldTransactionsM measure transactions
        Nothing           -> compute $ extractFromContract ci
    putYaml "Minimum UTxO"
      [
        "Requirement" .= value
      ]


-- | Check that transactions satisfy the execution-cost protocol limits.
checkExecutionCost :: MonadError CliError m
                   => MonadIO m
                   => Api.ProtocolParameters  -- ^ The protocol parameters.
                   -> ContractInstance        -- ^ The bundle of contract information.
                   -> [Transaction]           -- ^ The transaction-paths through the contract.
                   -> m ()                    -- ^ Action to print a report on validity of transaction execution costs.
checkExecutionCost protocol ContractInstance{..} transactions =
  do
    Api.CostModel costModel <-
      liftCliMaybe "Plutus cost model not found."
        $ Api.AnyPlutusScriptVersion Api.PlutusScriptV2 `M.lookup` Api.protocolParamCostModels protocol
    evaluationContext <- liftCli $ P.mkEvaluationContext costModel
    let
      referenceAddress =
        P.Address
          (P.PubKeyCredential "77777777777777777777777777777777777777777777777777777777")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      creatorAddress =
        P.Address
          (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      referenceInput =
        P.TxInInfo
          (P.TxOutRef "6666666666666666666666666666666666666666666666666666666666666666" 1)
          (P.TxOut referenceAddress (P.lovelaceValueOf 1) P.NoOutputDatum (Just semanticsScriptHash))
      executor = executeTransaction evaluationContext referenceInput creatorAddress $ MarloweParams ciRolesCurrency
    (steps, memory) <-
      unzip
        . fmap (\P.ExBudget{..} -> (exBudgetCPU, exBudgetMemory))
        <$> mapM executor transactions
    let
      P.ExCPU actualSteps = maximum steps
      maximumSteps = maybe 0 fromEnum $ Api.executionSteps <$> Api.protocolParamMaxTxExUnits protocol
      P.ExMemory actualMemory = maximum memory
      maximumMemory = maybe 0 fromEnum $ Api.executionMemory <$> Api.protocolParamMaxTxExUnits protocol
    putYaml "Execution Cost"
      [
        "Steps" .= object
                   [
                     "Actual" .= actualSteps
                   , "Maximum" .= maximumSteps
                   , "Percentage" .= (100 * fromIntegral actualSteps / fromIntegral maximumSteps :: Double)
                   , "Invalid" .= (fromEnum actualSteps > maximumSteps)
                   ]
      , "Memory" .= object
                    [
                      "Actual" .= actualMemory
                    , "Maximum" .= maximumMemory
                    , "Percentage" .= (100 * fromIntegral actualMemory / fromIntegral maximumMemory :: Double)
                    , "Invalid" .= (fromEnum actualMemory > maximumMemory)
                    ]
      ]


-- | Execute a Marlowe transaction.
executeTransaction :: MonadError CliError m
                   => P.EvaluationContext  -- ^ The Plutus evaluation context.
                   -> P.TxInInfo           -- ^ The reference-script input, if any.
                   -> P.Address            -- ^ The public-key address executing the transaction.
                   -> MarloweParams        -- ^ The parameters for the Marlowe contract instance.
                   -> Transaction          -- ^ The transaction to be executed.
                   -> m P.ExBudget         -- ^ Action to execute the transaction and to return the new state and contract along with the execution cost.
executeTransaction evaluationContext referenceInput creatorAddress marloweParams@MarloweParams{..} (marloweState, marloweContract, TransactionInput{..}, output) =
  do
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e               -> throwError . CliError $ show e
    let
      oneLovelace = P.lovelaceValueOf 1
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
        pure
          $ P.TxOut
            payoutAddress
            (P.singleton currency name amount)
            (P.OutputDatumHash . P.datumHash . P.Datum $ P.toBuiltinData (rolesCurrency, role))
            Nothing
      makePayment _ = mempty
      makePaymentDatum (Payment _ (Party (Role role)) _ _) =
        pure . P.Datum $ P.toBuiltinData (rolesCurrency, role)
      makePaymentDatum _ = mempty
      outPayments = concatMap makePayment txOutPayments
      outPaymentDatums = concatMap makePaymentDatum txOutPayments
      findRole :: InputContent -> [P.TokenName]
      findRole (IDeposit _ (Role role) _ _) = pure role
      findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
      findRole _ = mempty
      roles = fmap (flip (P.singleton rolesCurrency) 1) . foldMap findRole $ getInputContent <$> txInputs
      inRoles =
        uncurry (P.TxInInfo . P.TxOutRef "1111111111111111111111111111111111111111111111111111111111111111")
          <$> zip [1..] outRoles
      outRoles = [P.TxOut creatorAddress role P.NoOutputDatum Nothing | role <- roles]
      merkleDatums =
        concat
          [
            case input of
              MerkleizedInput _ _ contract -> pure . P.Datum $ P.toBuiltinData contract
              _                              -> mempty
          |
            input <- txInputs
          ]
      outMerkle =
        [
          P.TxOut creatorAddress oneLovelace (P.OutputDatumHash $ P.datumHash datum) Nothing
        |
          datum <- merkleDatums
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
      txInfoReferenceInputs = [referenceInput]
      txInfoOutputs =
           outChange
        :  outScriptTx
        <> outPayments
        <> outRoles
        <> outMerkle
      txInfoFee = oneLovelace
      txInfoMint = mempty
      txInfoDCert = mempty
      txInfoWdrl = AM.empty
      txInfoValidRange = P.Interval (P.LowerBound (P.Finite $ fst txInterval) True) (P.UpperBound (P.Finite $ snd txInterval) True)
      txInfoSignatories =
        case creatorAddress of
          P.Address (P.PubKeyCredential pkh) _ -> pure pkh
          _                                    -> mempty
      txInfoRedeemers = AM.singleton scriptContextPurpose redeemer
      txInfoData = AM.fromList $ ((,) =<< P.datumHash) <$> inDatum : outDatum <> outPaymentDatums <> merkleDatums
      txInfoId = "2222222222222222222222222222222222222222222222222222222222222222"
      scriptContextTxInfo = P.TxInfo{..}
      scriptContextPurpose = P.Spending inScriptTxRef
      scriptContext = P.ScriptContext{..}
    case evaluateSemantics evaluationContext (P.toData inDatum) (P.toData redeemer) (P.toData scriptContext) of
      (_, Right budget) -> pure budget
      (msg, Left err)   -> throwError . CliError $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."


-- | Compute the size of a multi-asset value.
computeValueSize :: S.Set Token  -- ^ The tokens present.
                 -> Int          -- ^ The number of bytes on the ledger.
computeValueSize tokens =
  let
    nTokens = S.size tokens
    nPolicies = S.size $ S.map (\(Token c _) -> c) tokens
    nNames = sum . fmap P.lengthOfByteString . toList $ S.map (\(Token _ (P.TokenName n)) -> n) tokens
    padWords x = (x + 7) `div` 8
  in
    8 * (6 + padWords (12 * nTokens + 28 * nPolicies + fromInteger nNames))


-- | A mutltiplate for a Marlowe contract.
data MarlowePlate f =
  MarlowePlate
  {
    contractPlate :: Contract -> f Contract
  , casePlate :: Case Contract -> f (Case Contract)
  , actionPlate :: Action -> f Action
  , valuePlate :: Value Observation -> f (Value Observation)
  , observationPlate :: Observation -> f Observation
  }

instance Multiplate MarlowePlate where
  multiplate child =
    let
      buildContract Close = pure Close
      buildContract (Pay a p t v c) = Pay a p t <$> valuePlate child v <*> contractPlate child c
      buildContract (If o c c') = If <$> observationPlate child o <*> contractPlate child c <*> contractPlate child c'
      buildContract (When cs t c) = When <$> sequenceA (casePlate child <$> cs) <*> pure t <*> contractPlate child c
      buildContract (Let i v c) = Let i <$> valuePlate child v <*> contractPlate child c
      buildContract (Assert o c) = Assert <$> observationPlate child o <*> contractPlate child c
      buildCase (Case a c) = Case <$> actionPlate child a <*> contractPlate child c
      buildCase (MerkleizedCase a h) = MerkleizedCase <$> actionPlate child a <*> pure h
      buildAction (Deposit a p t v) = Deposit a p t <$> valuePlate child v
      buildAction (Notify o) = Notify <$> observationPlate child o
      buildAction x = pure x
      buildValue (NegValue x) = NegValue <$> valuePlate child x
      buildValue (AddValue x y) = AddValue <$> valuePlate child x <*> valuePlate child y
      buildValue (SubValue x y) = SubValue <$> valuePlate child x <*> valuePlate child y
      buildValue (MulValue x y) = MulValue <$> valuePlate child x <*> valuePlate child y
      buildValue (DivValue x y) = DivValue <$> valuePlate child x <*> valuePlate child y
      buildValue (Cond o x y) = Cond <$> observationPlate child o <*> valuePlate child x <*> valuePlate child y
      buildValue x = pure x
      buildObservation (AndObs x y) = AndObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (OrObs x y) = OrObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (NotObs x) = NotObs <$> observationPlate child x
      buildObservation (ValueGE x y) = ValueGE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueGT x y) = ValueGT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLT x y) = ValueLT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLE x y) = ValueLE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueEQ x y) = ValueEQ <$> valuePlate child x <*> valuePlate child y
      buildObservation x = pure x
    in
      MarlowePlate
        buildContract
        buildCase
        buildAction
        buildValue
        buildObservation
  mkPlate build =
    MarlowePlate
      (build contractPlate)
      (build casePlate)
      (build actionPlate)
      (build valuePlate)
      (build observationPlate)


-- | Extract something using the Marlowe multiplate.
class Extract a where
  -- | Shallow extraction.
  extractor :: MarlowePlate (F.Constant (S.Set a))
  -- | Deep extraction.
  extractAll :: Ord a => Contract -> S.Set a
  extractAll = foldFor contractPlate $ preorderFold extractor

instance Extract Token where
  extractor =
    let
      single = F.Constant . S.singleton
      contractPlate' (Pay _ _ t _ _) = single t
      contractPlate' x = pure x
      actionPlate' (Deposit _ _ t _) = single t
      actionPlate' x = pure x
      valuePlate' (AvailableMoney _ t) = single t
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      }

instance Extract P.TokenName where
  extractor =
    let
      role (Role r) = S.singleton r
      role _ = S.empty
      contractPlate' (Pay a (Account p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' (Pay a (Party p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' x = pure x
      actionPlate' (Deposit a p _ _) = F.Constant $ role a <> role p
      actionPlate' x = pure x
      valuePlate' (AvailableMoney a _) = F.Constant $ role a
      valuePlate' (ChoiceValue (ChoiceId _ p)) = F.Constant $ role p
      valuePlate' x = pure x
      observationPlate' (ChoseSomething (ChoiceId _ p)) = F.Constant $ role p
      observationPlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      , observationPlate = observationPlate'
      }



-- | Extract something from a Marlowe contract.
extractFromContract :: Extract a
                    => Ord a
                    => ContractInstance  -- ^ The bundle of contract information.
                    -> S.Set a           -- ^ The extract.
extractFromContract ContractInstance{..} =
  M.foldr (S.union . extractAll) (extractAll ciContract) ciContinuations


-- | Format results of checking as YAML.
putYaml :: MonadIO m
        => String     -- ^ The section name.
        -> [Pair]     -- ^ The results.
        -> m ()       -- ^ Action to print the results as YAML.
putYaml section = liftIO . BS8.putStr . encode . object . (: []) . (fromString section .=) . object


-- | Visit transactions along all execution paths of a a contract.
foldTransactionsM :: Monad m
                  => Monoid a
                  => (Transaction -> m a)  -- ^ Function to collect results.
                  -> [Transaction]         -- ^ The transactions.
                  -> m a                   -- ^ The collected results.
foldTransactionsM f =
  (foldM $ (. f) . fmap . (<>)) mempty


-- | Complete information about a Marlowe semantics transaction.
type Transaction = (State, Contract, TransactionInput, TransactionOutput)


-- | Find transactions along all execution paths of a contract.
findTransactions :: MonadError CliError m
                 => MonadIO m
                 => ContractInstance                                            -- ^ The bundle of contract information.
                 -> m [Transaction]  -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions ci@ContractInstance{..} =
  do
    let
      creatorAddress =
        P.Address
          (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      prune (s, c, i, _) (s', c', i', _) = s == s' && c == c' && i == i'
    paths <- findPaths ci
    nubBy prune
      . concat
      <$> sequence
      [
         findTransactionPath ciContinuations state ciContract inputs
      |
        (minTime, inputs) <- paths
      , let state = State (AM.singleton (Address True creatorAddress, Token P.adaSymbol P.adaToken) 1) AM.empty AM.empty minTime
      ]


-- | Find the transactions corresponding to a path of demerkleized inputs.
findTransactionPath :: MonadError CliError m
                    => Continuations                                               -- ^ The continuations of the contact.
                    -> State                                                       -- ^ The initial contract.
                    -> Contract                                                    -- ^ The initial state.
                    -> [TransactionInput]                                          -- ^ The path of demerkleized inputs.
                    -> m [(State, Contract, TransactionInput, TransactionOutput)]  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransactionPath continuations state contract =
  let
    go ((state', contract'), previous) input =
      do
        (input', output) <- findTransaction continuations state' contract' input
        case output of
          TransactionOutput{..} -> pure ((txOutState, txOutContract), (state', contract', input', output) : previous)
          Error e               -> throwError . CliError $ show e
  in
    fmap snd . foldM go ((state, contract), [])


-- | Find the transaction corresponding to demerkleized input.
findTransaction :: MonadError CliError m
                => Continuations                            -- ^ The continuations of the contact.
                -> State                                    -- ^ The initial contract.
                -> Contract                                 -- ^ The initial state.
                -> TransactionInput                         -- ^ The demerkleized input to the contract.
                -> m (TransactionInput, TransactionOutput)  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransaction continuations state contract TransactionInput{..} =
  do
    input <- TransactionInput txInterval . snd <$> foldM (merkleizeInput txInterval continuations) ((state, contract), []) txInputs
    pure (input, computeTransaction input state contract)


-- | Find all of the paths through a Marlowe contract.
findPaths :: MonadError CliError m
          => MonadIO m
          => ContractInstance                       -- ^ The bundle of contract information.
          -> m [(P.POSIXTime, [TransactionInput])]  -- ^ The paths throught the Marlowe contract.
findPaths ContractInstance{..} =
  do
    unless (M.null ciContinuations)
      . liftIO $ hPutStrLn stderr "Demerkleizing contract . . ."
    contract <- runReaderT (deepDemerkleize ciContract) ciContinuations
    liftIO $ hPutStrLn stderr "Starting search for execution paths . . ."
    paths <- liftCliIO $ getAllInputs contract
    liftIO $ hPutStrLn stderr $ " . . . found " <> (show $ length paths) <> " execution paths."
    pure paths


-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics :: P.EvaluationContext                                 -- ^ The evaluation context.
                  -> P.Data                                              -- ^ The datum.
                  -> P.Data                                              -- ^ The redeemer.
                  -> P.Data                                              -- ^ The script context.
                  -> (P.LogOutput, Either P.EvaluationError P.ExBudget)  -- ^ The result.
evaluateSemantics evaluationContext datum redeemer context =
  P.evaluateScriptCounting
    P.PlutusV2 (P.ProtocolVersion 7 0)
    P.Verbose evaluationContext
    serialiseSemanticsValidator [datum, redeemer, context]


-- | Serialize the Marlowe semantics validator.
serialiseSemanticsValidator :: SBS.ShortByteString
serialiseSemanticsValidator =
    SBS.toShort
  . LBS.toStrict
  . serialise
  . P.getValidator
  . P.validatorScript
  $ marloweValidator


-- | Compute the address of the Marlowe semantics validator.
semanticsAddress :: P.Address
semanticsAddress =
  P.Address
    (P.ScriptCredential semanticsValidatorHash)
    (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")


-- | Compute the hash of the Marlowe semantics validator.
semanticsScriptHash :: P.ScriptHash
semanticsScriptHash =
  let
    P.ValidatorHash bytes = marloweValidatorHash
  in
    P.ScriptHash bytes


-- | Compute the hash of the Marlowe semantics validator.
semanticsValidatorHash :: P.ValidatorHash
semanticsValidatorHash = marloweValidatorHash


-- | Compute the address of the Marlowe payout validator.
payoutAddress :: P.Address
payoutAddress = P.scriptHashAddress rolePayoutValidatorHash
