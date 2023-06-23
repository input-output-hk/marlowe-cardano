{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Transaction.Safety (
  Continuations,
  checkContract,
  checkTransactions,
  makeSystemHistory,
  minAdaUpperBound,
  noContinuations,
) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Bifunctor (bimap, first)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.SOP.Strict (K (..), NP (..))
import Data.String (fromString)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Marlowe.Analysis.Safety.Ledger (
  checkAddresses,
  checkContinuations,
  checkNetwork,
  checkNetworks,
  checkRoleNames,
  checkTokens,
  worstMinimumUtxo',
 )
import Language.Marlowe.Analysis.Safety.Transaction (findTransactions)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..), Transaction (..))
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  MarloweTransactionMetadata (..),
  MarloweVersion (MarloweV1),
  TransactionScriptOutput (..),
 )
import Language.Marlowe.Runtime.Transaction.Api (Mint (..), RoleTokensConfig (..))
import Language.Marlowe.Runtime.Transaction.BuildConstraints (buildApplyInputsConstraints)
import Language.Marlowe.Runtime.Transaction.Constraints (
  MarloweContext (..),
  MarloweOutputConstraints (..),
  RoleTokenConstraints (..),
  SolveConstraints,
  TxConstraints (..),
  WalletContext (..),
 )

import qualified Cardano.Api as Cardano (Lovelace, NetworkId (Mainnet))
import qualified Cardano.Api.Shelley as Shelley (
  CardanoMode,
  ConsensusMode (..),
  EraHistory (..),
  ProtocolParameters (ProtocolParameters, protocolParamUTxOCostPerByte),
  SystemStart (..),
 )
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as M (Map, elems, empty, fromList, keys, map, mapKeys, singleton, size)
import qualified Data.Set as S (singleton)
import qualified Language.Marlowe.Core.V1.Merkle as V1 (MerkleizedContract (..))
import qualified Language.Marlowe.Core.V1.Plate as V1 (extractAllWithContinuations)
import qualified Language.Marlowe.Core.V1.Semantics as V1 (
  MarloweData (..),
  MarloweParams (..),
  TransactionInput (..),
  TransactionOutput (..),
 )
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1 (Party (Address), State (..), Token (..))
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1 (deserialiseAddress)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (
  Address (..),
  AssetId (..),
  Assets (..),
  DatumHash (..),
  PolicyId (..),
  SlotNo (..),
  TokenName (..),
  Tokens (..),
  TransactionMetadata (..),
  TransactionOutput (..),
  TxOutRef,
  UTxOs (..),
 )
import qualified Ouroboros.Consensus.BlockchainTime as Ouroboros (RelativeTime (..), mkSlotLength, toRelativeTime)
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  interpretQuery,
  mkInterpreter,
  summaryWithExactly,
  wallclockToSlot,
 )
import qualified Ouroboros.Consensus.Util.Counting as Ouroboros (Exactly (..))
import qualified Ouroboros.Network.Block as Ouroboros (SlotNo (..))
import qualified Plutus.V2.Ledger.Api as Plutus (
  CurrencySymbol (..),
  DatumHash (..),
  POSIXTime (..),
  TokenName (..),
  fromBuiltin,
  toBuiltin,
 )
import qualified PlutusTx.AssocMap as AM (toList)

-- FIXME: Relocate this definition when full support for Merkleization is added to Runtime.
type Continuations v = M.Map Chain.DatumHash (Contract v)

-- | No continuations for a contract.
noContinuations
  :: MarloweVersion v
  -> Continuations v
noContinuations MarloweV1 = M.empty

-- | Map Plutus continuations into chain-sync continuations.
remapContinuations
  :: M.Map Chain.DatumHash contract
  -> M.Map Plutus.DatumHash contract
remapContinuations = M.mapKeys $ Plutus.DatumHash . Plutus.toBuiltin . Chain.unDatumHash

-- | Compute a worst-case bound on the minimum UTxO value for a contract, assuming that the contract does not pay from the account in the initial state and that account only contains lovelace.
minAdaUpperBound
  :: Shelley.ProtocolParameters
  -> MarloweVersion v
  -> Contract v
  -> Continuations v
  -> Maybe Cardano.Lovelace
minAdaUpperBound Shelley.ProtocolParameters{protocolParamUTxOCostPerByte} MarloweV1 contract continuations =
  fromInteger
    . (\utxoCostPerByte -> worstMinimumUtxo' (toInteger utxoCostPerByte) contract $ remapContinuations continuations)
    <$> protocolParamUTxOCostPerByte

-- | Check a contract for design errors and ledger violations.
checkContract
  :: Cardano.NetworkId
  -> RoleTokensConfig
  -> MarloweVersion v
  -> Contract v
  -> Continuations v
  -> [SafetyError]
checkContract network config MarloweV1 contract continuations =
  let continuations' = remapContinuations continuations
      roles = toList $ V1.extractAllWithContinuations contract continuations'
      mintCheck =
        case (config, null roles) of
          (RoleTokensNone, False) -> pure MissingRolesCurrency
          (RoleTokensNone, True) -> mempty
          (_, True) -> pure ContractHasNoRoles
          (RoleTokensMint mint, False) ->
            let minted = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName <$> M.keys (unMint mint)
                missing = MissingRoleToken <$> filter (`notElem` minted) roles
                extra = ExtraRoleToken <$> filter (`notElem` roles) minted
             in missing <> extra
          _ -> mempty
      avoidDuplicateReport = True
      nameCheck = checkRoleNames avoidDuplicateReport Nothing contract continuations'
      tokenCheck = checkTokens Nothing contract continuations'
      continuationCheck = checkContinuations contract continuations'
      networksCheck =
        checkNetwork (network == Cardano.Mainnet) Nothing contract continuations'
          <> snd (checkNetworks Nothing contract continuations')
      addressCheck = checkAddresses Nothing contract continuations'
   in mintCheck <> nameCheck <> tokenCheck <> continuationCheck <> networksCheck <> addressCheck

-- | Mock-execute all possible transactions for a contract.
checkTransactions
  :: (MonadIO m)
  => SolveConstraints
  -> MarloweVersion v
  -> MarloweContext v
  -> Chain.PolicyId
  -> Chain.Address
  -> Integer
  -> Contract v
  -> Continuations v
  -> m (Either String [SafetyError])
checkTransactions solveConstraints version@MarloweV1 marloweContext rolesCurrency changeAddress minAda contract continuations =
  runExceptT $
    do
      let changeAddress' = uncurry V1.Address . fromJust . V1.deserialiseAddress $ Chain.unAddress changeAddress
      -- FIXME: The `findTransactions` function may be a long-running process if the
      --        contract is complex. Where should we guard against this with a timeout?
      transactions <-
        findTransactions changeAddress' minAda . V1.MerkleizedContract contract $ remapContinuations continuations
      either throwE (pure . mconcat)
        . forM transactions
        $ checkTransaction solveConstraints version marloweContext rolesCurrency changeAddress

-- | Check a transaction for safety issues.
checkTransaction
  :: SolveConstraints
  -> MarloweVersion v
  -> MarloweContext v
  -> Chain.PolicyId
  -> Chain.Address
  -> Transaction
  -> Either String [SafetyError]
checkTransaction solveConstraints version@MarloweV1 marloweContext@MarloweContext{..} (Chain.PolicyId rolesCurrency) changeAddress transaction@Transaction{..} =
  do
    let V1.TransactionInput{..} = txInput
        rolesCurrency' = V1.MarloweParams . Plutus.CurrencySymbol $ Plutus.toBuiltin rolesCurrency
        -- Truncate updward to slot number.
        earliest = posixTimeToUTCTime . (* 1000) . (`div` 1000) . (+ 999) . V1.minTime $ V1.marloweState marloweData
        -- Truncate inward to the slot number (inclusive).
        begin = posixTimeToUTCTime . (* 1000) . (`div` 1000) . (+ 999) $ fst txInterval
        -- Truncate inward to the slot number (exclusive).
        end = posixTimeToUTCTime . (* 1000) . (+ 2) . (`div` 1000) . (+ (-999)) $ snd txInterval
        -- Find the current time and slot-compatible interval.
        (now, intervalBegin, intervalEnd)
          -- For a timeout, the current time must not be before the timeout.
          | null txInputs =
              ( begin
              , Just begin
              , Just $ maximum [addUTCTime 1 begin, end]
              )
          -- For application of input, the current time must not be before the minimum time.
          | otherwise =
              ( maximum [begin, earliest]
              , Just $ minimum [begin, addUTCTime (-1) end]
              , Just $ maximum [end, addUTCTime 1 earliest]
              )
        marloweData = V1.MarloweData rolesCurrency' txState txContract
        scriptIncoming = foldMap (uncurry makeValue . first snd) . AM.toList . V1.accounts $ V1.marloweState marloweData
        marloweOutput = TransactionScriptOutput marloweAddress scriptIncoming scriptTxOutRef marloweData
        marloweContext' = marloweContext{scriptOutput = Just marloweOutput}
        metadata = MarloweTransactionMetadata Nothing $ Chain.TransactionMetadata mempty
        (start, history) =
          makeSystemHistory . posixTimeToUTCTime $ Plutus.POSIXTime 0
    tipSlot <-
      utcTimeToSlotNo start history now
    constraints <-
      bimap show snd $
        runIdentity $
          runExceptT $
            buildApplyInputsConstraints
              (const $ pure Nothing)
              start
              history
              version
              marloweOutput
              tipSlot
              metadata
              intervalBegin
              intervalEnd
              txInputs
    let walletContext = walletForConstraints version marloweContext changeAddress constraints
    pure
      . either (pure . TransactionValidationError transaction . show) (const $ TransactionWarning <$> V1.txOutWarnings txOutput)
      $ solveConstraints version marloweContext' walletContext constraints

-- | Create a wallet context that will satisfy the given constraints.
walletForConstraints
  :: MarloweVersion v
  -> MarloweContext v
  -> Chain.Address
  -> TxConstraints v
  -> WalletContext
walletForConstraints MarloweV1 MarloweContext{scriptOutput} changeAddress TxConstraints{..} =
  let padValue assets@(Chain.Assets _ (Chain.Tokens tokens)) = assets <> makeLovelace (toInteger $ 2_000_000 * M.size tokens)
      scriptIncoming = maybe (makeLovelace 0) (\(TransactionScriptOutput _ assets _ _) -> assets) scriptOutput
      scriptOutgoing =
        case marloweOutputConstraints of
          MarloweOutput assets _ -> assets
          _ -> mempty
      roles =
        case roleTokenConstraints of
          SpendRoleTokens assets -> foldMap (Chain.Assets 0 . Chain.Tokens . flip M.singleton 1) assets
          _ -> mempty
      payments = mconcat $ M.elems payToAddresses <> M.elems payToRoles
      requiredValue = roles <> payments <> scriptOutgoing <> negateAssets scriptIncoming
      bufferValue = makeLovelace 50_000_000 -- Generously cover min-UTxO and fee.
      workaround = True
      collateralUtxos = S.singleton collateralTxOutRef
      availableUtxos =
        Chain.UTxOs $
          M.fromList $
            if workaround -- FIXME: Workaround for coin-selection bug.
              then
                [ (collateralTxOutRef, Chain.TransactionOutput changeAddress bufferValue Nothing Nothing)
                , (fundingTxOutRef 0, Chain.TransactionOutput changeAddress (padValue roles) Nothing Nothing)
                , (fundingTxOutRef 1, Chain.TransactionOutput changeAddress (padValue payments) Nothing Nothing)
                ,
                  ( fundingTxOutRef 2
                  , Chain.TransactionOutput changeAddress (padValue $ scriptOutgoing <> negateAssets scriptIncoming) Nothing Nothing
                  )
                ]
              else
                [ (collateralTxOutRef, Chain.TransactionOutput changeAddress bufferValue Nothing Nothing)
                , (fundingTxOutRef 0, Chain.TransactionOutput changeAddress (padValue requiredValue) Nothing Nothing)
                ]
   in WalletContext{..}

-- | Negate asset values.
negateAssets
  :: Chain.Assets
  -> Chain.Assets
negateAssets Chain.Assets{..} =
  Chain.Assets
    { ada = negate ada
    , tokens = Chain.Tokens . M.map negate $ Chain.unTokens tokens
    }

-- | Convert an integer to lovelace.
makeLovelace
  :: Integer
  -> Chain.Assets
makeLovelace quantity = Chain.Assets (fromInteger quantity) $ Chain.Tokens M.empty

-- | Make a given quantity of token.
makeValue
  :: V1.Token
  -> Integer
  -> Chain.Assets
makeValue (V1.Token "" "") quantity = makeLovelace quantity
makeValue (V1.Token (Plutus.CurrencySymbol p) (Plutus.TokenName n)) quantity =
  Chain.Assets 0 . Chain.Tokens $
    M.singleton
      (Chain.AssetId (Chain.PolicyId $ Plutus.fromBuiltin p) (Chain.TokenName $ Plutus.fromBuiltin n))
      (fromInteger quantity)

-- | Convert UTC time to a slot number.
utcTimeToSlotNo
  :: Shelley.SystemStart
  -> Shelley.EraHistory Shelley.CardanoMode
  -> UTCTime
  -> Either String Chain.SlotNo
utcTimeToSlotNo systemStart (Shelley.EraHistory _ interpreter) time =
  do
    (slotNo, _, _) <-
      first show
        . Ouroboros.interpretQuery interpreter
        . Ouroboros.wallclockToSlot
        $ Ouroboros.toRelativeTime systemStart time
    pure . Chain.SlotNo $ Ouroboros.unSlotNo slotNo

-- | Convert POSIX time to UTC time.
posixTimeToUTCTime
  :: Plutus.POSIXTime
  -> UTCTime
posixTimeToUTCTime (Plutus.POSIXTime t) = posixSecondsToUTCTime . secondsToNominalDiffTime $ fromInteger t / 1000

-- | Make a system start and error history for the given start time.
makeSystemHistory
  :: UTCTime
  -> (Shelley.SystemStart, Shelley.EraHistory Shelley.CardanoMode)
makeSystemHistory time =
  let systemStart = Shelley.SystemStart $ addUTCTime (-1000) time
      eraParams =
        Ouroboros.EraParams
          { eraEpochSize = 1
          , eraSlotLength = Ouroboros.mkSlotLength 1
          , eraSafeZone = Ouroboros.UnsafeIndefiniteSafeZone
          }
      oneSecondBound i =
        Ouroboros.Bound
          { boundTime = Ouroboros.RelativeTime $ fromInteger i
          , boundSlot = fromInteger i
          , boundEpoch = fromInteger i
          }
      oneSecondEraSummary i =
        Ouroboros.EraSummary
          { eraStart = oneSecondBound i
          , eraEnd = Ouroboros.EraEnd . oneSecondBound $ i + 1
          , eraParams
          }
      unboundedEraSummary i =
        Ouroboros.EraSummary
          { eraStart = oneSecondBound i
          , eraEnd = Ouroboros.EraUnbounded
          , eraParams
          }
      eraHistory =
        Shelley.EraHistory Shelley.CardanoMode
          . Ouroboros.mkInterpreter
          . Ouroboros.summaryWithExactly
          . Ouroboros.Exactly
          $ K (oneSecondEraSummary 0) -- Byron lasted 1 second
            :* K (oneSecondEraSummary 1) -- Shelley lasted 1 second
            :* K (oneSecondEraSummary 2) -- Allegra lasted 1 second
            :* K (oneSecondEraSummary 3) -- Mary lasted 1 second
            :* K (oneSecondEraSummary 4) -- Alonzo lasted 1 second
            :* K (unboundedEraSummary 5) -- Babbage never ends
            :* Nil
   in (systemStart, eraHistory)

-- | A dummy collateral UTxO.
collateralTxOutRef :: Chain.TxOutRef
collateralTxOutRef = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#0"

-- | A dummy script reference.
scriptTxOutRef :: Chain.TxOutRef
scriptTxOutRef = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#1"

-- | A dummy TxOut reference.
fundingTxOutRef :: Int -> Chain.TxOutRef
fundingTxOutRef = fromString . ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#" <>) . show . (2 +)
