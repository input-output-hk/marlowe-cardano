{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Safety (
  Continuations,
  checkContract,
  checkTransactions,
  makeSystemHistory,
  minAdaUpperBound,
  mkAdjustMinimumUtxo,
  noContinuations,
) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Bifunctor (bimap, first)
import Data.Maybe (fromJust)
import Data.SOP.Strict (NP (..))
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
  worstValue,
 )
import Language.Marlowe.Analysis.Safety.Transaction (
  CurrentState (NotInitialized),
  findTransactions,
  firstRoleAuthorizationAnnotator,
 )
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..), Transaction (..), stripAnnotation)
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  MarloweTransactionMetadata (..),
  MarloweVersion (MarloweV1),
  TransactionScriptOutput (..),
 )
import Language.Marlowe.Runtime.Transaction.Api (Mint (..), RoleTokensConfig (..))
import Language.Marlowe.Runtime.Transaction.BuildConstraints (buildApplyInputsConstraints, safeLovelace)
import Language.Marlowe.Runtime.Transaction.Constraints (
  HelperScriptInfo (helperAddress),
  HelperScriptState (..),
  HelpersContext (..),
  MarloweContext (..),
  MarloweOutputConstraints (..),
  RoleTokenConstraints (..),
  TxConstraints (..),
  WalletContext (..),
  mkTxOutValue,
  solveConstraints,
 )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Api.Shelley as Shelley (
  EraHistory (..),
  SystemStart (..),
 )
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as M (
  Map,
  elems,
  empty,
  fromList,
  fromSet,
  intersection,
  keysSet,
  map,
  mapKeys,
  singleton,
  size,
  toList,
 )
import Data.SOP.BasicFunctors (K (..))
import qualified Data.SOP.Counting as Ouroboros
import qualified Data.Set as S (Set, intersection, map, singleton)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
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
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain (
  assetsToCardanoValue,
  fromCardanoDatumHash,
  toCardanoScriptData,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (
  Address (..),
  AssetId (..),
  Assets (..),
  Datum (B),
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
import qualified Language.Marlowe.Runtime.Plutus.V2.Api as Chain (
  fromPlutusTokenName,
  fromPlutusValue,
  toPlutusCurrencySymbol,
  toPlutusTokenName,
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
import qualified Ouroboros.Network.Block as Ouroboros (SlotNo (..))
import qualified PlutusLedgerApi.V2 as Plutus (
  CurrencySymbol (..),
  DatumHash (..),
  POSIXTime (..),
  TokenName (..),
  fromBuiltin,
  toBuiltin,
 )
import qualified PlutusTx.AssocMap as AM (fromList, toList)

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

-- | Compute a worst-case bound on the minimum UTxO value for a contract, assuming that the contract does not pay
--   from the account in the initial state and that account only contains lovelace. Assume that a datum hash is
--   present in the TxOut and that the address contains a stake credential.
minAdaUpperBound
  :: forall era v
   . C.BabbageEraOnwards era
  -> C.LedgerProtocolParameters era
  -> MarloweVersion v
  -> V1.State
  -> Contract v
  -> Continuations v
  -> Maybe Cardano.Lovelace
minAdaUpperBound era pps MarloweV1 state contract continuations =
  minAdaBound era pps MarloweV1
    . Chain.fromPlutusValue
    . worstValue (Just state) contract
    $ remapContinuations continuations

-- | Adjust the ada in a TxOut upwards, if needed to satisfy the minimum UTxO ledger rule. Assume that a datum hash is
-- present in the TxOut and that the address contains a stake credential.
mkAdjustMinimumUtxo
  :: forall era v
   . C.BabbageEraOnwards era
  -> C.LedgerProtocolParameters era
  -> MarloweVersion v
  -> Chain.Assets
  -> Chain.Assets
mkAdjustMinimumUtxo era pps MarloweV1 assets@Chain.Assets{ada, tokens} =
  let minLovelace =
        maybe 30_000_000 toInteger $ -- Safe for all UTxOs.
          minAdaBound era pps MarloweV1 assets
   in if minLovelace > toInteger ada
        then Chain.Assets (fromInteger minLovelace) tokens
        else assets

-- | Compute a worst-case bound on the minimum TxOut value for assets. Assume that a datum hash is present in the
--   TxOut and that the address contains a stake credential.
minAdaBound
  :: forall era v
   . C.BabbageEraOnwards era
  -> C.LedgerProtocolParameters era
  -> MarloweVersion v
  -> Chain.Assets
  -> Maybe Cardano.Lovelace
minAdaBound era pps MarloweV1 assets =
  do
    let -- Ugh, the era handling can be done much better, but this is how it's done elsewhere in the codebase.
        maryEraOnwards :: C.MaryEraOnwards era
        maryEraOnwards = C.babbageEraOnwardsToMaryEraOnwards era
        alonzoEraOnwards :: C.AlonzoEraOnwards era
        alonzoEraOnwards = C.babbageEraOnwardsToAlonzoEraOnwards era
        shelleyBasedEra = C.babbageEraOnwardsToShelleyBasedEra era
    address <-
      case era of
        C.BabbageEraOnwardsBabbage ->
          C.deserialiseAddress
            (C.AsAddressInEra C.AsBabbageEra)
            "addr1x8ur42seq20ytdzm33fhjf3c2pxskxf3gzxq706qk7p5muhc824pjq57gk69hrzn0ynrs5zdpvvnzsyvpul5pdurfheq28w2dx"
        C.BabbageEraOnwardsConway ->
          C.deserialiseAddress
            (C.AsAddressInEra C.AsConwayEra)
            "addr1x8ur42seq20ytdzm33fhjf3c2pxskxf3gzxq706qk7p5muhc824pjq57gk69hrzn0ynrs5zdpvvnzsyvpul5pdurfheq28w2dx"
    value <- Chain.assetsToCardanoValue assets
    let txOut =
          Cardano.TxOut
            address
            (mkTxOutValue maryEraOnwards value)
            (Cardano.TxOutDatumHash alonzoEraOnwards "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0")
            C.ReferenceScriptNone
    pure $ Cardano.calculateMinimumUTxO shelleyBasedEra txOut $ C.unLedgerProtocolParameters pps

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
      roles = V1.extractAllWithContinuations contract continuations'
      mintCheck =
        case (config, Set.null roles) of
          (RoleTokensNone, False) -> pure MissingRolesCurrency
          (RoleTokensNone, True) -> mempty
          (_, True) -> pure ContractHasNoRoles
          (RoleTokensMint mint, False) ->
            let minted = Set.map Chain.toPlutusTokenName $ NESet.toSet $ NEMap.keysSet $ unMint mint
                missing = MissingRoleToken <$> Set.toList (Set.difference roles minted)
                extra = ExtraRoleToken <$> Set.toList (Set.difference minted roles)
             in missing <> extra
          (RoleTokensUsePolicy _ distribution, False) -> do
            let distributedRoles = Set.map Chain.toPlutusTokenName $ M.keysSet distribution
            extraRole <- Set.toList $ Set.difference distributedRoles roles
            pure $ ExtraRoleToken extraRole
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
  => C.LedgerProtocolParameters era
  -> C.BabbageEraOnwards era
  -> MarloweVersion v
  -> MarloweContext v
  -> HelpersContext
  -> Chain.PolicyId
  -> Chain.TokenName
  -> Chain.Address
  -> Chain.Assets
  -> (Chain.Assets -> Chain.Assets)
  -> Contract v
  -> Continuations v
  -> m (Either String [SafetyError])
checkTransactions protocolParameters era version@MarloweV1 marloweContext helpersContext rolesCurrency threadRole changeAddress (Chain.Assets initialAda initialTokens) adjustMinUtxo contract continuations =
  runExceptT $
    do
      let changeAddress' = uncurry V1.Address . fromJust . V1.deserialiseAddress $ Chain.unAddress changeAddress
          initialValue =
            AM.fromList $
              (V1.Token "" "", fromIntegral initialAda)
                : [ (V1.Token (Chain.toPlutusCurrencySymbol p) (Chain.toPlutusTokenName n), fromIntegral quantity)
                  | (Chain.AssetId p n, quantity) <- M.toList $ Chain.unTokens initialTokens
                  ]
          helperRoles = M.keysSet $ helperScriptStates helpersContext
          intersectHelperRoles transaction@Transaction{txAnnotation} =
            transaction{txAnnotation = S.intersection txAnnotation $ S.map Chain.toPlutusTokenName helperRoles}
      transactions <- do
        let state = NotInitialized changeAddress' initialValue
            contract' = V1.MerkleizedContract contract $ remapContinuations continuations
        findTransactions firstRoleAuthorizationAnnotator False contract' state
      either throwE (pure . mconcat)
        . forM (filter (not . null . txAnnotation) $ intersectHelperRoles <$> transactions)
        $ checkTransaction
          protocolParameters
          era
          version
          marloweContext
          helpersContext
          rolesCurrency
          threadRole
          changeAddress
          adjustMinUtxo

-- | Check a transaction for safety issues.
checkTransaction
  :: C.LedgerProtocolParameters era
  -> C.BabbageEraOnwards era
  -> MarloweVersion v
  -> MarloweContext v
  -> HelpersContext
  -> Chain.PolicyId
  -> Chain.TokenName
  -> Chain.Address
  -> (Chain.Assets -> Chain.Assets)
  -> Transaction (S.Set Plutus.TokenName)
  -> Either String [SafetyError]
checkTransaction protocolParameters era version@MarloweV1 marloweContext@MarloweContext{..} helpersContext policyId@(Chain.PolicyId rolesCurrency) threadRole changeAddress adjustMinUtxo transaction@Transaction{..} =
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
              , Just $ max (addUTCTime 1 begin) end
              )
          -- For application of input, the current time must not be before the minimum time.
          | otherwise =
              ( max begin earliest
              , Just $ min begin (addUTCTime (-1) end)
              , Just $ max end (addUTCTime 1 earliest)
              )
        marloweData = V1.MarloweData rolesCurrency' txState txContract
        scriptIncoming = foldMap (uncurry makeValue . first snd) . AM.toList . V1.accounts $ V1.marloweState marloweData
        marloweOutput = TransactionScriptOutput marloweAddress scriptIncoming scriptTxOutRef marloweData
        marloweContext' = marloweContext{scriptOutput = Just marloweOutput}
        metadata = MarloweTransactionMetadata Nothing $ Chain.TransactionMetadata mempty
        (start, history) =
          makeSystemHistory . posixTimeToUTCTime $ Plutus.POSIXTime 0
        solveConstraints' = solveConstraints start (C.toLedgerEpochInfo history)
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
        helpersContext' = helpersForRoles policyId threadRole txAnnotation adjustMinUtxo helpersContext
    pure
      . either
        (pure . TransactionValidationError (stripAnnotation transaction) . show)
        (const $ TransactionWarning (stripAnnotation transaction) <$> V1.txOutWarnings txOutput)
      $ solveConstraints' era protocolParameters version (Left marloweContext') walletContext helpersContext' constraints

-- | Create a helpers context for the specified helper roles.
helpersForRoles
  :: Chain.PolicyId
  -> Chain.TokenName
  -> S.Set Plutus.TokenName
  -> (Chain.Assets -> Chain.Assets)
  -> HelpersContext
  -> HelpersContext
helpersForRoles policyId threadRole helperRoles adjustMinUtxo helpersContext =
  let activeHelperScripts =
        M.toList
          . M.intersection (helperScriptStates helpersContext)
          . M.fromSet (const ())
          $ S.map Chain.fromPlutusTokenName helperRoles
      buildHelperState ix (role, helperScriptInfo -> helperScriptInfo) =
        ( role
        , let helperTxOutRef = helperTxOutRef' ix
              address = helperAddress helperScriptInfo
              assets = adjustMinUtxo . Chain.Assets safeLovelace . Chain.Tokens $ M.fromList [(Chain.AssetId policyId role, 1)]
              datumHash =
                Chain.fromCardanoDatumHash
                  . C.hashScriptDataBytes
                  . C.unsafeHashableScriptData
                  . Chain.toCardanoScriptData
                  <$> datum
              datum = Just $ Chain.B $ Chain.unTokenName threadRole
              helperTransactionOutput = Chain.TransactionOutput{..}
           in HelperScriptState helperScriptInfo $ Just (helperTxOutRef, helperTransactionOutput)
        )
   in helpersContext{helperScriptStates = M.fromList $ uncurry buildHelperState <$> zip [0 ..] activeHelperScripts}

-- | Create a wallet context that will satisfy the given constraints.
walletForConstraints
  :: MarloweVersion v
  -> MarloweContext v
  -> Chain.Address
  -> TxConstraints era v
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
  -> Shelley.EraHistory
  -> UTCTime
  -> Either String Chain.SlotNo
utcTimeToSlotNo systemStart (Shelley.EraHistory interpreter) time =
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
  -> (Shelley.SystemStart, Shelley.EraHistory)
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
        C.EraHistory
          . Ouroboros.mkInterpreter
          . Ouroboros.summaryWithExactly
          . Ouroboros.Exactly
          $ K (oneSecondEraSummary 0) -- Byron lasted 1 second
            :* K (oneSecondEraSummary 1) -- Shelley lasted 1 second
            :* K (oneSecondEraSummary 2) -- Allegra lasted 1 second
            :* K (oneSecondEraSummary 3) -- Mary lasted 1 second
            :* K (oneSecondEraSummary 4) -- Alonzo lasted 1 second
            :* K (oneSecondEraSummary 5) -- Babbage lasted 1 second
            :* K (unboundedEraSummary 6) -- Conway never ends
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

-- | A dummy TxOut reference.
helperTxOutRef' :: Int -> Chain.TxOutRef
helperTxOutRef' = fromString . ("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE#" <>) . show
