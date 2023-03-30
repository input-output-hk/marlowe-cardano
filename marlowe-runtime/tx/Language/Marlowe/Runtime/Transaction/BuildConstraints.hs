{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints
  ( ApplyResults
  , buildApplyInputsConstraints
  , buildCreateConstraints
  , buildWithdrawConstraints
  ) where

import Cardano.Api (CardanoMode, EraHistory(..))
import qualified Cardano.Api.Byron as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.BaseTypes as CL (Network(..))
import Control.Category ((>>>))
import Control.Error (note)
import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT(runWriterT), tell)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (find, sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Base (Alternative((<|>)))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoSlotNo, plutusScriptHash, toCardanoAddressAny, toCardanoPlutusScript, toCardanoSlotNo)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , AssetId(..)
  , Assets(..)
  , Lovelace(..)
  , Metadata(..)
  , PaymentKeyHash(..)
  , PolicyId(..)
  , ScriptHash(..)
  , SlotNo
  , TokenName(..)
  , TransactionMetadata(..)
  , TransactionOutput(..)
  , UTxO(UTxO)
  , toUTxOsList
  )
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import Language.Marlowe.Runtime.Core.Api
  ( IsMarloweVersion(..)
  , MarloweTransactionMetadata(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , TransactionScriptOutput(..)
  , withMarloweVersion
  )
import Language.Marlowe.Runtime.Plutus.V2.Api
  ( fromPlutusScript
  , fromPlutusValue
  , toAssetId
  , toPlutusAddress
  , toPlutusCurrencySymbol
  , toPlutusTokenName
  , toPlutusTxOutRef
  )
import qualified Language.Marlowe.Runtime.Plutus.V2.Scripts.MarloweV1.RoleTokensPolicy as RoleTokensPolicy
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , CreateBuildupError(AddressDecodingFailed, MintingScriptDecodingFailed, MintingUtxoSelectionFailed)
  , CreateError(..)
  , Mint(unMint)
  , RoleTokensConfig(..)
  , WithdrawError
  , encodeRoleTokenMetadata
  )
import Language.Marlowe.Runtime.Transaction.Constraints
  ( TxConstraints(..)
  , WalletContext(WalletContext)
  , mustConsumeMarloweOutput
  , mustMintRoleToken
  , mustPayToAddress
  , mustPayToRole
  , mustSendMarloweOutput
  , mustSpendRoleToken
  , requiresMetadata
  , requiresSignature
  )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime, toRelativeTime)
import Ouroboros.Consensus.HardFork.History
  ( Bound(..)
  , EraEnd(..)
  , EraSummary(..)
  , Interpreter
  , PastHorizonException(..)
  , Summary(..)
  , interpretQuery
  , slotToWallclock
  , wallclockToSlot
  )
import Ouroboros.Consensus.Util.Counting (NonEmpty(..))
import qualified Ouroboros.Network.Block as O
import qualified Plutus.V2.Ledger.Api as P
import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx.AssocMap as AM
import Unsafe.Coerce (unsafeCoerce)

maxFees :: Lovelace
maxFees = Lovelace 2_170_000

-- FIXME: This is arbitrary value - adjust this better.
minAdaPerTokenOutput :: Lovelace
minAdaPerTokenOutput  = Lovelace 10_000

type TxConstraintsBuilderM err v a = WriterT (TxConstraints v) (Either err) a

runTxConstraintsBuilder
  :: MarloweVersion v -> TxConstraintsBuilderM err v a -> Either err (a, TxConstraints v)
runTxConstraintsBuilder v = runWriterT . withMarloweVersion v

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraints
  :: forall v
   . MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> WalletContext  -- ^ The wallet used to mint tokens.
  -> RoleTokensConfig  -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata -- ^ Metadata to add to the transaction.
  -> Lovelace -- ^ Min Lovelace value which should be used on the Marlowe output.
  -> Contract v -- ^ The contract being instantiated.
  -> Either (CreateError v) ((Datum v, Assets, PolicyId), TxConstraints v)
buildCreateConstraints version walletCtx roles metadata minAda contract = case version of
  MarloweV1 -> runTxConstraintsBuilder version $ buildCreateConstraintsV1 walletCtx roles metadata minAda contract

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraintsV1
  :: WalletContext  -- ^ The wallet used to mint tokens.
  -> RoleTokensConfig -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata -- ^ Metadata to add to the transaction.
  -> Lovelace -- ^ Min Lovelace value which should be used on the Marlowe output.
  -> Contract 'V1 -- ^ The contract being instantiated.
  -> TxConstraintsBuilderM (CreateError 'V1) 'V1 (Datum 'V1, Assets, PolicyId)
buildCreateConstraintsV1 walletCtx roles metadata minAda contract = do
  -- Output constraints.
  -- Role tokens minting and distribution.
  policyId <- mintRoleTokens

  tell . requiresMetadata $ metadata { transactionMetadata = nftsMetadata policyId <> transactionMetadata metadata }

  -- Marlowe script output.
  (datum, assets) <- sendMarloweOutput policyId

  pure (datum, assets, policyId)
  where
    nftsMetadata (PolicyId policyId) = case roles of
      RoleTokensMint (Map.toList . unMint -> minting) -> do
        let
          tokensMetadata = catMaybes $ minting <&> \case
            (tokenName, (_, Just roleTokenMetadata)) -> do
              let
                tokenName' = unTokenName tokenName
              -- From CIP-25: In version 2 the the raw bytes of the asset_name are used.
              Just (MetadataBytes tokenName', encodeRoleTokenMetadata roleTokenMetadata)
            _ -> Nothing
        case tokensMetadata of
          [] -> mempty
          metadata' -> TransactionMetadata (Map.singleton 721 (MetadataMap [(MetadataBytes policyId, MetadataMap metadata')]))
      _ -> mempty

    liftMaybe err = lift . note (CreateBuildupFailed err)

    sendMarloweOutput policyId = do
      datum <- mkMarloweDatum policyId
      let assets = adaAsset minAda
      tell $ mustSendMarloweOutput assets datum
      pure (datum, assets)

    mkMarloweDatum :: PolicyId -> TxConstraintsBuilderM (CreateError 'V1) 'V1 (Datum 'V1)
    mkMarloweDatum policyId = do
      marloweState <- mkInitialMarloweState
      let
        marloweParams = V1.MarloweParams . toPlutusCurrencySymbol $ policyId
      pure $ V1.MarloweData marloweParams marloweState contract

    mkInitialMarloweState :: TxConstraintsBuilderM (CreateError 'V1) 'V1 V1.State
    mkInitialMarloweState = do
      let
        WalletContext { changeAddress=minAdaProvider } = walletCtx
      (net, addr) <- liftMaybe (AddressDecodingFailed minAdaProvider) do
        address <- toPlutusAddress minAdaProvider
        network <- toMarloweNetwork minAdaProvider
        pure (network, address)
      let
        accountId = V1.Address net addr
        adaToken = V1.Token PV2.adaSymbol PV2.adaToken
        initialAccounts :: V1.Accounts
        initialAccounts = AM.singleton (accountId, adaToken) (toInteger minAda)
      pure (V1.emptyState (PV2.POSIXTime 0)) { V1.accounts = initialAccounts }

    toMarloweNetwork :: Address -> Maybe V1.Network
    toMarloweNetwork = toCardanoAddressAny >=> \case
        C.AddressByron (C.ByronAddress _) -> Nothing
        C.AddressShelley (C.ShelleyAddress CL.Mainnet _ _) -> Just V1.mainnet
        C.AddressShelley _ -> Just V1.testnet

    adaAsset :: Lovelace -> Assets
    adaAsset amount = Assets amount mempty

    -- Role token distribution constraints
    mintRoleTokens :: TxConstraintsBuilderM (CreateError 'V1) 'V1 PolicyId
    mintRoleTokens = case roles of
      RoleTokensUsePolicy policyId -> pure policyId
      RoleTokensMint (unMint -> minting) -> do
        let
          WalletContext { availableUtxos } = walletCtx
          txLovelaceRequirementEstimate = adaAsset $
            minAda
            + maxFees
            + Lovelace (fromInteger . toInteger . length $ minting) * minAdaPerTokenOutput
          utxoAssets UTxO {transactionOutput = TransactionOutput { assets }} = assets
          possibleInput = (find ((<) txLovelaceRequirementEstimate . utxoAssets) . sortBy (compare `on` utxoAssets) . toUTxOsList $ availableUtxos)
            <|> listToMaybe (toUTxOsList availableUtxos)

        UTxO txOutRef _ <- liftMaybe MintingUtxoSelectionFailed possibleInput
        let
          txOutRef' = toPlutusTxOutRef txOutRef

          roleTokens = RoleTokensPolicy.mkRoleTokens (map ((, 1) . toPlutusTokenName) . Map.keys $ minting)
          plutusScript = fromPlutusScript . PV2.getMintingPolicy . RoleTokensPolicy.policy roleTokens $ txOutRef'

        (script, scriptHash) <- liftMaybe (MintingScriptDecodingFailed plutusScript) do
          script <- toCardanoPlutusScript plutusScript
          scriptHash <- plutusScriptHash plutusScript
          pure (script, scriptHash)
        let
          witness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            (C.PScript script)
            C.NoScriptDatumForMint
            (C.fromPlutusData $ PV2.toData RoleTokensPolicy.Mint)
            (C.ExecutionUnits 0 0)
          policyId = PolicyId . unScriptHash $ scriptHash

        for_ (Map.toList minting) \(tokenName, (address, _)) ->
          tell $ mustMintRoleToken txOutRef witness (AssetId policyId tokenName) address
        pure policyId
      RoleTokensNone -> do
        let
          -- We use ADA currency symbol as a placeholder which
          -- carries really no semantics in this context.
          uselessRolePolicyId  = PolicyId . PV2.fromBuiltin . PV2.unCurrencySymbol $ PV2.adaSymbol
        pure uselessRolePolicyId

type ApplyResults v = (UTCTime, UTCTime, Maybe (Assets, Datum v))

-- applies an input to a contract.
buildApplyInputsConstraints
  :: SystemStart
  -> EraHistory CardanoMode -- ^ The era history for converting times to slots.
  -> MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v -- ^ The previous script output for the contract
  -> SlotNo -- ^ The current tip slot
  -> MarloweTransactionMetadata -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
                   -- If not specified, this is computed from the the timeouts
                   -- in the contract.
  -> Inputs v -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError v) (ApplyResults v, TxConstraints v)
buildApplyInputsConstraints systemStart eraHistory version marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs =
  case version of
    MarloweV1 -> buildApplyInputsConstraintsV1 systemStart eraHistory marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraintsV1
  :: SystemStart
  -> EraHistory CardanoMode -- ^ The era history for converting times to slots.
  -> TransactionScriptOutput 'V1 -- ^ The previous script output for the contract with raw TxOut.
  -> SlotNo
  -> MarloweTransactionMetadata -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
  -> Inputs 'V1 -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError 'V1) (ApplyResults 'V1, TxConstraints 'V1)
buildApplyInputsConstraintsV1 systemStart eraHistory marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs = runWriterT do
  let
    TransactionScriptOutput _ _ _ datum = marloweOutput
    V1.MarloweData params state contract = datum
    V1.MarloweParams currencySymbol = params

    requiredParties = Set.fromList $ maybeToList . marloweInputParty <$> inputs
    roleAssetId = toAssetId currencySymbol

    tipSlot' = toCardanoSlotNo tipSlot

  invalidBefore' <- lift $ maybe (pure tipSlot') utcTimeToSlotNo invalidBefore

  lift $ unless (invalidBefore' <= tipSlot') $ Left $ ValidityLowerBoundTooHigh tipSlot $ fromCardanoSlotNo invalidBefore'

  invalidHereafter' <- lift case invalidHereafter of
    Nothing -> do
      invalidBefore'' <- slotStart invalidBefore'                             -- Find the start time of the validity range.
      pure case nextMarloweTimeoutAfter invalidBefore'' contract of           -- Find the next timeout after the range start, if any.
        Nothing -> maxSafeSlot                                                -- There is no future timeout.
        Just subsequentTimeout -> case utcTimeToSlotNo' subsequentTimeout of  -- There is a future timeout.
          Right slot' -> slot'                                                -- The next timeout is before the safe horizon, but note that this might be an empty interval.
          _ ->  maxSafeSlot                                                   -- The next timeout is beyond the safe horizon.
    Just t -> utcTimeToSlotNo t

  -- Construct inputs constraints.
  -- Consume UTXOs containing Marlowe script.
  tell $ mustConsumeMarloweOutput @'V1 invalidBefore' invalidHereafter' inputs

  -- Consume UTXOs containing all necessary role tokens and send them back.
  for_ requiredParties $ traverse_ $ \case
    V1.Role role -> tell $ mustSpendRoleToken $ roleAssetId role
    _ -> pure ()

  -- Require signature of an every party which is authorized through an address.
  for_ requiredParties $ traverse_ $ \case
    V1.Address _ address -> case address of
      P.Address (P.PubKeyCredential (P.PubKeyHash pkh)) _ ->
        tell $ requiresSignature $ PaymentKeyHash $ P.fromBuiltin pkh
      _ -> pure ()
    _ -> pure ()

  -- Require transaction metadata for all specified keys
  tell $ requiresMetadata metadata

  -- Apply inputs.
  let slotNoToPOSIXTime = fmap utcToPOSIXTime . slotStart
  txInterval <- lift $ (,)
    <$> slotNoToPOSIXTime invalidBefore'
    -- We subtract 1 here because invalidHereafter' is the first invalid slot.
    -- However, the Marlowe Semantics time interval upper bound is the last
    -- valid millisecond. So we subtract 1 millisecond from the start of the
    -- first invalid slot to get the last millisecond in the last valid slot.
    <*> (subtract 1 <$> slotNoToPOSIXTime invalidHereafter')
  let transactionInput = V1.TransactionInput { txInterval, txInputs = inputs }
  (possibleContinuation, payments) <- case V1.computeTransaction transactionInput state contract of
     V1.Error err -> lift $ Left $ ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed $ show err)
     V1.TransactionOutput _ payments _ V1.Close ->
       pure (Nothing, payments)
     V1.TransactionOutput _ payments state' contract' ->
       pure (Just (state', contract'), payments)

  -- Construct outputs constraints.
  -- Require Marlowe output if the contract is not closed.
  output <- for possibleContinuation \(state'@V1.State { accounts }, contract') -> do
      let
        datum' = V1.MarloweData params state' contract'
        assets = fromPlutusValue $ V1.totalBalance accounts
      tell $ mustSendMarloweOutput assets datum'
      pure (assets, datum')

  -- For every payment require an output either to the role
  -- payout script or directly to the party address.
  for_ payments \(V1.Payment _ payee token quantity) -> do
    let
      assets = case token of
        V1.Token "" "" -> Assets (Lovelace $ fromInteger quantity) mempty
        V1.Token cs tn -> do
          let
            assetId = toAssetId cs tn
            quantity' = fromInteger quantity
          Assets (Lovelace 0) (CS.Tokens $ Map.singleton assetId quantity')
    case payee of
      V1.Party (V1.Address net addr) -> do
        let addr' = Address $ V1.serialiseAddress net addr
        tell $ mustPayToAddress assets addr'
      V1.Party (V1.Role role) ->
        tell $ mustPayToRole assets $ roleAssetId role
      V1.Account _ -> pure ()

  pure
    ( posixTimeToUTCTime $ fst txInterval
    , posixTimeToUTCTime $ snd txInterval + 1 -- Add the millisecond back to convert the upper bound back to an exclusive bound (ledger semantics)
    , output
    )

  where
    marloweInputContent (V1.NormalInput c) = c
    marloweInputContent (V1.MerkleizedInput c _ _) = c

    marloweInputParty = marloweInputContent >>> \case
      V1.IDeposit _ party _ _         -> Just party
      V1.IChoice (V1.ChoiceId _ party) _ -> Just party
      V1.INotify                      -> Nothing

    EraHistory _ interpreter = eraHistory

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo :: UTCTime -> Either (ApplyInputsError 'V1) C.SlotNo
    utcTimeToSlotNo = first (SlotConversionFailed . show) . utcTimeToSlotNo'

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo' :: UTCTime -> Either PastHorizonException C.SlotNo
    utcTimeToSlotNo' time = do
      let relativeTime = toRelativeTime systemStart time
      (slotNo, _, _) <- interpretQuery interpreter
        $ wallclockToSlot relativeTime
      pure $ C.SlotNo $ O.unSlotNo slotNo

    slotStart :: C.SlotNo -> Either (ApplyInputsError 'V1) UTCTime
    slotStart (C.SlotNo slotNo) = do
      (relativeTime, _) <- first (SlotConversionFailed . show)
        $ interpretQuery interpreter
        $ slotToWallclock
        $ O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

    utcToPOSIXTime :: UTCTime -> PV2.POSIXTime
    utcToPOSIXTime = PV2.POSIXTime . floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

    posixTimeToUTCTime :: PV2.POSIXTime -> UTCTime
    posixTimeToUTCTime (P.POSIXTime t) = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger t / 1000

    nextMarloweTimeoutAfter :: UTCTime -> V1.Contract -> Maybe UTCTime
    nextMarloweTimeoutAfter limit (V1.When _ timeout c)
      | posixTimeToUTCTime timeout > limit = Just $ posixTimeToUTCTime timeout
      | otherwise = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter _ V1.Close = Nothing
    nextMarloweTimeoutAfter limit (V1.Pay _ _ _ _ c) = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter limit (V1.If _ c1 c2) = on min (nextMarloweTimeoutAfter limit) c1 c2
    nextMarloweTimeoutAfter limit (V1.Let _ _ c) = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter limit (V1.Assert _ c) = nextMarloweTimeoutAfter limit c

    maxSafeSlot :: O.SlotNo
    maxSafeSlot = getMaxSafeSlotFromSummary $ unInterpreter interpreter

    unInterpreter :: Interpreter xs -> Summary xs
    unInterpreter = unsafeCoerce -- Interpreter xs is a newtype wrapper around Summary xs.
                                 -- Unfortunately, the provided query language is not able to extract the max safe slot,
                                 -- So we have to reach unsafely into the internals to get
                                 -- it ourselves. https://input-output-hk.github.io/ouroboros-network/ouroboros-consensus/Ouroboros-Consensus-HardFork-History-Qry.html#t:Interpreter

    getMaxSafeSlotFromSummary :: Summary xs -> O.SlotNo
    getMaxSafeSlotFromSummary (Summary eras) = case eras of
      NonEmptyOne era -> getMaxSafeSlotFromEraSummary era
      NonEmptyCons _ eras' -> getMaxSafeSlotFromSummary (Summary eras')

    getMaxSafeSlotFromEraSummary :: EraSummary -> O.SlotNo
    getMaxSafeSlotFromEraSummary EraSummary{..} = case eraEnd of
      EraEnd Bound{..} -> O.SlotNo $ O.unSlotNo boundSlot - 1 -- subtract 1 because the era end bound is exclusive
      EraUnbounded -> maxBound

-- | Creates a set of Tx constraints that are used to build a transaction that
-- withdraws payments from a payout validator.

buildWithdrawConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> PayoutDatum v -- ^ The role token from which to withdraw funds.
  -> Either (WithdrawError v) (TxConstraints v)
buildWithdrawConstraints = \case
  MarloweV1 -> Right . buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1 :: AssetId -> TxConstraints 'V1
    buildWithdrawConstraintsV1 =
      TxConstraints.mustConsumePayouts <> TxConstraints.mustSpendRoleToken
