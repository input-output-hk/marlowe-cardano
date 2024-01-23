{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints (
  ApplyResults,
  MkRoleTokenMintingPolicy,
  buildApplyInputsConstraints,
  buildCreateConstraints,
  buildWithdrawConstraints,
  initialMarloweState,
  safeLovelace,
) where

import Cardano.Api (CardanoMode, EraHistory (..))
import qualified Cardano.Api.Byron as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.BaseTypes as CL (Network (..))
import Control.Category ((>>>))
import Control.Error (ExceptT, note)
import Control.Monad (guard, join, unless, when, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Bifunctor (first)
import Data.Foldable (Foldable (..), for_, traverse_)
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.SOP.Counting (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import GHC.Base (Alternative ((<|>)))
import Language.Marlowe.Core.V1.Semantics (TransactionInput)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoSlotNo,
  plutusScriptHash,
  toCardanoAddressAny,
  toCardanoPlutusScript,
  toCardanoSlotNo,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (..),
  AssetId (..),
  Assets (..),
  Lovelace (..),
  Metadata (..),
  PaymentKeyHash (..),
  PolicyId (..),
  ScriptHash (..),
  SlotNo,
  TokenName (..),
  Tokens (..),
  TransactionMetadata (..),
  TransactionOutput (..),
  TxOutRef,
  UTxO (UTxO),
  toUTxOsList,
  unInterpreter,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import Language.Marlowe.Runtime.Core.Api (
  IsMarloweVersion (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (..),
  TransactionScriptOutput (..),
  fromChainPayoutDatum,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..))
import Language.Marlowe.Runtime.Plutus.V2.Api (
  fromPlutusValue,
  toAssetId,
  toPlutusAddress,
  toPlutusCurrencySymbol,
  toPlutusTokenName,
 )
import Language.Marlowe.Runtime.Transaction.Api (
  Account (..),
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  CreateBuildupError (AddressDecodingFailed, MintingScriptDecodingFailed, MintingUtxoSelectionFailed),
  CreateError (..),
  Destination (..),
  Mint (unMint),
  MintRole (..),
  RoleTokensConfig (..),
  WithdrawError (..),
  encodeRoleTokenMetadata,
  getTokenQuantities,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  TxConstraints (..),
  WalletContext (WalletContext),
  mustConsumeMarloweOutput,
  mustDistributeRoleToken,
  mustMintRoleToken,
  mustPayToAddress,
  mustPayToRole,
  mustSendMarloweOutput,
  mustSpendRoleToken,
  requiresMetadata,
  requiresSignature,
 )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime, toRelativeTime)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraSummary (..),
  PastHorizonException (..),
  Summary (..),
  interpretQuery,
  slotToWallclock,
  wallclockToSlot,
 )
import qualified Ouroboros.Network.Block as O
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx.AssocMap as AM

type TxConstraintsBuilderM err era v m a = WriterT (TxConstraints era v) (ExceptT err m) a

runTxConstraintsBuilder
  :: MarloweVersion v
  -> TxConstraintsBuilderM err era v m a
  -> m (Either err (a, TxConstraints era v))
runTxConstraintsBuilder v = runExceptT . runWriterT . withMarloweVersion v

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraints
  :: forall era v m
   . (Monad m)
  => MkRoleTokenMintingPolicy m
  -- ^ A validator creator for the role token minting policy.
  -> C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -- ^ The era in which the transaction is being built. Requires reference scripts.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> WalletContext
  -- ^ The wallet used to mint tokens.
  -> TokenName
  -- ^ The thread token name for the contract.
  -> RoleTokensConfig
  -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata
  -- ^ Metadata to add to the transaction.
  -> Lovelace
  -- ^ The lower bound on the ada in the initial state.
  -> Map Account Assets
  -- ^ User-defined initial account balances
  -> (Assets -> Assets)
  -- ^ Adjust a value to account for the minimum UTxO ledger rule.
  -> Contract v
  -- ^ The contract being instantiated.
  -> m (Either CreateError ((Datum v, Assets, PolicyId), TxConstraints era v))
buildCreateConstraints mkRoleTokenMintingPolicy era version walletCtx roles threadName metadata minAda accounts adjustMinUtxo contract = case version of
  MarloweV1 ->
    runTxConstraintsBuilder version $
      buildCreateConstraintsV1
        mkRoleTokenMintingPolicy
        era
        walletCtx
        roles
        threadName
        metadata
        minAda
        accounts
        adjustMinUtxo
        contract

type MkRoleTokenMintingPolicy m = TxOutRef -> Map TokenName Integer -> m CS.PlutusScript

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraintsV1
  :: forall era m
   . (Monad m)
  => MkRoleTokenMintingPolicy m
  -- ^ A validator creator for the role token minting policy.
  -> C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -- ^ The era in which the transaction is being built. Requires reference scripts.
  -> WalletContext
  -- ^ The wallet used to mint tokens.
  -> TokenName
  -- ^ The thread token name for the contract.
  -> RoleTokensConfig
  -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata
  -- ^ Metadata to add to the transaction.
  -> Lovelace
  -- ^ The lower bound on the ada in the initial state.
  -> Map Account Assets
  -- ^ User-defined initial account balances
  -> (Assets -> Assets)
  -- ^ Adjust a value to account for the minimum UTxO ledger rule.
  -> Contract 'V1
  -- ^ The contract being instantiated.
  -> TxConstraintsBuilderM CreateError era 'V1 m (Datum 'V1, Assets, PolicyId)
buildCreateConstraintsV1 mkRoleTokenMintingPolicy era walletCtx threadTokenName roles metadata minAda accounts adjustMinUtxo contract = do
  -- Output constraints.

  -- Role tokens minting and distribution.
  (policyId, threadToken) <- buildRoleTokenConstraints

  tell . requiresMetadata $ metadata{transactionMetadata = nftsMetadata policyId <> transactionMetadata metadata}

  -- Marlowe script output.
  (datum, assets) <- sendMarloweOutput policyId threadToken

  pure (datum, assets, policyId)
  where
    nftsMetadata (PolicyId policyId) = case roles of
      RoleTokensMint (unMint -> minting) -> do
        let tokensMetadata = flip NEMap.foldMapWithKey minting \tokenName MintRole{..} ->
              flip foldMap roleMetadata \roleTokenMetadata -> do
                let tokenName' = unTokenName tokenName
                -- From CIP-25: In version 2 the the raw bytes of the asset_name are used.
                pure (MetadataBytes tokenName', encodeRoleTokenMetadata roleTokenMetadata)
        case tokensMetadata of
          [] -> mempty
          metadata' -> TransactionMetadata (Map.singleton 721 (MetadataMap [(MetadataBytes policyId, MetadataMap metadata')]))
      _ -> mempty

    liftMaybe err = lift . except . note (CreateBuildupFailed err)

    sendMarloweOutput policyId threadToken = do
      (assets, marloweState) <-
        lift . except $
          initialMarloweStateV1 adjustMinUtxo accounts threadToken minAda walletCtx
      datum <- mkMarloweDatum policyId marloweState
      tell $ mustSendMarloweOutput assets datum
      pure (datum, assets)

    mkMarloweDatum :: PolicyId -> V1.State -> TxConstraintsBuilderM CreateError era 'V1 m (Datum 'V1)
    mkMarloweDatum policyId marloweState = do
      let marloweParams = V1.MarloweParams . toPlutusCurrencySymbol $ policyId
      pure $ V1.MarloweData marloweParams marloweState contract

    adaAsset :: Assets -> Assets
    adaAsset (Assets amount _) = Assets amount mempty

    -- Role token distribution constraints
    buildRoleTokenConstraints :: TxConstraintsBuilderM CreateError era 'V1 m (PolicyId, Maybe AssetId)
    buildRoleTokenConstraints = case roles of
      RoleTokensUsePolicy policyId distribution -> do
        for_ (Map.toList distribution) \(tokenName, dist') ->
          for_ (Map.toList dist') \(destination, quantity) -> do
            let destination' = case destination of
                  ToScript script' -> Right (AssetId policyId threadTokenName, script')
                  ToAddress addr -> Left addr
            tell $ mustDistributeRoleToken (AssetId policyId tokenName) destination' quantity
        pure
          ( policyId
          , AssetId policyId threadTokenName <$ guard (any (Map.member (ToScript OpenRoleScript)) distribution)
          )
      RoleTokensMint mint -> do
        let WalletContext{availableUtxos} = walletCtx
            threadTokenName' =
              threadTokenName
                <$ guard (any (NEMap.member (ToScript OpenRoleScript) . roleTokenRecipients) $ unMint mint)
            tokenQuantities = maybe id (flip Map.insert 1) threadTokenName' $ NEMap.toMap $ getTokenQuantities mint
            txLovelaceRequirementEstimate =
              adaAsset
                . adjustMinUtxo
                . Assets safeLovelace
                . Tokens
                . Map.mapKeysMonotonic (AssetId "")
                $ tokenQuantities
            utxoAssets UTxO{transactionOutput = TransactionOutput{assets}} = assets
            possibleInput =
              ( find ((<) txLovelaceRequirementEstimate . utxoAssets) . sortBy (compare `on` utxoAssets) . toUTxOsList $ availableUtxos
              )
                <|> listToMaybe (toUTxOsList availableUtxos)

        UTxO txOutRef _ <- liftMaybe MintingUtxoSelectionFailed possibleInput
        plutusScript <- lift $ lift $ mkRoleTokenMintingPolicy txOutRef $ fromIntegral <$> tokenQuantities

        (script, scriptHash) <- liftMaybe (MintingScriptDecodingFailed plutusScript) do
          script <- toCardanoPlutusScript plutusScript
          scriptHash <- plutusScriptHash plutusScript
          pure (script, scriptHash)
        let plutusScriptV2InEra :: C.ScriptLanguageInEra C.PlutusScriptV2 era
            plutusScriptV2InEra = case era of
              C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> C.PlutusScriptV2InBabbage
              C.ReferenceTxInsScriptsInlineDatumsInConwayEra -> C.PlutusScriptV2InConway
            witness =
              C.PlutusScriptWitness
                plutusScriptV2InEra
                C.PlutusScriptV2
                (C.PScript script)
                C.NoScriptDatumForMint
                (C.unsafeHashableScriptData $ C.ScriptDataConstructor 0 []) -- This corresponds to the Mint action in the validator.
                (C.ExecutionUnits 0 0)
            policyId = PolicyId . unScriptHash $ scriptHash

        for_ (NEMap.toList $ unMint mint) \(tokenName, MintRole{..}) ->
          for_ (NEMap.toList roleTokenRecipients) \(destination, quantity) -> do
            let destination' = case destination of
                  ToScript script' -> Right (AssetId policyId threadTokenName, script')
                  ToAddress addr -> Left addr
            tell $ mustMintRoleToken txOutRef witness (AssetId policyId tokenName) destination' quantity
        pure (policyId, AssetId policyId <$> threadTokenName')
      RoleTokensNone -> do
        let -- We use ADA currency symbol as a placeholder which
            -- carries really no semantics in this context.
            uselessRolePolicyId = PolicyId . PV2.fromBuiltin . PV2.unCurrencySymbol $ PV2.adaSymbol
        pure (uselessRolePolicyId, Nothing)

toMarloweNetwork :: Address -> Maybe V1.Network
toMarloweNetwork =
  toCardanoAddressAny >=> \case
    C.AddressByron (C.ByronAddress _) -> Nothing
    C.AddressShelley (C.ShelleyAddress CL.Mainnet _ _) -> Just V1.mainnet
    C.AddressShelley _ -> Just V1.testnet

safeLovelace :: Lovelace
safeLovelace = 750_000 -- Enough lovelace to avoid the Cardano.Api minimum-UTxO bug.

initialMarloweState
  :: forall v
   . (Assets -> Assets)
  -> MarloweVersion v
  -> Map Account Assets
  -> Maybe AssetId
  -> Lovelace
  -> WalletContext
  -> Either CreateError (Assets, V1.State)
initialMarloweState adjustMinUtxo version accounts threadToken minAda walletCtx = case version of
  MarloweV1 ->
    initialMarloweStateV1 adjustMinUtxo accounts threadToken minAda walletCtx

initialMarloweStateV1
  :: (Assets -> Assets)
  -> Map Account Assets
  -> Maybe AssetId
  -> Lovelace
  -> WalletContext
  -> Either CreateError (Assets, V1.State)
initialMarloweStateV1 adjustMinUtxo accounts threadToken minAda walletCtx = do
  -- All assets from user-defined initial accounts
  let accountAssets = fold accounts
      -- The thread token as a tokens collection
      threadTokenTokens = Tokens $ foldMap (flip Map.singleton 1) threadToken
      -- The result of computing the min ada upper bound starting with the initial assets
      minAda' =
        ada
          . adjustMinUtxo
          . Assets (max safeLovelace minAda)
          . (tokens accountAssets <>)
          $ threadTokenTokens
      WalletContext{changeAddress = creatorAddress} = walletCtx
      -- The assets to be assigned to the contract creator include the min ADA deposit and the thread token.
      creatorAssets = Assets minAda' threadTokenTokens
      -- Merge these with the user-defined initial account balances.
      initialAccounts = Map.unionWith (<>) (Map.singleton (AddressAccount creatorAddress) creatorAssets) accounts
  -- Convert it to the format required by the Plutus script datum.
  initialAccountsPlutus <-
    AM.fromList . join <$> for (Map.toAscList initialAccounts) \(account, Assets{..}) -> do
      accountId <- case account of
        RoleAccount role -> pure $ V1.Role $ toPlutusTokenName role
        AddressAccount address -> note (CreateBuildupFailed $ AddressDecodingFailed address) do
          addressPlutus <- toPlutusAddress address
          network <- toMarloweNetwork address
          pure $ V1.Address network addressPlutus
      let adaToken = V1.Token PV2.adaSymbol PV2.adaToken
      pure
        . Map.toAscList
        . (if ada > 0 then Map.insert (accountId, adaToken) $ toInteger ada else id)
        . Map.mapKeys (\(AssetId cs tn) -> (accountId, V1.Token (toPlutusCurrencySymbol cs) (toPlutusTokenName tn)))
        . fmap toInteger
        $ unTokens tokens

  pure
    ( fold initialAccounts
    , (V1.emptyState (PV2.POSIXTime 0))
        { V1.accounts = initialAccountsPlutus
        }
    )

type ApplyResults v = (UTCTime, UTCTime, Maybe (Assets, Datum v), Inputs v)

-- applies an input to a contract.
buildApplyInputsConstraints
  :: (Monad m)
  => (TransactionInput -> m (Maybe TransactionInput))
  -> SystemStart
  -> EraHistory CardanoMode
  -- ^ The era history for converting times to slots.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v
  -- ^ The previous script output for the contract
  -> SlotNo
  -- ^ The current tip slot
  -> MarloweTransactionMetadata
  -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime
  -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime
  -- ^ The maximum bound of the validity interval (exclusive).
  -- If not specified, this is computed from the the timeouts
  -- in the contract.
  -> Inputs v
  -- ^ The inputs to apply to the contract.
  -> ExceptT ApplyInputsError m (ApplyResults v, TxConstraints era v)
buildApplyInputsConstraints merkleizeInputs systemStart eraHistory version marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs =
  case version of
    MarloweV1 ->
      buildApplyInputsConstraintsV1
        merkleizeInputs
        systemStart
        eraHistory
        marloweOutput
        tipSlot
        metadata
        invalidBefore
        invalidHereafter
        inputs

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraintsV1
  :: forall era m
   . (Monad m)
  => (TransactionInput -> m (Maybe TransactionInput))
  -> SystemStart
  -> EraHistory CardanoMode
  -- ^ The era history for converting times to slots.
  -> TransactionScriptOutput 'V1
  -- ^ The previous script output for the contract with raw TxOut.
  -> SlotNo
  -> MarloweTransactionMetadata
  -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime
  -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime
  -- ^ The maximum bound of the validity interval (exclusive).
  -> Inputs 'V1
  -- ^ The inputs to apply to the contract.
  -> ExceptT ApplyInputsError m (ApplyResults 'V1, TxConstraints era 'V1)
buildApplyInputsConstraintsV1 merkleizeInputs systemStart eraHistory marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs = runWriterT do
  let TransactionScriptOutput _ _ _ datum = marloweOutput
      V1.MarloweData params state contract = datum
      V1.MarloweParams currencySymbol = params

      requiredParties = Set.fromList $ maybeToList . marloweInputParty <$> inputs
      roleAssetId = toAssetId currencySymbol

      tipSlot' = toCardanoSlotNo tipSlot

  invalidBefore' <- lift $ maybe (pure tipSlot') utcTimeToSlotNo invalidBefore

  lift $
    unless (invalidBefore' <= tipSlot') $
      throwE $
        ValidityLowerBoundTooHigh tipSlot $
          fromCardanoSlotNo invalidBefore'

  invalidHereafter' <- lift $ case invalidHereafter of
    Nothing -> do
      invalidBefore'' <- slotStart invalidBefore' -- Find the start time of the validity range.
      pure case nextMarloweTimeoutAfter invalidBefore'' contract of -- Find the next timeout after the range start, if any.
        Nothing -> maxSafeSlot -- There is no future timeout.
        Just subsequentTimeout -> case utcTimeToSlotNo' subsequentTimeout of -- There is a future timeout.
          Right slot' -> slot' -- The next timeout is before the safe horizon, but note that this might be an empty interval.
          _ -> maxSafeSlot -- The next timeout is beyond the safe horizon.
    Just t -> utcTimeToSlotNo t

  -- Consume UTXOs containing all necessary role tokens and send them back.
  for_ requiredParties $ traverse_ $ \case
    V1.Role role -> tell $ mustSpendRoleToken $ roleAssetId role
    _ -> pure ()

  -- Require signature of an every party which is authorized through an address.
  for_ requiredParties $ traverse_ $ \case
    V1.Address _ address -> case address of
      PV2.Address (PV2.PubKeyCredential (PV2.PubKeyHash pkh)) _ ->
        tell $ requiresSignature $ PaymentKeyHash $ PV2.fromBuiltin pkh
      _ -> pure ()
    _ -> pure ()

  -- Require transaction metadata for all specified keys
  tell $ requiresMetadata metadata

  -- Apply inputs.
  let slotNoToPOSIXTime = fmap utcToPOSIXTime . slotStart
  txInterval <-
    lift $
      (,)
        <$> slotNoToPOSIXTime invalidBefore'
        -- We subtract 1 here because invalidHereafter' is the first invalid slot.
        -- However, the Marlowe Semantics time interval upper bound is the last
        -- valid millisecond. So we subtract 1 millisecond from the start of the
        -- first invalid slot to get the last millisecond in the last valid slot.
        <*> (subtract 1 <$> slotNoToPOSIXTime invalidHereafter')
  let transactionInput = V1.TransactionInput{txInterval, txInputs = inputs}

  -- Try and auto-merkleize the inputs if possible.
  transactionInput' <- lift $ lift $ fromMaybe transactionInput <$> merkleizeInputs transactionInput

  -- Construct inputs constraints.
  -- Consume UTXOs containing Marlowe script.
  tell $ mustConsumeMarloweOutput @'V1 invalidBefore' invalidHereafter' $ V1.txInputs transactionInput'

  (possibleContinuation, payments) <- case V1.computeTransaction transactionInput' state contract of
    V1.Error err -> lift $ throwE $ ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed err)
    V1.TransactionOutput _ payments _ V1.Close ->
      pure (Nothing, payments)
    V1.TransactionOutput _ payments state' contract' ->
      pure (Just (state', contract'), payments)

  -- Construct outputs constraints.
  -- Require Marlowe output if the contract is not closed.
  output <- for possibleContinuation \(state'@V1.State{accounts}, contract') -> do
    let datum' = V1.MarloweData params state' contract'
        assets = fromPlutusValue $ V1.totalBalance accounts
    tell $ mustSendMarloweOutput assets datum'
    pure (assets, datum')

  -- For every payment require an output either to the role
  -- payout script or directly to the party address.
  for_ payments \(V1.Payment _ payee token quantity) -> do
    let assets = case token of
          V1.Token "" "" -> Assets (Lovelace $ fromInteger quantity) mempty
          V1.Token cs tn -> do
            let assetId = toAssetId cs tn
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
    , V1.txInputs transactionInput'
    )
  where
    marloweInputContent (V1.NormalInput c) = c
    marloweInputContent (V1.MerkleizedInput c _ _) = c

    marloweInputParty =
      marloweInputContent >>> \case
        V1.IDeposit _ party _ _ -> Just party
        V1.IChoice (V1.ChoiceId _ party) _ -> Just party
        V1.INotify -> Nothing

    EraHistory _ interpreter = eraHistory

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo :: UTCTime -> ExceptT ApplyInputsError m C.SlotNo
    utcTimeToSlotNo = withExceptT (SlotConversionFailed . show) . except . utcTimeToSlotNo'

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo' :: UTCTime -> Either PastHorizonException C.SlotNo
    utcTimeToSlotNo' time = do
      let relativeTime = toRelativeTime systemStart time
      (slotNo, _, _) <-
        interpretQuery interpreter $
          wallclockToSlot relativeTime
      pure $ C.SlotNo $ O.unSlotNo slotNo

    slotStart :: C.SlotNo -> ExceptT ApplyInputsError m UTCTime
    slotStart (C.SlotNo slotNo) = do
      (relativeTime, _) <-
        except $
          first (SlotConversionFailed . show) $
            interpretQuery interpreter $
              slotToWallclock $
                O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

    utcToPOSIXTime :: UTCTime -> PV2.POSIXTime
    utcToPOSIXTime = PV2.POSIXTime . floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

    posixTimeToUTCTime :: PV2.POSIXTime -> UTCTime
    posixTimeToUTCTime (PV2.POSIXTime t) = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger t / 1000

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
  :: forall m era v
   . (Monad m)
  => TxConstraints.PayoutContext
  -- ^ The payout context for the current transaction.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> Set TxOutRef
  -- ^ The payouts to withdraw
  -> ExceptT WithdrawError m (Map TxOutRef (Payout v), TxConstraints era v)
buildWithdrawConstraints TxConstraints.PayoutContext{..} = \case
  MarloweV1 -> buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1
      :: Set TxOutRef -> ExceptT WithdrawError m (Map TxOutRef (Payout 'V1), TxConstraints era 'V1)
    buildWithdrawConstraintsV1 payouts = runWriterT do
      when (Set.null payouts) $ lift $ throwE EmptyPayouts
      let payoutsList = Set.toAscList payouts
      traverse_ (tell . TxConstraints.mustConsumePayout) payoutsList
      Map.fromDistinctAscList <$> for payoutsList \payoutRef -> do
        let notFoundError = WithdrawConstraintError $ TxConstraints.PayoutNotFound payoutRef
        TransactionOutput{..} <- lift $ except $ note notFoundError $ Map.lookup payoutRef payoutOutputs
        let invalidError = WithdrawConstraintError $ TxConstraints.InvalidPayoutDatum payoutRef datum
        roleToken <- lift $ except $ note invalidError $ fromChainPayoutDatum MarloweV1 =<< datum
        tell $ mustSpendRoleToken roleToken
        pure (payoutRef, Payout address assets roleToken)
