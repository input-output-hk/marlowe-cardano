{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Client where
import Cardano.Api (AddressInEra (..), PaymentCredential (..), SerialiseAsRawBytes (serialiseToRawBytes), ShelleyEra,
                    StakeAddressReference (..))
import Cardano.Api.Shelley (StakeCredential (..))
import qualified Cardano.Api.Shelley as Shelley
import Control.Category ((<<<), (>>>))
import Control.Lens
import Control.Monad (forM_, guard, void, when)
import Control.Monad.Error.Lens (catching, handling, throwing, throwing_)
import Control.Monad.Extra (concatMapM)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Monoid (Last)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Marlowe.Client.History (History (..), MarloweTxOutRef, RolePayout (..), foldlHistory, marloweHistory,
                                        marloweHistoryFrom, marloweUtxoStatesAt, toMarloweState, toRolePayout,
                                        txRoleData)
import Language.Marlowe.Scripts
import Language.Marlowe.Semantics
import qualified Language.Marlowe.Semantics as Marlowe
import Language.Marlowe.Semantics.Types hiding (Contract, getAction)
import qualified Language.Marlowe.Semantics.Types as Marlowe
import Language.Marlowe.Util (extractNonMerkleizedContractRoles)
import Ledger (CurrencySymbol, Datum (..), POSIXTime (..), PaymentPubKeyHash (..), PubKeyHash (..), TokenName,
               TxOut (..), TxOutRef (txOutRefId), dataHash, txOutValue)
import qualified Ledger
import Ledger.Ada (adaSymbol, adaToken, adaValueOf, lovelaceValueOf)
import Ledger.Address (Address, StakePubKeyHash (StakePubKeyHash), pubKeyHashAddress, scriptHashAddress)
import Ledger.Constraints
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Interval as Interval
import Ledger.Scripts (datumHash, unitRedeemer)
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange, scSlotLength, slotToPOSIXTimeRange)
import Ledger.Tx (txId)
import qualified Ledger.Tx as Tx
import Ledger.Typed.Scripts
import qualified Ledger.Typed.Scripts as Typed
import qualified Ledger.Typed.Tx as Typed
import qualified Ledger.Value as Val
import Numeric.Natural (Natural)
import Plutus.ChainIndex (ChainIndexTx (..), Page, PageQuery, _ValidTx, citxOutputs, citxTxId, nextPageQuery, pageItems)
import Plutus.ChainIndex.Api (paget)
import Plutus.Contract as Contract hiding (OtherContractError, _OtherContractError)
import qualified Plutus.Contract as Contract (ContractError (..))
import Plutus.Contract.Request (txoRefsAt, txsFromTxIds)
import Plutus.Contract.Unsafe (unsafeGetSlotConfig)
import Plutus.Contract.Wallet (getUnspentOutput)
import qualified Plutus.Contracts.Currency as Currency
import Plutus.V1.Ledger.Api (toBuiltin)
import PlutusPrelude (foldMapM, (<|>))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as P
import PlutusTx.Traversable (for)



data MarloweClientInput = ClientInput InputContent
                        | ClientMerkleizedInput InputContent Marlowe.Contract
  deriving stock (Eq, Show, Generic)

instance FromJSON MarloweClientInput where
  parseJSON json = uncurry ClientMerkleizedInput <$> parseJSON json <|> ClientInput <$> parseJSON json

instance ToJSON MarloweClientInput where
  toJSON (ClientInput content)                    = toJSON content
  toJSON (ClientMerkleizedInput content contract) = toJSON (content, contract)


type CreateEndpointSchema = (UUID, AssocMap.Map Val.TokenName (AddressInEra ShelleyEra), Marlowe.Contract)
type ApplyInputsEndpointSchema = (UUID, MarloweParams, Maybe TimeInterval, [MarloweClientInput])
type ApplyInputsNonMerkleizedEndpointSchema = (UUID, MarloweParams, Maybe TimeInterval, [InputContent])
type AutoEndpointSchema = (UUID, MarloweParams, Party, POSIXTime)
type RedeemEndpointSchema = (UUID, MarloweParams, TokenName, AddressInEra ShelleyEra)
type CloseEndpointSchema = UUID

type MarloweSchema =
        Endpoint "create" CreateEndpointSchema
        .\/ Endpoint "apply-inputs" ApplyInputsEndpointSchema
        .\/ Endpoint "apply-inputs-nonmerkleized" ApplyInputsNonMerkleizedEndpointSchema
        .\/ Endpoint "auto" AutoEndpointSchema
        .\/ Endpoint "redeem" RedeemEndpointSchema
        .\/ Endpoint "close" CloseEndpointSchema


data MarloweEndpointResult =
    CreateResponse MarloweParams
    | ApplyInputsResponse
    | AutoResponse
    | RedeemResponse
    | CloseResponse
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

type MarloweCompanionSchema = EmptySchema
type MarloweFollowSchema = Endpoint "follow" MarloweParams


data MarloweError =
      TransitionError
    | AmbiguousOnChainState
    | UnableToExtractTransition
    | OnChainStateNotFound
    | MarloweEvaluationError TransactionError
    | OtherContractError ContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


makeClassyPrisms ''MarloweError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError

instance AsCheckpointError MarloweError where
    _CheckpointError = _OtherContractError . _CheckpointError

data PartyAction
             = PayDeposit AccountId Party Token Integer
             | WaitForTimeout POSIXTime
             | WaitOtherActionUntil POSIXTime
             | NotSure
             | CloseContract
  deriving (Show)

type RoleOwners = AssocMap.Map Val.TokenName (AddressInEra ShelleyEra)

-- FIXME: We should probably switch to the plain `FollowerContractState` here
-- (which is just `(Maybe MarloweHistory, UnspentPayouts)`)
-- so we can capture and report all the possible on chain states.
-- Now we are not able to notify about role payouts before the contract is on the chain.
data ContractHistory =
    ContractHistory
        { chParams         :: MarloweParams      -- ^ The "instance id" of the contract
        , chInitialData    :: MarloweData        -- ^ The initial Contract + State
        , chHistory        :: [TransactionInput] -- ^ All the transaction that affected the contract.
                                                 --   The current state and intermediate states can
                                                 --   be recalculated by using computeTransaction
                                                 --   of each TransactionInput to the initial state
        , chAddress        :: Address            -- ^ The script address of the marlowe contract
        , chUnspentPayouts :: UnspentPayouts     -- ^ All UTxOs associated with our payout script.
                                                 --   Please note that in theory we include here outpus
                                                 --   which possible were created by an "external" transactions.
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)

-- We need a semigroup instance to be able to update the state of the FollowerContract via `tell`.
-- For most of the fields we just use the initial values as they are not expected to change,
-- and we only concatenate new TransactionInputs
instance Semigroup ContractHistory where
    _ <> last  = last

-- The FollowerContractNotification is a Maybe because the Contract monad requires the state
-- to have a Monoid instance. `Nothing` is the initial state of the contract, and then
-- with the first `tell` we have a valid initial ContractHistory
type FollowerContractNotification = Maybe ContractHistory

newtype OnChainState = OnChainState {ocsTxOutRef :: MarloweTxOutRef}
    deriving stock (Generic, Eq)

newtype Transition = Transition History     -- ^ The state machine instance transitioned to a new state
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PayoutsChange
              = PayoutSpent ChainIndexTx
              | PayoutProduced (NonEmpty ChainIndexTx)
              deriving stock (Show, Generic)


-- We need full payouts set to better diff on chain changes
newtype Redeemed = Redeemed { redeemed :: Bool }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Payouts = Payouts [(RolePayout, Redeemed)]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (Semigroup, Monoid)

-- We expose unordered payouts set so we can compute this state
-- from the chain easily and we can avoid traversing the full history.
newtype UnspentPayouts = UnspentPayouts [RolePayout]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (Semigroup, Monoid)

fromPayouts :: Payouts -> UnspentPayouts
fromPayouts (Payouts p) = UnspentPayouts <<< map fst <<< filter (not <<< redeemed <<< snd) $ p

data ContractProgress = InProgress | Finished
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup ContractProgress where
    _ <> Finished     = Finished
    any <> InProgress = any

instance Monoid ContractProgress where
    mempty = InProgress

type EndpointName = String

data EndpointResponse a err =
    EndpointSuccess UUID a
    -- TODO: The EndpointName should be a part of `err` if
    --       the user decides to, but we need to refactor MarloweError and
    --       the Marlowe Plutus App, so I leave this for a separate PR.
    | EndpointException UUID EndpointName err
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

-- The Semigroup instance is thought so that when we call `tell`, we inform
-- the FrontEnd of the last response. It is the responsability of the FE to
-- tie together a request and a response with the UUID.
instance Semigroup (EndpointResponse a err) where
    _ <> last      = last

type MarloweEndpointResponse = EndpointResponse MarloweEndpointResult MarloweError

type MarloweContractState = Maybe MarloweEndpointResponse

mkMarloweTypedValidator :: MarloweParams -> SmallTypedValidator
mkMarloweTypedValidator = smallUntypedValidator

minLovelaceDeposit :: Integer
minLovelaceDeposit = 2_000_000

debugMsg :: String -> String -> String
debugMsg fnName msg = "[DEBUG:" <> fnName <> "] " <> msg

-- TODO: Move to debug log.
debug :: forall st sc err. String -> String -> Contract st sc err ()
debug fnName msg = logDebug $ debugMsg fnName msg

-- | During first pass the counter equals to 0 - first pass is not a retry
newtype RetryCounter = RetryCounter Int
newtype MaxRetries = MaxRetries Int

retryTillJust :: Monad m => MaxRetries -> (RetryCounter -> m (Maybe a)) -> m (Maybe a)
retryTillJust (MaxRetries maxRetries) action = go 0
  where
    go cnt
      | maxRetries <= cnt = pure Nothing
      | otherwise = do
          (action $ RetryCounter cnt) >>= \case
            Nothing -> go (cnt + 1)
            res     -> pure res

-- | Our retries defaults
pollingInterval :: Natural
pollingInterval = 2

maxRetries :: MaxRetries
maxRetries = MaxRetries 4

newtype CallStackTrace = CallStackTrace [String]
  deriving newtype (Semigroup, Monoid)

pushFnName :: String -> CallStackTrace -> CallStackTrace
pushFnName fn (CallStackTrace st) = CallStackTrace (fn : st)

printCallStackTrace :: CallStackTrace -> String
printCallStackTrace (CallStackTrace trace) = show $ intercalate ":" trace

debugTrace :: CallStackTrace -> String -> Contract st sc err ()
debugTrace trace =
  debug (printCallStackTrace trace)


-- | The same as above but specializd to the PAB Contract monad with
-- | constant delay between retries.
-- |
-- | Used to do polling of the PAB because we have to
-- | wait till chain index catches up with
-- | the recent responses from the cardano-node (PAB STM)
retryRequestTillJust :: AsContractError err => CallStackTrace -> MaxRetries -> (RetryCounter -> Contract st sc err (Maybe a)) -> Contract st sc err (Maybe a)
retryRequestTillJust trace maxRetries query = do
  retryTillJust maxRetries $ \cnt@(RetryCounter cntVal) -> do
    when (cntVal > 0) $ do
      debugTrace (pushFnName "retryRequestTillJust" trace) $ "Still waiting for desired change - iteration: " <> show cntVal
      void $ waitNSlots pollingInterval
    query cnt

retryRequestTillJust' :: AsContractError err => CallStackTrace -> Contract st sc err (Maybe a) -> Contract st sc err (Maybe a)
retryRequestTillJust' trace action = retryRequestTillJust trace maxRetries (const action)

retryTillDiffers :: Monad m => Eq a => MaxRetries -> a -> (RetryCounter -> m a) -> m (Maybe a)
retryTillDiffers maxRetries known action = do
  retryTillJust maxRetries $ \cnt -> do
    new <- action cnt
    if new == known
      then pure Nothing
      else pure $ Just new

-- | The same as above but specializd to the PAB Contract monad with
-- | constant delay between retries.
retryTillResponseDiffers :: Eq a => AsContractError err => CallStackTrace -> MaxRetries -> a -> (RetryCounter -> Contract st sc err a) -> Contract st sc err (Maybe a)
retryTillResponseDiffers trace maxRetries known query = do
  retryTillDiffers maxRetries known $ \cnt@(RetryCounter cntVal) -> do
    when (cntVal > 0) $ do
      debugTrace (pushFnName "retryTillResponseDiffers" trace) $ "Still waiting for desired change - iteration: " <> show cntVal
      void $ waitNSlots pollingInterval
    query cnt

retryTillResponseDiffers' :: Eq a => AsContractError err => CallStackTrace -> a -> Contract st sc err a -> Contract st sc err (Maybe a)
retryTillResponseDiffers' trace a query = retryTillResponseDiffers trace maxRetries a (const query)

-- | Queries which perform some extra polling to possibly sync the chain index
awaitTxConfirmed' :: AsContractError e => CallStackTrace -> MaxRetries -> Ledger.TxId -> Contract w s e Bool
awaitTxConfirmed' trace maxRetries txId = do
  awaitTxConfirmed txId
  fmap isJust $ retryRequestTillJust (pushFnName "awaitTxConfirmed'" trace)  maxRetries $ const (listToMaybe <$> txsFromTxIds [txId])

awaitUtxoProduced' :: AsContractError e => CallStackTrace -> Address -> Contract w s e (NonEmpty ChainIndexTx)
awaitUtxoProduced' trace addr = do
  prev <- utxosAt addr
  txns <- awaitUtxoProduced addr
  void $ retryTillResponseDiffers' (pushFnName "awaitUtxoProduced'" trace) prev (utxosAt addr)
  pure txns

utxoIsProduced' :: AsContractError e => CallStackTrace -> Address -> Promise w s e (NonEmpty ChainIndexTx)
utxoIsProduced' trace addr = do
  promiseBind (utxoIsProduced addr) $ \txns -> do
    void $ retryRequestTillJust' (pushFnName "utxoIsProduced'" trace)  $ do
      for_ (NonEmpty.toList txns) $ \tx -> do
        listToMaybe <$> txsFromTxIds [ tx ^. citxTxId ]
      pure $ Just txns
    -- FIXME: We should just throw here and report inconsistency problem
    --        when the previous check returns `Nothing`.
    pure txns

utxoIsSpent' :: AsContractError e => CallStackTrace -> TxOutRef -> Promise w s e ChainIndexTx
utxoIsSpent' trace utxo = promiseBind (utxoIsSpent utxo) $ \tx -> do
  void $ retryRequestTillJust' (pushFnName "utxoIsSpent'" trace)  $ listToMaybe <$> txsFromTxIds [ tx ^. citxTxId ]
  pure tx


-- | Trivial data type (a bit more redable than `Maybe`) which helps fully embed contract into `checkpointLoop`
data QueryResult a
  = UnknownOnChainState
  | LastResult a
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

type FollowerM a = Contract FollowerContractNotification MarloweFollowSchema MarloweError a

type FollowerPromise a = Promise FollowerContractNotification MarloweFollowSchema MarloweError a

-- In theory we can have role payouts "outside" of the contract - our payout query
-- doesn't prevent that (we filter just on the payout script).
type FollowerContractState = (Maybe History, Payouts)

data FollowerContractUpdate = PayoutChange | HistoryChange

-- Follower puts a single `null` to the websocket automatically.
-- You can expect another `null` (so two `null`s) if you start the follower
-- before the actual contract is on the chain.
marloweFollowContract :: FollowerM ()
marloweFollowContract = awaitPromise $ endpoint @"follow" $ \params ->
  do
    debug' $ "call parameters: " <> show params <> "."

    let
      printState (Just history, payouts) = "{ inputs = Just" <> show (foldInputs history) <> ", " <> "payouts = " <> show payouts <> "}"
      printState (Nothing, payouts) = "{ inputs = Nothing," <> "payouts = " <> show payouts <> "}"

      fetchOnChainState :: FollowerM FollowerContractState
      fetchOnChainState = (,) <$> marloweHistory unsafeGetSlotConfig params <*> payoutsAtCurrency (rolesCurrency params)

      awaitNewState :: FollowerContractState -> FollowerM FollowerContractState
      awaitNewState (prevHistory, prevPayouts) = do

        let
          -- In both cases we bring back one of the transactions which have woken us up.
          -- We do this only to perform the logging.
          waitForContractChange :: FollowerPromise ChainIndexTx
          waitForContractChange = case prevHistory >>= continuationUtxo of
            Nothing   -> NonEmpty.head <$> (utxoIsProduced' trace $ validatorAddress $ mkMarloweTypedValidator params)
            Just utxo -> utxoIsSpent' trace utxo
            where
              trace = CallStackTrace ["waitForContractChange", "awaitNewState", "marloweFollowContract"]

          waitForPayoutChange :: FollowerPromise ChainIndexTx
          waitForPayoutChange =
            let
              payoutScriptAddress = scriptHashAddress $ mkRolePayoutValidatorHash $ rolesCurrency params
              UnspentPayouts payouts = fromPayouts prevPayouts
              trace = CallStackTrace ["waitForPayoutChange", "awaitNewState", "marloweFollowContract"]

              waitTillPayoutIsSpent = fmap (utxoIsSpent' trace <<< rolePayoutTxOutRef) payouts
              waitTillPayoutIsProduced = NonEmpty.head <$> utxoIsProduced' trace payoutScriptAddress
            in
              raceList $ waitTillPayoutIsProduced : waitTillPayoutIsSpent

        -- We are here notified that there should be a new state on the chain...
        changeNotification <- awaitPromise
          (selectEither waitForPayoutChange waitForContractChange)

        debug' $ either
          (mappend "Payout change detected through txId =" <<< show <<< _citxTxId)
          (mappend "Contract change detected through txId " <<< show <<< _citxTxId)
          changeNotification

        fetchOnChainState

      -- Push a possible state update to the stream
      notify :: FollowerContractState -> FollowerM ()
      notify st@(Just history, payouts) = do
        debug' $ "notifying new state = " <> printState st
        debug' $ "status = " <> show (status history)
        case history of
          Created {historyData} -> do
            tell @FollowerContractNotification $
              Just $ mkContractHistory params historyData (foldInputs history) (fromPayouts payouts)
            pure ()
          _ -> do
            throwError $ OtherContractError $ Contract.OtherContractError $ "Invalid history trace head found: " <> T.pack (show history)
      notify (Nothing, _) = do
        debug' "notifying about empty state"
        tell @FollowerContractNotification $ Nothing

      -- In `checkpointLoop` we tail rec by returning `Right` ~ `whileRight` loop.
      rec :: forall a err sc st w. st -> Contract w sc err (Either a st)
      rec st = pure $ Right st

      -- `follower` is a loop which iterates over the on chain updates:
      --  * we use simple `QueryResult` wrapper so *every* query is wrapped in the `checkpointLoop`
      --  * we pass in it the last known state and put it to the stream
      --  * we try to use only previous state pieces when constrcuting async requests
      --  * we wait for the changes on the chain
      --  * we ask (up to `maxRetries * pollingInterval`) the chain index for the update
      --    till it actually provides the new state by using local versions of queries
      --    which perform active "sync check polling"
      --  * we loop back (by returning `Right`) with the new state.
      follow :: QueryResult FollowerContractState -> FollowerM (Either () (QueryResult FollowerContractState))
      follow UnknownOnChainState = do
        debug' "Staring follower loop..."
        currOnChainState <- fetchOnChainState
        notify currOnChainState
        rec $ LastResult currOnChainState

      follow (LastResult prevState) = do
        newState <- awaitNewState prevState
        if newState /= prevState
          then do
            notify newState
            rec $ LastResult newState
          else do
            debug' $ "Unable to detect new state. prevState = " <> printState prevState
            rec $ LastResult prevState

    checkpointLoop follow UnknownOnChainState
  where
    debug' = debug "Language.Marlowe.Client.marloweFollowContract"

    isClosed = last >>> \case
      Closed {} -> True
      _         -> False

    status history = if isClosed history then Finished else InProgress

    last h = foldlHistory step h h
      where
        step _ next = next

    continuationUtxo = last >>> \case
      Created { historyTxOutRef }      -> Just historyTxOutRef
      InputApplied { historyTxOutRef } -> Just historyTxOutRef
      _                                -> Nothing

    foldInputs = reverse <<< foldlHistory step []
      where
        step acc InputApplied {historyInput} = historyInput : acc
        step acc Closed{historyInput}        = historyInput : acc
        step acc _                           = acc

    mkContractHistory params historyData inputs payouts = ContractHistory
      { chParams = params
      , chInitialData = historyData
      , chHistory = inputs
      , chAddress = validatorAddress $ mkMarloweTypedValidator params
      , chUnspentPayouts = payouts
      }

newtype MaxPages = MaxPages Int

-- | `Left` means that there are more pages to grab...
txOutRefsAt ::
    forall w s e.
    ( AsContractError e
    )
    => Address
    -> MaxPages
    -> Contract w s e (Either ([TxOutRef], PageQuery TxOutRef) [TxOutRef])
txOutRefsAt _ (MaxPages maxPages) | maxPages <= 0 = pure $ Right []
txOutRefsAt addr (MaxPages maxPages) = go 1 [] (Just def)
  where
    go _ acc Nothing = pure $ Right acc
    go pn acc (Just pq) = do
      page <- paget <$> txoRefsAt pq addr
      let
        acc' = acc <> pageItems page
        next = nextPageQuery page
      case (pn == maxPages, next) of
        (True, Just pq) -> pure $ Left (acc', pq)
        _               -> go (pn + 1) acc' next

payoutsAtCurrency :: AsContractError e
                  => CurrencySymbol
                  -> Contract w s e Payouts
payoutsAtCurrency rolesCurrency = do
  let
    address = scriptHashAddress $ mkRolePayoutValidatorHash rolesCurrency
  utxosRefs <- Map.keys <$> utxosTxOutTxAt address
  -- FIXME: We should notify through the API that we have possibly more payouts on the chain and not just
  --        ignore our payouts buffer overflow here.
  txoutRefs <- txOutRefsAt address (MaxPages 30) <&> \case
    Left (items, _) -> items
    Right items     -> items
  let
    txids = map txOutRefId txoutRefs
  txs <- txsFromTxIds txids
  let
    rolePayoutTxs = concatMap txRoleData txs
    rolePayouts = map toRolePayout rolePayoutTxs
    markRedeemed r@RolePayout { rolePayoutTxOutRef } =
      (r, Redeemed (rolePayoutTxOutRef `notElem` utxosRefs))
  pure $ Payouts <<< map markRedeemed $ rolePayouts

unspentPayoutsAtCurrency :: AsContractError e
                         => CurrencySymbol
                         -> Contract w s e UnspentPayouts
unspentPayoutsAtCurrency rolesCurrency = do
  let
    address = scriptHashAddress $ mkRolePayoutValidatorHash rolesCurrency
  txs <- do
    utxosMap <- utxosTxOutTxAt address
    pure $ fmap snd $ Map.elems utxosMap
  let
    rolePayoutTxs = concatMap txRoleData txs
  pure $ UnspentPayouts . map toRolePayout $ rolePayoutTxs


{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = selectList [create, apply, applyNonmerkleized, auto, redeem, close]
  where
    debug' endpoint msg = debug ("Language.Marlowe.Client.marlowePlutusContract:" <> endpoint) msg
    catchError reqId endpointName handler = catching _MarloweError
        (void $ mapError (review _MarloweError) handler)
        (\err -> do
            logWarn $ "Error " <> show err
            tell $ Just $ EndpointException reqId endpointName err
            marlowePlutusContract)
    -- [UC-CONTRACT-1][1] Start a new marlowe contract
    create = endpoint @"create" $ \(reqId, owners, contract) -> catchError reqId "create" $ do
        let
          debug'' = debug' "create"
        debug'' $ "slotConfig = " <> show unsafeGetSlotConfig
        -- Create a transaction with the role tokens and pay them to the contract creator
        -- See Note [The contract is not ready]
        ownPubKey <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
        debug'' $ "ownPubKey = " <> show ownPubKey
        let roles = extractNonMerkleizedContractRoles contract
        debug'' $ "roles = " <> show roles
        (params, distributeRoleTokens, lkps) <- setupMarloweParams owners roles
        debug'' $ "params = " <> show params
        time <- currentTime
        debug'' $ "Marlowe contract created with parameters: " <> show params <> " at " <> show time
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = State
                    { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) minLovelaceDeposit
                    , choices  = AssocMap.empty
                    , boundValues = AssocMap.empty
                    , minTime = time } }
        debug'' $ "marloweData = " <> show marloweData
        let minAdaTxOut = lovelaceValueOf minLovelaceDeposit
        let typedValidator = mkMarloweTypedValidator params
        let tx = mustPayToTheScript marloweData minAdaTxOut <> distributeRoleTokens
        debug'' $ "tx = " <> show tx
        let lookups = Constraints.typedValidatorLookups typedValidator <> lkps
        debug'' $ "lookups = " <> show lookups
        -- Create the Marlowe contract and pay the role tokens to the owners
        utx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx lookups tx)
        debug'' $ "utx = " <> show utx
        btx <- balanceTx $ Constraints.adjustUnbalancedTx utx
        debug'' $ "btx = " <> show btx
        stx <- submitBalancedTx btx
        debug'' $ "stx = " <> show stx
        let txId = Tx.getCardanoTxId stx
        debug'' $ "txId = " <> show txId
        confirmed <- awaitTxConfirmed' (CallStackTrace ["create", "marlowePlutusContract"]) (MaxRetries 3) txId
        if confirmed
          then do
            debug'' $ "MarloweApp contract creation confirmed for parameters " <> show params <> "."
            tell $ Just $ EndpointSuccess reqId $ CreateResponse params
            marlowePlutusContract
          else do
            debug'' $ "MarloweApp contract creation failed for parameters " <> show params <> "."
            -- TODO: Introduce custom error value
            throwError $ OtherContractError $ Contract.OtherContractError "MarloweApp contract creation failed"
    apply = endpoint @"apply-inputs" $ \(reqId, params, timeInterval, inputs) -> catchError reqId "apply-inputs" $ do
        let
          debug'' = debug' "apply-inputs"
        debug'' $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator timeInterval inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        debug'' $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    applyNonmerkleized = endpoint @"apply-inputs-nonmerkleized" $ \(reqId, params, timeInterval, inputs) -> catchError reqId "apply-inputs-nonmerkleized" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator timeInterval $ ClientInput <$> inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        debug' "apply-inputs-nonmerkleized" $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
    redeem = promiseMap (mapError (review _MarloweError)) $ endpoint @"redeem" $ \(reqId, MarloweParams{rolesCurrency}, role, paymentAddress) -> catchError reqId "redeem" $ do
        let
          debug'' = debug' "redeem"
        debug'' $ "rolesCurrency = " <> show rolesCurrency
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        debug'' $ "address = " <> show address
        utxos <- utxosAt address
        let
          spendable txout =
            let
              expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
              dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
            in
              dh == Just expectedDatumHash
          utxosToSpend = Map.filter spendable utxos
          spendPayoutConstraints tx ref txout =
            do
              let amount = view Ledger.ciTxOutValue txout
              previousConstraints <- tx
              payOwner <- mustPayToShelleyAddress paymentAddress amount
              pure
                $ previousConstraints
                <> payOwner -- pay to a token owner
                <> Constraints.mustSpendScriptOutput ref unitRedeemer -- spend the rolePayoutScript address

        spendPayouts <- Map.foldlWithKey spendPayoutConstraints (pure mempty) utxosToSpend
        if spendPayouts == mempty
        then do
            debug'' $ "MarloweApp contract redemption empty for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse
        else do
            let
              constraints = spendPayouts
                  -- must spend a role token for authorization
                  <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
              -- lookup for payout validator and role payouts
              validator = rolePayoutScript rolesCurrency
            debug'' $ "constraints = " <> show constraints
            ownAddressLookups <- ownShelleyAddress paymentAddress
            let
              lookups = Constraints.otherScript validator
                  <> Constraints.unspentOutputs utxosToSpend
                  <> ownAddressLookups
            debug'' $ "lookups = " <> show lookups
            tx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx @Void lookups constraints)
            debug'' $ "tx = " <> show tx
            _ <- submitTxConfirmed $ Constraints.adjustUnbalancedTx tx
            debug'' $ "MarloweApp contract redemption confirmed for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse

        marlowePlutusContract
    auto = endpoint @"auto" $ \(reqId, params, party, untilTime) -> catchError reqId "auto" $ do
        let typedValidator = mkMarloweTypedValidator params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                then autoExecuteContract reqId params typedValidator party md
                else do
                    tell $ Just $ EndpointSuccess reqId AutoResponse
                    marlowePlutusContract

        maybeState <- getOnChainState typedValidator
        case maybeState of
            Nothing ->
                waitForTimeoutOrTransition typedValidator untilTime >>= \case
                    Left _ -> do
                        logInfo $ "Contract Timeout for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition Closed{}) -> do
                        logInfo $ "Contract Ended for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition InputApplied{historyData}) -> continueWith historyData
                    Right (Transition Created{historyData}) -> continueWith historyData
            Just OnChainState{ocsTxOutRef=st} -> do
                let marloweData = toMarloweState st
                continueWith marloweData
    -- The MarloweApp contract is closed implicitly by not returning
    -- itself (marlowePlutusContract) as a continuation
    close = endpoint @"close" $ \reqId -> tell $ Just $ EndpointSuccess reqId CloseResponse


    autoExecuteContract :: UUID
                      -> MarloweParams
                      -> SmallTypedValidator
                      -> Party
                      -> MarloweData
                      -> Contract MarloweContractState MarloweSchema MarloweError ()
    autoExecuteContract reqId params typedValidator party marloweData = do
        time <- currentTime
        let timeRange = (time, time + defaultTxValidationRange)
        let action = getAction timeRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo $ "PayDeposit " <> show amount <> " at within time " <> show timeRange
                let payDeposit = do
                        marloweData <- mkStep params typedValidator timeRange [ClientInput $ IDeposit acc p token amount]
                        continueWith marloweData
                catching _MarloweError payDeposit $ \err -> do
                    logWarn $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 seconds"
                    _ <- awaitTime (time + 2_000)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo $ "WaitForTimeout " <> show timeout
                _ <- awaitTime timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo $ "WaitOtherActionUntil " <> show timeout
                waitForTimeoutOrTransition typedValidator timeout >>= \case
                    Left _ -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    Right (Transition Closed{}) -> do
                        logInfo @String $ "Contract Ended"
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Right (Transition InputApplied{historyData}) -> continueWith historyData
                    Right (Transition Created{historyData}) -> continueWith historyData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                let closeContract = do
                        _ <- mkStep params typedValidator timeRange []
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract

                catching _MarloweError closeContract $ \err -> do
                    logWarn $ "Error " <> show err
                    logInfo @String $ "Retry CloseContract in 2 seconds"
                    _ <- awaitTime (time + 2000)
                    continueWith marloweData
            NotSure -> do
                logInfo @String $ "NotSure"
                tell $ Just $ EndpointSuccess reqId AutoResponse
                marlowePlutusContract

          where
            continueWith = autoExecuteContract reqId params typedValidator party


setupMarloweParams
    :: forall s e i o a.
    (AsMarloweError e)
    => RoleOwners
    -> Set Val.TokenName
    -> Contract MarloweContractState s e
        (MarloweParams, TxConstraints i o, ScriptLookups a)
setupMarloweParams owners roles = mapError (review _MarloweError) $
    if Set.null roles
    then do
        let params = marloweParams adaSymbol
        pure (params, mempty, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = (, 1) <$> Set.toList roles
        txOutRef@(Ledger.TxOutRef h i) <- getUnspentOutput
        -- TODO: Move to debug log.
        debug "setupMarloweParams" $ "txOutRef = " <> show txOutRef
        txOut <-
          maybe
            (throwing _ContractError . Contract.OtherContractError . T.pack $ show txOutRef <> " was not found on the chain index. Please verify that plutus-chain-index is 100% synced.")
            pure
            =<< txOutFromRef txOutRef
        -- TODO: Move to debug log.
        debug "setupMarloweParams" $ "txOut = " <> show txOut
        let utxo = Map.singleton txOutRef txOut
        let theCurrency = Currency.OneShotCurrency
                { curRefTransactionOutput = (h, i)
                , curAmounts              = AssocMap.fromList tokens
                }
            curVali     = Currency.curPolicy theCurrency
            lookups     = Constraints.mintingPolicy curVali
                            <> Constraints.unspentOutputs utxo
            mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                            <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
        let rolesSymbol = Ledger.scriptCurrencySymbol curVali
        let minAdaTxOut = adaValueOf 2
        let giveToParty (role, addr) =
              mustPayToShelleyAddress addr (Val.singleton rolesSymbol role 1 <> minAdaTxOut)
        distributeRoleTokens <- foldMapM giveToParty $ AssocMap.toList owners
        let params = marloweParams rolesSymbol
        pure (params, mintTx <> distributeRoleTokens, lookups)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ Contract.OtherContractError message

ownShelleyAddress
  :: AddressInEra ShelleyEra
  -> Contract MarloweContractState s MarloweError (ScriptLookups Void)
ownShelleyAddress addr = Constraints.ownPaymentPubKeyHash . fst <$> shelleyAddressToKeys addr

mustPayToShelleyAddress
  :: AddressInEra ShelleyEra
  -> Val.Value
  -> Contract MarloweContractState s MarloweError (TxConstraints i o)
mustPayToShelleyAddress addr value = do
  (ppkh, skh) <- shelleyAddressToKeys addr
  pure $ ($ value) $ maybe
    (Constraints.mustPayToPubKey ppkh)
    (Constraints.mustPayToPubKeyAddress ppkh)
    skh

shelleyAddressToKeys
  :: AddressInEra ShelleyEra
  -> Contract MarloweContractState s MarloweError (PaymentPubKeyHash, Maybe StakePubKeyHash)
shelleyAddressToKeys (AddressInEra _ (Shelley.ShelleyAddress _ paymentCredential stakeRef)) =
  case Shelley.fromShelleyPaymentCredential paymentCredential of
    PaymentCredentialByScript _ -> throwError $ OtherContractError $ Contract.OtherContractError "Script payment addresses not supported"
    PaymentCredentialByKey hash ->
      let ppkh = PaymentPubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes hash
      in
        case Shelley.fromShelleyStakeReference stakeRef of
          StakeAddressByValue (StakeCredentialByScript _) ->
            throwError $ OtherContractError $ Contract.OtherContractError "Script stake addresses not supported"
          StakeAddressByPointer _ ->
            throwError $ OtherContractError $ Contract.OtherContractError "Pointer stake addresses not supported"
          NoStakeAddress -> pure (ppkh, Nothing)
          StakeAddressByValue (StakeCredentialByKey stakeHash) ->
            pure (ppkh,  Just . StakePubKeyHash . PubKeyHash . toBuiltin $ serialiseToRawBytes stakeHash)
shelleyAddressToKeys _ = throwError $ OtherContractError $ Contract.OtherContractError "Byron Addresses not supported"

getAction :: MarloweTimeRange -> Party -> MarloweData -> PartyAction
getAction timeRange party MarloweData{marloweContract,marloweState} = let
    env = Environment timeRange
    in case reduceContractUntilQuiescent env marloweState marloweContract of
        ContractQuiescent _reduced _warnings _payments state contract ->
            -- here the contract is either When or Close
            case contract of
                When [Case (Deposit acc depositParty tok value) _] _ _
                    | party == depositParty -> let
                        amount = Marlowe.evalValue env state value
                        in PayDeposit acc party tok amount
                When [Case (Deposit _ depositParty _ _) _] timeout _
                    | party /= depositParty    ->
                        WaitOtherActionUntil timeout
                When [] timeout _ -> WaitForTimeout timeout
                Close -> CloseContract
                _ -> NotSure
        -- When timeout is in the time range
        RRAmbiguousTimeIntervalError ->
            {- FIXME
                Consider contract:
                    When [cases] (POSIXTime 100) (When [Case Deposit Close]] (POSIXTime 105) Close)

                For a time range (95, 105) we get RRAmbiguousTimeIntervalError
                because timeout 100 is inside the time range.
                Now, we wait for time 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousTimeIntervalError (i.e. POSIXTime 100).
                Then we'd rather wait until time 100 instead and would make the Deposit.
                I propose to modify RRAmbiguousTimeIntervalError to include the expected timeout.
             -}
            WaitForTimeout (snd timeRange)



canAutoExecuteContractForParty :: Party -> Marlowe.Contract -> Bool
canAutoExecuteContractForParty party = check
  where
    check cont =
        case cont of
            Close                                    -> True
            When [] _ cont                           -> check cont
            When [Case Deposit{} cont] _ timeoutCont -> check cont && check timeoutCont
            When cases _ timeoutCont                 -> all checkCase cases && check timeoutCont
            Pay _ _ _ _ cont                         -> check cont
            If _ c1 c2                               -> check c1 && check c2
            Let _ _ cont                             -> check cont
            Assert _ cont                            -> check cont


    checkCase (Case (Choice (ChoiceId _ p) _) cont) | p /= party = check cont
    checkCase _                                     = False


applyInputs :: AsMarloweError e
    => MarloweParams
    -> SmallTypedValidator
    -> Maybe TimeInterval
    -> [MarloweClientInput]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params typedValidator timeInterval inputs = mapError (review _MarloweError) $ do
    -- Wait until a block is produced, so we have an accurate current time and slot.
    void $ waitNSlots 1
    nowSlot <- currentSlot
    nowTime <- currentTime
    -- TODO: Move to debug log.
    debug "applyInputs" $ "current slot = " <> show nowSlot
    debug "applyInputs" $ "time range for slot = " <> show (slotToPOSIXTimeRange unsafeGetSlotConfig nowSlot)
    debug "applyInputs" $ "current time = " <> show nowTime
    debug "applyInputs" $ "inputs = " <> show inputs
    debug "applyInputs" $ "params = " <> show params
    debug "applyInputs" $ "timeInterval = " <> show timeInterval
    let resolution = scSlotLength unsafeGetSlotConfig
    let floor'   (POSIXTime i) = POSIXTime $ resolution * (i `div` resolution)
    let ceiling' (POSIXTime i) = POSIXTime $ resolution * ((i + resolution - 1) `div` resolution)
    timeRange <- case timeInterval of
            Just (l, h) -> pure (ceiling' l, floor' h)
            Nothing -> do
                time <- currentTime
                pure (ceiling' time, floor' $ time + defaultTxValidationRange)
    -- TODO: Move to debug log.
    debug "applyInputs" $ "timeRange = " <> show timeRange
    let POSIXTime delta = fst timeRange - nowTime
    debug "applyInputs" $ "delta = " <> show delta
    -- Guard against early submission, but only for three minutes.
    when (delta < 180_000)
      . void $ awaitTime $ fst timeRange
    nowSlot' <- currentSlot
    debug "applyInputs" $ "current slot at submission = " <> show nowSlot'
    nowTime' <- currentTime
    debug "applyInputs" $ "current time at submission = " <> show nowTime'
    mkStep params typedValidator timeRange inputs

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency}


defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


newtype CompanionState = CompanionState (Map MarloweParams MarloweData)
  deriving (Eq, Show)
  deriving (Semigroup,Monoid) via (Map MarloweParams MarloweData)

instance ToJSON CompanionState where
    toJSON (CompanionState m) = toJSON $ Map.toList m

instance FromJSON CompanionState where
    parseJSON v = CompanionState . Map.fromList <$> parseJSON v

{-|
    [UC-CONTRACT-2][0] Receive a role token for a marlowe contract

    Contract that monitors a user wallet for receiving a Marlowe role token.
    When it sees that a Marlowe contract exists on chain with a role currency
    of a token the user owns it updates its @CompanionState@
    with contract's @MarloweParams@ and @MarloweData@
-}
marloweCompanionContract :: Contract CompanionState MarloweCompanionSchema MarloweError ()
marloweCompanionContract = checkExistingRoleTokens
  where
    checkExistingRoleTokens = do
        -- Get the existing unspend outputs of the wallet that activated the companion contract
        pkh <- Contract.ownPaymentPubKeyHash
        let ownAddress = pubKeyHashAddress pkh Nothing
        -- Filter those outputs for role tokens and notify the WebSocket subscribers
        -- NOTE: CombinedWSStreamToServer has an API to subscribe to WS notifications
        utxo <- utxosAt ownAddress
        let txOuts = Ledger.toTxOut <$> Map.elems utxo
        forM_ txOuts notifyOnNewContractRoles
        -- This contract will run in a loop forever (because we always return Right)
        -- checking for updates to the UTXO's for a given address.
        -- The contract could be stopped via /contract/<instance>/stop but we are
        -- currently not doing that.
        checkpointLoop (fmap Right <$> checkForUpdates) ownAddress
    checkForUpdates ownAddress = do
        txns <- NonEmpty.toList <$> awaitUtxoProduced' (CallStackTrace ["checkForUpdates", "marloweCompanionContract"]) ownAddress
        debug "checkForUpdates" $ "txns = " <> show txns
        let txOuts = txns >>= view (citxOutputs . _ValidTx)
        debug "checkForUpdates" $ "txOuts = " <> show txOuts
        forM_ txOuts notifyOnNewContractRoles
        pure ownAddress

notifyOnNewContractRoles :: TxOut
    -> Contract CompanionState MarloweCompanionSchema MarloweError ()
notifyOnNewContractRoles txout = do
    -- Filter the CurrencySymbol's of this transaction output that might be
    -- a role token symbol. Basically, any non-ADA symbols is a prospect to
    -- to be a role token, but it could also be an NFT for example.
    let curSymbols = filterRoles txout
    debug "notifyOnNewContractRoles" $ "curSymbols = " <> show curSymbols
    forM_ curSymbols $ \cs -> do
        debug "notifyOnNewContractRoles" $ "cs = " <> show cs
        -- Check if there is a Marlowe contract on chain that uses this currency
        contract <- findMarloweContractsOnChainByRoleCurrency cs
        debug "notifyOnNewContractRoles" $ "contract = " <> show contract
        case contract of
            Just (params, md) -> do
                logInfo $ "WalletCompanion found currency symbol " <> show cs <> " with on-chain state " <> show (params, md) <> "."
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
            -- The result will be empty if:
            --   * Note [The contract is not ready]: When you create a Marlowe contract first we create
            --                                       the role tokens, pay them to the contract creator and
            --                                       then we create the Marlowe contract.
            --   * If the marlowe contract is closed.
                -- TODO: Change for debug
                logWarn $ "WalletCompanion found currency symbol " <> show cs <> " but no on-chain state."
                pure ()


filterRoles :: TxOut -> [CurrencySymbol]
filterRoles TxOut { txOutValue, txOutDatumHash = Nothing } =
    let curSymbols = filter (/= adaSymbol) $ AssocMap.keys $ Val.getValue txOutValue
    in  curSymbols
filterRoles _ = []


findMarloweContractsOnChainByRoleCurrency
    :: CurrencySymbol
    -> Contract CompanionState
                MarloweCompanionSchema
                MarloweError
                (Maybe (MarloweParams, MarloweData))
findMarloweContractsOnChainByRoleCurrency curSym = do
    let params = marloweParams curSym
    let typedValidator = mkMarloweTypedValidator params
    maybeState <- handling _AmbiguousOnChainState (const $ pure Nothing) $ getOnChainState typedValidator
    case maybeState of
        Just OnChainState{ocsTxOutRef} -> do
            let marloweData = toMarloweState ocsTxOutRef
            pure $ Just (params, marloweData)
        Nothing -> pure Nothing


getOnChainStateTxOuts ::
    SmallTypedValidator
    -> Contract w schema MarloweError (Maybe (OnChainState, Map Ledger.TxOutRef Tx.ChainIndexTxOut))
getOnChainStateTxOuts validator = do
    (outRefs, utxos) <- mapError (review _MarloweError) $ marloweUtxoStatesAt validator
    case outRefs of
        []       -> do
          debug "Language.Marlowe.Client.getOnChainState" "No state found on the chain"
          pure Nothing
        [outRef] -> pure $ Just (OnChainState outRef, utxos)
        _        -> do
          debug "Language.Marlowe.Client.getOnChainState" "Multiple Marlowe UTxOs found"
          throwing_ _AmbiguousOnChainState

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
getOnChainState ::
    SmallTypedValidator
    -> Contract w schema MarloweError (Maybe OnChainState)
getOnChainState validator = fmap fst <$> getOnChainStateTxOuts validator

mkStep ::
    MarloweParams
    -> SmallTypedValidator
    -> TimeInterval
    -> [MarloweClientInput]
    -> Contract w MarloweSchema MarloweError MarloweData
mkStep MarloweParams{..} typedValidator timeInterval@(minTime, maxTime) clientInputs = do
    debug "mkStep" $ "clientInputs = " <> show clientInputs
    let
      times =
        Interval.Interval
          (Interval.LowerBound (Interval.Finite minTime) True )
          (Interval.UpperBound (Interval.Finite maxTime) False)
      range' =
        posixTimeRangeToContainedSlotRange
          unsafeGetSlotConfig
          times
    maybeState <- retryRequestTillJust' (CallStackTrace ["mkStep"]) $ do
      getOnChainStateTxOuts typedValidator
    case maybeState of
        Nothing -> throwError OnChainStateNotFound
        Just (OnChainState{ocsTxOutRef}, utxo) -> do
            let currentState = toMarloweState ocsTxOutRef
            -- TODO: Move to debug log.
            debug "mkStep" $ "currentState = " <> show currentState
            let marloweTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef

            (allConstraints, marloweData) <- evaluateTxContstraints currentState times marloweTxOutRef
            -- TODO: Move to debug log.
            debug "mkStep" $ "allConstraints = " <> show allConstraints
            debug "mkStep" $ "marloweData = " <> show marloweData

            pk <- Contract.ownPaymentPubKeyHash
            -- TODO: Move to debug log.
            debug "mkStep" $ "pk = " <> show pk
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxo
            let lookups:: ScriptLookups TypedMarloweValidator
                lookups = lookups1 { Constraints.slOwnPaymentPubKeyHash = Just pk }
            utx <- either (throwing _ConstraintResolutionContractError)
                        pure
                        (Constraints.mkTx lookups allConstraints)
            let utx' = utx
                        {
                          unBalancedTxTx = (unBalancedTxTx utx) {Tx.txValidRange = range'}
                        , unBalancedTxValidityTimeRange = times
                        }
            -- TODO: Move to debug log.
            debug "mkStep" $ "utx' = " <> show utx'
            btx <- balanceTx $ Constraints.adjustUnbalancedTx utx'
            -- TODO: Move to debug log.
            debug "mkStep" $ "btx = " <> show btx
            stx <- submitBalancedTx btx
            -- TODO: Move to debug log.
            debug "mkStep" $ "stx = " <> show stx
            let txId = Tx.getCardanoTxId stx
            confirmed <- awaitTxConfirmed' (CallStackTrace ["mkStep"]) (MaxRetries 3) txId
            if confirmed
              then do
                -- TODO: Move to debug log.
                debug "mkStep" $ "confirmed txId = " <> show txId
                pure marloweData
              else do
                -- TODO: Introduce custom error value
                debug "mkStep" $ "tx confirmation failed txId = " <> show txId
                throwError $ OtherContractError $ Contract.OtherContractError "mkStep failed to confirm the transaction"
  where
    evaluateTxContstraints :: MarloweData
        -> Ledger.POSIXTimeRange
        -> Tx.TxOutRef
        -> Contract w MarloweSchema MarloweError (TxConstraints [MarloweTxInput] MarloweData, MarloweData)
    evaluateTxContstraints MarloweData{..} times marloweTxOutRef = do
        let (inputs, inputsConstraints) = foldMap clientInputToInputAndConstraints clientInputs
        let txInput = TransactionInput {
                txInterval = timeInterval,
                txInputs = inputs }

        case computeTransaction txInput marloweState marloweContract of
            TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
                let marloweData = MarloweData {
                        marloweContract = txOutContract,
                        marloweState = txOutState }
                let allConstraints = let
                        ownInputsConstraints =
                            [ ScriptInputConstraint
                                { icRedeemer = marloweTxInputsFromInputs inputs
                                , icTxOutRef = marloweTxOutRef
                                }
                            ]
                        payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty txOutPayments
                        constraints = inputsConstraints
                            <> payoutConstraints payoutsByParty
                            <> mustValidateIn times
                        txConstraints = constraints { txOwnInputs = ownInputsConstraints
                                                    , txOwnOutputs = []
                                                    }
                        in case txOutContract of
                            Close -> txConstraints
                            _ -> let
                                finalBalance = let
                                    contractBalance = totalBalance (accounts marloweState)
                                    totalIncome = P.foldMap (collectDeposits . getInputContent) inputs
                                    totalPayouts = P.foldMap snd payoutsByParty
                                    in contractBalance P.+ totalIncome P.- totalPayouts
                                in txConstraints { txOwnOutputs =
                                    [ ScriptOutputConstraint
                                        { ocDatum = marloweData
                                        , ocValue = finalBalance
                                        }
                                    ]
                                    }
                pure (allConstraints, marloweData)

            Error e -> throwError $ MarloweEvaluationError e

    clientInputToInputAndConstraints :: MarloweClientInput -> ([Input], TxConstraints Void Void)
    clientInputToInputAndConstraints = \case
        ClientInput input -> ([NormalInput input], inputContentConstraints input)
        ClientMerkleizedInput input continuation -> let
            builtin = PlutusTx.toBuiltinData continuation
            hash = dataHash builtin
            constraints = inputContentConstraints input <> mustIncludeDatum (Datum builtin)
            in ([MerkleizedInput input hash continuation], constraints)
      where
        inputContentConstraints :: InputContent ->  TxConstraints Void Void
        inputContentConstraints input =
            case input of
                IDeposit _ party _ _         -> partyWitnessConstraint party
                IChoice (ChoiceId _ party) _ -> partyWitnessConstraint party
                INotify                      -> P.mempty
          where
            partyWitnessConstraint (PK pk)     = mustBeSignedBy (PaymentPubKeyHash pk)
            partyWitnessConstraint (Role role) = mustSpendRoleToken role

            mustSpendRoleToken :: TokenName -> TxConstraints Void Void
            mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1


    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> TxConstraints i0 o0
    payoutConstraints payoutsByParty = foldMap payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey (PaymentPubKeyHash pk) value
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                in mustPayToOtherScript rolePayoutValidatorHash dataValue value

waitForTransition ::
    forall w schema.
       SmallTypedValidator
    -> Contract w schema MarloweError (Promise w schema MarloweError Transition)
waitForTransition typedValidator = do
    let
      addr = validatorAddress typedValidator
      debug' = debug "Language.Marlowe.Client.waitForTransition"
    debug' $ "Marlowe validator address which we query" <> show addr
    currentState <- getOnChainState typedValidator
    case currentState of
            Nothing -> do
                debug' "Current state on chain is empty so waiting..."
                -- There is no on-chain state, so we wait for an output to appear
                -- at the address. Any output that appears needs to be checked
                -- with scChooser'
                pure $ promiseBind (utxoIsProduced' (CallStackTrace ["waitForTransition"]) addr) $ \txns -> do
                    -- See NOTE: Chain index / cardano-node query consistency
                    -- void $ retryTillResponseDiffers' (CallStackTrace ["waitForTransition->Nothing"]) mempty (utxosAt addr)
                    produced <- concatMapM (marloweHistoryFrom typedValidator unsafeGetSlotConfig) $ NonEmpty.toList txns
                    case produced of
                        -- empty list shouldn't be possible, because we're waiting for txns with OnChainState
                        [history] -> pure $ Transition history
                        _         -> throwing_ _AmbiguousOnChainState
            Just OnChainState{ocsTxOutRef=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}} -> do
                debug' $ "wait till utxo is spent = " <> show tyTxOutRefRef
                pure $ promiseBind (utxoIsSpent' (CallStackTrace ["waitForTransition"]) tyTxOutRefRef) $ \txn -> do
                    -- void $ retryTillResponseDiffers' (CallStackTrace ["waitForTimeoutOrTransition->Just"]) currentState $ getOnChainState typedValidator
                    spent <- marloweHistoryFrom typedValidator unsafeGetSlotConfig txn
                    case spent of
                        [history] -> pure $ Transition history
                        _         -> throwing_ _UnableToExtractTransition

waitForTimeoutOrTransition ::
    forall w schema.
       SmallTypedValidator
    -> POSIXTime
    -> Contract w schema MarloweError (Either POSIXTime Transition)
waitForTimeoutOrTransition validator timeout = do
  waitForTransitionPromise <- waitForTransition validator
  awaitPromise $ selectEither (isTime timeout) waitForTransitionPromise

-- | Actually safe version of `selectList` which returns a `Promise`
raceList :: [Promise w s e a] -> Promise w s e a
raceList [] = never
raceList l  = foldr1 select . reverse $ l

getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ _citxInputs tx
    PlutusTx.fromBuiltinData r
