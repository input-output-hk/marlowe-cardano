module Capability.Marlowe
  ( ApplyInputError(..)
  , CreateError(..)
  , RedeemError(..)
  , InstantiateContractError
  , applyTransactionInput
  , class ManageMarlowe
  , initializeContract
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (stopContract)
import Capability.PlutusApps.MarloweApp (applyInputs, createContract, redeem) as MarloweApp
import Capability.Wallet (class ManageWallet)
import Component.ContractSetup.Types (ContractParams)
import Control.Concurrent.AVarMap as AVarMap
import Control.Logger.Structured (error, info, info', warning')
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Fork.Class (class MonadBracket)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.UUID (class MonadUUID)
import Control.Parallel (parOneOf)
import Data.Argonaut (encodeJson)
import Data.Bifunctor (lmap)
import Data.ContractTimeout as CT
import Data.ContractValue (_value)
import Data.DateTime.Instant (Instant, instant)
import Data.Either (note')
import Data.Filterable (filterMap)
import Data.Lens (view)
import Data.NewContract (NewContract(..))
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _address
  , _marloweAppId
  , _walletId
  )
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Generic (class Constructors, mkConstructors')
import Effect.Aff (Aff, Error, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift
  ( class MonadUnliftAff
  , UnliftAff
  , askUnliftAff
  , unliftAff
  )
import Env (_marloweAppTimeoutBlocks, _redeemAvarMap)
import Errors.Debuggable (class Debuggable)
import Errors.Explain (class Explain)
import Halogen (HalogenM)
import Halogen.Store.Monad (emitSelected, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Halogen.Subscription.Extra (subscribeOnce)
import Language.Marlowe.Client (MarloweError(..))
import Marlowe.Execution.State (removePendingTransaction, setPendingTransaction)
import Marlowe.Extended (resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata (ContractTemplate, _extendedContract)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Server (Api) as MarloweApp
import Marlowe.Run.Server as Marlowe
import Marlowe.Semantics (MarloweParams, TransactionInput)
import Marlowe.Semantics as Semantic
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Plutus.Contract.Error as P
import Plutus.PAB.Webserver (Api) as PAB
import Servant.PureScript (class MonadAjax)
import Store as Store
import Store.RoleTokens (Payout)
import Text.Pretty (text)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Types (AjaxResponse, JsonAjaxErrorRow)

data InstantiateContractError =
  InstantiateContractError Instant ContractTemplate ContractParams

instance Explain InstantiateContractError where
  explain _ = text
    "We couldn't create an instance of the contract with the provided parameters"

instance Debuggable InstantiateContractError where
  debuggable (InstantiateContractError currentTime template params) =
    encodeJson
      { errorType: "Contract instantiation"
      , currentTime: show currentTime
      , template
      , params
      }

type InstantiateContractErrorRow r =
  (instantiateContractError :: InstantiateContractError | r)

type InitializeContractError = Variant
  (JsonAjaxErrorRow + InstantiateContractErrorRow + ())

initializeContractError
  :: forall c. Constructors InitializeContractError c => c
initializeContractError = mkConstructors'
  (Proxy :: Proxy InitializeContractError)

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ManageWallet m <=
  ManageMarlowe m where
  initializeContract
    :: Instant
    -> ContractTemplate
    -> ContractParams
    -> PABConnectedWallet
    -> m
         ( Either
             InitializeContractError
             (NewContract /\ Aff (Either CreateError MarloweParams))
         )
  applyTransactionInput
    :: PABConnectedWallet
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff (Either ApplyInputError Unit)))
  redeem
    :: PABConnectedWallet
    -> Payout
    -> m (AjaxResponse (Aff (Either RedeemError Unit)))

awaitAndHandleResult
  :: forall f m e a
   . MonadUnliftAff m
  => MonadBracket Error f m
  => MonadAjax Marlowe.Api m
  => MonadAjax PAB.Api m
  => MonadRec m
  => PlutusAppId
  -> UnliftAff (AppM m)
  -> Aff (Either MarloweError a)
  -> e
  -> (MarloweError -> e)
  -> (Either e a -> AppM m Unit)
  -> AppM m (Aff (Either e a))
awaitAndHandleResult
  marloweAppId
  u
  aff
  timeoutError
  contractError
  handleResult = do
  -- Already fork and await the aff here, so we don't have to wait for the
  -- caller to await it to run any updates.
  fiber <- liftAff $ forkAff do
    -- | Give the result Aff a certain number of blocks to resolve, or cancel
    -- | it, stop the MarloweApp, and return an error.
    blocksToWait <- unliftAff u $ asks $ view _marloweAppTimeoutBlocks
    tipSlotE <- unliftAff u $ emitSelected (selectEq _.tipSlot)
    -- Create an emitter that increments every time the tip slot changes.
    let changeCountE = HS.fold (const $ add 1) tipSlotE 0
    -- subscribe to the first time this count reaches or exceeds the limit.
    mResult <- parOneOf
      [ subscribeOnce $ Nothing <$ HS.filter (_ > blocksToWait) changeCountE
      , Just <<< lmap contractError <$> aff
      ]
    unliftAff u case mResult of
      Nothing -> do
        warning' "MarloweApp timed out, stoping instance."
        stopResult <- stopContract marloweAppId
        case stopResult of
          Left err -> error "Failed to stop MarloweApp" err
          Right _ -> info' "MarloweApp stopped successfully."
        handleResult $ Left timeoutError
        pure $ Left timeoutError
      Just a -> do
        handleResult a
        pure a
  pure $ joinFiber fiber

instance
  ( MonadUnliftAff m
  , MonadBracket Error f m
  , MonadRec m
  , MonadAjax PAB.Api m
  , MonadAjax MarloweApp.Api m
  , MonadUUID m
  ) =>
  ManageMarlowe (AppM m) where

  initializeContract currentInstant template params wallet = do
    info' "Initializing contract"
    u <- askUnliftAff
    runExceptT do
      let
        { instantiateContractError, jsonAjaxError } = initializeContractError
        { nickname, roles } = params
        marloweAppId = view _marloweAppId wallet
        walletId = view _walletId wallet
      -- To initialize a Marlowe Contract we first need to make an instance
      -- of a Core.Marlowe contract. We do this by replazing template parameters
      -- from the Extended.Marlowe template and then calling toCore. This can
      -- fail with `instantiateContractError` if not all params were provided.
      contract <- except
        $ lmap instantiateContractError
        $ instantiateExtendedContract currentInstant template params
      -- Call the PAB to create the new contract. It returns a request id and a function
      -- that we can use to block and wait for the response
      reqId /\ awaitContractCreation <-
        withExceptT jsonAjaxError $ ExceptT $
          MarloweApp.createContract walletId marloweAppId roles contract

      -- We save in the store the request of a created contract with
      -- the information relevant to show a placeholder of a starting contract.
      let
        newContract =
          NewContract reqId nickname template.metaData Nothing contract
      lift do
        updateStore $ Store.ContractCreated newContract
        Tuple newContract <$> awaitAndHandleResult
          marloweAppId
          u
          awaitContractCreation
          CreateTimeout
          CreateError
          -- Update the contract's representation in the store to use its
          -- MarloweParams if successful, or show an error otherwise.
          case _ of
            Left (CreateError marloweError) -> do
              updateStore $ Store.ContractStartFailed newContract marloweError
            Right marloweParams -> do
              updateStore $ Store.ContractStarted newContract marloweParams
            _ ->
              -- Do nothing. We might still receive the marlowe params from the
              -- wallet companion and start this contract.
              pure unit

  -- "apply-inputs" to a Marlowe contract on the blockchain
  applyTransactionInput wallet marloweParams transactionInput = do
    info "Applying input " $ encodeJson { marloweParams, transactionInput }
    u <- askUnliftAff
    runExceptT do
      let marloweAppId = view _marloweAppId wallet
      let walletId = view _walletId wallet
      awaitResult <- ExceptT
        $ MarloweApp.applyInputs walletId marloweAppId marloweParams
            transactionInput
      updateStore
        $ Store.ModifySyncedContract marloweParams
        $ setPendingTransaction transactionInput
      lift $ awaitAndHandleResult
        marloweAppId
        u
        awaitResult
        ApplyInputTimeout
        ApplyInputError
        case _ of
          Left _ -> updateStore
            $ Store.ModifySyncedContract marloweParams removePendingTransaction
          Right _ -> do
            -- Note, we don't removed the pending transaction yet. If we do,
            -- the contract card will still show the old, stale state for
            -- several seconds while we wait for a follower update. We will
            -- remove any pending transaction when we receive the next follower
            -- update.
            info' "Input applied"

  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem wallet payout = do
    redeemAvarMap <- asks $ view _redeemAvarMap
    u <- askUnliftAff
    -- put an AVar in the map, preventing future redeem attempts from
    -- succeeding.
    wasAbleToPutAvar <- AVarMap.tryPut payout unit redeemAvarMap
    if wasAbleToPutAvar then
      pure
        $ Right
        $ pure
        $ Left
        $ RedeemError
        $ OtherContractError
        $ P.OtherContractError "Already redeemed this payment"
    else do
      info "Redeeming payout" $ encodeJson payout
      runExceptT do
        let marloweAppId = view _marloweAppId wallet
        let walletId = view _walletId wallet
        let address = view _address wallet
        awaitResult <- catchError
          (ExceptT $ MarloweApp.redeem walletId marloweAppId payout address)
          \err -> AVarMap.take payout redeemAvarMap *> throwError err
        lift $ awaitAndHandleResult
          marloweAppId
          u
          awaitResult
          RedeemTimeout
          RedeemError
          case _ of
            Right _ -> do
              info "Payout redeemed" $ encodeJson payout
              -- Do not take the avar again. We still don't want to allow this
              -- payout to be redeemed again.
              pure unit
            Left (RedeemError _) -> do
              error "Failed to redeem payout" $ encodeJson payout
              -- Take the Avar so we can try again
              AVarMap.take payout redeemAvarMap
            Left RedeemTimeout ->
              -- Don't take the avar again here! The payout may still be processed
              -- in which case we should get a follower update eventually
              pure unit

instance ManageMarlowe m => ManageMarlowe (HalogenM state action slots msg m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails tokenName =
    lift $ redeem walletDetails tokenName

instance ManageMarlowe m => ManageMarlowe (MaybeT m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails tokenName =
    lift $ redeem walletDetails tokenName

instance ManageMarlowe m => ManageMarlowe (ReaderT r m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails tokenName =
    lift $ redeem walletDetails tokenName

data ApplyInputError = ApplyInputTimeout | ApplyInputError MarloweError

instance Explain ApplyInputError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain ApplyInputTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem applying an input"

instance Debuggable ApplyInputError where
  debuggable (ApplyInputError error) = encodeJson error
  debuggable ApplyInputTimeout = encodeJson "ApplyInputTimeout"

data RedeemError = RedeemTimeout | RedeemError MarloweError

instance Explain RedeemError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain RedeemTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem redeeming a payment"

instance Debuggable RedeemError where
  debuggable (RedeemError error) = encodeJson error
  debuggable RedeemTimeout = encodeJson "RedeemTimeout"

data CreateError = CreateTimeout | CreateError MarloweError

instance Explain CreateError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain CreateTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem creating a contract"

instance Debuggable CreateError where
  debuggable (CreateError error) = encodeJson error
  debuggable CreateTimeout = encodeJson "CreateTimeout"

instantiateExtendedContract
  :: Instant
  -> ContractTemplate
  -> ContractParams
  -> Either InstantiateContractError Semantic.Contract
instantiateExtendedContract now template params =
  let
    extendedContract = view (_extendedContract) template

    { timeouts, values } = params

    timeContent = filterMap (instant <<< CT.toDuration) timeouts

    valueContent = view _value <$> values

    templateContent = TemplateContent { timeContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes now filledContract
  in
    note'
      (\_ -> InstantiateContractError now template params)
      $ toCore absoluteFilledContract
