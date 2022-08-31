module Component.ConfirmContractActionDialog.State
  ( component
  ) where

import Prologue

import Capability.Marlowe (class ManageMarlowe, applyTransactionInput)
import Capability.Toast (class Toast, addToast)
import Component.ConfirmContractActionDialog.Types
  ( Action(..)
  , DSL
  , Derived
  , Input
  , Input'
  , Msg(..)
  , _executionState
  , _pendingFiber
  , _txInput
  , _wallet
  )
import Component.ConfirmContractActionDialog.Types as CCAD
import Component.ConfirmContractActionDialog.View (render)
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Component.Toast.Types (successToast)
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Fork.Class (class MonadKill, fork, kill)
import Control.Monad.Fork.Class as MF
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Foldable (for_)
import Data.Lens (assign, use, view)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable as Unfoldable
import Effect.Aff (Error, Fiber, error)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, askUnliftAff, unliftAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Env (Env, _followerBus)
import Errors (globalError)
import Halogen as H
import Halogen.Component.Reactive as HR
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (selectEq)
import Language.Marlowe.Core.V1.Semantics.Types (ChosenNum)
import Language.Marlowe.Core.V1.Semantics.Types as Semantic
import Marlowe.Execution.State (mkTx)
import Marlowe.Execution.Types (NamedAction(..))
import Marlowe.PAB (applyInputsFee)
import Store as Store
import Store.Contracts (getFollowerContract)

--
component
  :: forall m
   . MonadUnliftAff m
  => MonadKill Error Fiber m
  => MonadAsk Env m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => MonadTime m
  => Toast m
  => H.Component CCAD.Query Input Msg m
component = connect (selectEq selectSlice) $
  HR.mkReactiveComponent
    { deriveState
    , initialTransient:
        { pendingFiber: Nothing
        }
    , render
    , eval: HR.fromHandleAction handleAction
    }
  where
  selectSlice { currentTime, roleTokens } = { currentTime, roleTokens }

deriveState :: Input' -> Derived
deriveState { context, input } =
  let
    { action, executionState, chosenNum } = input
    { currentTime } = context
    { mNextTimeout } = executionState
    contractInput = toInput action chosenNum
  in
    { transactionFeeQuote: applyInputsFee
    , txInput:
        mkTx currentTime mNextTimeout $ Unfoldable.fromMaybe contractInput
    }

handleAction
  :: forall m
   . ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => MonadUnliftAff m
  => MonadAsk Env m
  => MonadKill Error Fiber m
  => MonadTime m
  => MonadLogger StructuredLog m
  => Toast m
  => Action
  -> DSL m Unit
handleAction CancelConfirmation = do
  mPendingFiber <- use _pendingFiber
  case mPendingFiber of
    Nothing -> H.raise DialogClosed
    Just pendingFiber -> do
      assign _pendingFiber Nothing
      H.lift $ kill (error "killed") pendingFiber
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Left "Cancelled")

{- [UC-CONTRACT-3][0] Apply an input to a contract -}
handleAction (ConfirmAction) = do
  wallet <- use _wallet
  executionState <- use _executionState
  txInput <- use _txInput
  let { marloweParams } = executionState
  ajaxApplyInputsF <-
    H.lift $ fork $ applyTransactionInput wallet marloweParams txInput
  assign _pendingFiber $ Just $ void ajaxApplyInputsF
  ajaxApplyInputs <- H.lift $ MF.join ajaxApplyInputsF
  assign _pendingFiber Nothing
  case ajaxApplyInputs of
    Left ajaxError -> do
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Left "Error")
      globalError "Failed to submit transaction." ajaxError
    Right awaitResult -> do
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Right "")
      addToast $ successToast "Transaction submitted, awating confirmation."
      H.raise DialogClosed
      mResult <- liftAff awaitResult
      case mResult of
        Right _ -> do
          -- Wait to get a follower update before showing this message, because
          -- otherwise it happens far too early.
          followerBus <- asks $ view _followerBus
          u <- H.lift askUnliftAff
          store <- getStore
          let mFollowerId = getFollowerContract marloweParams store.contracts
          for_ mFollowerId \followerId ->
            liftAff do
              void $ EventBus.subscribeOnce followerBus.emitter followerId
              unliftAff u $ addToast $ successToast "Contract update applied."
        Left error -> do
          globalError "Failed to update contract" error

toInput :: NamedAction -> Maybe ChosenNum -> Maybe Semantic.Input
toInput (MakeDeposit accountId party token value) _ = Just $ Semantic.IDeposit
  accountId
  party
  token
  value

toInput (MakeChoice choiceId _) (Just chosenNum) = Just $ Semantic.IChoice
  choiceId
  chosenNum

-- WARNING:
--       This is possible in the types but should never happen in runtime. And I prefer to explicitly throw
--       an error if it happens than silently omit it by returning Nothing (which in case of Input, it has
--       the semantics of an empty transaction).
--       The reason we use Maybe in the chosenNum is that we use the same NamedAction data type
--       for triggering the action and to display to the user what choice did he/she made. And we need
--       to represent that initialy no choice is made, and eventually you can type an option and delete it.
--       Another way to do this would be to duplicate the NamedAction data type with just that difference, which
--       seems like an overkill.
toInput (MakeChoice _ _) Nothing = unsafeThrow
  "A choice action has been triggered"

toInput (MakeNotify _) _ = Just $ Semantic.INotify

toInput _ _ = Nothing
