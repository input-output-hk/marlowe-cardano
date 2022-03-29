module Component.ConfirmContractActionDialog.State
  ( component
  ) where

import Prologue

import Capability.Marlowe (class ManageMarlowe, applyTransactionInput)
import Capability.Toast (class Toast, addToast)
import Component.ConfirmContractActionDialog.Types
  ( Action(..)
  , DSL
  , Input
  , Msg(..)
  , State
  )
import Component.ConfirmContractActionDialog.View (render)
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Fork.Class (class MonadKill, fork, kill)
import Control.Monad.Fork.Class as MF
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.State (get)
import Data.ContractUserParties (contractUserParties)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable as Unfoldable
import Effect.Aff (Error, Fiber, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Errors (globalError)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, updateStore)
import Marlowe.Execution.State (mkTx, setPendingTransaction)
import Marlowe.Execution.Types (NamedAction(..))
import Marlowe.PAB (transactionFee)
import Marlowe.Semantics (ChosenNum)
import Marlowe.Semantics as Semantic
import Store as Store
import Toast.Types (successToast)

--
component
  :: forall query m
   . MonadAff m
  => MonadKill Error Fiber m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => MonadTime m
  => Toast m
  => H.Component query Input Msg m
component =
  H.mkComponent
    { initialState: mkInitialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

mkInitialState :: Input -> State
mkInitialState
  { action
  , executionState
  , wallet
  , chosenNum
  } =
  let
    { marloweParams, contract } = executionState
  in
    { action
    , executionState
    , contractUserParties: contractUserParties wallet marloweParams contract
    , transactionFeeQuote: transactionFee
    , txInput: Nothing
    , wallet
    , chosenNum
    , pendingFiber: Nothing
    }

handleAction
  :: forall m
   . ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => MonadAff m
  => MonadKill Error Fiber m
  => MonadTime m
  => MonadLogger StructuredLog m
  => Toast m
  => Action
  -> DSL m Unit
handleAction CancelConfirmation = do
  mPendingFiber <- H.gets _.pendingFiber
  case mPendingFiber of
    Nothing -> H.raise DialogClosed
    Just pendingFiber -> do
      H.modify_ _ { pendingFiber = Nothing }
      H.lift $ kill (error "killed") pendingFiber
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Left "Cancelled")

{- [UC-CONTRACT-3][0] Apply an input to a contract -}
handleAction (ConfirmAction namedAction) = do
  { wallet, executionState, chosenNum } <- get
  currentTime <- now
  let
    { marloweParams, contract } = executionState
    contractInput = toInput namedAction chosenNum
    txInput = mkTx currentTime contract $ Unfoldable.fromMaybe contractInput
  ajaxApplyInputsF <-
    H.lift $ fork $ applyTransactionInput wallet marloweParams txInput
  H.modify_ _ { pendingFiber = Just $ void ajaxApplyInputsF }
  ajaxApplyInputs <- H.lift $ MF.join ajaxApplyInputsF
  H.modify_ _ { pendingFiber = Nothing }
  case ajaxApplyInputs of
    Left ajaxError -> do
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Left "Error")
      globalError "Failed to submit transaction." ajaxError
    Right mResult -> do
      updateStore $ Store.ModifySyncedContract marloweParams $
        setPendingTransaction txInput
      void $ H.tell _submitButtonSlot "action-confirm-button" $ SubmitResult
        (Milliseconds 600.0)
        (Right "")
      addToast $ successToast "Transaction submitted, awating confirmation."
      H.raise DialogClosed
      liftAff mResult
      addToast $ successToast "Contract update applied."

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
