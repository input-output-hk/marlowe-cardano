module Component.Template.State (component) where

import Prologue

import Capability.Marlowe (class ManageMarlowe, initializeContract)
import Capability.Toast (class Toast, addToast)
import Component.ContractSetup.Types (ContractFields)
import Component.ContractSetup.Types as CS
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Component.Template.Types
  ( Action(..)
  , ChildSlots
  , Component
  , Input
  , Msg(..)
  , State
  , Wizard(..)
  )
import Component.Template.View (render)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Now (class MonadTime, now)
import Data.ContractTimeout as CT
import Data.DateTime.Instant (unInstant)
import Data.Either (hush)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Errors (globalError)
import Examples.PureScript.ContractForDifferences as ContractForDifferences
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.EscrowWithCollateral as EscrowWithCollateral
import Examples.PureScript.Swap as Swap
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Halogen (HalogenM)
import Halogen as H
import Halogen.Form.Injective (blank, inject)
import Halogen.Store.Monad (class MonadStore)
import Marlowe.Extended (ContractType(..))
import Marlowe.Extended.Metadata (ContractTemplate, NumberFormat(..))
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Party(..))
import Marlowe.Template
  ( TemplateContent(..)
  , getPlaceholderIds
  , initializeTemplateContent
  )
import Store as Store
import Toast.Types (successToast)

component
  :: forall m
   . MonadAff m
  => MonadTime m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => Component m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      }
  }

initialState :: Input -> State
initialState wallet = { wallet, wizard: Start }

setup :: ContractTemplate -> Maybe ContractFields -> Wizard
setup { metaData, extendedContract } mFields = Setup
  { metaData, extendedContract }
  { templateRoles: getParties extendedContract # Set.mapMaybe case _ of
      Role name -> Just name
      _ -> Nothing
  , templateTimeouts: Map.mapMaybeWithKey getTimeout timeContent
  , templateValues: mapWithIndex getValue valueContent
  , templateName: metaData.contractName
  , fields: fromMaybe blank mFields
  , metaData
  }
  where
  TemplateContent { timeContent, valueContent } =
    initializeTemplateContent $ getPlaceholderIds extendedContract
  defaultTimeContent = case metaData.contractType of
    Escrow -> Escrow.defaultTimeContent
    EscrowWithCollateral -> EscrowWithCollateral.defaultTimeContent
    Swap -> Swap.defaultTimeContent
    ZeroCouponBond -> ZeroCouponBond.defaultTimeContent
    ContractForDifferences -> ContractForDifferences.defaultTimeContent
    _ -> Map.empty
  getTimeout key value =
    let
      instant = fromMaybe value $ Map.lookup key defaultTimeContent
    in
      hush $ CT.fromDuration $ unInstant instant
  getValue key _ = maybe DefaultFormat _.valueParameterFormat
    $ OMap.lookup key metaData.valueParameterInfo

handleAction
  :: forall m
   . MonadAff m
  => MonadTime m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => Toast m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction OnReset = do
  H.modify_ _ { wizard = Start }

handleAction (OnTemplateChosen template) = do
  H.modify_ _ { wizard = Overview template Nothing }

handleAction (OnSetup template fields) = do
  H.modify_ _ { wizard = setup template fields }

handleAction (OpenCreateWalletCard _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction (OnStartContract template params) = do
  {- [UC-CONTRACT-1][0] Start a new marlowe contract
   The flow of creating a new marlowe contract starts when we submit the
   Template form. In here we apply the contract parameters to the Marlowe
   Extended contract to receive a Marlowe Core contract, and we call the
   PAB endpoint to create and distribute the role tokens. We also create
   a placeholder so the user can see that that the contract is being created
  -}
  wallet <- H.gets _.wallet
  currentInstant <- now
  instantiateResponse <-
    initializeContract currentInstant template params wallet
  case instantiateResponse of
    Left error -> do
      H.tell _submitButtonSlot "action-pay-and-start"
        $ SubmitResult (Milliseconds 600.0) (Left "Error")
      globalError "Failed to initialize contract." error
    Right (newContract /\ awaitContractCreation) -> do
      H.tell _submitButtonSlot "action-pay-and-start"
        $ SubmitResult (Milliseconds 600.0) (Right "")
      addToast $ successToast
        "The request to initialize this contract has been submitted."
      H.raise $ ContractStarted newContract awaitContractCreation

handleAction OnBack = H.modify_ \s -> s
  { wizard = case s.wizard of
      Start -> Start
      Overview _ _ -> Start
      Setup template { fields } -> Overview template (Just fields)
      AddContact _ template input -> Setup template input
      Review template params -> setup template (Just $ inject params)
  }

handleAction (OnContractSetupMsg CS.BackClicked) = handleAction OnBack

handleAction (OnContractSetupMsg (CS.ReviewClicked params)) =
  H.gets _.wizard >>= case _ of
    Setup template _ -> H.modify_ _ { wizard = Review template params }
    _ -> pure unit

handleAction (OnContractSetupMsg (CS.FieldsUpdated fields)) =
  H.gets _.wizard >>= case _ of
    Setup template input -> H.modify_ _
      { wizard = Setup template input { fields = fields } }
    _ -> pure unit

handleAction (OnContractSetupMsg (CS.NewContactRequested name)) = do
  { wizard } <- H.get
  case wizard of
    Setup template input ->
      H.modify_ _ { wizard = AddContact name template input }
    _ -> pure unit

handleAction (OnAddContactMsg _) =
  handleAction OnBack
