module Component.Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  ) where

import Prologue

import Component.ContractSetupForm (ContractParams(..))
import Component.Template.Types (Action(..), State(..))
import Data.ContractTimeout as CT
import Data.ContractValue as CV
import Data.Lens (view)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Extended (resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata (ContractTemplate, _extendedContract)
import Marlowe.Semantics (Contract) as Semantic
import Marlowe.Semantics (Slot)
import Marlowe.Template (TemplateContent(..), fillTemplate)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = initialState

initialState :: State
initialState = Start

handleAction
  :: forall m
   . MonadAff m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction OnReset = do
  H.put Start

handleAction (OnTemplateChosen template) = do
  H.put $ Overview template

handleAction (OnSetup template params) = do
  H.put $ Setup template params

handleAction (OnReview template params) = do
  H.put $ Review template params

handleAction (OpenCreateWalletCard _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction (OnStartContract _ _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction OnBack = H.modify_ case _ of
  Start -> Start
  Overview _ -> Start
  Setup template _ -> Overview template
  Review template params -> Setup template (Just params)

instantiateExtendedContract
  :: Slot -> ContractTemplate -> ContractParams -> Maybe Semantic.Contract
instantiateExtendedContract currentSlot template params =
  let
    extendedContract = view (_extendedContract) template

    ContractParams _ _ slots values = params

    slotContent = CT.toBigInt <$> slots

    valueContent = CV.toBigInt <$> values

    templateContent = TemplateContent { slotContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes currentSlot filledContract
  in
    toCore absoluteFilledContract
