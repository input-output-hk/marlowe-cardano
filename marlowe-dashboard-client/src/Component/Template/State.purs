module Component.Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  ) where

import Prologue

import Component.ContractSetup.Types (ContractFields, ContractParams)
import Component.ContractSetup.Types as CS
import Component.Template.Types (Action(..), State(..))
import Data.ContractTimeout as CT
import Data.ContractValue (_value)
import Data.Either (hush)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (view)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Examples.PureScript.ContractForDifferences as ContractForDifferences
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.EscrowWithCollateral as EscrowWithCollateral
import Examples.PureScript.Swap as Swap
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Halogen (HalogenM)
import Halogen as H
import Halogen.Form.Injective (blank, inject)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Extended (ContractType(..), resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata
  ( ContractTemplate
  , NumberFormat(..)
  , _extendedContract
  )
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Contract) as Semantic
import Marlowe.Semantics (Party(..), Slot)
import Marlowe.Template
  ( TemplateContent(..)
  , fillTemplate
  , getPlaceholderIds
  , initializeTemplateContent
  )

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = initialState

initialState :: State
initialState = Start

setup :: ContractTemplate -> Maybe ContractFields -> State
setup { metaData, extendedContract } mFields = Setup
  { metaData, extendedContract }
  { templateRoles: getParties extendedContract # Set.mapMaybe case _ of
      Role name -> Just name
      _ -> Nothing
  , templateTimeouts: Map.mapMaybeWithKey getTimeout slotContent
  , templateValues: mapWithIndex getValue valueContent
  , templateName: metaData.contractName
  , fields: fromMaybe blank mFields
  }
  where
  TemplateContent { slotContent, valueContent } =
    initializeTemplateContent $ getPlaceholderIds extendedContract
  defaultSlotContent = case metaData.contractType of
    Escrow -> Escrow.defaultSlotContent
    EscrowWithCollateral -> EscrowWithCollateral.defaultSlotContent
    Swap -> Swap.defaultSlotContent
    ZeroCouponBond -> ZeroCouponBond.defaultSlotContent
    ContractForDifferences -> ContractForDifferences.defaultSlotContent
    _ -> Map.empty
  getTimeout key value =
    let
      bigIntValue = fromMaybe value $ Map.lookup key defaultSlotContent
    in
      hush $ CT.fromBigInt bigIntValue
  getValue key _ = maybe DefaultFormat _.valueParameterFormat
    $ OMap.lookup key metaData.valueParameterInfo

handleAction
  :: forall m
   . MonadAff m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction OnReset = do
  H.put Start

handleAction (OnTemplateChosen template) = do
  H.put $ Overview template Nothing

handleAction (OnSetup template fields) = H.put $ setup template fields

handleAction (OpenCreateWalletCard _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction (OnStartContract _ _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction OnBack = H.modify_ case _ of
  Start -> Start
  Overview _ _ -> Start
  Setup template { fields } -> Overview template (Just fields)
  Review template params -> setup template (Just $ inject params)

handleAction (OnContractSetupMsg CS.BackClicked) = handleAction OnBack

handleAction (OnContractSetupMsg (CS.ReviewClicked params)) =
  H.get >>= case _ of
    Setup template _ -> H.put $ Review template params
    _ -> pure unit

handleAction (OnContractSetupMsg (CS.FieldsUpdated fields)) =
  H.modify_ case _ of
    Setup template input -> Setup template input { fields = fields }
    s -> s

instantiateExtendedContract
  :: Slot -> ContractTemplate -> ContractParams -> Maybe Semantic.Contract

instantiateExtendedContract currentSlot template params =
  let
    extendedContract = view (_extendedContract) template

    { timeouts, values } = params

    slotContent = CT.toBigInt <$> timeouts

    valueContent = view _value <$> values

    templateContent = TemplateContent { slotContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes currentSlot filledContract
  in
    toCore absoluteFilledContract
