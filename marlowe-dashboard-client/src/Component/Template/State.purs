module Component.Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  ) where

import Prologue

import Component.ContractSetup.Types (ContractParams)
import Component.ContractSetup.Types as CS
import Component.Template.Types (Action(..), State(..))
import Data.ContractTimeout as CT
import Data.ContractValue as CV
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
import Halogen.Form.Projective (blank)
import Halogen.Form.Types as HF
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

setup :: ContractTemplate -> Maybe ContractParams -> State
setup { metaData, extendedContract } params = do
  Setup
    { metaData, extendedContract }
    { templateRoles
    , templateTimeouts
    , templateValues
    , templateName: metaData.contractName
    , initialize:
        { nickname: HF.fromMaybe $ _.nickname <$> params
        , roles: mapWithIndex (const <<< HF.fromMaybe <<< lookupRole)
            $ Set.toMap templateRoles
        , timeouts: HF.FromOutput <$> templateTimeouts
        , values: mapWithIndex (const <<< HF.fromMaybe <<< lookupValue)
            templateValues
        }
    }
    blank
  where
  lookupRole tokenName = Map.lookup tokenName =<< _.roles <$> params
  lookupValue name = Map.lookup name =<< _.values <$> params
  partyToRole (Role tokenName) = Just tokenName
  partyToRole _ = Nothing
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
  templateRoles = Set.mapMaybe partyToRole $ getParties extendedContract
  templateTimeouts = Map.mapMaybeWithKey getTimeout slotContent
  templateValues = mapWithIndex getValue valueContent

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
  H.put $ setup template params

handleAction (OpenCreateWalletCard _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction (OnStartContract _ _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction OnBack = H.modify_ case _ of
  Start -> Start
  Overview _ -> Start
  Setup template _ _ -> Overview template
  Review template params -> setup template (Just params)

handleAction (OnContractSetupMsg CS.BackClicked) = handleAction OnBack

handleAction (OnContractSetupMsg (CS.ReviewClicked params)) =
  H.get >>= case _ of
    Setup template _ _ -> H.put $ Review template params
    _ -> pure unit

handleAction (OnContractSetupMsg (CS.FieldsUpdated fields)) =
  H.modify_ case _ of
    Setup template input _ -> Setup template input fields
    s -> s

instantiateExtendedContract
  :: Slot -> ContractTemplate -> ContractParams -> Maybe Semantic.Contract

instantiateExtendedContract currentSlot template params =
  let
    extendedContract = view (_extendedContract) template

    { timeouts, values } = params

    slotContent = CT.toBigInt <$> timeouts

    valueContent = CV.toBigInt <$> values

    templateContent = TemplateContent { slotContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes currentSlot filledContract
  in
    toCore absoluteFilledContract
