module Component.Template.State
  ( InstantiateContractError(..)
  , InstantiateContractErrorRow
  , dummyState
  , handleAction
  , initialState
  , instantiateExtendedContract
  ) where

import Prologue

import Component.ContractSetup.Types (ContractFields, ContractParams)
import Component.ContractSetup.Types as CS
import Component.Template.Types (Action(..), State(..))
import Data.Argonaut (encodeJson)
import Data.ContractTimeout as CT
import Data.ContractValue (_value)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (hush, note')
import Data.Filterable (filterMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (view)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Errors (class Debuggable, class Explain)
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
import Marlowe.Semantics (Party(..))
import Marlowe.Template
  ( TemplateContent(..)
  , fillTemplate
  , getPlaceholderIds
  , initializeTemplateContent
  )
import Text.Pretty (text)

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
  , templateTimeouts: Map.mapMaybeWithKey getTimeout timeContent
  , templateValues: mapWithIndex getValue valueContent
  , templateName: metaData.contractName
  , fields: fromMaybe blank mFields
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
