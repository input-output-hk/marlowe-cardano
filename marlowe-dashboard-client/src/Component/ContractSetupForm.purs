module Component.ContractSetupForm where

import Prologue

import Component.Icons (Icon, icon)
import Component.Icons as Icon
import Component.Template.Types (Action(..), ContractSetupStage(..))
import Component.Template.Types as Template
import Control.Monad.Rec.Class (class MonadRec)
import Css as Css
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as CN
import Data.ContractTimeout (ContractTimeout)
import Data.ContractTimeout as CT
import Data.ContractValue (ContractValue)
import Data.ContractValue as CV
import Data.Either (note)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Validation.Semigroup (V(..))
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Forms (InputSlots)
import Forms as Forms
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (Form)
import Halogen.Form as F
import Halogen.Form.Component as FC
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Marlowe.Semantics (TokenName)
import Polyform.Validator (liftFnV)
import Type.Proxy (Proxy(..))

type Input =
  { roles :: Set TokenName
  , timeouts :: Map String ContractTimeout
  , values :: Map String ContractValue
  , addressBook :: AddressBook
  , contractName :: String
  }

type ContractInput =
  { nickname :: String
  , roles :: Map TokenName String
  , timeouts :: Map String String
  , values :: Map String String
  }

_nickname :: Lens' ContractInput String
_nickname = prop (Proxy :: _ "nickname")

_roles :: Lens' ContractInput (Map TokenName String)
_roles = prop (Proxy :: _ "roles")

_timeouts :: Lens' ContractInput (Map String String)
_timeouts = prop (Proxy :: _ "timeouts")

_values :: Lens' ContractInput (Map String String)
_values = prop (Proxy :: _ "values")

data ContractParams =
  ContractParams
    ContractNickname
    (Map TokenName Address)
    (Map String ContractTimeout)
    (Map String ContractValue)

derive instance Eq ContractParams

type Component q m =
  H.Component q Input Template.Action m

contractNicknameForm
  :: forall parentAction s m
   . Monad m
  => Form parentAction (InputSlots s) m String ContractNickname
contractNicknameForm =
  F.mkForm
    { validator: CN.validator
    , render: Forms.input "contract-nickname" "Contract title" case _ of
        CN.Empty -> "Required."
    }

roleAssignmentForm
  :: forall s m
   . Monad m
  => AddressBook
  -> TokenName
  -> Form Template.Action (InputSlots s) m String Address
roleAssignmentForm addressBook roleName =
  F.mkForm
    { validator
    , render: Forms.input ("role-input-" <> roleName) roleName case _ of
        WN.Empty -> "Required."
        WN.Exists -> "Already exists."
        WN.DoesNotExist -> "Not found."
        WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
    }
  where
  validator =
    liftFnV $
      V <<<
        ( note WN.DoesNotExist <<< flip AddressBook.lookupAddress addressBook
            <=< WN.fromString
        )

timeoutForm
  :: forall parentAction s m
   . Monad m
  => String
  -> Form parentAction (InputSlots s) m String ContractTimeout
timeoutForm name =
  F.mkForm
    { validator: CT.validator
    , render: Forms.intInput ("slot-input-" <> name) name case _ of
        CT.Empty -> "Required."
        CT.Past -> "Must be in the future."
        CT.Invalid -> "Must be a number of slots from contract start."
    }

valueForm
  :: forall parentAction s m
   . Monad m
  => String
  -> Form parentAction (InputSlots s) m String ContractValue
valueForm name =
  F.mkForm
    { validator: CV.validator
    , render: Forms.adaInput ("value-input-" <> name) name case _ of
        CV.Empty -> "Required."
        CV.Negative -> "Must by positive."
        CV.Invalid -> "Must by a number."
    }

templateInputsSection
  :: forall w i. Icon -> String -> HH.HTML w i
templateInputsSection icon' heading = HH.h3
  [ classNames
      [ "flex"
      , "gap-1"
      , "items-center"
      , "leading-none"
      , "text-sm"
      , "font-semibold"
      , "pb-2"
      , "mb-4"
      , "border-gray"
      , "border-b"
      ]
  ]
  [ icon icon' [ "text-purple" ]
  , HH.text heading
  ]

component :: forall q m. MonadAff m => MonadRec m => Component q m
component = Hooks.component \{ outputToken } input -> Hooks.do
  let { contractName, addressBook, roles, timeouts, values } = input
  Tuple result putResult <- usePutState Nothing
  form <- Hooks.captures { addressBook } Hooks.useMemo \_ ->
    FC.component
      { formClasses: [ "overflow-y-auto", "p-4", "flex", "flex-col", "gap-2" ]
      , form: ado
          F.html $ HH.h2
            [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
            [ HH.text $ contractName <> " setup" ]

          name <- F.subform _nickname contractNicknameForm

          F.html (templateInputsSection Icon.Roles "Roles")

          roles' <- F.subform _roles $ F.traverseFormsWithIndex
            (const <<< roleAssignmentForm addressBook)
            (Set.toMap roles)

          F.html (templateInputsSection Icon.Terms "Terms")

          timeouts' <- F.subform _timeouts
            $ F.traverseFormsWithIndex (const <<< timeoutForm) timeouts

          values' <- F.subform _values
            $ F.traverseFormsWithIndex (const <<< valueForm) values

          in ContractParams name roles' timeouts' values'
      }
  let
    initialInput =
      { nickname: ""
      , roles: Map.fromFoldable $ Set.map (flip Tuple "") roles
      , timeouts: CT.toString <$> timeouts
      , values: CV.toString <$> values
      }
  Hooks.pure do
    HH.div [ classNames [ "h-full", "grid", "grid-rows-1fr-auto" ] ]
      [ HH.slot (Proxy :: _ "form") unit form initialInput case _ of
          FC.Updated res -> putResult res
          FC.Raised action -> Hooks.raise outputToken action
      , HH.div
          [ classNames
              [ "flex", "items-baseline", "p-4", "border-gray", "border-t" ]
          ]
          [ HH.a
              [ classNames [ "flex-1", "text-center" ]
              , onClick_ $ Hooks.raise outputToken $ SetContractSetupStage
                  Overview
              ]
              [ HH.text "Back" ]
          , HH.button
              [ classNames $
                  Css.primaryButton
                    <> [ "flex-1", "text-left" ]
                    <> Css.withIcon Icon.ArrowRight
              , onClick_ $ Hooks.raise outputToken $ SetContractSetupStage
                  Review
              , HP.enabled $ isJust result
              ]
              [ HH.text "Review" ]
          ]
      ]
