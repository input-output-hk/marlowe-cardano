module Component.ContractSetupForm where

import Prologue

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
import Data.Set (Set)
import Data.Set as Set
import Data.Validation.Semigroup (V(..))
import Data.WalletNickname (WalletNicknameError(..))
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Forms (InputSlots)
import Forms as Forms
import Halogen as H
import Halogen.Form (Form, subform, useForm)
import Halogen.Form as Form
import Halogen.Hooks as Hooks
import Marlowe.Semantics (TokenName)
import Polyform.Validator (liftFnV)
import Type.Proxy (Proxy(..))

type Input =
  { roles :: Set TokenName
  , timeouts :: Set String
  , values :: Set String
  , addressBook :: AddressBook
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

data Msg
  = Back
  | Next ContractParams

type Component q m =
  H.Component q Input Msg m

contractNicknameForm
  :: forall s m. Monad m => Form (InputSlots s) m String ContractNickname
contractNicknameForm =
  Forms.input "contract-nickname" "Contract title" CN.validator case _ of
    CN.Empty -> "Required."

roleAssignmentForm
  :: forall s m
   . Monad m
  => AddressBook
  -> TokenName
  -> Form (InputSlots s) m String Address
roleAssignmentForm addressBook roleName =
  Forms.input ("role-input-" <> roleName) roleName validator case _ of
    WN.Empty -> "Required."
    WN.Exists -> "Already exists."
    WN.DoesNotExist -> "Not found."
    WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
  where
  validator =
    liftFnV $
      V <<<
        ( note WN.DoesNotExist <<< flip AddressBook.lookupAddress addressBook
            <=< WN.fromString
        )

roleForms
  :: forall s m
   . Monad m
  => AddressBook
  -> Form (InputSlots s) m (Map TokenName String) (Map TokenName Address)
roleForms addressBook =
  Form.multiWithIndex $ roleAssignmentForm $ addressBook

timeoutForm
  :: forall s m
   . Monad m
  => String
  -> Form (InputSlots s) m String ContractTimeout
timeoutForm name =
  Forms.input ("slot-input-" <> name) name CT.validator case _ of
    CT.Empty -> "Required."
    CT.Past -> "Must be in the future."
    CT.Invalid -> "Must be a number of slots from contract start."

valueForm
  :: forall s m
   . Monad m
  => String
  -> Form (InputSlots s) m String ContractValue
valueForm name =
  Forms.input ("value-input-" <> name) name CV.validator case _ of
    CV.Empty -> "Required."
    CV.Negative -> "Must by positive."
    CV.Invalid -> "Must by a number."

mkForm
  :: forall slots m
   . Monad m
  => AddressBook
  -> Form (InputSlots slots) m ContractInput ContractParams
mkForm addressBook =
  ContractParams
    <$> subform _nickname contractNicknameForm
    <*> subform _roles (roleForms addressBook)
    <*> subform _timeouts (Form.multiWithIndex timeoutForm)
    <*> subform _values (Form.multiWithIndex valueForm)

initialFormInput :: Set TokenName -> Set String -> Set String -> ContractInput
initialFormInput roles timeouts values =
  { nickname: ""
  , roles: Map.fromFoldable $ Set.map (flip Tuple "") roles
  , timeouts: Map.fromFoldable $ Set.map (flip Tuple "") timeouts
  , values: Map.fromFoldable $ Set.map (flip Tuple "") values
  }

component :: forall q m. MonadAff m => Component q m
component = Hooks.component \{ outputToken } input -> Hooks.do
  let { addressBook, roles, timeouts, values } = input
  form <- Hooks.captures { addressBook } Hooks.useMemo \_ -> mkForm addressBook
  { result, html } <- useForm form (initialFormInput roles timeouts values)
  let back = Hooks.raise outputToken Back
  let next = Hooks.raise outputToken <<< Next <$> result
  Hooks.pure do
    html []
