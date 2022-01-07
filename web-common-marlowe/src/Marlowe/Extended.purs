module Marlowe.Extended where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Reader (runReaderT)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , caseJsonObject
  , decodeJson
  , encodeJson
  )
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (foldMap, traverse)
import Data.Tuple (Tuple(..))
import Marlowe.Semantics (caseConstantFrom, getProp, object, requireProp)
import Marlowe.Semantics as S
import Marlowe.Template
  ( class Fillable
  , class Template
  , Placeholders(..)
  , TemplateContent
  , fillTemplate
  , getPlaceholderIds
  )
import Text.Pretty
  ( class Args
  , class Pretty
  , genericHasArgs
  , genericHasNestedArgs
  , genericPretty
  , pretty
  )

data ContractType
  = Escrow
  | EscrowWithCollateral
  | ZeroCouponBond
  | CouponBondGuaranteed
  | Swap
  | ContractForDifferences
  | Other

derive instance genericContractType :: Generic ContractType _

derive instance eqContractType :: Eq ContractType

derive instance ordContractType :: Ord ContractType

instance enumContractType :: Enum ContractType where
  succ = genericSucc
  pred = genericPred

instance boundedContractType :: Bounded ContractType where
  bottom = genericBottom
  top = genericTop

instance showContractType :: Show ContractType where
  show v = genericShow v

contractTypeArray :: Array ContractType
contractTypeArray =
  [ Escrow
  , EscrowWithCollateral
  , ZeroCouponBond
  , CouponBondGuaranteed
  , Swap
  , ContractForDifferences
  , Other
  ]

contractTypeInitials :: ContractType -> String
contractTypeInitials Escrow = "ES"

contractTypeInitials EscrowWithCollateral = "EC"

contractTypeInitials ZeroCouponBond = "ZC"

contractTypeInitials CouponBondGuaranteed = "CB"

contractTypeInitials Swap = "S"

contractTypeInitials ContractForDifferences = "CD"

contractTypeInitials Other = "O"

contractTypeName :: ContractType -> String
contractTypeName Escrow = "Escrow"

contractTypeName EscrowWithCollateral = "Escrow with Collateral"

contractTypeName ZeroCouponBond = "Zero Coupon Bond"

contractTypeName CouponBondGuaranteed = "Coupon Bond Guaranteed"

contractTypeName Swap = "Swap"

contractTypeName ContractForDifferences = "Contract for Differences"

contractTypeName Other = "Other"

initialsToContractType :: String -> ContractType
initialsToContractType "ES" = Escrow

initialsToContractType "EC" = EscrowWithCollateral

initialsToContractType "ZC" = ZeroCouponBond

initialsToContractType "CB" = CouponBondGuaranteed

initialsToContractType "S" = Swap

initialsToContractType "CD" = ContractForDifferences

initialsToContractType _ = Other

instance encodeJsonContractType :: EncodeJson ContractType where
  encodeJson = E.encode E.enum

instance decodeJsonContractType :: DecodeJson ContractType where
  decodeJson = D.decode D.enum

class ToCore a b where
  toCore :: a -> Maybe b

-- TODO: Should this be in here or in Marlowe.Template?
class HasChoices a where
  getChoiceNames :: a -> Set String

instance arrayHasChoices :: HasChoices a => HasChoices (Array a) where
  getChoiceNames = foldMap getChoiceNames

instance sChoiceIdHasChoices :: HasChoices S.ChoiceId where
  getChoiceNames (S.ChoiceId choiceName _) = Set.singleton choiceName

data Timeout
  = SlotParam String
  | Slot BigInt

derive instance genericTimeout :: Generic Timeout _

derive instance eqTimeout :: Eq Timeout

derive instance ordTimeout :: Ord Timeout

instance encodeJsonTimeout :: EncodeJson Timeout where
  encodeJson (SlotParam str) = encodeJson { slot_param: str }
  encodeJson (Slot val) = encodeJson val

instance decodeJsonTimeout :: DecodeJson Timeout where
  decodeJson json =
    lmap (Named "Timeout")
      $ caseJsonObject
          (lmap (Named "constructor Slot") $ Slot <$> decodeJson json)
          ( lmap (Named "constructor SlotParam")
              <<< runReaderT (SlotParam <$> requireProp "slot_param")
          )
          json

instance showTimeout :: Show Timeout where
  show (Slot x) = BigInt.toString x
  show v = genericShow v

instance prettyTimeout :: Pretty Timeout where
  pretty (Slot x) = pretty x
  pretty v = genericPretty v

instance hasArgsTimeout :: Args Timeout where
  hasArgs (Slot _) = false
  hasArgs x = genericHasArgs x
  hasNestedArgs (Slot _) = false
  hasNestedArgs x = genericHasNestedArgs x

instance toCoreTimeout :: ToCore Timeout S.Slot where
  toCore (SlotParam _) = Nothing
  toCore (Slot x) = Just (S.Slot x)

instance templateTimeout :: Template Timeout Placeholders where
  getPlaceholderIds (SlotParam slotParamId) = Placeholders
    (unwrap (mempty :: Placeholders))
      { slotPlaceholderIds = Set.singleton slotParamId }
  getPlaceholderIds (Slot _) = mempty

instance fillableTimeout :: Fillable Timeout TemplateContent where
  fillTemplate placeholders v@(SlotParam slotParamId) = maybe v Slot $
    Map.lookup slotParamId (unwrap placeholders).slotContent
  fillTemplate _ (Slot x) = Slot x

data Value
  = AvailableMoney S.AccountId S.Token
  | Constant BigInt
  | ConstantParam String
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | DivValue Value Value
  | ChoiceValue S.ChoiceId
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue S.ValueId
  | Cond Observation Value Value

derive instance genericValue :: Generic Value _

derive instance eqValue :: Eq Value

derive instance ordValue :: Ord Value

instance encodeJsonValue :: EncodeJson Value where
  encodeJson (AvailableMoney accId tok) =
    encodeJson
      { amount_of_token: tok
      , in_account: accId
      }
  encodeJson (Constant val) = encodeJson val
  encodeJson (ConstantParam str) = encodeJson { constant_param: str }
  encodeJson (NegValue val) =
    encodeJson
      { negate: val
      }
  encodeJson (AddValue lhs rhs) =
    encodeJson
      { add: lhs
      , and: rhs
      }
  encodeJson (SubValue lhs rhs) =
    encodeJson
      { value: lhs
      , minus: rhs
      }
  encodeJson (MulValue lhs rhs) =
    encodeJson
      { multiply: lhs
      , times: rhs
      }
  encodeJson (DivValue lhs rhs) =
    encodeJson
      { divide: lhs
      , by: rhs
      }
  encodeJson (ChoiceValue choiceId) =
    encodeJson
      { value_of_choice: choiceId
      }
  encodeJson SlotIntervalStart = encodeJson "slot_interval_start"
  encodeJson SlotIntervalEnd = encodeJson "slot_interval_end"
  encodeJson (UseValue valueId) =
    encodeJson
      { use_value: valueId
      }
  encodeJson (Cond cond thenValue elseValue) =
    encodeJson
      { if: cond
      , then: thenValue
      , else: elseValue
      }

instance decodeJsonValue :: DecodeJson Value where
  decodeJson = caseConstantFrom valueConstants \json ->
    Constant <$> decodeJson json <|> decodeObject json
    where
    valueConstants =
      Map.fromFoldable
        [ Tuple "slot_interval_start" SlotIntervalStart
        , Tuple "slot_interval_end" SlotIntervalEnd
        ]

    decodeObject =
      object "Value" do
        inAccount <- getProp "in_account"
        amountOfToken <- getProp "amount_of_token"
        constantParam <- getProp "constant_param"
        negate <- getProp "negate"
        add <- getProp "add"
        and <- getProp "and"
        value <- getProp "value"
        minus <- getProp "minus"
        divide <- getProp "divide"
        multiply <- getProp "multiply"
        by <- getProp "by"
        times <- getProp "times"
        valueOfChoices <- getProp "value_of_choice"
        useValue <- getProp "use_value"
        if_ <- getProp "if"
        then_ <- getProp "then"
        else_ <- getProp "else"
        pure
          $ (AvailableMoney <$> inAccount <*> amountOfToken)
              <|> (NegValue <$> negate)
              <|> (AddValue <$> add <*> and)
              <|> (ConstantParam <$> constantParam)
              <|> (SubValue <$> value <*> minus)
              <|> (DivValue <$> divide <*> by)
              <|> (MulValue <$> multiply <*> times)
              <|> (ChoiceValue <$> valueOfChoices)
              <|> (UseValue <$> useValue)
              <|> (Cond <$> if_ <*> then_ <*> else_)

instance showValue :: Show Value where
  show (Constant c) = BigInt.toString c
  show v = genericShow v

instance prettyValue :: Pretty Value where
  pretty v = genericPretty v

instance hasArgsValue :: Args Value where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCoreValue :: ToCore Value S.Value where
  toCore (Constant c) = Just $ S.Constant c
  toCore (ConstantParam _) = Nothing
  toCore (AvailableMoney accId tok) = S.AvailableMoney <$> pure accId <*> pure
    tok
  toCore (NegValue v) = S.NegValue <$> toCore v
  toCore (AddValue lhs rhs) = S.AddValue <$> toCore lhs <*> toCore rhs
  toCore (SubValue lhs rhs) = S.SubValue <$> toCore lhs <*> toCore rhs
  toCore (MulValue lhs rhs) = S.MulValue <$> toCore lhs <*> toCore rhs
  toCore (DivValue lhs rhs) = S.DivValue <$> toCore lhs <*> toCore rhs
  toCore (ChoiceValue choId) = Just $ S.ChoiceValue choId
  toCore SlotIntervalStart = Just $ S.SlotIntervalStart
  toCore SlotIntervalEnd = Just $ S.SlotIntervalEnd
  toCore (UseValue vId) = Just $ S.UseValue vId
  toCore (Cond obs lhs rhs) = S.Cond <$> toCore obs <*> toCore lhs <*> toCore
    rhs

instance templateValue :: Template Value Placeholders where
  getPlaceholderIds (ConstantParam constantParamId) = Placeholders
    (unwrap (mempty :: Placeholders))
      { valuePlaceholderIds = Set.singleton constantParamId }
  getPlaceholderIds (Constant _) = mempty
  getPlaceholderIds (AvailableMoney _ _) = mempty
  getPlaceholderIds (NegValue v) = getPlaceholderIds v
  getPlaceholderIds (AddValue lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (SubValue lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (MulValue lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (DivValue lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (ChoiceValue _) = mempty
  getPlaceholderIds SlotIntervalStart = mempty
  getPlaceholderIds SlotIntervalEnd = mempty
  getPlaceholderIds (UseValue _) = mempty
  getPlaceholderIds (Cond obs lhs rhs) = getPlaceholderIds obs
    <> getPlaceholderIds lhs
    <> getPlaceholderIds rhs

instance fillableValue :: Fillable Value TemplateContent where
  fillTemplate placeholders val = case val of
    Constant _ -> val
    ConstantParam constantParamId -> maybe val Constant $ Map.lookup
      constantParamId
      (unwrap placeholders).valueContent
    AvailableMoney _ _ -> val
    NegValue v -> NegValue $ go v
    AddValue lhs rhs -> AddValue (go lhs) (go rhs)
    SubValue lhs rhs -> SubValue (go lhs) (go rhs)
    MulValue lhs rhs -> MulValue (go lhs) (go rhs)
    DivValue lhs rhs -> DivValue (go lhs) (go rhs)
    ChoiceValue _ -> val
    SlotIntervalStart -> val
    SlotIntervalEnd -> val
    UseValue _ -> val
    Cond obs lhs rhs -> Cond (go obs) (go lhs) (go rhs)
    where
    go :: forall a. (Fillable a TemplateContent) => a -> a
    go = fillTemplate placeholders

instance valueHasChoices :: HasChoices Value where
  getChoiceNames (AvailableMoney _ _) = Set.empty
  getChoiceNames (Constant _) = Set.empty
  getChoiceNames (ConstantParam _) = Set.empty
  getChoiceNames (NegValue val) = getChoiceNames val
  getChoiceNames (AddValue lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (SubValue lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (MulValue lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (DivValue lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (ChoiceValue choId) = getChoiceNames choId
  getChoiceNames SlotIntervalStart = Set.empty
  getChoiceNames SlotIntervalEnd = Set.empty
  getChoiceNames (UseValue _) = Set.empty
  getChoiceNames (Cond obs lhs rhs) = getChoiceNames obs <> getChoiceNames lhs
    <> getChoiceNames rhs

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething S.ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs

derive instance genericObservation :: Generic Observation _

derive instance eqObservation :: Eq Observation

derive instance ordObservation :: Ord Observation

instance encodeJsonObservation :: EncodeJson Observation where
  encodeJson (AndObs lhs rhs) =
    encodeJson
      { both: lhs
      , and: rhs
      }
  encodeJson (OrObs lhs rhs) =
    encodeJson
      { either: lhs
      , or: rhs
      }
  encodeJson (NotObs obs) =
    encodeJson
      { not: obs
      }
  encodeJson (ChoseSomething choiceId) =
    encodeJson
      { chose_something_for: choiceId
      }
  encodeJson (ValueGE lhs rhs) =
    encodeJson
      { value: lhs
      , ge_than: rhs
      }
  encodeJson (ValueGT lhs rhs) =
    encodeJson
      { value: lhs
      , gt: rhs
      }
  encodeJson (ValueLT lhs rhs) =
    encodeJson
      { value: lhs
      , lt: rhs
      }
  encodeJson (ValueLE lhs rhs) =
    encodeJson
      { value: lhs
      , le_than: rhs
      }
  encodeJson (ValueEQ lhs rhs) =
    encodeJson
      { value: lhs
      , equal_to: rhs
      }
  encodeJson TrueObs = encodeJson true
  encodeJson FalseObs = encodeJson false

instance decodeJsonObservation :: DecodeJson Observation where
  decodeJson json = caseConstantFrom observationConstants decodeObject json
    where
    observationConstants =
      Map.fromFoldable
        [ Tuple true TrueObs
        , Tuple false FalseObs
        ]

    decodeObject =
      object "Observation" do
        both <- getProp "both"
        and <- getProp "and"
        either <- getProp "either"
        or <- getProp "or"
        not <- getProp "not"
        choseSomethingFor <- getProp "chose_something_for"
        value <- getProp "value"
        gte <- getProp "ge_than"
        gt <- getProp "gt"
        lt <- getProp "lt"
        lte <- getProp "le_than"
        equalTo <- getProp "equal_to"
        pure
          $ (AndObs <$> both <*> and)
              <|> (OrObs <$> either <*> or)
              <|> (NotObs <$> not)
              <|> (ChoseSomething <$> choseSomethingFor)
              <|> (ValueGE <$> value <*> gte)
              <|> (ValueGT <$> value <*> gt)
              <|> (ValueLT <$> value <*> lt)
              <|> (ValueLE <$> value <*> lte)
              <|> (ValueEQ <$> value <*> equalTo)

instance showObservation :: Show Observation where
  show o = genericShow o

instance prettyObservation :: Pretty Observation where
  pretty v = genericPretty v

instance hasArgsObservation :: Args Observation where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCoreObservation :: ToCore Observation S.Observation where
  toCore (AndObs lhs rhs) = S.AndObs <$> toCore lhs <*> toCore rhs
  toCore (OrObs lhs rhs) = S.OrObs <$> toCore lhs <*> toCore rhs
  toCore (NotObs v) = S.NotObs <$> toCore v
  toCore (ChoseSomething choId) = Just $ S.ChoseSomething choId
  toCore (ValueGE lhs rhs) = S.ValueGE <$> toCore lhs <*> toCore rhs
  toCore (ValueGT lhs rhs) = S.ValueGT <$> toCore lhs <*> toCore rhs
  toCore (ValueLT lhs rhs) = S.ValueLT <$> toCore lhs <*> toCore rhs
  toCore (ValueLE lhs rhs) = S.ValueLE <$> toCore lhs <*> toCore rhs
  toCore (ValueEQ lhs rhs) = S.ValueEQ <$> toCore lhs <*> toCore rhs
  toCore TrueObs = Just S.TrueObs
  toCore FalseObs = Just S.FalseObs

instance templateObservation :: Template Observation Placeholders where
  getPlaceholderIds (AndObs lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (OrObs lhs rhs) = getPlaceholderIds lhs <> getPlaceholderIds
    rhs
  getPlaceholderIds (NotObs v) = getPlaceholderIds v
  getPlaceholderIds (ChoseSomething _) = mempty
  getPlaceholderIds (ValueGE lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (ValueGT lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (ValueLT lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (ValueLE lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds (ValueEQ lhs rhs) = getPlaceholderIds lhs <>
    getPlaceholderIds rhs
  getPlaceholderIds TrueObs = mempty
  getPlaceholderIds FalseObs = mempty

instance fillableObservation :: Fillable Observation TemplateContent where
  fillTemplate placeholders obs = case obs of
    AndObs lhs rhs -> AndObs (go lhs) (go rhs)
    OrObs lhs rhs -> OrObs (go lhs) (go rhs)
    NotObs v -> NotObs (go v)
    ChoseSomething _ -> obs
    ValueGE lhs rhs -> ValueGE (go lhs) (go rhs)
    ValueGT lhs rhs -> ValueGT (go lhs) (go rhs)
    ValueLT lhs rhs -> ValueLT (go lhs) (go rhs)
    ValueLE lhs rhs -> ValueLE (go lhs) (go rhs)
    ValueEQ lhs rhs -> ValueEQ (go lhs) (go rhs)
    TrueObs -> obs
    FalseObs -> obs
    where
    go :: forall a. (Fillable a TemplateContent) => a -> a
    go = fillTemplate placeholders

instance observationHasChoices :: HasChoices Observation where
  getChoiceNames (AndObs lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (OrObs lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (NotObs v) = getChoiceNames v
  getChoiceNames (ChoseSomething a) = getChoiceNames a
  getChoiceNames (ValueGE lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (ValueGT lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (ValueLT lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (ValueLE lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames (ValueEQ lhs rhs) = getChoiceNames lhs <> getChoiceNames rhs
  getChoiceNames TrueObs = Set.empty
  getChoiceNames FalseObs = Set.empty

data Action
  = Deposit S.AccountId S.Party S.Token Value
  | Choice S.ChoiceId (Array S.Bound)
  | Notify Observation

derive instance genericAction :: Generic Action _

derive instance eqAction :: Eq Action

derive instance ordAction :: Ord Action

instance encodeJsonAction :: EncodeJson Action where
  encodeJson (Deposit accountId party token value) =
    encodeJson
      { party: party
      , deposits: value
      , of_token: token
      , into_account: accountId
      }
  encodeJson (Choice choiceId boundArray) =
    encodeJson
      { choose_between: boundArray
      , for_choice: choiceId
      }
  encodeJson (Notify obs) =
    encodeJson
      { notify_if: obs
      }

instance decodeJsonAction :: DecodeJson Action where
  decodeJson =
    object "Action" do
      intoAccount <- getProp "into_account"
      party <- getProp "party"
      ofToken <- getProp "of_token"
      deposits <- getProp "deposits"
      forChoice <- getProp "for_choice"
      chooseBetween <- getProp "choose_between"
      notifyIf <- getProp "notify_if"
      pure
        $ (Deposit <$> intoAccount <*> party <*> ofToken <*> deposits)
            <|> (Choice <$> forChoice <*> chooseBetween)
            <|> (Notify <$> notifyIf)

instance showAction :: Show Action where
  show (Choice cid bounds) = "(Choice " <> show cid <> " " <> show bounds <> ")"
  show v = genericShow v

instance prettyAction :: Pretty Action where
  pretty v = genericPretty v

instance hasArgsAction :: Args Action where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCoreAction :: ToCore Action S.Action where
  toCore (Deposit accId party tok val) = S.Deposit <$> pure accId <*> pure party
    <*> pure tok
    <*> toCore val
  toCore (Choice choId bounds) = Just $ S.Choice choId bounds
  toCore (Notify obs) = S.Notify <$> toCore obs

instance templateAction :: Template Action Placeholders where
  getPlaceholderIds (Deposit _ _ _ val) = getPlaceholderIds val
  getPlaceholderIds (Choice _ _) = mempty
  getPlaceholderIds (Notify obs) = getPlaceholderIds obs

instance fillableAction :: Fillable Action TemplateContent where
  fillTemplate placeholders action = case action of
    Deposit accId party tok val -> Deposit accId party tok $ go val
    Choice _ _ -> action
    Notify obs -> Notify $ go obs
    where
    go :: forall a. (Fillable a TemplateContent) => a -> a
    go = fillTemplate placeholders

instance actionHasChoices :: HasChoices Action where
  getChoiceNames (Deposit _ _ _ value) = getChoiceNames value
  getChoiceNames (Choice choId _) = getChoiceNames choId
  getChoiceNames (Notify obs) = getChoiceNames obs

data Payee
  = Account S.AccountId
  | Party S.Party

derive instance genericPayee :: Generic Payee _

derive instance eqPayee :: Eq Payee

derive instance ordPayee :: Ord Payee

instance encodeJsonPayee :: EncodeJson Payee where
  encodeJson (Account accountId) = encodeJson { account: accountId }
  encodeJson (Party party) = encodeJson { party: party }

instance decodeJsonPayee :: DecodeJson Payee where
  decodeJson =
    object "Payee" do
      account <- getProp "account"
      party <- getProp "party"
      pure $ (Account <$> account) <|> (Party <$> party)

instance showPayee :: Show Payee where
  show v = genericShow v

instance prettyPayee :: Pretty Payee where
  pretty v = genericPretty v

instance hasArgsPayee :: Args Payee where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCorePayee :: ToCore Payee S.Payee where
  toCore (Account accId) = Just $ S.Account accId
  toCore (Party roleName) = Just $ S.Party roleName

data Case
  = Case Action Contract

derive instance genericCase :: Generic Case _

derive instance eqCase :: Eq Case

derive instance ordCase :: Ord Case

instance encodeJsonCase :: EncodeJson Case where
  encodeJson (Case action cont) =
    encodeJson
      { case: action
      , then: cont
      }

instance decodeJsonCase :: DecodeJson Case where
  decodeJson =
    object "Case"
      $ Just
          <$> (Case <$> requireProp "case" <*> requireProp "then")

instance showCase :: Show Case where
  show (Case action contract) = "Case " <> show action <> " " <> show contract

instance prettyCase :: Pretty Case where
  pretty v = genericPretty v

instance hasArgsCase :: Args Case where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCoreCase :: ToCore Case S.Case where
  toCore (Case act c) = S.Case <$> toCore act <*> toCore c

instance templateCase :: Template Case Placeholders where
  getPlaceholderIds (Case act c) = getPlaceholderIds act <> getPlaceholderIds c

instance fillableCase :: Fillable Case TemplateContent where
  fillTemplate placeholders (Case act c) = Case (go act) (go c)
    where
    go :: forall a. (Fillable a TemplateContent) => a -> a
    go = fillTemplate placeholders

instance caseHasChoices :: HasChoices Case where
  getChoiceNames (Case action contract) = getChoiceNames action <>
    getChoiceNames contract

data Contract
  = Close
  | Pay S.AccountId Payee S.Token Value Contract
  | If Observation Contract Contract
  | When (Array Case) Timeout Contract
  | Let S.ValueId Value Contract
  | Assert Observation Contract

derive instance genericContract :: Generic Contract _

derive instance eqContract :: Eq Contract

derive instance ordContract :: Ord Contract

instance encodeJsonContract :: EncodeJson Contract where
  encodeJson Close = encodeJson "close"
  encodeJson (Pay accId payee token val cont) =
    encodeJson
      { pay: val
      , token: token
      , from_account: accId
      , to: payee
      , then: cont
      }
  encodeJson (If obs contTrue contFalse) =
    encodeJson
      { if: obs
      , then: contTrue
      , else: contFalse
      }
  encodeJson (When cases timeout cont) =
    encodeJson
      { when: cases
      , timeout: timeout
      , timeout_continuation: cont
      }
  encodeJson (Let valId val cont) =
    encodeJson
      { let: valId
      , be: val
      , then: cont
      }
  encodeJson (Assert obs cont) =
    encodeJson
      { assert: obs
      , then: cont
      }

instance decodeJsonContract :: DecodeJson Contract where
  decodeJson = caseConstantFrom (Map.singleton "close" Close) decodeObject
    where
    decodeObject =
      object "Contract" do
        fromAccount <- getProp "from_account"
        to <- getProp "to"
        token <- getProp "token"
        pay <- getProp "pay"
        _then <- getProp "then"
        _if <- getProp "if"
        _else <- getProp "else"
        when <- getProp "when"
        timeout <- getProp "timeout"
        timeoutContinuation <- getProp "timeout_continuation"
        _let <- getProp "let"
        be <- getProp "be"
        assert <- getProp "assert"
        pure
          $ (Pay <$> fromAccount <*> to <*> token <*> pay <*> _then)
              <|> (If <$> _if <*> _then <*> _else)
              <|> (When <$> when <*> timeout <*> timeoutContinuation)
              <|> (Let <$> _let <*> be <*> _then)
              <|> (Assert <$> assert <*> _then)

instance showContract :: Show Contract where
  show v = genericShow v

instance prettyContract :: Pretty Contract where
  pretty v = genericPretty v

instance hasArgsContract :: Args Contract where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

instance toCoreContract :: ToCore Contract S.Contract where
  toCore Close = Just S.Close
  toCore (Pay accId payee tok val cont) = S.Pay <$> pure accId <*> toCore payee
    <*> pure tok
    <*> toCore val
    <*> toCore cont
  toCore (If obs cont1 cont2) = S.If <$> toCore obs <*> toCore cont1 <*> toCore
    cont2
  toCore (When cases tim cont) = S.When <$> traverse toCore cases <*> toCore tim
    <*> toCore cont
  toCore (Let varId val cont) = S.Let <$> pure varId <*> toCore val <*> toCore
    cont
  toCore (Assert obs cont) = S.Assert <$> toCore obs <*> toCore cont

instance templateContract :: Template Contract Placeholders where
  getPlaceholderIds Close = mempty
  getPlaceholderIds (Pay _ _ _ val cont) = getPlaceholderIds val <>
    getPlaceholderIds cont
  getPlaceholderIds (If obs cont1 cont2) = getPlaceholderIds obs
    <> getPlaceholderIds cont1
    <> getPlaceholderIds cont2
  getPlaceholderIds (When cases tim cont) = foldMap getPlaceholderIds cases
    <> getPlaceholderIds tim
    <> getPlaceholderIds cont
  getPlaceholderIds (Let _ val cont) = getPlaceholderIds val <>
    getPlaceholderIds cont
  getPlaceholderIds (Assert obs cont) = getPlaceholderIds obs <>
    getPlaceholderIds cont

instance fillableContract :: Fillable Contract TemplateContent where
  fillTemplate placeholders contract = case contract of
    Close -> Close
    Pay accId payee tok val cont -> Pay accId payee tok (go val) (go cont)
    If obs cont1 cont2 -> If (go obs) (go cont1) (go cont2)
    When cases tim cont -> When (map go cases) (go tim) (go cont)
    Let varId val cont -> Let varId (go val) (go cont)
    Assert obs cont -> Assert (go obs) (go cont)
    where
    go :: forall a. (Fillable a TemplateContent) => a -> a
    go = fillTemplate placeholders

instance contractHasChoices :: HasChoices Contract where
  getChoiceNames Close = Set.empty
  getChoiceNames (Pay _ _ _ val cont) = getChoiceNames val <> getChoiceNames
    cont
  getChoiceNames (If obs cont1 cont2) = getChoiceNames obs
    <> getChoiceNames cont1
    <> getChoiceNames cont2
  getChoiceNames (When cases _ cont) = getChoiceNames cases <> getChoiceNames
    cont
  getChoiceNames (Let _ val cont) = getChoiceNames val <> getChoiceNames cont
  getChoiceNames (Assert obs cont) = getChoiceNames obs <> getChoiceNames cont

-- In the extended marlowe we are treating Slot's as relative times to an initial slot and the
-- SlotParam as absolute times. This function will recurse on a contract making the relative slots
-- absolute
resolveRelativeTimes :: S.Slot -> Contract -> Contract
resolveRelativeTimes (S.Slot baseSlot) contract = relativeContract contract
  where
  relativeContract = case _ of
    Close -> Close
    Pay a p t v contract' -> Pay a p t v (relativeContract contract')
    If obs contract1 contract2 -> If obs (relativeContract contract1)
      (relativeContract contract2)
    When cases timeout contract' -> When (relativeCase <$> cases)
      (relativeTimeout timeout)
      (relativeContract contract')
    Let vid v contract' -> Let vid v (relativeContract contract')
    Assert obs contract' -> Assert obs (relativeContract contract')

  relativeTimeout = case _ of
    Slot t -> Slot $ t + baseSlot
    slotParam -> slotParam

  relativeCase (Case action contract') = Case action
    (relativeContract contract')
