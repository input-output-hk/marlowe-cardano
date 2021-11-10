module Marlowe.Semantics where

import Prologue
import Control.Alt ((<|>))
import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  , getField
  , getFieldOptional
  )
import Data.Argonaut.Core (fromArray)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Decode.Decoders (decodeJArray, decodeJObject)
import Data.Argonaut.Encode.Aeson as E
import Data.Argonaut.Encode.Encoders (encodeArray)
import Data.Array (catMaybes, (!!))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.BigInt.Argonaut as BigInt
import Data.Either (note')
import Data.Foldable (class Foldable, any, foldl, minimum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over, to, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable, reverse, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs, signum)
import Data.Show.Generic (genericShow)
import Data.String (joinWith, toLower)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Text.Pretty
  ( class Args
  , class Pretty
  , genericHasArgs
  , genericHasNestedArgs
  , genericPretty
  , pretty
  , text
  )
import Type.Proxy (Proxy(..))

caseConstantFrom
  :: forall a b
   . Ord a
  => Show a
  => DecodeJson a
  => Map a b
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError b
caseConstantFrom values onFail json = case decodeJson json of
  Left _ -> onFail json
  Right value -> note' mkError $ Map.lookup value values
  where
  mkError _ =
    TypeMismatch
      $ joinWith " | " (map show $ Array.fromFoldable $ Map.keys values)

object
  :: forall a
   . String
  -> (ReaderT (Object Json) (Either JsonDecodeError) (Maybe a))
  -> Json
  -> Either JsonDecodeError a
object name decoder json =
  lmap (Named name) do
    obj <- decodeJObject json
    result <- runReaderT decoder obj
    maybe (Left $ UnexpectedValue json) Right result

getProp
  :: forall a
   . DecodeJson a
  => String
  -> ReaderT (Object Json) (Either JsonDecodeError) (Maybe a)
getProp key = ReaderT $ flip getFieldOptional key

requireProp
  :: forall a
   . DecodeJson a
  => String
  -> ReaderT (Object Json) (Either JsonDecodeError) a
requireProp key = ReaderT $ flip getField key

array
  :: forall a
   . String
  -> (RWST (Array Json) Unit Int (Either JsonDecodeError) a)
  -> Json
  -> Either JsonDecodeError a
array name decoder json =
  lmap (Named name) do
    arr <- decodeJArray json
    fst <$> evalRWST decoder arr 0

next
  :: forall a
   . DecodeJson a
  => RWST (Array Json) Unit Int (Either JsonDecodeError) a
next =
  RWST \arr ix ->
    RWSResult
      <$> pure (ix + 1)
      <*> maybe (Left $ AtIndex ix MissingValue) decodeJson (arr !! ix)
      <*> pure unit

type PubKey
  = String

type PubKeyHash
  = String

data Party
  = PK PubKey
  | Role TokenName

derive instance genericParty :: Generic Party _

derive instance eqParty :: Eq Party

derive instance ordParty :: Ord Party

instance encodeJsonParty :: EncodeJson Party where
  encodeJson (PK pubKey) = encodeJson { pk_hash: pubKey }
  encodeJson (Role tokName) = encodeJson { role_token: tokName }

instance decodeJsonParty :: DecodeJson Party where
  decodeJson =
    object "Party" do
      pkHash <- getProp "pk_hash"
      roleToken <- getProp "role_token"
      pure
        $ (PK <$> pkHash)
            <|> (Role <$> roleToken)

instance showParty :: Show Party where
  show = genericShow

instance prettyParty :: Pretty Party where
  pretty = genericPretty

instance hasArgsParty :: Args Party where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

type Timeout
  = Slot

type Money
  = Assets

type CurrencySymbol
  = String

type TokenName
  = String

data Token
  = Token CurrencySymbol TokenName

instance encodeJsonToken :: EncodeJson Token where
  encodeJson (Token cur tok) =
    encodeJson
      { currency_symbol: cur
      , token_name: tok
      }

type TokenJson
  =
  { currency :: { unCurrencySymbol :: String }
  , token :: { unTokenName :: String }
  }

instance decodeJsonToken :: DecodeJson Token where
  decodeJson =
    object "Token"
      $ Just
          <$>
            ( Token <$> requireProp "currency_symbol" <*> requireProp
                "token_name"
            )

derive instance genericToken :: Generic Token _

instance eqToken :: Eq Token where
  eq (Token cur1 tok1) (Token cur2 tok2) = eq (toLower cur1) (toLower cur2) &&
    eq tok1 tok2

instance ordToken :: Ord Token where
  compare (Token cur1 tok1) (Token cur2 tok2) =
    case compare (toLower cur1) (toLower cur2) of
      EQ -> compare tok1 tok2
      other -> other

instance showToken :: Show Token where
  show (Token cur tok) = genericShow (Token (toLower cur) tok)

instance prettyToken :: Pretty Token where
  pretty = genericPretty

instance hasArgsToken :: Args Token where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

type ChosenNum
  = BigInt

type Accounts
  = Map (Tuple AccountId Token) BigInt

type ChoiceName
  = String

newtype Assets
  = Assets (Map CurrencySymbol (Map TokenName BigInt))

derive instance genericAssets :: Generic Assets _

derive instance newtypeAssets :: Newtype Assets _

derive instance eqAssets :: Eq Assets

derive instance ordAssets :: Ord Assets

derive newtype instance showAssets :: Show Assets

derive newtype instance encodeAssets :: EncodeJson Assets

derive newtype instance decodeAssets :: DecodeJson Assets

instance semigroupAssets :: Semigroup Assets where
  append (Assets a) (Assets b) = Assets (Map.unionWith f a b)
    where
    f = Map.unionWith (+)

instance monoidAssets :: Monoid Assets where
  mempty = Assets Map.empty

newtype Slot
  = Slot BigInt

derive newtype instance encodeJsonSlot :: EncodeJson Slot

derive newtype instance decodeJsonSlot :: DecodeJson Slot

derive instance genericSlot :: Generic Slot _

derive instance newtypeSlot :: Newtype Slot _

derive instance eqSlot :: Eq Slot

derive instance ordSlot :: Ord Slot

instance showSlot :: Show Slot where
  show (Slot s) = BigInt.toString s

derive newtype instance semiRingSlot :: Semiring Slot

derive newtype instance ringSlot :: Ring Slot

instance commutativeRingSlot :: CommutativeRing Slot

derive newtype instance euclideanRingSlot :: EuclideanRing Slot

derive newtype instance prettySlot :: Pretty Slot

derive newtype instance hasArgsSlot :: Args Slot

newtype Ada
  = Lovelace BigInt

derive newtype instance encodeJsonAda :: EncodeJson Ada

derive newtype instance decodeJsonAda :: DecodeJson Ada

derive instance genericAda :: Generic Ada _

derive instance newtypeAda :: Newtype Ada _

derive instance eqAda :: Eq Ada

derive instance ordAda :: Ord Ada

instance showAda :: Show Ada where
  show (Lovelace a) = BigInt.toString a

derive newtype instance semiringAda :: Semiring Ada

derive newtype instance ringAda :: Ring Ada

derive newtype instance euclideanRingAda :: EuclideanRing Ada

instance commutativeRingAda :: CommutativeRing Ada

type AccountId
  = Party

data ChoiceId
  = ChoiceId String Party

derive instance genericChoiceId :: Generic ChoiceId _

derive instance eqChoiceId :: Eq ChoiceId

derive instance ordChoiceId :: Ord ChoiceId

instance encodeJsonChoiceId :: EncodeJson ChoiceId where
  encodeJson (ChoiceId name owner) =
    encodeJson
      { choice_name: name
      , choice_owner: owner
      }

instance decodeJsonChoiceId :: DecodeJson ChoiceId where
  decodeJson =
    object "ChoiceId"
      $ Just
          <$>
            ( ChoiceId <$> requireProp "choice_name" <*> requireProp
                "choice_owner"
            )

instance showChoiceId :: Show ChoiceId where
  show (ChoiceId name owner) = "(ChoiceId " <> show name <> " " <> show owner <>
    ")"

instance prettyChoiceId :: Pretty ChoiceId where
  pretty = genericPretty

instance hasArgsChoiceId :: Args ChoiceId where
  hasArgs = genericHasArgs
  hasNestedArgs = genericHasNestedArgs

choiceOwner :: ChoiceId -> Party
choiceOwner (ChoiceId _ owner) = owner

newtype ValueId
  = ValueId String

derive instance genericValueId :: Generic ValueId _

derive instance newtypeValueId :: Newtype ValueId _

derive instance eqValueId :: Eq ValueId

derive instance ordValueId :: Ord ValueId

derive newtype instance encodeJsonValueId :: EncodeJson ValueId

derive newtype instance decodeJsonValueId :: DecodeJson ValueId

instance showValueId :: Show ValueId where
  show (ValueId valueId) = show valueId

instance prettyValueId :: Pretty ValueId where
  pretty (ValueId valueId) = text $ show valueId

instance hasArgsValueId :: Args ValueId where
  hasArgs _ = false
  hasNestedArgs _ = false

data Rational
  = Rational BigInt BigInt

derive instance genericRational :: Generic Rational _

derive instance eqRational :: Eq Rational

derive instance ordRational :: Ord Rational

instance encodeJsonRational :: EncodeJson Rational where
  encodeJson (Rational n d) = encodeJson [ n, d ]

instance decodeJsonRational :: DecodeJson Rational where
  decodeJson = array "Rational" $ Rational <$> next <*> next

instance showRational :: Show Rational where
  show (Rational n d) = "(" <> show (pretty n) <> "%" <> show (pretty d) <> ")"

instance prettyRational :: Pretty Rational where
  pretty r = text $ show r

instance hasArgsRational :: Args Rational where
  hasArgs _ = false
  hasNestedArgs _ = false

data Value
  = AvailableMoney AccountId Token
  | Constant BigInt
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | DivValue Value Value
  | ChoiceValue ChoiceId
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue ValueId
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
  decodeJson =
    caseConstantFrom valueConstants \json ->
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
              <|> (SubValue <$> value <*> minus)
              <|> (DivValue <$> divide <*> by)
              <|> (MulValue <$> multiply <*> times)
              <|> (ChoiceValue <$> valueOfChoices)
              <|> (UseValue <$> useValue)
              <|> (Cond <$> if_ <*> then_ <*> else_)

instance showValue :: Show Value where
  show v = genericShow v

instance prettyValue :: Pretty Value where
  pretty v = genericPretty v

instance hasArgsValue :: Args Value where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
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

validInterval :: SlotInterval -> Boolean
validInterval (SlotInterval from to) = from <= to

above :: Slot -> SlotInterval -> Boolean
above v (SlotInterval _ to) = v > to

anyWithin :: forall f. Foldable f => Slot -> f SlotInterval -> Boolean
anyWithin v = any (\(SlotInterval from to) -> v >= from && v <= to)

data SlotInterval
  = SlotInterval Slot Slot

derive instance genericSlotInterval :: Generic SlotInterval _

derive instance eqSlotInterval :: Eq SlotInterval

derive instance ordSlotInterval :: Ord SlotInterval

instance showSlotInterval :: Show SlotInterval where
  show (SlotInterval from to) = "(Slot " <> show from <> " " <> show to <> ")"

instance genericEncodeSlotInterval :: EncodeJson SlotInterval where
  encodeJson (SlotInterval a b) = encodeArray encodeJson [ a, b ]

instance genericDecodeJsonSlotInterval :: DecodeJson SlotInterval where
  decodeJson = array "SlotInterval" $ SlotInterval <$> next <*> next

ivFrom :: SlotInterval -> Slot
ivFrom (SlotInterval from _) = from

ivTo :: SlotInterval -> Slot
ivTo (SlotInterval _ to) = to

data Bound
  = Bound BigInt BigInt

derive instance genericBound :: Generic Bound _

derive instance eqBound :: Eq Bound

derive instance orBound :: Ord Bound

instance encodeJsonBound :: EncodeJson Bound where
  encodeJson (Bound fromSlot toSlot) =
    encodeJson
      { from: fromSlot
      , to: toSlot
      }

instance decodeJsonBound :: DecodeJson Bound where
  decodeJson =
    object "Bound"
      $ Just
          <$> (Bound <$> requireProp "from" <*> requireProp "to")

instance showBound :: Show Bound where
  show (Bound from to) = "(Bound " <> BigInt.toString from <> " "
    <> BigInt.toString to
    <> ")"

instance prettyBound :: Pretty Bound where
  pretty v = genericPretty v

instance hasArgsBound :: Args Bound where
  hasArgs a = genericHasArgs a
  hasNestedArgs a = genericHasNestedArgs a

getEncompassBound :: forall f. Foldable f => Functor f => f Bound -> Bound
getEncompassBound bounds =
  -- NOTE: We can't use the Min/Max semigroup with foldMap and we need to make a fromInt top/bottom
  --       because BigInt doesn't have a Bounded instance. This should be fine in reality
  --       but it could lead to a bug if the lower bound is bigger than the biggest Int or if
  --       the highest bound is lower than the smallest Int.
  -- NOTE': We fold the datastructure twice instead of doing it in a single pass for simplicity
  --        in reality we shouldn't have many bounds, so it should be fine.
  let
    minBound = foldl min (fromInt top) $ map (\(Bound lower _) -> lower) bounds

    maxBound = foldl max (fromInt bottom) $ map (\(Bound _ higher) -> higher)
      bounds
  in
    Bound minBound maxBound

-- Possible actions that can be taken inside a `When` contract
data Action
  = {-
    Wait until `Party` makes a `Deposit` into `AccountId` of the ammount `Value` with `Token` currency.
  -} Deposit AccountId Party Token Value
  {-
    Wait for `ChoiceId _ Party` to take the named `ChoiceId String _` choice between the different Bound
  -}
  | Choice ChoiceId (Array Bound)
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

data Payee
  = Account AccountId
  | Party Party

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

data Contract
  = Close
  | Pay AccountId Payee Token Value Contract
  | If Observation Contract Contract
  | When (Array Case) Timeout Contract
  | Let ValueId Value Contract
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

newtype State
  = State
  { accounts :: Accounts
  , choices :: Map ChoiceId ChosenNum
  , boundValues :: Map ValueId BigInt
  -- The minSlot is a lower bound for the current slot. When we are in the context of a Wallet or Dashboard
  -- we can just ask the time and calculate the current blockchain slot. But when we just have the context
  -- of a running contract we can't know the exact slot as transactions only provide an interval
  -- [lowSlot, highSlot].
  -- So the minSlot is the maximum number of the lowSlot we had so far, and we know that the current slot is
  -- higher or equal than that (because slots don't go back in time).
  -- The reason we keep track of it, is so that we can refine transaction intervals.
  -- If in a new transaction we have a lowSlot that is smaller than the minSlot, we can narrow the interval
  -- from [lowSlot, highSlot] to [minSlot, highSlot]
  , minSlot :: Slot
  }

derive instance genericState :: Generic State _

derive instance newtypeState :: Newtype State _

derive instance eqState :: Eq State

derive instance ordState :: Ord State

instance showState :: Show State where
  show v = genericShow v

derive newtype instance encodeState :: EncodeJson State

derive newtype instance decodeState :: DecodeJson State

_accounts :: Lens' State (Accounts)
_accounts = _Newtype <<< prop (Proxy :: _ "accounts")

_choices :: Lens' State (Map ChoiceId ChosenNum)
_choices = _Newtype <<< prop (Proxy :: _ "choices")

_boundValues :: Lens' State (Map ValueId BigInt)
_boundValues = _Newtype <<< prop (Proxy :: _ "boundValues")

_minSlot :: Lens' State Slot
_minSlot = _Newtype <<< prop (Proxy :: _ "minSlot")

newtype Environment
  = Environment { slotInterval :: SlotInterval }

derive instance genericEnvironment :: Generic Environment _

derive instance newtypeEnvironment :: Newtype Environment _

derive instance eqEnvironment :: Eq Environment

derive instance ordEnvironment :: Ord Environment

instance showEnvironment :: Show Environment where
  show v = genericShow v

_slotInterval :: Lens' Environment SlotInterval
_slotInterval = _Newtype <<< prop (Proxy :: _ "slotInterval")

makeEnvironment :: BigInt -> BigInt -> Environment
makeEnvironment l h = Environment
  { slotInterval: SlotInterval (Slot h) (Slot l) }

data Input
  = IDeposit AccountId Party Token BigInt
  | IChoice ChoiceId ChosenNum
  | INotify

derive instance genericInput :: Generic Input _

derive instance eqInput :: Eq Input

derive instance ordInput :: Ord Input

instance showInput :: Show Input where
  show v = genericShow v

instance encodeJsonInput :: EncodeJson Input where
  encodeJson (IDeposit accId party tok amount) =
    encodeJson
      { input_from_party: party
      , that_deposits: amount
      , of_token: tok
      , into_account: accId
      }
  encodeJson (IChoice choiceId chosenNum) =
    encodeJson
      { input_that_chooses_num: chosenNum
      , for_choice_id: choiceId
      }
  encodeJson INotify = encodeJson "input_notify"

instance decodeJsonInput :: DecodeJson Input where
  decodeJson =
    caseConstantFrom
      (Map.singleton "input_notify" INotify)
      decodeObject
    where
    decodeObject =
      object "Action" do
        intoAccount <- getProp "into_account"
        inputFromParty <- getProp "input_from_party"
        ofToken <- getProp "of_token"
        thatDeposits <- getProp "that_deposits"
        forChoiceId <- getProp "for_choice_id"
        inputThatChoosesNum <- getProp "input_that_chooses_num"
        pure
          $
            ( IDeposit
                <$> intoAccount
                <*> inputFromParty
                <*> ofToken
                <*> thatDeposits
            )
              <|> (IChoice <$> forChoiceId <*> inputThatChoosesNum)

-- Processing of slot interval
data IntervalError
  = InvalidInterval SlotInterval
  | IntervalInPastError Slot SlotInterval

derive instance genericIntervalError :: Generic IntervalError _

derive instance eqIntervalError :: Eq IntervalError

derive instance ordIntervalError :: Ord IntervalError

instance showIntervalError :: Show IntervalError where
  show (InvalidInterval interval) = "Invalid interval: " <> show interval
  show (IntervalInPastError slot interval) =
    "Interval is in the past, the current slot is " <> show slot
      <> " but the interval is "
      <> show interval

instance genericEncodeIntervalError :: EncodeJson IntervalError where
  encodeJson = case _ of
    InvalidInterval a -> E.encodeTagged "InvalidInterval" a E.value
    IntervalInPastError a b -> E.encodeTagged "IntervalInPastError" (a /\ b)
      E.value

instance genericDecodeJsonIntervalError :: DecodeJson IntervalError where
  decodeJson =
    D.decode
      $ D.sumType "IntervalError"
      $ Map.fromFoldable
          [ "InvalidInterval" /\ D.content (InvalidInterval <$> D.value)
          , "IntervalInPastError" /\ D.content
              (uncurry IntervalInPastError <$> D.value)
          ]

data IntervalResult
  = IntervalTrimmed Environment State
  | IntervalError IntervalError

derive instance genericIntervalResult :: Generic IntervalResult _

derive instance eqIntervalResult :: Eq IntervalResult

derive instance ordIntervalResult :: Ord IntervalResult

instance showIntervalResult :: Show IntervalResult where
  show v = genericShow v

data Payment
  = Payment AccountId Payee Money

derive instance genericPayment :: Generic Payment _

derive instance eqPayment :: Eq Payment

derive instance ordPayment :: Ord Payment

instance showPayment :: Show Payment where
  show = genericShow

instance encodePayment :: EncodeJson Payment where
  encodeJson (Payment a p m) = fromArray
    [ encodeJson a, encodeJson p, encodeJson m ]

instance decodePayment :: DecodeJson Payment where
  decodeJson = D.decode $ D.tuple $ Payment </$\> D.value </*\> D.value </*\>
    D.value

data ReduceEffect
  = ReduceWithPayment Payment
  | ReduceNoPayment

derive instance genericReduceEffect :: Generic ReduceEffect _

derive instance eqReduceEffect :: Eq ReduceEffect

instance showReduceEffect :: Show ReduceEffect where
  show = genericShow

data ReduceWarning
  = ReduceNoWarning
  | ReduceNonPositivePay AccountId Payee Token BigInt
  ---------------------- v src v dest v paid v expected
  | ReducePartialPay AccountId Payee Token BigInt BigInt
  -------------------------- v oldVal  v newVal
  | ReduceShadowing ValueId BigInt BigInt
  | ReduceAssertionFailed

derive instance genericReduceWarning :: Generic ReduceWarning _

derive instance eqReduceWarning :: Eq ReduceWarning

derive instance ordReduceWarning :: Ord ReduceWarning

instance showReduceWarning :: Show ReduceWarning where
  show = genericShow

data ReduceStepResult
  = Reduced ReduceWarning ReduceEffect State Contract
  | NotReduced
  | AmbiguousSlotIntervalReductionError

derive instance genericReduceStepResult :: Generic ReduceStepResult _

derive instance eqReduceStepResult :: Eq ReduceStepResult

instance showReduceStepResult :: Show ReduceStepResult where
  show = genericShow

data ReduceResult
  = ContractQuiescent Boolean (List ReduceWarning) (List Payment) State Contract
  | RRAmbiguousSlotIntervalError

derive instance genericReduceResult :: Generic ReduceResult _

derive instance eqReduceResult :: Eq ReduceResult

instance showReduceResult :: Show ReduceResult where
  show = genericShow

data ApplyWarning
  = ApplyNoWarning
  | ApplyNonPositiveDeposit Party AccountId Token BigInt

derive instance genericApplyWarning :: Generic ApplyWarning _

derive instance eqApplyWarning :: Eq ApplyWarning

derive instance ordApplyWarning :: Ord ApplyWarning

instance showApplyWarning :: Show ApplyWarning where
  show = genericShow

data ApplyResult
  = Applied ApplyWarning State Contract
  | ApplyNoMatchError

derive instance genericApplyResult :: Generic ApplyResult _

derive instance eqApplyResult :: Eq ApplyResult

derive instance ordApplyResult :: Ord ApplyResult

instance showApplyResult :: Show ApplyResult where
  show = genericShow

data ApplyAllResult
  = ApplyAllSuccess Boolean (List TransactionWarning) (List Payment) State
      Contract
  | ApplyAllNoMatchError
  | ApplyAllAmbiguousSlotIntervalError

derive instance genericApplyAllResult :: Generic ApplyAllResult _

derive instance eqApplyAllResult :: Eq ApplyAllResult

instance showApplyAllResult :: Show ApplyAllResult where
  show = genericShow

data TransactionWarning
  = TransactionNonPositiveDeposit Party AccountId Token BigInt
  | TransactionNonPositivePay AccountId Payee Token BigInt
  | TransactionPartialPay AccountId Payee Token BigInt BigInt
  --                         ^ src    ^ dest       ^ paid     ^ expected
  | TransactionShadowing ValueId BigInt BigInt
  --                           oldVal ^  newVal ^
  | TransactionAssertionFailed

derive instance genericTransactionWarning :: Generic TransactionWarning _

derive instance eqTransactionWarning :: Eq TransactionWarning

derive instance ordTransactionWarning :: Ord TransactionWarning

instance showTransactionWarning :: Show TransactionWarning where
  show = genericShow

instance encodeTransactionWarning :: EncodeJson TransactionWarning where
  encodeJson TransactionAssertionFailed = encodeJson "assertion_failed"
  encodeJson (TransactionNonPositiveDeposit party accId tok amount) =
    encodeJson
      { party: party
      , asked_to_deposit: amount
      , of_token: tok
      , in_account: accId
      }
  encodeJson (TransactionNonPositivePay accId payee tok amount) =
    encodeJson
      { account: accId
      , asked_to_pay: amount
      , of_token: tok
      , to_payee: payee
      }
  encodeJson (TransactionPartialPay accId payee tok paid expected) =
    encodeJson
      { account: accId
      , asked_to_pay: expected
      , of_token: tok
      , to_payee: payee
      , but_only_paid: paid
      }
  encodeJson (TransactionShadowing valId oldVal newVal) =
    encodeJson
      { value_id: valId
      , had_value: oldVal
      , is_now_assigned: newVal
      }

instance decodeTransactionWarning :: DecodeJson TransactionWarning where
  decodeJson =
    caseConstantFrom
      (Map.singleton "assertion_failed" TransactionAssertionFailed)
      decodeObject
    where
    decodeObject =
      object "TransactionWarning" do
        party <- getProp "party"
        inAccount <- getProp "in_account"
        ofToken <- getProp "of_token"
        askedToDeposit <- getProp "asked_to_deposit"
        account <- getProp "account"
        toPayee <- getProp "to_payee"
        butOnlyPaid <- getProp "but_only_paid"
        askedToPay <- getProp "asked_to_pay"
        valueId <- getProp "value_id"
        hadValue <- getProp "had_value"
        isNowAssigned <- getProp "is_now_assigned"
        pure
          $
            ( TransactionNonPositiveDeposit
                <$> party
                <*> inAccount
                <*> ofToken
                <*> askedToDeposit
            )
              <|>
                ( TransactionPartialPay
                    <$> account
                    <*> toPayee
                    <*> ofToken
                    <*> butOnlyPaid
                    <*> askedToPay
                )
              <|>
                ( TransactionNonPositivePay
                    <$> account
                    <*> toPayee
                    <*> ofToken
                    <*> askedToPay
                )
              <|>
                ( TransactionShadowing
                    <$> valueId
                    <*> hadValue
                    <*> isNowAssigned
                )

-- | Transaction error
data TransactionError
  = TEAmbiguousSlotIntervalError
  | TEApplyNoMatchError
  | TEIntervalError IntervalError
  | TEUselessTransaction

derive instance genericTransactionError :: Generic TransactionError _

derive instance eqTransactionError :: Eq TransactionError

derive instance ordTransactionError :: Ord TransactionError

instance showTransactionError :: Show TransactionError where
  show TEAmbiguousSlotIntervalError = "Abiguous slot interval"
  show TEApplyNoMatchError =
    "At least one of the inputs in the transaction is not allowed by the contract"
  show (TEIntervalError err) = show err
  show TEUselessTransaction = "Useless Transaction"

instance genericEncodeTransactionError :: EncodeJson TransactionError where
  encodeJson = case _ of
    TEAmbiguousSlotIntervalError -> E.encodeTagged
      "TEAmbiguousSlotIntervalError"
      unit
      E.null
    TEApplyNoMatchError -> E.encodeTagged "TEApplyNoMatchError" unit E.null
    TEIntervalError e -> E.encodeTagged "TEIntervalError" e E.value
    TEUselessTransaction -> E.encodeTagged "TEUselessTransaction" unit E.null

instance genericDecodeJsonTransactionError :: DecodeJson TransactionError where
  decodeJson =
    D.decode
      $ D.sumType "TransactionError"
      $ Map.fromFoldable
          [ "TEAmbiguousSlotIntervalError" /\ D.content
              (TEAmbiguousSlotIntervalError <$ D.null)
          , "TEApplyNoMatchError" /\ D.content (TEApplyNoMatchError <$ D.null)
          , "TEIntervalError" /\ D.content (TEIntervalError <$> D.value)
          , "TEUselessTransaction" /\ D.content (TEUselessTransaction <$ D.null)
          ]

newtype TransactionInput
  = TransactionInput
  { interval :: SlotInterval
  , inputs :: (List Input)
  }

derive instance genericTransactionInput :: Generic TransactionInput _

derive instance newtypeTransactionInput :: Newtype TransactionInput _

derive instance eqTransactionInput :: Eq TransactionInput

derive instance ordTransactionInput :: Ord TransactionInput

instance showTransactionInput :: Show TransactionInput where
  show = genericShow

instance encodeTransactionInput :: EncodeJson TransactionInput where
  encodeJson
    ( TransactionInput
        { interval: (SlotInterval (Slot fromSlot) (Slot toSlot))
        , inputs: txInps
        }
    ) =
    encodeJson
      { tx_interval:
          { from: fromSlot
          , to: toSlot
          }
      , tx_inputs: txInps
      }

instance decodeTransactionInput :: DecodeJson TransactionInput where
  decodeJson =
    object "TransactionInput" do
      intervalObject <- requireProp "tx_interval"
      inputs <- requireProp "tx_inputs"
      interval <-
        ReaderT \_ ->
          flip (object "nested SlotInterval") intervalObject
            $ Just
                <$>
                  ( SlotInterval
                      <$> (Slot <$> requireProp "from")
                      <*> (Slot <$> requireProp "to")
                  )
      pure $ Just $ TransactionInput { interval, inputs: inputs }

data TransactionOutput
  = TransactionOutput
      { txOutWarnings :: List TransactionWarning
      , txOutPayments :: List Payment
      , txOutState :: State
      , txOutContract :: Contract
      }
  | Error TransactionError

derive instance genericTransactionOutput :: Generic TransactionOutput _

derive instance eqTransactionOutput :: Eq TransactionOutput

instance showTransactionOutput :: Show TransactionOutput where
  show = genericShow

newtype MarloweData
  = MarloweData
  { marloweContract :: Contract
  , marloweState :: State
  }

derive instance eqMarloweData :: Eq MarloweData

derive instance newtypeMarloweData :: Newtype MarloweData _

derive instance genericMarloweData :: Generic MarloweData _

derive newtype instance encodeMarloweData :: EncodeJson MarloweData

derive newtype instance decodeMarloweData :: DecodeJson MarloweData

_marloweContract :: Lens' MarloweData Contract
_marloweContract = _Newtype <<< prop (Proxy :: _ "marloweContract")

_marloweState :: Lens' MarloweData State
_marloweState = _Newtype <<< prop (Proxy :: _ "marloweState")

newtype MarloweParams
  = MarloweParams
  { rolePayoutValidatorHash :: ValidatorHash
  , rolesCurrency :: { unCurrencySymbol :: CurrencySymbol } -- this is to ensure the serialisation matches the backend
  }

derive instance eqMarloweParams :: Eq MarloweParams

derive instance ordMarloweParams :: Ord MarloweParams

derive instance newtypeMarloweParams :: Newtype MarloweParams _

derive instance genericMarloweParams :: Generic MarloweParams _

derive newtype instance encodeJsonMarloweParams :: EncodeJson MarloweParams

derive newtype instance decodeMarloweParams :: DecodeJson MarloweParams

_rolePayoutValidatorHash :: Lens' MarloweParams ValidatorHash
_rolePayoutValidatorHash = _Newtype <<< prop
  (Proxy :: _ "rolePayoutValidatorHash")

_rolesCurrency :: Lens' MarloweParams CurrencySymbol
_rolesCurrency = _Newtype <<< prop (Proxy :: _ "rolesCurrency") <<< prop
  (Proxy :: _ "unCurrencySymbol")

type ValidatorHash
  = String

emptyState :: Slot -> State
emptyState sn =
  State
    { accounts: Map.empty
    , choices: Map.empty
    , boundValues: Map.empty
    , minSlot: sn
    }

inBounds :: ChosenNum -> Array Bound -> Boolean
inBounds num = any (\(Bound l u) -> num >= l && num <= u)

boundFrom :: Bound -> BigInt
boundFrom (Bound from _) = from

boundTo :: Bound -> BigInt
boundTo (Bound _ to) = to

-- Note: We use guards here because currently nested ifs break purty formatting
--       We need to upgrade purty and purescript to fix
fixInterval :: SlotInterval -> State -> IntervalResult
fixInterval interval@(SlotInterval from to) (State state)
  | (not <<< validInterval) interval = IntervalError (InvalidInterval interval)
  | state.minSlot `above` interval = IntervalError
      (IntervalInPastError state.minSlot interval)
  | otherwise =
      let
        -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
        newLow = max from state.minSlot

        -- We know high is greater or equal than newLow (prove)
        currentInterval = SlotInterval newLow to

        env = Environment { slotInterval: currentInterval }

        newState = State (state { minSlot = newLow })
      in
        IntervalTrimmed env newState

-- EVALUATION
-- | Evaluate a @Value@ to Integer
evalValue :: Environment -> State -> Value -> BigInt
evalValue env state value =
  let
    eval = evalValue env state
  in
    case value of
      AvailableMoney accId token -> moneyInAccount accId token
        (unwrap state).accounts
      Constant integer -> integer
      NegValue val -> negate (eval val)
      AddValue lhs rhs -> eval lhs + eval rhs
      SubValue lhs rhs -> eval lhs - eval rhs
      MulValue lhs rhs -> eval lhs * eval rhs
      DivValue lhs rhs ->
        let
          n = eval lhs
        in
          if n == fromInt 0 then
            fromInt 0
          else
            let
              d = eval rhs
            in
              if d == fromInt 0 then
                fromInt 0
              else
                let
                  q = n `div` d

                  r = n `mod` d

                  ar = abs r * fromInt 2

                  ad = abs d
                in
                  if ar < ad then
                    q -- reminder < 1/2
                  else if ar > ad then
                    q + signum n * signum d -- reminder > 1/2
                  else
                    let -- reminder == 1/2
                      qIsEven = q `mod` fromInt 2 == fromInt 0
                    in
                      if qIsEven then q else q + signum n * signum d
      ChoiceValue choiceId -> fromMaybe zero $ Map.lookup choiceId
        (unwrap state).choices
      SlotIntervalStart -> view (_slotInterval <<< to ivFrom <<< to unwrap) env
      SlotIntervalEnd -> view (_slotInterval <<< to ivTo <<< to unwrap) env
      UseValue valId -> fromMaybe zero $ Map.lookup valId
        (unwrap state).boundValues
      Cond cond thn els ->
        if evalObservation env state cond then eval thn else eval els

-- | Evaluate an @Observation@ to Bool
evalObservation :: Environment -> State -> Observation -> Boolean
evalObservation env state obs =
  let
    evalObs = evalObservation env state

    evalVal = evalValue env state
  in
    case obs of
      AndObs lhs rhs -> evalObs lhs && evalObs rhs
      OrObs lhs rhs -> evalObs lhs || evalObs rhs
      NotObs subObs -> not (evalObs subObs)
      ChoseSomething choiceId -> choiceId `Map.member` (unwrap state).choices
      ValueGE lhs rhs -> evalVal lhs >= evalVal rhs
      ValueGT lhs rhs -> evalVal lhs > evalVal rhs
      ValueLT lhs rhs -> evalVal lhs < evalVal rhs
      ValueLE lhs rhs -> evalVal lhs <= evalVal rhs
      ValueEQ lhs rhs -> evalVal lhs == evalVal rhs
      TrueObs -> true
      FalseObs -> false

asset :: CurrencySymbol -> TokenName -> BigInt -> Assets
asset cur tok balance = Assets (Map.singleton cur (Map.singleton tok balance))

-- | Pick the first account with money in it
refundOne :: Accounts -> Maybe (Tuple (Tuple Party Money) Accounts)
refundOne accounts = case Map.toUnfoldable accounts of
  Nil -> Nothing
  Tuple (Tuple accId (Token cur tok)) balance : rest ->
    if balance > zero then
      Just (Tuple (Tuple accId (asset cur tok balance)) (Map.fromFoldable rest))
    else
      refundOne (Map.fromFoldable rest)

-- | Obtains the amount of money available an account
moneyInAccount :: AccountId -> Token -> Accounts -> BigInt
moneyInAccount accId token accounts = fromMaybe zero
  (Map.lookup (Tuple accId token) accounts)

-- | Sets the amount of money available in an account
updateMoneyInAccount :: AccountId -> Token -> BigInt -> Accounts -> Accounts
updateMoneyInAccount accId token amount =
  if amount <= zero then Map.delete (Tuple accId token)
  else Map.insert (Tuple accId token) amount

{-| Add the given amount of money to an account (only if it is positive).
    Return the updated Map
-}
addMoneyToAccount :: AccountId -> Token -> BigInt -> Accounts -> Accounts
addMoneyToAccount accId token amount accounts =
  let
    balance = moneyInAccount accId token accounts

    newBalance = balance + amount
  in
    if amount <= zero then
      accounts
    else
      updateMoneyInAccount accId token newBalance accounts

{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
giveMoney
  :: AccountId
  -> Payee
  -> Token
  -> BigInt
  -> Accounts
  -> Tuple ReduceEffect Accounts
giveMoney accountId payee token@(Token cur tok) amount accounts =
  let
    newAccounts = case payee of
      Party _ -> accounts
      Account accId -> addMoneyToAccount accId token amount accounts
  in
    Tuple (ReduceWithPayment (Payment accountId payee (asset cur tok amount)))
      newAccounts

-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of
  Close -> case refundOne (unwrap state).accounts of
    Just (Tuple (Tuple party money) newAccounts) ->
      let
        oldState = unwrap state

        newState = wrap (oldState { accounts = newAccounts })
      in
        Reduced ReduceNoWarning
          (ReduceWithPayment (Payment party (Party party) money))
          newState
          Close
    Nothing -> NotReduced
  Pay accId payee tok val cont ->
    let
      amountToPay = evalValue env state val
    in
      if amountToPay <= zero then
        let
          warning = ReduceNonPositivePay accId payee tok amountToPay
        in
          Reduced warning ReduceNoPayment state cont
      else
        let
          balance = moneyInAccount accId tok (unwrap state).accounts

          paidAmount = min balance amountToPay

          newBalance = balance - paidAmount

          newAccs = updateMoneyInAccount accId tok newBalance
            (unwrap state).accounts

          warning =
            if paidAmount < amountToPay then
              ReducePartialPay accId payee tok paidAmount amountToPay
            else
              ReduceNoWarning

          (Tuple payment finalAccs) = giveMoney accId payee tok paidAmount
            newAccs

          newState = wrap ((unwrap state) { accounts = finalAccs })
        in
          Reduced warning payment newState cont
  If obs cont1 cont2 ->
    let
      cont = if evalObservation env state obs then cont1 else cont2
    in
      Reduced ReduceNoWarning ReduceNoPayment state cont
  When _ timeout nextContract ->
    let
      startSlot = view (_slotInterval <<< to ivFrom) env

      endSlot = view (_slotInterval <<< to ivTo) env
    in
      if endSlot < timeout then
        NotReduced
      else if timeout <= startSlot then
        Reduced ReduceNoWarning ReduceNoPayment state nextContract
      else
        AmbiguousSlotIntervalReductionError
  Let valId val nextContract ->
    let
      evaluatedValue = evalValue env state val

      newState = over _boundValues (Map.insert valId evaluatedValue) state

      warn = case Map.lookup valId (unwrap state).boundValues of
        Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
        Nothing -> ReduceNoWarning
    in
      Reduced warn ReduceNoPayment newState nextContract
  Assert obs cont ->
    let
      warning =
        if evalObservation env state obs then
          ReduceNoWarning
        else
          ReduceAssertionFailed
    in
      Reduced warning ReduceNoPayment state cont

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent startEnv startState startContract =
  let
    reductionLoop
      :: Boolean
      -> Environment
      -> State
      -> Contract
      -> (List ReduceWarning)
      -> (List Payment)
      -> ReduceResult
    reductionLoop reduced env state contract warnings payments =
      case reduceContractStep env state contract of
        Reduced warning effect newState nextContract ->
          let
            newWarnings =
              if warning == ReduceNoWarning then warnings
              else warning : warnings

            newPayments = case effect of
              ReduceWithPayment payment -> payment : payments
              ReduceNoPayment -> payments
          in
            reductionLoop true env newState nextContract newWarnings newPayments
        AmbiguousSlotIntervalReductionError -> RRAmbiguousSlotIntervalError
        -- this is the last invocation of reductionLoop, so we can reverse lists
        NotReduced -> ContractQuiescent reduced (reverse warnings)
          (reverse payments)
          state
          contract
  in
    reductionLoop false startEnv startState startContract mempty mempty

applyCases :: Environment -> State -> Input -> List Case -> ApplyResult
applyCases env state input cases = case input, cases of
  IDeposit accId1 party1 tok1 amount,
  (Case (Deposit accId2 party2 tok2 val) cont) : rest ->
    if
      accId1 == accId2 && party1 == party2 && tok1 == tok2
        && amount
          == evalValue env state val then
      let
        warning =
          if amount > zero then
            ApplyNoWarning
          else
            ApplyNonPositiveDeposit party2 accId2 tok2 amount

        newAccounts = addMoneyToAccount accId1 tok1 amount
          (unwrap state).accounts

        newState = wrap ((unwrap state) { accounts = newAccounts })
      in
        Applied warning newState cont
    else
      applyCases env state input rest
  IChoice choId1 choice, (Case (Choice choId2 bounds) cont) : rest ->
    let
      newState = over _choices (Map.insert choId1 choice) state
    in
      if choId1 == choId2 && inBounds choice bounds then
        Applied ApplyNoWarning newState cont
      else
        applyCases env state input rest
  INotify, (Case (Notify obs) cont) : _
    | evalObservation env state obs -> Applied ApplyNoWarning state cont
  _, _ : rest -> applyCases env state input rest
  _, Nil -> ApplyNoMatchError

applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input
  (fromFoldable cases)

applyInput _ _ _ _ = ApplyNoMatchError

convertReduceWarnings :: List ReduceWarning -> List TransactionWarning
convertReduceWarnings Nil = Nil

convertReduceWarnings (first : rest) =
  ( case first of
      ReduceNoWarning -> Nil
      ReduceNonPositivePay accId payee tok amount ->
        (TransactionNonPositivePay accId payee tok amount) : Nil
      ReducePartialPay accId payee tok paid expected ->
        (TransactionPartialPay accId payee tok paid expected) : Nil
      ReduceShadowing valId oldVal newVal ->
        (TransactionShadowing valId oldVal newVal) : Nil
      ReduceAssertionFailed -> TransactionAssertionFailed : Nil
  )
    <> convertReduceWarnings rest

convertApplyWarning :: ApplyWarning -> List TransactionWarning
convertApplyWarning warn = case warn of
  ApplyNoWarning -> Nil
  ApplyNonPositiveDeposit party accId tok amount ->
    (TransactionNonPositiveDeposit party accId tok amount) : Nil

-- | Apply a list of Inputs to the contract
applyAllInputs
  :: Environment -> State -> Contract -> (List Input) -> ApplyAllResult
applyAllInputs startEnv startState startContract startInputs =
  let
    applyAllLoop
      :: Boolean
      -> Environment
      -> State
      -> Contract
      -> List Input
      -> List TransactionWarning
      -> List Payment
      -> ApplyAllResult
    applyAllLoop contractChanged env state contract inputs warnings payments =
      case reduceContractUntilQuiescent env state contract of
        RRAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
        ContractQuiescent reduced reduceWarns pays curState cont ->
          case inputs of
            Nil ->
              ApplyAllSuccess (contractChanged || reduced)
                (warnings <> (convertReduceWarnings reduceWarns))
                (payments <> pays)
                curState
                cont
            (input : rest) -> case applyInput env curState input cont of
              Applied applyWarn newState nextContract ->
                applyAllLoop true env newState nextContract rest
                  ( warnings <> (convertReduceWarnings reduceWarns)
                      <> (convertApplyWarning applyWarn)
                  )
                  (payments <> pays)
              ApplyNoMatchError -> ApplyAllNoMatchError
  in
    applyAllLoop false startEnv startState startContract startInputs mempty
      mempty

isClose :: Contract -> Boolean
isClose Close = true

isClose _ = false

-- | Try to compute outputs of a transaction give its input
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract =
  let
    inputs = (unwrap tx).inputs
  in
    case fixInterval (unwrap tx).interval state of
      IntervalTrimmed env fixState ->
        case applyAllInputs env fixState contract inputs of
          ApplyAllSuccess reduced warnings payments newState cont ->
            if
              not reduced &&
                ( not (isClose contract) ||
                    (Map.isEmpty $ (unwrap state).accounts)
                ) then
              Error TEUselessTransaction
            else
              TransactionOutput
                { txOutWarnings: warnings
                , txOutPayments: payments
                , txOutState: newState
                , txOutContract: cont
                }
          ApplyAllNoMatchError -> Error TEApplyNoMatchError
          ApplyAllAmbiguousSlotIntervalError -> Error
            TEAmbiguousSlotIntervalError
      IntervalError error -> Error (TEIntervalError error)

moneyInContract :: State -> Money
moneyInContract state =
  foldMapWithIndex
    (\(Tuple _ (Token cur tok)) balance -> asset cur tok balance)
    (unwrap state).accounts

newtype Timeouts
  = Timeouts { maxTime :: Timeout, minTime :: Maybe Timeout }

derive instance newtypeTimeouts :: Newtype Timeouts _

-- The eq and show instances are only needed for quickcheck
derive newtype instance eqTimeouts :: Eq Timeouts

derive newtype instance showTimeouts :: Show Timeouts

class HasTimeout a where
  timeouts :: a -> Timeouts

instance hasTimeoutContract :: HasTimeout Contract where
  timeouts Close = Timeouts { maxTime: zero, minTime: Nothing }
  timeouts (Pay _ _ _ _ contract) = timeouts contract
  timeouts (If _ contractTrue contractFalse) = timeouts
    [ contractTrue, contractFalse ]
  timeouts (When cases timeout contract) =
    timeouts
      [ timeouts cases
      , Timeouts { maxTime: timeout, minTime: Just timeout }
      , timeouts contract
      ]
  timeouts (Let _ _ contract) = timeouts contract
  timeouts (Assert _ contract) = timeouts contract

instance hasTimeoutCase :: HasTimeout Case where
  timeouts (Case _ contract) = timeouts contract

instance hasTimeoutArrayOfTimeouts :: HasTimeout (Array Timeouts) where
  timeouts ts =
    Timeouts
      { maxTime: maxOf (map (_.maxTime <<< unwrap) ts)
      , minTime: minOf (map (_.minTime <<< unwrap) ts)
      }
else instance hasTimeoutArray :: HasTimeout a => HasTimeout (Array a) where
  timeouts vs = timeouts $ map timeouts vs

maxOf :: Array Timeout -> Timeout
maxOf = foldl max zero

minOf :: Array (Maybe Timeout) -> Maybe Timeout
minOf as = minimum $ catMaybes as
