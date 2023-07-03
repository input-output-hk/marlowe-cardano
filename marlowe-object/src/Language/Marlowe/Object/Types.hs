{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Marlowe.Object.Types where

import Cardano.Api (
  Address,
  AsType (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  deserialiseFromBech32,
  deserialiseFromRawBytes,
  serialiseToBech32,
 )
import Cardano.Api.Byron (ShelleyAddr)
import Control.Applicative (Alternative (many), empty)
import Control.Monad (join)
import Data.Aeson hiding (Object, String, Value)
import qualified Data.Aeson as A hiding (Object)
import Data.Aeson.Applicative (parseObject)
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.Binary (Binary (..), getWord8, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16', encodeBase16)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.Foldable (asum, fold, traverse_)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as LNE
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import GHC.Base (Any)
import GHC.Generics (Generic)
import GHC.Read (Read (..), lexP)
import GHC.Show (showSpace)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Core.V1.Semantics.Types.Address (serialiseAddressBech32)
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Core
import Network.Protocol.Codec.Spec (Variations (..), varyAp)
import Plutus.V2.Ledger.Api (BuiltinByteString, POSIXTime (..))
import qualified Plutus.V2.Ledger.Api as PV2
import Text.Read (Lexeme (..), ReadPrec, parens, prec, reset, step)
import Unsafe.Coerce (unsafeCoerce)

newtype ObjectBundle = ObjectBundle {getObjects :: [LabelledObject]}
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Variations)

instance Binary ObjectBundle where
  put = traverse_ put . getObjects
  get = ObjectBundle <$> many get

data ObjectType a where
  ValueType :: ObjectType Value
  ObservationType :: ObjectType Observation
  ContractType :: ObjectType Contract
  PartyType :: ObjectType Party
  TokenType :: ObjectType Token
  ActionType :: ObjectType Action

deriving instance Show (ObjectType a)
deriving instance Eq (ObjectType a)
deriving instance Ord (ObjectType a)

instance TestEquality ObjectType where
  testEquality ValueType ValueType = Just Refl
  testEquality ValueType _ = Nothing
  testEquality ObservationType ObservationType = Just Refl
  testEquality ObservationType _ = Nothing
  testEquality ContractType ContractType = Just Refl
  testEquality ContractType _ = Nothing
  testEquality PartyType PartyType = Just Refl
  testEquality PartyType _ = Nothing
  testEquality TokenType TokenType = Just Refl
  testEquality TokenType _ = Nothing
  testEquality ActionType ActionType = Just Refl
  testEquality ActionType _ = Nothing

instance ToJSON (ObjectType a) where
  toJSON = \case
    ValueType -> "value"
    ObservationType -> "observation"
    ContractType -> "contract"
    PartyType -> "party"
    TokenType -> "token"
    ActionType -> "action"

newtype SomeObjectType = UnsafeSomeObjectType (ObjectType Any)
  deriving newtype (ToJSON)

instance Eq SomeObjectType where
  SomeObjectType x == SomeObjectType y = isJust $ testEquality x y

instance Ord SomeObjectType where
  compare (SomeObjectType ValueType) (SomeObjectType ValueType) = EQ
  compare (SomeObjectType ValueType) _ = LT
  compare _ (SomeObjectType ValueType) = GT
  compare (SomeObjectType ObservationType) (SomeObjectType ObservationType) = EQ
  compare (SomeObjectType ObservationType) _ = LT
  compare _ (SomeObjectType ObservationType) = GT
  compare (SomeObjectType ContractType) (SomeObjectType ContractType) = EQ
  compare (SomeObjectType ContractType) _ = LT
  compare _ (SomeObjectType ContractType) = GT
  compare (SomeObjectType PartyType) (SomeObjectType PartyType) = EQ
  compare (SomeObjectType PartyType) _ = LT
  compare _ (SomeObjectType PartyType) = GT
  compare (SomeObjectType TokenType) (SomeObjectType TokenType) = EQ
  compare (SomeObjectType TokenType) _ = LT
  compare _ (SomeObjectType TokenType) = GT
  compare (SomeObjectType ActionType) (SomeObjectType ActionType) = EQ

{-# COMPLETE SomeObjectType #-}
pattern SomeObjectType :: ObjectType a -> SomeObjectType
pattern SomeObjectType x <- UnsafeSomeObjectType x
  where
    SomeObjectType x = UnsafeSomeObjectType (unsafeCoerce x)

instance Show SomeObjectType where
  showsPrec p (SomeObjectType t) =
    showParen
      (p >= 11)
      ( showString "SomeObjectType"
          . showSpace
          . showsPrec 11 t
      )

readObjectType :: ReadPrec SomeObjectType
readObjectType = do
  Ident ctor <- lexP
  case ctor of
    "ValueType" -> pure $ SomeObjectType ValueType
    "ObservationType" -> pure $ SomeObjectType ObservationType
    "ContractType" -> pure $ SomeObjectType ContractType
    "PartyType" -> pure $ SomeObjectType PartyType
    "TokenType" -> pure $ SomeObjectType TokenType
    "ActionType" -> pure $ SomeObjectType ActionType
    _ -> empty

instance Read SomeObjectType where
  readPrec = parens $ prec 10 do
    Ident "SomeObjectType" <- lexP
    step readObjectType

instance FromJSON SomeObjectType where
  parseJSON = withText "ObjectType" \case
    "value" -> pure $ SomeObjectType ValueType
    "observation" -> pure $ SomeObjectType ObservationType
    "contract" -> pure $ SomeObjectType ContractType
    "party" -> pure $ SomeObjectType PartyType
    "token" -> pure $ SomeObjectType TokenType
    "action" -> pure $ SomeObjectType ActionType
    _ ->
      parseFail $
        "expected one of: "
          <> intercalate
            ", "
            [ "value"
            , "observation"
            , "contract"
            , "party"
            , "token"
            , "action"
            ]

instance Binary SomeObjectType where
  put (SomeObjectType t) = putWord8 case t of
    ValueType -> 0
    ObservationType -> 1
    ContractType -> 2
    PartyType -> 3
    TokenType -> 4
    ActionType -> 5
  get = do
    tag <- getWord8
    case tag of
      0 -> pure $ SomeObjectType ValueType
      1 -> pure $ SomeObjectType ObservationType
      2 -> pure $ SomeObjectType ContractType
      3 -> pure $ SomeObjectType PartyType
      4 -> pure $ SomeObjectType TokenType
      5 -> pure $ SomeObjectType ActionType
      _ -> fail $ "Invalid tag byte " <> show tag

instance Variations SomeObjectType where
  variations =
    LNE.fromList
      [ SomeObjectType ValueType
      , SomeObjectType ObservationType
      , SomeObjectType ContractType
      , SomeObjectType PartyType
      , SomeObjectType TokenType
      , SomeObjectType ActionType
      ]

data LabelledObject = forall a.
  LabelledObject
  { label :: Label
  , type_ :: ObjectType a
  , value :: a
  }

instance Show LabelledObject where
  showsPrec _ LabelledObject{..} =
    showString "LabelledObject { label = "
      . shows label
      . showString ", type_ = "
      . shows type_
      . showString ", value = "
      . ( case type_ of
            ValueType -> shows value
            ObservationType -> shows value
            ContractType -> shows value
            PartyType -> shows value
            TokenType -> shows value
            ActionType -> shows value
        )
      . showString " }"

instance Read LabelledObject where
  readPrec =
    asum
      [ readNormal
      , readRecord
      ]
    where
      readNormal :: ReadPrec LabelledObject
      readNormal = parens $ prec 10 $ do
        Ident "LabelledObject" <- lexP
        label :: Label <- step readPrec
        SomeObjectType type_ <- step readObjectType
        case type_ of
          ValueType -> LabelledObject label type_ <$> step readPrec
          ObservationType -> LabelledObject label type_ <$> step readPrec
          ContractType -> LabelledObject label type_ <$> step readPrec
          PartyType -> LabelledObject label type_ <$> step readPrec
          TokenType -> LabelledObject label type_ <$> step readPrec
          ActionType -> LabelledObject label type_ <$> step readPrec

      readRecord :: ReadPrec LabelledObject
      readRecord = do
        Ident "LabelledObject" <- lexP
        Punc "{" <- lexP
        Ident "label" <- lexP
        Punc "=" <- lexP
        label :: Label <- reset readPrec
        Punc "," <- lexP
        Ident "type_" <- lexP
        Punc "=" <- lexP
        SomeObjectType type_ <- reset readObjectType
        Punc "," <- lexP
        Ident "value" <- lexP
        Punc "=" <- lexP
        x <- case type_ of
          ValueType -> LabelledObject label type_ <$> reset readPrec
          ObservationType -> LabelledObject label type_ <$> reset readPrec
          ContractType -> LabelledObject label type_ <$> reset readPrec
          PartyType -> LabelledObject label type_ <$> reset readPrec
          TokenType -> LabelledObject label type_ <$> reset readPrec
          ActionType -> LabelledObject label type_ <$> reset readPrec
        Punc "}" <- lexP
        pure x

instance Eq LabelledObject where
  LabelledObject l t v == LabelledObject l' t' v' =
    l == l' && case (t, t') of
      (ValueType, ValueType) -> v == v'
      (ValueType, _) -> False
      (ObservationType, ObservationType) -> v == v'
      (ObservationType, _) -> False
      (ContractType, ContractType) -> v == v'
      (ContractType, _) -> False
      (PartyType, PartyType) -> v == v'
      (PartyType, _) -> False
      (TokenType, TokenType) -> v == v'
      (TokenType, _) -> False
      (ActionType, ActionType) -> v == v'
      (ActionType, _) -> False

instance Ord LabelledObject where
  compare (LabelledObject l t v) (LabelledObject l' t' v') =
    fold
      [ compare l l'
      , case (t, t') of
          (ValueType, ValueType) -> compare v v'
          (ValueType, _) -> LT
          (_, ValueType) -> GT
          (ObservationType, ObservationType) -> compare v v'
          (ObservationType, _) -> LT
          (_, ObservationType) -> GT
          (ContractType, ContractType) -> compare v v'
          (ContractType, _) -> LT
          (_, ContractType) -> GT
          (PartyType, PartyType) -> compare v v'
          (PartyType, _) -> LT
          (_, PartyType) -> GT
          (TokenType, TokenType) -> compare v v'
          (TokenType, _) -> LT
          (_, TokenType) -> GT
          (ActionType, ActionType) -> compare v v'
      ]

instance ToJSON LabelledObject where
  toJSON LabelledObject{..} =
    object
      [ "label" .= label
      , "type" .= type_
      , case type_ of
          ValueType -> "value" .= value
          ObservationType -> "value" .= value
          ContractType -> "value" .= value
          PartyType -> "value" .= value
          TokenType -> "value" .= value
          ActionType -> "value" .= value
      ]

instance FromJSON LabelledObject where
  parseJSON = withObject "LabelledObject" \obj -> do
    label :: Label <- obj .: "label"
    SomeObjectType type_ <- obj .: "type"
    case type_ of
      ValueType -> LabelledObject label type_ <$> obj .: "value"
      ObservationType -> LabelledObject label type_ <$> obj .: "value"
      ContractType -> LabelledObject label type_ <$> obj .: "value"
      PartyType -> LabelledObject label type_ <$> obj .: "value"
      TokenType -> LabelledObject label type_ <$> obj .: "value"
      ActionType -> LabelledObject label type_ <$> obj .: "value"

instance Binary LabelledObject where
  put LabelledObject{..} = do
    put label
    put $ SomeObjectType type_
    case type_ of
      ValueType -> put value
      ObservationType -> put value
      ContractType -> put value
      PartyType -> put value
      TokenType -> put value
      ActionType -> put value

  get = do
    label :: Label <- get
    SomeObjectType type_ <- get
    case type_ of
      ValueType -> LabelledObject label type_ <$> get
      ObservationType -> LabelledObject label type_ <$> get
      ContractType -> LabelledObject label type_ <$> get
      PartyType -> LabelledObject label type_ <$> get
      TokenType -> LabelledObject label type_ <$> get
      ActionType -> LabelledObject label type_ <$> get

instance Variations LabelledObject where
  variations = do
    (label :: Label, type_) <- variations
    case type_ of
      SomeObjectType ValueType -> LabelledObject label ValueType <$> variations
      SomeObjectType ObservationType -> LabelledObject label ObservationType <$> variations
      SomeObjectType ContractType -> LabelledObject label ContractType <$> variations
      SomeObjectType PartyType -> LabelledObject label PartyType <$> variations
      SomeObjectType TokenType -> LabelledObject label TokenType <$> variations
      SomeObjectType ActionType -> LabelledObject label ActionType <$> variations

data Object
  = ValueObject Value
  | ObservationObject Observation
  | ContractObject Contract
  | PartyObject Party
  | TokenObject Token
  | ActionObject Action
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

newtype Label = Label {unLabel :: ByteString}
  deriving (Show, Read, Generic, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary, Variations, Hashable)

data Contract
  = Close
  | Pay AccountId Payee Token Value Contract
  | If Observation Contract Contract
  | When [Case] Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract
  | ContractRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON Contract where
  toJSON Close = "close"
  toJSON (Pay accountId payee token value contract) =
    object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= contract
      ]
  toJSON (If obs cont1 cont2) =
    object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When cases timeout cont) =
    object
      [ "when" .= cases
      , "timeout" .= timeout
      , "timeout_continuation" .= cont
      ]
  toJSON (Let valId value cont) =
    object
      [ "let" .= valId
      , "be" .= value
      , "then" .= cont
      ]
  toJSON (Assert obs cont) =
    object
      [ "assert" .= obs
      , "then" .= cont
      ]
  toJSON (ContractRef label) = object ["ref" .= label]

instance FromJSON Contract where
  parseJSON = \case
    A.String "close" -> pure Close
    v ->
      flip (parseObject "Contract") v $
        asum
          [ Pay <$> "from_account" <*> "to" <*> "token" <*> "pay" <*> "then"
          , If <$> "if" <*> "then" <*> "else"
          , When <$> "when" <*> "timeout" <*> "timeout_continuation"
          , Let <$> "let" <*> "be" <*> "then"
          , Assert <$> "assert" <*> "then"
          , ContractRef <$> "ref"
          ]

fromCoreContract :: Core.Contract -> Contract
fromCoreContract = \case
  Core.Close -> Close
  Core.Pay account payee token value contract ->
    Pay
      (fromCoreParty account)
      (fromCorePayee payee)
      (fromCoreToken token)
      (fromCoreValue value)
      (fromCoreContract contract)
  Core.If obs a b ->
    If (fromCoreObservation obs) (fromCoreContract a) (fromCoreContract b)
  Core.When cases timeout contract ->
    When (fromCoreCase <$> cases) (fromCoreTimeout timeout) (fromCoreContract contract)
  Core.Let valueId value contract ->
    Let (fromCoreValueId valueId) (fromCoreValue value) (fromCoreContract contract)
  Core.Assert obs contract ->
    Assert (fromCoreObservation obs) (fromCoreContract contract)

data Case
  = Case Action Contract
  | MerkleizedCase Action ContractHash
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON Case where
  toJSON (Case act cont) =
    object
      [ "case" .= act
      , "then" .= cont
      ]
  toJSON (MerkleizedCase act hash) =
    object
      [ "case" .= act
      , "merkleized_then" .= hash
      ]

instance FromJSON Case where
  parseJSON =
    parseObject "Case" $
      asum
        [ Case <$> "case" <*> "then"
        , MerkleizedCase <$> "case" <*> "merkleized_then"
        ]

fromCoreCase :: Core.Case Core.Contract -> Case
fromCoreCase = \case
  Core.Case action contract ->
    Case (fromCoreAction action) (fromCoreContract contract)
  Core.MerkleizedCase action hash ->
    MerkleizedCase (fromCoreAction action) (fromCoreContractHash hash)

data Action
  = Deposit AccountId Party Token Value
  | Choice ChoiceId [Bound]
  | Notify Observation
  | ActionRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Action where
  toJSON (Deposit accountId party token val) =
    object
      [ "into_account" .= accountId
      , "party" .= party
      , "of_token" .= token
      , "deposits" .= val
      ]
  toJSON (Choice choiceId bounds) =
    object
      [ "for_choice" .= choiceId
      , "choose_between" .= bounds
      ]
  toJSON (Notify obs) = object ["notify_if" .= obs]
  toJSON (ActionRef label) = object ["ref" .= label]

instance FromJSON Action where
  parseJSON =
    parseObject "Action" $
      asum
        [ Deposit <$> "into_account" <*> "party" <*> "of_token" <*> "deposits"
        , Choice <$> "for_choice" <*> "choose_between"
        , Notify <$> "notify_if"
        , ActionRef <$> "ref"
        ]

fromCoreAction :: Core.Action -> Action
fromCoreAction = \case
  Core.Deposit accountId party token val ->
    Deposit (fromCoreParty accountId) (fromCoreParty party) (fromCoreToken token) (fromCoreValue val)
  Core.Choice choiceId bounds ->
    Choice (fromCoreChoiceId choiceId) (fromCoreBound <$> bounds)
  Core.Notify obs ->
    Notify (fromCoreObservation obs)

data Value
  = AvailableMoney AccountId Token
  | Constant Integer
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | DivValue Value Value
  | ChoiceValue ChoiceId
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue ValueId
  | Cond Observation Value Value
  | ValueRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON Value where
  toJSON (AvailableMoney accountId token) =
    object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
  toJSON (NegValue x) =
    object
      ["negate" .= x]
  toJSON (AddValue lhs rhs) =
    object
      [ "add" .= lhs
      , "and" .= rhs
      ]
  toJSON (SubValue lhs rhs) =
    object
      [ "value" .= lhs
      , "minus" .= rhs
      ]
  toJSON (MulValue lhs rhs) =
    object
      [ "multiply" .= lhs
      , "times" .= rhs
      ]
  toJSON (DivValue lhs rhs) =
    object
      [ "divide" .= lhs
      , "by" .= rhs
      ]
  toJSON (ChoiceValue choiceId) =
    object
      ["value_of_choice" .= choiceId]
  toJSON TimeIntervalStart = "time_interval_start"
  toJSON TimeIntervalEnd = "time_interval_end"
  toJSON (UseValue valueId) =
    object
      ["use_value" .= valueId]
  toJSON (Cond obs tv ev) =
    object
      [ "if" .= obs
      , "then" .= tv
      , "else" .= ev
      ]
  toJSON (ValueRef label) = object ["ref" .= label]

instance FromJSON Value where
  parseJSON = \case
    A.Number n -> Constant <$> parseJSON (A.Number n)
    A.String s -> case s of
      "time_interval_start" -> pure TimeIntervalStart
      "time_interval_end" -> pure TimeIntervalEnd
      _ ->
        parseFail $
          "Invalid string value. Valid string options are: "
            <> intercalate
              ", "
              [ show @String "time_interval_start"
              , show @String "time_interval_end"
              ]
    v ->
      flip (parseObject "Value") v $
        asum
          [ AvailableMoney <$> "in_account" <*> "amount_of_token"
          , NegValue <$> "negate"
          , AddValue <$> "add" <*> "and"
          , SubValue <$> "value" <*> "minus"
          , MulValue <$> "multiply" <*> "times"
          , DivValue <$> "divide" <*> "by"
          , ChoiceValue <$> "value_of_choice"
          , UseValue <$> "use_value"
          , Cond <$> "if" <*> "then" <*> "else"
          , ValueRef <$> "ref"
          ]

instance Num Value where
  (+) = AddValue
  (*) = MulValue
  abs v = Cond (ValueGE v 0) v (-v)
  signum v = Cond (ValueGT v 0) 1 $ Cond (ValueLT v 0) (-1) 0
  fromInteger = Constant
  negate = NegValue

fromCoreValue :: Core.Value Core.Observation -> Value
fromCoreValue = \case
  Core.AvailableMoney a b -> AvailableMoney (fromCoreParty a) (fromCoreToken b)
  Core.AddValue a b -> on AddValue fromCoreValue a b
  Core.SubValue a b -> on SubValue fromCoreValue a b
  Core.MulValue a b -> on MulValue fromCoreValue a b
  Core.DivValue a b -> on DivValue fromCoreValue a b
  Core.NegValue a -> NegValue $ fromCoreValue a
  Core.Constant a -> Constant a
  Core.ChoiceValue a -> ChoiceValue $ fromCoreChoiceId a
  Core.UseValue a -> UseValue $ fromCoreValueId a
  Core.TimeIntervalStart -> TimeIntervalStart
  Core.TimeIntervalEnd -> TimeIntervalEnd
  Core.Cond a b c -> Cond (fromCoreObservation a) (fromCoreValue b) (fromCoreValue c)

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
  | ObservationRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON Observation where
  toJSON (AndObs lhs rhs) =
    object
      [ "both" .= lhs
      , "and" .= rhs
      ]
  toJSON (OrObs lhs rhs) =
    object
      [ "either" .= lhs
      , "or" .= rhs
      ]
  toJSON (NotObs v) =
    object
      ["not" .= v]
  toJSON (ChoseSomething choiceId) =
    object
      ["chose_something_for" .= choiceId]
  toJSON (ValueGE lhs rhs) =
    object
      [ "value" .= lhs
      , "ge_than" .= rhs
      ]
  toJSON (ValueGT lhs rhs) =
    object
      [ "value" .= lhs
      , "gt" .= rhs
      ]
  toJSON (ValueLT lhs rhs) =
    object
      [ "value" .= lhs
      , "lt" .= rhs
      ]
  toJSON (ValueLE lhs rhs) =
    object
      [ "value" .= lhs
      , "le_than" .= rhs
      ]
  toJSON (ValueEQ lhs rhs) =
    object
      [ "value" .= lhs
      , "equal_to" .= rhs
      ]
  toJSON TrueObs = toJSON True
  toJSON FalseObs = toJSON False
  toJSON (ObservationRef label) = object ["ref" .= label]

instance FromJSON Observation where
  parseJSON = \case
    A.Bool True -> pure TrueObs
    A.Bool False -> pure FalseObs
    v ->
      flip (parseObject "Observation") v $
        asum
          [ AndObs <$> "both" <*> "and"
          , OrObs <$> "either" <*> "or"
          , NotObs <$> "not"
          , ChoseSomething <$> "chose_something_for"
          , ValueGE <$> "value" <*> "ge_than"
          , ValueGT <$> "value" <*> "gt"
          , ValueLE <$> "value" <*> "le_than"
          , ValueLT <$> "value" <*> "lt"
          , ValueEQ <$> "value" <*> "equal_to"
          , ObservationRef <$> "ref"
          ]

fromCoreObservation :: Core.Observation -> Observation
fromCoreObservation = \case
  Core.AndObs a b -> on AndObs fromCoreObservation a b
  Core.OrObs a b -> on OrObs fromCoreObservation a b
  Core.NotObs a -> NotObs $ fromCoreObservation a
  Core.ChoseSomething a -> ChoseSomething $ fromCoreChoiceId a
  Core.ValueGE a b -> on ValueGE fromCoreValue a b
  Core.ValueGT a b -> on ValueGT fromCoreValue a b
  Core.ValueLE a b -> on ValueLE fromCoreValue a b
  Core.ValueLT a b -> on ValueLT fromCoreValue a b
  Core.ValueEQ a b -> on ValueEQ fromCoreValue a b
  Core.TrueObs -> TrueObs
  Core.FalseObs -> FalseObs

type AccountId = Party

data Party
  = Address !ShelleyAddress
  | Role !TokenName
  | PartyRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Party where
  toJSON (Address address) = object ["address" .= address]
  toJSON (Role token) = object ["role_token" .= token]
  toJSON (PartyRef ref) = object ["ref" .= ref]

instance FromJSON Party where
  parseJSON =
    parseObject "Party" $
      asum
        [ Address <$> "address"
        , Role <$> "role_token"
        , PartyRef <$> "ref"
        ]

fromCoreParty :: Core.Party -> Party
fromCoreParty = \case
  Core.Address network addr -> Address $ fromCoreAddress network addr
  Core.Role role -> Role $ fromCoreTokenName role

newtype ShelleyAddress = ShelleyAddress {unShelleyAddress :: Address ShelleyAddr}
  deriving (Eq, Ord)

instance Show ShelleyAddress where
  show = show . serialiseToBech32 . unShelleyAddress

instance Read ShelleyAddress where
  readPrec = do
    String s <- lexP
    Right addr <- pure $ deserialiseFromBech32 (AsAddress AsShelleyAddr) $ T.pack s
    pure $ ShelleyAddress addr

instance IsString ShelleyAddress where
  fromString =
    either (error . mappend "ShelleyAddress.fromString: bad address: " . show) ShelleyAddress
      . deserialiseFromBech32 (AsAddress AsShelleyAddr)
      . T.pack

instance Binary ShelleyAddress where
  put = put . serialiseToRawBytes . unShelleyAddress
  get =
    maybe (fail "failed to deserialize address") (pure . ShelleyAddress)
      . deserialiseFromRawBytes (AsAddress AsShelleyAddr)
      =<< get

instance Variations ShelleyAddress where
  variations =
    LNE.fromList
      [ "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
      ]

instance ToJSON ShelleyAddress where
  toJSON (ShelleyAddress address) = A.String $ serialiseToBech32 address

instance FromJSON ShelleyAddress where
  parseJSON =
    withText "ShelleyAddress" $
      either (parseFail . show) (pure . ShelleyAddress) . deserialiseFromBech32 (AsAddress AsShelleyAddr)

fromCoreAddress :: Core.Network -> PV2.Address -> ShelleyAddress
fromCoreAddress network addr =
  ShelleyAddress $
    fromRight (error "fromRight: Left") $
      deserialiseFromBech32 (AsAddress AsShelleyAddr) $
        serialiseAddressBech32 network addr

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving (Show, Generic, Read, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary, Variations)

fromCoreTokenName :: PV2.TokenName -> TokenName
fromCoreTokenName = TokenName . PV2.fromBuiltin . PV2.unTokenName

toCoreTokenName :: TokenName -> PV2.TokenName
toCoreTokenName = PV2.TokenName . PV2.toBuiltin . unTokenName

data ChoiceId = ChoiceId Text Party
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) =
    object
      [ "choice_name" .= name
      , "choice_owner" .= party
      ]

instance FromJSON ChoiceId where
  parseJSON = parseObject "Payee" $ ChoiceId <$> "choice_name" <*> "choice_owner"

fromCoreChoiceId :: Core.ChoiceId -> ChoiceId
fromCoreChoiceId (Core.ChoiceId name owner) = ChoiceId (T.decodeUtf8 $ PV2.fromBuiltin name) (fromCoreParty owner)

newtype ValueId = ValueId {unValueId :: ByteString}
  deriving (Show, Generic, Read, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary, Variations)

fromCoreValueId :: Core.ValueId -> ValueId
fromCoreValueId (Core.ValueId bs) = ValueId $ PV2.fromBuiltin bs

toCoreValueId :: ValueId -> Core.ValueId
toCoreValueId (ValueId bs) = Core.ValueId $ PV2.toBuiltin bs

data Payee
  = Party !Party
  | Account !Party
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Payee where
  toJSON (Account acc) = object ["account" .= acc]
  toJSON (Party party) = object ["party" .= party]

instance FromJSON Payee where
  parseJSON =
    parseObject "Payee" $
      asum
        [ Party <$> "party"
        , Account <$> "account"
        ]

fromCorePayee :: Core.Payee -> Payee
fromCorePayee = \case
  Core.Account acc -> Account $ fromCoreParty acc
  Core.Party p -> Party $ fromCoreParty p

data Token
  = Token CurrencySymbol TokenName
  | TokenRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Token where
  toJSON (Token cs tokName) =
    object
      [ "currency_symbol" .= cs
      , "token_name" .= tokName
      ]
  toJSON (TokenRef label) = object ["ref" .= label]

instance FromJSON Token where
  parseJSON =
    parseObject "Token" $
      asum
        [ Token <$> "currency_symbol" <*> "token_name"
        , TokenRef <$> "ref"
        ]

fromCoreToken :: Core.Token -> Token
fromCoreToken (Core.Token cs tokName) = Token (fromCoreCurrencySymbol cs) (fromCoreTokenName tokName)

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: ByteString}
  deriving newtype (Eq, Ord, Binary)
  deriving (Show, Read, IsString, ToJSON, FromJSON, Variations) via Base16

fromCoreCurrencySymbol :: PV2.CurrencySymbol -> CurrencySymbol
fromCoreCurrencySymbol = CurrencySymbol . PV2.fromBuiltin . PV2.unCurrencySymbol

toCoreCurrencySymbol :: CurrencySymbol -> PV2.CurrencySymbol
toCoreCurrencySymbol = PV2.CurrencySymbol . PV2.toBuiltin . unCurrencySymbol

newtype ContractHash = ContractHash {unContractHash :: ByteString}
  deriving newtype (Eq, Ord, Binary)
  deriving (Show, Read, IsString, ToJSON, FromJSON, Variations) via Base16

fromCoreContractHash :: BuiltinByteString -> ContractHash
fromCoreContractHash = ContractHash . PV2.fromBuiltin

newtype Base16 = Base16 {unBase16 :: ByteString}
  deriving newtype (Eq, Ord, Variations)

instance Show Base16 where
  show = show . encodeBase16 . unBase16

instance Read Base16 where
  readPrec = do
    String s <- lexP
    Right parsed <- pure $ decodeBase16' $ T.pack s
    pure $ Base16 parsed

instance IsString Base16 where
  fromString = read

instance ToJSON Base16 where
  toJSON = A.String . encodeBase16 . unBase16

instance ToJSONKey Base16 where
  toJSONKey = toJSONKeyText $ encodeBase16 . unBase16

instance FromJSON Base16 where
  parseJSON = withText "Base16" $ either (fail . T.unpack) (pure . Base16) . decodeBase16'

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack) (pure . Base16) . decodeBase16'

newtype Verbatim = Verbatim {unVerbatim :: ByteString}
  deriving newtype (Eq, Ord, Show, Read, IsString, Variations)

instance ToJSON Verbatim where
  toJSON = A.String . T.pack . BS8.unpack . unVerbatim

instance ToJSONKey Verbatim where
  toJSONKey = toJSONKeyText $ T.pack . BS8.unpack . unVerbatim

instance FromJSON Verbatim where
  parseJSON = withText "Verbatim" $ pure . Verbatim . BS8.pack . T.unpack

instance FromJSONKey Verbatim where
  fromJSONKey = FromJSONKeyText $ Verbatim . BS8.pack . T.unpack

data Bound = Bound Integer Integer
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Bound where
  toJSON (Bound from to) = object ["from" .= from, "to" .= to]

instance FromJSON Bound where
  parseJSON = withObject "Bound" \obj -> do
    from <- obj .: "from"
    to <- obj .: "to"
    pure $ Bound from to

fromCoreBound :: Core.Bound -> Bound
fromCoreBound (Core.Bound lo hi) = Bound lo hi

toCoreBound :: Bound -> Core.Bound
toCoreBound (Bound lo hi) = Core.Bound lo hi

newtype Timeout = Timeout {unTimeout :: UTCTime}
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (Variations)

instance Num Timeout where
  (+) =
    fmap (Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime)
      . on (+) (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout)
  (*) =
    fmap (Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime)
      . on (*) (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout)
  abs =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . abs
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout
  signum =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . signum
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout
  fromInteger = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . fromInteger
  negate =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . negate
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout

instance Binary Timeout where
  put = put @Integer . floor . (* 1000000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout
  get = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000000) . fromInteger <$> get

instance ToJSON Timeout where
  toJSON = toJSON @Integer . floor . (* 1000000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout

instance FromJSON Timeout where
  parseJSON v = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000000) . fromInteger <$> parseJSON v

toCoreTimeout :: Timeout -> Core.Timeout
toCoreTimeout = PV2.POSIXTime . floor . (* 1000000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout

fromCoreTimeout :: Core.Timeout -> Timeout
fromCoreTimeout = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000000) . fromInteger . getPOSIXTime

-- The following require manual instances to avoid infinite recursion.
instance Variations Contract where
  variations =
    join $
      LNE.fromList
        [ pure Close
        , Pay <$> variations `varyAp` variations `varyAp` variations `varyAp` variations <*> pure Close
        , If <$> variations <*> pure Close <*> pure Close
        , When <$> variations `varyAp` variations <*> pure Close
        , Let <$> variations `varyAp` variations <*> pure Close
        , Assert <$> variations <*> pure Close
        , ContractRef <$> variations
        ]

instance Variations Case where
  variations =
    join $
      LNE.fromList
        [ Case <$> variations <*> pure Close
        , MerkleizedCase <$> variations `varyAp` variations
        ]

instance Variations Observation where
  variations =
    join $
      LNE.fromList
        [ pure $ AndObs FalseObs FalseObs
        , pure $ OrObs FalseObs FalseObs
        , pure $ NotObs FalseObs
        , ChoseSomething <$> variations
        , pure $ ValueGE TimeIntervalStart TimeIntervalStart
        , pure $ ValueLE TimeIntervalStart TimeIntervalStart
        , pure $ ValueGT TimeIntervalStart TimeIntervalStart
        , pure $ ValueLT TimeIntervalStart TimeIntervalStart
        , pure $ ValueEQ TimeIntervalStart TimeIntervalStart
        , pure FalseObs
        , pure TrueObs
        , ObservationRef <$> variations
        ]

instance Variations Value where
  variations =
    join $
      LNE.fromList
        [ AvailableMoney <$> variations `varyAp` variations
        , Constant <$> variations
        , pure $ NegValue (Constant 1)
        , pure $ AddValue (Constant 1) (Constant 1)
        , pure $ SubValue (Constant 1) (Constant 1)
        , pure $ MulValue (Constant 1) (Constant 1)
        , pure $ DivValue (Constant 1) (Constant 1)
        , ChoiceValue <$> variations
        , pure TimeIntervalStart
        , pure TimeIntervalEnd
        , UseValue <$> variations
        , pure $ Cond FalseObs (Constant 1) (Constant 1)
        , ValueRef <$> variations
        ]
