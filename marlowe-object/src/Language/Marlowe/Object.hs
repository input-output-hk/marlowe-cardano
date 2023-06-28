{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Marlowe.Object where

import Cardano.Api (
  Address,
  AsType (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  deserialiseFromBech32,
  deserialiseFromRawBytes,
  serialiseToBech32,
 )
import Cardano.Api.Byron (ShelleyAddr)
import Control.Applicative (Alternative (many))
import Data.Aeson hiding (Object, String, Value)
import qualified Data.Aeson as A hiding (Object)
import Data.Aeson.Applicative (parseObject, requiredExplicit)
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16', encodeBase16)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (asum, traverse_)
import Data.Function (on)
import Data.List (intercalate)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import GHC.Read (Read (..), lexP)
import Text.Read (Lexeme (..))

newtype ObjectBundle = ObjectBundle {getObjects :: [LabelledObject]}
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance Binary ObjectBundle where
  put = traverse_ put . getObjects
  get = ObjectBundle <$> many get

data LabelledObject = LabelledObject Label Object
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON LabelledObject where
  toJSON (LabelledObject label obj) =
    object $
      ("label" .= label)
        : case obj of
          ValueObject a -> ["type" .= ("value" :: String), "value" .= a]
          ObservationObject a -> ["type" .= ("observation" :: String), "value" .= a]
          ContractObject a -> ["type" .= ("contract" :: String), "value" .= a]
          PartyObject a -> ["type" .= ("party" :: String), "value" .= a]
          TokenObject a -> ["type" .= ("token" :: String), "value" .= a]
          ActionObject a -> ["type" .= ("action" :: String), "value" .= a]

instance FromJSON LabelledObject where
  parseJSON = withObject "LabelledObject" \obj -> do
    label <- obj .: "label"
    type_ :: String <- obj .: "type"
    value <- obj .: "value"
    LabelledObject label <$> case type_ of
      "value" -> ValueObject <$> parseJSON value
      "observation" -> ObservationObject <$> parseJSON value
      "contract" -> ContractObject <$> parseJSON value
      "party" -> PartyObject <$> parseJSON value
      "token" -> TokenObject <$> parseJSON value
      "action" -> ActionObject <$> parseJSON value
      _ ->
        parseFail $
          mappend "Invalid object type. Valid options are: " $
            intercalate ", " $
              show @String
                <$> [ "value"
                    , "observation"
                    , "contract"
                    , "party"
                    , "token"
                    , "action"
                    ]

data Object
  = ValueObject Value
  | ObservationObject Observation
  | ContractObject Contract
  | PartyObject Party
  | TokenObject Token
  | ActionObject Action
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

newtype Label = Label {unLabel :: ByteString}
  deriving (Show, Read, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary)

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

data Action
  = Deposit AccountId Party Token Value
  | Choice ChoiceId [Bound]
  | Notify Observation
  | ActionRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

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

type AccountId = Party

data Party
  = Address !ShelleyAddress
  | Role !TokenName
  | PartyRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

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

newtype ShelleyAddress = ShelleyAddress {unShelleyAddress :: Address ShelleyAddr}
  deriving (Eq, Ord)

instance Show ShelleyAddress where
  show = T.unpack . serialiseToBech32 . unShelleyAddress

instance Read ShelleyAddress where
  readPrec = do
    String s <- lexP
    Right addr <- pure $ deserialiseFromBech32 (AsAddress AsShelleyAddr) $ T.pack s
    pure $ ShelleyAddress addr

instance IsString ShelleyAddress where
  fromString = read

instance Binary ShelleyAddress where
  put = put . serialiseToRawBytes . unShelleyAddress
  get =
    maybe (fail "failed to deserialize address") (pure . ShelleyAddress)
      . deserialiseFromRawBytes (AsAddress AsShelleyAddr)
      =<< get

instance ToJSON ShelleyAddress where
  toJSON (ShelleyAddress address) = A.String $ serialiseToBech32 address

instance FromJSON ShelleyAddress where
  parseJSON =
    withText "ShelleyAddress" $
      either (parseFail . show) (pure . ShelleyAddress) . deserialiseFromBech32 (AsAddress AsShelleyAddr)

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving (Show, Read, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary)

data ChoiceId = ChoiceId ByteString Party
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) =
    object
      [ "choice_name" .= (A.String $ T.decodeUtf8 name)
      , "choice_owner" .= party
      ]

instance FromJSON ChoiceId where
  parseJSON =
    parseObject "Payee" $
      ChoiceId
        <$> requiredExplicit "choice_name" (withText "" $ pure . T.encodeUtf8)
        <*> "choice_owner"

newtype ValueId = ValueId {unValueId :: ByteString}
  deriving (Show, Read, Eq, Ord)
  deriving (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary)

data Payee
  = Party !Party
  | Account !Party
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

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

data Token
  = Token CurrencySymbol TokenName
  | TokenRef Label
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (Binary)

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

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: ByteString}
  deriving newtype (Eq, Ord, Binary)
  deriving (Show, Read, IsString, ToJSON, FromJSON) via Base16

newtype ContractHash = ContractHash {unContractHash :: ByteString}
  deriving newtype (Eq, Ord, Binary)
  deriving (Show, Read, IsString, ToJSON, FromJSON) via Base16

newtype Base16 = Base16 {unBase16 :: ByteString}
  deriving (Eq, Ord)

instance Show Base16 where
  show = T.unpack . encodeBase16 . unBase16

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
  deriving newtype (Eq, Ord, Show, Read, IsString)

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
  deriving anyclass (Binary)

instance ToJSON Bound where
  toJSON (Bound from to) = object ["from" .= from, "to" .= to]

instance FromJSON Bound where
  parseJSON = withObject "Bound" \obj -> do
    from <- obj .: "from"
    to <- obj .: "to"
    pure $ Bound from to

newtype Timeout = Timeout {unTimeout :: UTCTime}
  deriving stock (Generic, Show, Read, Eq, Ord)

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
