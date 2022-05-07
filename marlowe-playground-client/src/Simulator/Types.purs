module Simulator.Types where

import Prologue

import Control.Monad.Reader (ReaderT(..))
import Data.Argonaut (JsonDecodeError(..), decodeJson)
import Data.Argonaut as JSON
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson (Decoder, (</$\>), (</*\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<))
import Data.Argonaut.Encode.Aeson as E
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int (round)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Marlowe.Holes (Holes, Term)
import Marlowe.Holes as T
import Marlowe.Semantics
  ( AccountId
  , Assets
  , Bound
  , ChoiceId
  , ChosenNum
  , Input
  , Party(..)
  , Payment
  , TimeInterval
  , Token
  , TransactionError
  , TransactionInput
  , TransactionWarning
  )
import Marlowe.Semantics as S
import Marlowe.Template (TemplateContent)
import Monaco (IMarker)

data ActionInputId
  = DepositInputId AccountId Party Token BigInt
  | ChoiceInputId ChoiceId
  | NotifyInputId
  | MoveToTimeId MoveToTimeType

derive instance eqActionInputId :: Eq ActionInputId

derive instance ordActionInputId :: Ord ActionInputId

derive instance genericActionInputId :: Generic ActionInputId _

instance encodeJsonActionInputId :: EncodeJson ActionInputId where
  encodeJson (DepositInputId accountId party token amount) =
    encodeJson
      { tag: "DepositInputId"
      , contents:
          [ encodeJson accountId
          , encodeJson party
          , encodeJson token
          , encodeJson amount
          ]
      }
  encodeJson (ChoiceInputId choiceId) =
    encodeJson
      { tag: "ChoiceInputId"
      , contents: encodeJson choiceId
      }
  encodeJson NotifyInputId = encodeJson { tag: "NotifyInputId" }
  encodeJson (MoveToTimeId moveType) = encodeJson
    { tag: "MoveToTimeId", contents: encodeJson moveType }

instance decodeJsonActionInputId :: DecodeJson ActionInputId where
  decodeJson =
    D.decode $ D.sumType "ActionInputId"
      $ Map.fromFoldable
          [ "DepositInputId" /\ D.content
              ( D.tuple $ DepositInputId </$\> D.value </*\> D.value
                  </*\> D.value
                  </*\> D.value
              )
          , "ChoiceInputId" /\ D.content (ChoiceInputId <$> D.value)
          , "NotifyInputId" /\ pure NotifyInputId
          , "MoveToTimeId" /\ D.content (MoveToTimeId <$> D.value)
          ]

-- These are the types of move to time actions available in the Simulator
data MoveToTimeType
  = NextTime
  -- ^ Next possible emulated time (aka next minute)
  | NextTimeout
  -- ^ Next meaningfull time (when the next timeout occurs)
  | ExpirationTime -- <- Final expiration of the contract

instance Show MoveToTimeType where
  show = case _ of
    NextTime -> "NextTime"
    NextTimeout -> "NextTimeout"
    ExpirationTime -> "ExpirationTime"

derive instance Eq MoveToTimeType

derive instance Ord MoveToTimeType

instance EncodeJson MoveToTimeType where
  encodeJson a = JSON.fromString $ show a

instance DecodeJson MoveToTimeType where
  decodeJson value = do
    stringValue <- decodeJson value
    case stringValue of
      "NextTime" -> Right NextTime
      "NextTimeout" -> Right NextTimeout
      "ExpirationTime" -> Right ExpirationTime
      _ -> Left (TypeMismatch "MoveToTimeType")

-- | On the front end we need Actions however we also need to keep track of the current
-- | choice that has been set for Choices
data ActionInput
  = DepositInput AccountId Party Token BigInt
  | ChoiceInput ChoiceId (Array Bound) ChosenNum
  | NotifyInput
  | MoveToTime MoveToTimeType Instant

derive instance eqActionInput :: Eq ActionInput

derive instance ordActionInput :: Ord ActionInput

derive instance genericActionInput :: Generic ActionInput _

instance Show ActionInput where
  show = genericShow

instance encodeJsonActionInput :: EncodeJson ActionInput where
  encodeJson (DepositInput accountId party token amount) =
    encodeJson
      { tag: "DepositInput"
      , contents:
          [ encodeJson accountId
          , encodeJson party
          , encodeJson token
          , encodeJson amount
          ]
      }
  encodeJson (ChoiceInput choiceId bounds chosen) =
    encodeJson
      { tag: "ChoiceInput"
      , contents:
          [ encodeJson choiceId
          , encodeJson bounds
          , encodeJson chosen
          ]
      }
  encodeJson NotifyInput = encodeJson { tag: "NotifyInput" }
  encodeJson (MoveToTime moveType time) =
    encodeJson
      { tag: "MoveToTime"
      , contents: encodeJson
          [ encodeJson moveType
          , encodeJson $ round $ unwrap $ unInstant time
          ]
      }

instantDecoder :: Decoder Instant
instantDecoder =
  ReaderT \json -> do
    numberValue <- decodeJson json
    note (TypeMismatch "Instant") $ instant (Milliseconds numberValue)

instance decodeJsonActionInput :: DecodeJson ActionInput where
  decodeJson =
    D.decode $ D.sumType "ActionInputId"
      $ Map.fromFoldable
          [ "DepositInput" /\ D.content
              ( D.tuple $ DepositInput </$\> D.value </*\> D.value
                  </*\> D.value
                  </*\> D.value
              )
          , "ChoiceInput" /\ D.content
              ( D.tuple $ ChoiceInput </$\> D.value </*\> D.value </*\>
                  D.value
              )
          , "NotifyInput" /\ pure NotifyInput
          , "MoveToTime" /\ D.content
              ( D.tuple $ MoveToTime </$\> D.value </*\> instantDecoder
              )
          ]

newtype PartiesAction = PartiesAction
  (Map Party (Map ActionInputId ActionInput))

derive instance newtypeParties :: Newtype PartiesAction _

instance semigroupParties :: Semigroup PartiesAction where
  append (PartiesAction a) (PartiesAction b) = PartiesAction $ Map.unionWith
    Map.union
    a
    b

instance monoidParties :: Monoid PartiesAction where
  mempty = PartiesAction $ Map.empty

instance encodeJsonParties :: EncodeJson PartiesAction where
  encodeJson = E.encode $ unwrap >$< E.dictionary E.value E.value

instance decodeJsonParties :: DecodeJson PartiesAction where
  decodeJson = D.decode $ PartiesAction <$> D.dictionary D.value D.value

moveToTimePartyAction :: MoveToTimeType -> Instant -> PartiesAction
moveToTimePartyAction moveType time = PartiesAction
  $ Map.singleton otherActionsParty
  $ Map.singleton (MoveToTimeId moveType) (MoveToTime moveType time)

-- We have a special person for notifications
otherActionsParty :: Party
otherActionsParty = Role "marlowe_other_actions"

data LogEntry
  = StartEvent Instant
  | InputEvent TransactionInput
  | OutputEvent TimeInterval Payment
  | CloseEvent TimeInterval

derive instance genericLogEntry :: Generic LogEntry _

instance encodeJsonLogEntry :: EncodeJson LogEntry where
  encodeJson (StartEvent instant) =
    encodeJson
      { tag: "StartEvent"
      , contents: encodeJson $ round $ unwrap $ unInstant instant
      }
  encodeJson (InputEvent input) =
    encodeJson
      { tag: "InputEvent"
      , contents: encodeJson input
      }
  encodeJson (OutputEvent interval payment) =
    encodeJson
      { tag: "OutputEvent"
      , contents:
          [ encodeJson interval
          , encodeJson payment
          ]
      }
  encodeJson (CloseEvent interval) =
    encodeJson
      { tag: "CloseEvent"
      , contents: encodeJson interval
      }

instance decodeJsonLogEntry :: DecodeJson LogEntry where
  decodeJson =
    D.decode $ D.sumType "LogEntry"
      $ Map.fromFoldable
          [ "StartEvent" /\ D.content
              ( StartEvent
                  <<< fromMaybe bottom
                  <<< instant
                  <<< Milliseconds
                  <<< Int.toNumber
                  <$> D.value
              )
          , "InputEvent" /\ D.content (InputEvent <$> D.value)
          , "OutputEvent" /\ D.content (uncurry OutputEvent <$> D.value)
          , "CloseEvent" /\ D.content (CloseEvent <$> D.value)
          ]

type ExecutionStateRecord =
  { possibleActions :: PartiesAction
  , pendingInputs :: Array Input
  , transactionError :: Maybe TransactionError
  , transactionWarnings :: Array TransactionWarning
  , log :: Array LogEntry
  , state :: S.State
  , time :: Instant
  , moneyInContract :: Assets
  -- This is the remaining of the contract to be executed
  , contract :: Term T.Contract
  }

type InitialConditionsRecord =
  { initialTime :: Instant
  , termContract :: Term T.Contract
  , templateContent :: TemplateContent
  }

data ExecutionState
  = SimulationRunning ExecutionStateRecord
  | SimulationNotStarted InitialConditionsRecord

type MarloweState =
  { executionState :: Maybe ExecutionState
  , holes :: Holes
  -- NOTE: as part of the marlowe editor and simulator split this part of the
  --       state wont be used, but it is left as it is because it may make sense
  --       to use it as part of task SCP-1642
  , editorErrors :: Array IMarker
  , editorWarnings :: Array IMarker
  }
