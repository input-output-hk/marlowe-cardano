module Simulator.Types where

import Prologue
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson as E
import Data.Argonaut.Encode.Aeson ((>$<))
import Data.BigInt.Argonaut (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Marlowe.Template (TemplateContent)
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
  , Slot
  , SlotInterval
  , Token
  , TransactionError
  , TransactionInput
  , TransactionWarning
  )
import Marlowe.Semantics as S
import Monaco (IMarker)

data ActionInputId
  = DepositInputId AccountId Party Token BigInt
  | ChoiceInputId ChoiceId
  | NotifyInputId
  | MoveToSlotId

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
  encodeJson MoveToSlotId = encodeJson { tag: "MoveToSlotId" }

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
          , "MoveToSlotId" /\ pure MoveToSlotId
          ]

-- | On the front end we need Actions however we also need to keep track of the current
-- | choice that has been set for Choices
data ActionInput
  = DepositInput AccountId Party Token BigInt
  | ChoiceInput ChoiceId (Array Bound) ChosenNum
  | NotifyInput
  | MoveToSlot Slot

derive instance eqActionInput :: Eq ActionInput

derive instance ordActionInput :: Ord ActionInput

derive instance genericActionInput :: Generic ActionInput _

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
  encodeJson (MoveToSlot slot) =
    encodeJson
      { tag: "MoveToSlot"
      , contents: encodeJson slot
      }

instance decodeJsonActionInput :: DecodeJson ActionInput where
  decodeJson =
    D.decode $ D.sumType "ActionInputId"
      $ Map.fromFoldable
          [ "DepositInput" /\ D.content
              ( D.tuple $ DepositInput </$\> D.value </*\> D.value </*\> D.value
                  </*\> D.value
              )
          , "ChoiceInput" /\ D.content
              (D.tuple $ ChoiceInput </$\> D.value </*\> D.value </*\> D.value)
          , "NotifyInput" /\ pure NotifyInput
          , "MoveToSlot" /\ D.content (MoveToSlot <$> D.value)
          ]

-- TODO: Probably rename to PartiesActions or similar
newtype Parties
  = Parties (Map Party (Map ActionInputId ActionInput))

derive instance newtypeParties :: Newtype Parties _

instance semigroupParties :: Semigroup Parties where
  append (Parties a) (Parties b) = Parties $ Map.unionWith (const identity) a b

instance monoidParties :: Monoid Parties where
  mempty = Parties $ Map.empty

instance encodeJsonParties :: EncodeJson Parties where
  encodeJson = E.encode $ unwrap >$< E.dictionary E.value E.value

instance decodeJsonParties :: DecodeJson Parties where
  decodeJson = D.decode $ Parties <$> D.dictionary D.value D.value

-- We have a special person for notifications
otherActionsParty :: Party
otherActionsParty = Role "marlowe_other_actions"

data LogEntry
  = StartEvent Slot
  | InputEvent TransactionInput
  | OutputEvent SlotInterval Payment
  | CloseEvent SlotInterval

derive instance genericLogEntry :: Generic LogEntry _

instance encodeJsonLogEntry :: EncodeJson LogEntry where
  encodeJson (StartEvent slot) =
    encodeJson
      { tag: "StartEvent"
      , contents: encodeJson slot
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
          [ "StartEvent" /\ D.content (StartEvent <$> D.value)
          , "InputEvent" /\ D.content (InputEvent <$> D.value)
          , "OutputEvent" /\ D.content (uncurry OutputEvent <$> D.value)
          , "CloseEvent" /\ D.content (CloseEvent <$> D.value)
          ]

type ExecutionStateRecord =
  { possibleActions :: Parties
  , pendingInputs :: Array Input
  , transactionError :: Maybe TransactionError
  , transactionWarnings :: Array TransactionWarning
  , log :: Array LogEntry
  , state :: S.State
  , slot :: Slot
  , moneyInContract :: Assets
  -- This is the remaining of the contract to be executed
  , contract :: Term T.Contract
  }

type InitialConditionsRecord =
  { initialSlot :: Slot
  -- TODO: Should we remove the Maybe and just not set InitialConditionsRecord if we cannot
  --       parse the contract?
  , termContract :: Maybe (Term T.Contract)
  , templateContent :: TemplateContent
  }

data ExecutionState
  = SimulationRunning ExecutionStateRecord
  | SimulationNotStarted InitialConditionsRecord

type MarloweState =
  { executionState :: ExecutionState
  , holes :: Holes
  -- NOTE: as part of the marlowe editor and simulator split this part of the
  --       state wont be used, but it is left as it is because it may make sense
  --       to use it as part of task SCP-1642
  , editorErrors :: Array IMarker
  , editorWarnings :: Array IMarker
  }
