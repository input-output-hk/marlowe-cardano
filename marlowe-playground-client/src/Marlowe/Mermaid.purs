module Marlowe.Mermaid where

import Prologue
import Data.Array (fromFoldable) as Array
import Data.Foldable (foldMap)
import Data.List (List(..), singleton) as List
import Data.List (List, (:))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll) as String
import Data.Tuple.Nested ((/\))
import Marlowe.Holes (Action(..), Case(..), ChoiceId(..), Contract(..), Location(..), Observation(..), Party(..), Term(..), TermWrapper(..), Timeout, Value(..), ValueId(..), getLocation)
import Text.Pretty (pretty)

type Edge
  = { start :: Term Contract, end :: Term Contract, label :: Maybe String }

flatten :: Term Contract -> List Edge
flatten (Hole _ _) = List.Nil

flatten start@(Term c _) = case c of
  Close -> List.Nil
  Pay _ _ _ _ end -> { start, end, label: Nothing } : flatten end
  Let _ _ end -> { start, end, label: Nothing } : flatten end
  Assert _ end -> { start, end, label: Nothing } : flatten end
  If _ trueContract falseContract ->
    { start, end: trueContract, label: Just "True" } : flatten trueContract
      <> { start, end: falseContract, label: Just "False" }
      : flatten falseContract
  When cases t end -> do
    let
      step (Term (Case a c') _) = { start, end: c', label: Just $ actionShow a } : flatten c'

      step (Hole name _) = List.singleton { start, end: Hole "to be defined" (BlockId "test"), label: Just name }

      cases' = foldMap step cases
    { start, end, label: Just $ show $ "slot >= " <> timeoutShow t } : cases'

-- | Escape a string for display in mermaid.  In mermaid everything needs to
-- be in quotes, so we just swap any double-quotes with single-quotes.
escape :: String -> String
escape s = "\"" <> s' <> "\""
  where
  s' = String.replaceAll (String.Pattern "\"") (String.Replacement "'") s

partyShow :: Term Party -> String
partyShow h@(Hole _ _) = show $ pretty h

partyShow (Term (Role r) _) = r

partyShow (Term pubKey _) = show $ pretty pubKey

-- | A concise representation of an action for display on an edge.
actionShow :: Term Action -> String
actionShow (Hole name _) = name

actionShow (Term action _) = escape <<< showAction $ action
  where
  showAction (Deposit accountId party _ val) =
    partyShow party
      <> " deposits "
      <> valueShow val
      <> " lovelaces into "
      <> partyShow accountId
      <> " account"

  showAction (Choice (ChoiceId id party) _) = partyShow party <> " makes a choice in " <> show id

  showAction (Notify obs) = "A notification on " <> observationShow obs

timeoutShow :: Term Timeout -> String
timeoutShow t = escape $ show $ pretty t

-- | A concise representation of an Observation for display in a node.
observationShow :: Term Observation -> String
observationShow (Hole name _) = escape name

observationShow (Term a _) = escape <<< repr $ a
  where
  repr (AndObs obs1 obs2) = observationShow obs1 <> " && " <> observationShow obs2

  repr (OrObs obs1 obs2) = "(" <> observationShow obs1 <> ") || (" <> observationShow obs2 <> ")"

  repr (NotObs obs) = "!" <> observationShow obs

  repr (ChoseSomething (ChoiceId id party)) = partyShow party <> " choice on " <> show id

  repr (ValueGE val1 val2) = valueShow val1 <> " >= " <> valueShow val2

  repr (ValueGT val1 val2) = valueShow val1 <> " > " <> valueShow val2

  repr (ValueLT val1 val2) = valueShow val1 <> " < " <> valueShow val2

  repr (ValueLE val1 val2) = valueShow val1 <> " <= " <> valueShow val2

  repr (ValueEQ val1 val2) = valueShow val1 <> " == " <> valueShow val2

  repr TrueObs = "true"

  repr FalseObs = "false"

-- | A concise representaiton of an individual 'Contract' for displaying on
-- nodes in the mermaid graph.
contractShow :: Term Contract -> String
contractShow (Hole name _) = name

contractShow (Term c _) =
  escape
    $ case c of
        Close -> "Close"
        Pay from to _ v _ -> "Pay " <> valueShow v <> " from " <> show from <> " to " <> show to
        If obs _ _ -> observationShow obs
        When _ _ _ -> "When ..."
        Let (TermWrapper (ValueId valId) _) val _ -> "let " <> show valId <> " = " <> valueShow val
        Assert obs _ -> observationShow obs

valueShow :: Term Value -> String
valueShow (Hole name _) = name

valueShow (Term v _) =
  escape
    $ case v of
        Constant n -> show $ pretty n
        ConstantParam param -> show param
        AddValue val1 val2 -> "(" <> valueShow val1 <> " + " <> valueShow val2 <> ")"
        NegValue val1 -> "-" <> valueShow val1
        SubValue val1 val2 -> "(" <> valueShow val1 <> " - " <> valueShow val2 <> ")"
        MulValue val1 val2 -> "(" <> valueShow val1 <> " * " <> valueShow val2 <> ")"
        DivValue val1 val2 -> "(" <> valueShow val1 <> " / " <> valueShow val2 <> ")"
        ChoiceValue (ChoiceId id party) -> partyShow party <> " choice on " <> show id
        Cond obs val1 val2 -> "(" <> observationShow obs <> " ? " <> valueShow val1 <> " : " <> valueShow val2 <> ")"
        UseValue (TermWrapper (ValueId id) _) -> show id
        AvailableMoney party _ -> partyShow party <> "'s available money"
        SlotIntervalStart -> "slot interval start"
        SlotIntervalEnd -> "slot interval end"

-- | Convert a 'Contract' into a string that can be printed and loaded into the
-- mermaid live editor: <https://mermaid-js.github.io/mermaid-live-editor/>.
toMermaid :: Term Contract -> String
toMermaid (Hole name _) = name

toMermaid n@(Term _ _) = String.joinWith "\n" <<< Array.fromFoldable $ "graph TB" : map f (flatten n)
  where
  shapes =
    { circle: "((" /\ "))"
    , stadium: "([" /\ "])"
    , rhombus: "{" /\ "}"
    , subrutine: "[[" /\ "]]"
    , flag: ">" /\ "]"
    }

  brackets = case _ of
    Close -> shapes.circle
    Pay _ _ _ _ _ -> shapes.stadium
    If _ _ _ -> shapes.rhombus
    When _ _ _ -> shapes.rhombus
    Let _ _ _ -> shapes.subrutine
    Assert _ _ -> shapes.flag

  brackets' (Hole _ _) = shapes.circle

  brackets' (Term c _) = brackets c

  f { start, end, label } = do
    let
      locationId = case _ of
        BlockId b -> "block" <> b
        Range ({ startLineNumber, startColumn }) -> "start" <> show startLineNumber <> "x" <> show startColumn
        NoLocation -> "NoLocation?"

      node c = do
        let
          bl /\ br = brackets' c

          loc = getLocation c
        locationId loc <> bl <> contractShow c <> br

      label' = fromMaybe "Then" label
    node start <> " -->|" <> label' <> "| " <> node end
