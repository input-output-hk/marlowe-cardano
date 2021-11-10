module BridgeTests
  ( all
  ) where

import Prologue
import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError
  , printJsonDecodeError
  )
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (fromInt)
import Data.Map as Map
import Data.String.Regex (replace)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Language.Haskell.Interpreter (CompilationError)
import Marlowe.Semantics
  ( Action(..)
  , Bound(..)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , Observation(..)
  , Party(..)
  , Payee(..)
  , Slot(..)
  , State(..)
  , Token(..)
  , Value(..)
  , ValueId(..)
  )
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Test.Unit (TestSuite, Test, failure, success, suite, test)
import Test.Unit.Assert (equal)

all :: TestSuite
all =
  suite "JSON Serialization" do
    jsonHandling
    serializationTest

jsonHandling :: TestSuite
jsonHandling = do
  test "Json handling" do
    response1 :: Either JsonDecodeError String <- decodeFile
      "test/evaluation_response1.json"
    assertRight response1
    error1 :: Either JsonDecodeError (Array CompilationError) <- decodeFile
      "test/evaluation_error1.json"
    assertRight error1

serializationTest :: TestSuite
serializationTest =
  test "Contract Serialization" do
    -- A simple test that runs the Escrow contract to completion
    let
      ada = Token "" ""

      alicePk = PK "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"

      bobRole = Role "Bob"

      const = Constant (fromInt 100)

      choiceId = ChoiceId "choice" alicePk

      valueExpr = AddValue const (SubValue const (NegValue const))

      token = Token "aa" "name"

      contract =
        Assert TrueObs
          ( When
              [ Case (Deposit alicePk alicePk ada valueExpr)
                  ( Let (ValueId "x") valueExpr
                      ( Pay alicePk (Party bobRole) ada
                          ( Cond TrueObs (UseValue (ValueId "x"))
                              (UseValue (ValueId "y"))
                          )
                          Close
                      )
                  )
              , Case
                  ( Choice choiceId
                      [ Bound (fromInt 0) (fromInt 1)
                      , Bound (fromInt 10) (fromInt 20)
                      ]
                  )
                  ( If
                      ( ChoseSomething choiceId `OrObs`
                          (ChoiceValue choiceId `ValueEQ` const)
                      )
                      ( Pay alicePk (Account alicePk) token
                          (DivValue (AvailableMoney alicePk token) const)
                          Close
                      )
                      Close
                  )
              , Case
                  ( Notify
                      ( AndObs (SlotIntervalStart `ValueLT` SlotIntervalEnd)
                          TrueObs
                      )
                  )
                  Close
              ]
              (Slot (fromInt 100))
              Close
          )

      state =
        State
          { accounts: Map.singleton (Tuple alicePk token) (fromInt 12)
          , choices: Map.singleton choiceId (fromInt 42)
          , boundValues: Map.fromFoldable
              [ Tuple (ValueId "x") (fromInt 1)
              , Tuple (ValueId "y") (fromInt 2)
              ]
          , minSlot: (Slot $ fromInt 123)
          }

      json = encodeStringifyJson contract

      jsonState = encodeStringifyJson state
    expectedStateJson <- liftEffect $ FS.readTextFile UTF8 "test/state.json"
    bridgedJson <- liftEffect $ FS.readTextFile UTF8
      "generated/JSON/contract.json"
    bridgedStateJson <- liftEffect $ FS.readTextFile UTF8
      "generated/JSON/state.json"
    let
      rx = unsafeRegex "\\s+"
        ( RegexFlags
            { dotAll: false
            , global: true
            , ignoreCase: true
            , multiline: true
            , sticky: false
            , unicode: true
            }
        )

      expectedState = replace rx "" expectedStateJson
    equal expectedState jsonState
    equal (Right contract) (lmap printJsonDecodeError $ parseDecodeJson json)
    equal (Right contract)
      (lmap printJsonDecodeError $ parseDecodeJson bridgedJson)
    equal (Right state)
      (lmap printJsonDecodeError $ parseDecodeJson bridgedStateJson)

assertRight :: forall a. Either JsonDecodeError a -> Test
assertRight (Left err) = failure (printJsonDecodeError err)

assertRight (Right _) = success

decodeFile
  :: forall m a
   . MonadAff m
  => DecodeJson a
  => String
  -> m (Either JsonDecodeError a)
decodeFile filename = do
  contents <- liftEffect $ FS.readTextFile UTF8 filename
  pure (parseDecodeJson contents)
