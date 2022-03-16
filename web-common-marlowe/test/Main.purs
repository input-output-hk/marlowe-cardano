module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , caseJsonObject
  , decodeJson
  , encodeJson
  , jsonParser
  , stringify
  )
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class.Console (log)
import Effect.Exception (Error, throw)
import Examples.PureScript.ContractForDifferences
  ( defaultSlotContent
  , extendedContract
  ) as ContractForDifferences
import Examples.PureScript.Escrow (defaultSlotContent, fixedTimeoutContract) as Escrow
import Examples.PureScript.EscrowWithCollateral
  ( defaultSlotContent
  , fixedTimeoutContract
  ) as EscrowWithCollateral
import Examples.PureScript.Swap (defaultSlotContent, fixedTimeoutContract) as Swap
import Examples.PureScript.ZeroCouponBond
  ( defaultSlotContent
  , fixedTimeoutContract
  ) as ZeroCouponBond
import Marlowe.Extended (toCore)
import Marlowe.Semantics (Contract)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

mkContracts :: Maybe (Array (FilePath /\ Contract))
mkContracts = do
  let
    swapContent =
      TemplateContent
        { slotContent: Map.empty
        , valueContent: Map.insert "Amount of Ada" (fromInt 500) $ Map.insert
            "Amount of dollars"
            (fromInt 1100)
            Swap.defaultSlotContent
        }
    escrowContent =
      TemplateContent
        { slotContent: Map.empty
        , valueContent: Map.insert "Price" (fromInt 1500)
            Escrow.defaultSlotContent
        }
    escrowWithCollateralContent =
      TemplateContent
        { slotContent: Map.empty
        , valueContent: Map.insert "Collateral amount" (fromInt 5000) $
            Map.insert "Price" (fromInt 15000)
              EscrowWithCollateral.defaultSlotContent
        }
    zeroCouponBondContent =
      TemplateContent
        { slotContent: Map.empty
        , valueContent: Map.insert "Amount" (fromInt 2000) $ Map.insert
            "Interest"
            (fromInt 3)
            ZeroCouponBond.defaultSlotContent
        }
    contractForDifferencesContent =
      TemplateContent
        { slotContent: ContractForDifferences.defaultSlotContent
        , valueContent: Map.fromFoldable
            [ "Amount paid by party" /\ fromInt 25000
            , "Amount paid by counterparty" /\ fromInt 1800
            ]
        }
  swap <- toCore $ fillTemplate swapContent Swap.fixedTimeoutContract
  escrow <- toCore $ fillTemplate escrowContent Escrow.fixedTimeoutContract
  escrowWithCollateral <- toCore $ fillTemplate escrowWithCollateralContent
    EscrowWithCollateral.fixedTimeoutContract
  zeroCouponBond <- toCore $ fillTemplate zeroCouponBondContent
    ZeroCouponBond.fixedTimeoutContract
  contractForDifferences <- toCore $ fillTemplate contractForDifferencesContent
    ContractForDifferences.extendedContract
  -- traceM $ fillTemplate contractForDifferencesContent ContractForDifferences.extendedContract
  pure
    [ "./test/fixtures/swap.json" /\ swap
    , "./test/fixtures/escrow.json" /\ escrow
    , "./test/fixtures/escrowWithCollateral.json" /\ escrowWithCollateral
    , "./test/fixtures/zeroCouponBond.json" /\ zeroCouponBond
    , "./test/fixtures/contractForDifferences.json" /\ contractForDifferences
    ]

buildFixtures :: Effect (Array (String /\ Json /\ Contract))
buildFixtures = case mkContracts of
  Nothing -> do
    throw "Failure during test setup"
  Just contracts -> do
    for_ contracts \(fileName /\ contract) -> do
      fileExists <- FS.exists fileName
      if fileExists then pure unit
      else
        FS.writeTextFile UTF8 fileName $ stringify $ encodeJson contract
    for contracts \(fileName /\ contract) -> do
      jsonString <- FS.readTextFile UTF8 fileName
      case jsonParser jsonString of
        Left err -> do
          throw $ "Parsing JSON failed: " <> err
        Right json -> do
          pure $ fileName /\ json /\ contract

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Eq t
  => (t -> String)
  -> t
  -> t
  -> m Unit
shouldEqual f v1 v2 =
  when (v1 /= v2)
    $ fail
    $ f v1 <> " ≠ " <> f v2

-- FIX ME: must wrap main do with Data.BigInt.Argonaut.withJsonPatch. but adapted for Effect
main :: Effect Unit
main = do
  fixtures <- buildFixtures
  launchAff_ $ runSpec [ consoleReporter ] do
    describe "Core Contract Golden Tests" do
      for_ fixtures \(fileName /\ expectJson /\ testContract) -> do
        it ("produces json that matches the " <> fileName <> " golden file") do
          encodeJson testContract `shouldEqual stringify` expectJson