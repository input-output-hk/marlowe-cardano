module Test.Main where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , caseJsonObject
  , decodeJson
  , encodeJson
  )
import Data.Argonaut.Core (stringify)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (log)
import Examples.PureScript.ContractForDifferences
  ( defaultSlotContent
  , fixedTimeoutContract
  ) as ContractForDifferences
import Examples.PureScript.Escrow (defaultSlotContent, fixedTimeoutContract) as Escrow
import Examples.PureScript.Swap (defaultSlotContent, fixedTimeoutContract) as Swap
import Examples.PureScript.ZeroCouponBond
  ( defaultSlotContent
  , fixedTimeoutContract
  ) as ZeroCouponBond
import Marlowe.Extended (toCore)
import Marlowe.Semantics (Contract)
import Marlowe.Semantics as S
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)

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
        { slotContent: Map.empty
        , valueContent: Map.insert "Amount paid by party" (fromInt 2000) $
            Map.insert "Amount paid by counterparty" (fromInt 3000)
              ContractForDifferences.defaultSlotContent
        }

  swap <- toCore $ fillTemplate swapContent Swap.fixedTimeoutContract
  escrow <- toCore $ fillTemplate escrowContent Escrow.fixedTimeoutContract
  zeroCouponBond <- toCore $ fillTemplate zeroCouponBondContent
    ZeroCouponBond.fixedTimeoutContract
  contractForDifferences <- toCore $ fillTemplate contractForDifferencesContent
    ContractForDifferences.fixedTimeoutContract
  -- traceM $ fillTemplate zeroCouponBondContent ZeroCouponBond.fixedTimeoutContract
  pure
    [ "swap.json" /\ swap
    , "escrow.json" /\ escrow
    , "zeroCouponBond.json" /\ zeroCouponBond
    , "contractForDifferences.json" /\ contractForDifferences
    ]

-- FIX ME: must wrap main do with Data.BigInt.Argonaut.withJsonPatch. but adapted for Effect
main :: Effect Unit
main = do
  case mkContracts of
    Nothing -> do
      log "Failure during test setup"
    Just contracts -> do
      for_ contracts \(fileName /\ contract) -> do
        fileExists <- FS.exists fileName
        if fileExists then pure unit
        else
          FS.writeTextFile UTF8 fileName $ stringify $ encodeJson contract