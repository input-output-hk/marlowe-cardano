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
import Examples.PureScript.Swap (fixedTimeoutContract) as Swap
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
        , valueContent: Map.fromFoldable
            [ "Amount of Ada" /\ fromInt 500
            , "Amount of dollars" /\ fromInt 1100
            ]
        }
  swap <- toCore $ fillTemplate swapContent Swap.fixedTimeoutContract
  pure
    [ "swap.json" /\ swap
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

-- for_ - goes through the loop but does not aggregate results

-- contracts.each do |contract|
-- if !File.open(contract.file_name).exists?
--   file = File.create(contract.file_name ) = contract.json
--   file.save
-- end
