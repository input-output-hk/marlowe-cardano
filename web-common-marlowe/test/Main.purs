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
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (log)
import Examples.PureScript.Swap (fixedTimeoutContract) as Swap
import Marlowe.Extended (toCore)
import Marlowe.Semantics as S
import Marlowe.Template (TemplateContent(..), fillTemplate)

--   [ "swap.json" /\ swap ]
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

writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Effect Unit

main :: Effect Unit
main = do
  case mkContracts of
    Nothing -> do
      log "Failure during test setup"
    Just contracts -> do
      for_ contracts \(fileName /\ contract) -> do
        fileExists <- FS.exists fileName
        if fileExists
          then pure unit
        else

-- for_ - goes through the loop but does not aggregate results

-- contracts.each do |contract|
-- if !File.open(contract.file_name).exists?
--   file = File.create(contract.file_name ) = contract.json
--   file.save
-- end
