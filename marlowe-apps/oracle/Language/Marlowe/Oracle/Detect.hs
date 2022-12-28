

{-# LANGUAGE DataKinds #-}


module Language.Marlowe.Oracle.Detect
  ( containsOracleAction
  , contractReadyForOracle
  , hasOracleAction
  , oraclePresent
  , oracleReady
  ) where


import Control.Monad.Extra (pureIf)
import Data.Maybe (mapMaybe, maybeToList)
import Language.Marlowe.Core.V1.Plate (extract, extractAll)
import Language.Marlowe.Core.V1.Semantics.Types (Action(Choice), ChoiceId(..), Contract, Party)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), contractFromStream)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import Network.Oracle (Oracle, toOracleSymbol)
import Plutus.V2.Ledger.Api (fromBuiltin)

import Data.ByteString.Char8 as BS8 (unpack)


hasOracleAction
  :: Party
  -> Action
  -> [String]
hasOracleAction oracleParty (Choice (ChoiceId symbol choiceParty) _) =
  pureIf (oracleParty == choiceParty)
    . BS8.unpack
    $ fromBuiltin symbol
hasOracleAction _ _ = mempty


contractReadyForOracle
  :: Party
  -> Contract
  -> [String]
contractReadyForOracle = (. extract) . foldMap . hasOracleAction


containsOracleAction
  :: Party
  -> Contract
  -> [String]
containsOracleAction = (. extractAll) . foldMap . hasOracleAction


oraclePresent
  :: Party
  -> ContractStream 'V1
  -> [Oracle]
oraclePresent party =
  mapMaybe toOracleSymbol
    . concatMap (containsOracleAction party)
    . maybeToList
    . contractFromStream


oracleReady
  :: Party
  -> ContractStream 'V1
  -> [Oracle]
oracleReady party =
  mapMaybe toOracleSymbol
    . concatMap (contractReadyForOracle party)
    . maybeToList
    . contractFromStream
