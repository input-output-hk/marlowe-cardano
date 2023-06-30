{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Oracle.Detect (
  containsOracleAction,
  contractReadyForOracle,
  hasOracleAction,
  oraclePresent,
  oracleReady,
  OracleRequest (..),
  choiceName',
) where

import Data.Maybe (mapMaybe, maybeToList)
import Language.Marlowe.Core.V1.Plate (extractAll)
import Language.Marlowe.Core.V1.Semantics.Types (Action (Choice), Case (..), ChoiceId (..), Contract (When), Party)
import Language.Marlowe.Oracle.Types (OracleRequest (..), choiceName')
import Language.Marlowe.Runtime.App.Stream (ContractStream (..), contractFromStream)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag (V1))
import Network.Oracle (Oracle, toOracleSymbol)

hasOracleAction
  :: Party
  -> Case Contract
  -> [OracleRequest]
hasOracleAction oracleParty (Case (Choice (ChoiceId choiceName choiceParty) bounds) continuation')
  | oracleParty == choiceParty = let continuation = Right continuation' in pure OracleRequest{..}
hasOracleAction oracleParty (MerkleizedCase (Choice (ChoiceId choiceName choiceParty) bounds) continuation')
  | oracleParty == choiceParty = let continuation = Left continuation' in pure OracleRequest{..}
hasOracleAction _ _ = mempty

contractReadyForOracle
  :: Party
  -> Contract
  -> [OracleRequest]
contractReadyForOracle party (When cs _ _) = concatMap (hasOracleAction party) cs
contractReadyForOracle _ _ = mempty

containsOracleAction
  :: Party
  -> Contract
  -> [OracleRequest]
containsOracleAction = (. extractAll) . foldMap . hasOracleAction

toOracleSymbol'
  :: OracleRequest
  -> Maybe (Oracle, OracleRequest)
toOracleSymbol' oracleRequest =
  fmap (,oracleRequest)
    . toOracleSymbol
    $ choiceName' oracleRequest

oraclePresent
  :: Party
  -> ContractStream 'V1
  -> [(Oracle, OracleRequest)]
oraclePresent party =
  mapMaybe toOracleSymbol'
    . concatMap (containsOracleAction party)
    . maybeToList
    . contractFromStream

oracleReady
  :: Party
  -> ContractStream 'V1
  -> [(Oracle, OracleRequest)]
oracleReady party =
  mapMaybe toOracleSymbol'
    . concatMap (contractReadyForOracle party)
    . maybeToList
    . contractFromStream
