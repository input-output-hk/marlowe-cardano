

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}


module Language.Marlowe.Oracle
  ( containsOracleAction
  , contractReadyForOracle
  , hasOracleAction
  , oraclePresent
  , oracleReady
  ) where


import Control.Monad.Extra (pureIf)
import Data.Maybe (mapMaybe)
import Language.Marlowe.Core.V1.Plate (extractAll)
import Language.Marlowe.Core.V1.Semantics (MarloweData(MarloweData, marloweContract))
import Language.Marlowe.Core.V1.Semantics.Types (Action(Choice), ChoiceId(..), Contract(When), Party, getAction)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..))
import Language.Marlowe.Runtime.Core.Api
  ( MarloweVersionTag(V1)
  , Transaction(Transaction, output)
  , TransactionOutput(TransactionOutput, scriptOutput)
  , TransactionScriptOutput(TransactionScriptOutput, datum)
  )
import Language.Marlowe.Runtime.History.Api (ContractStep(ApplyTransaction), CreateStep(CreateStep, createOutput))
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
contractReadyForOracle party (When cases _ _) = foldMap (hasOracleAction party . getAction) cases
contractReadyForOracle _ _ = mempty


containsOracleAction
  :: Party
  -> Contract
  -> [String]
containsOracleAction = (. extractAll) . foldMap . hasOracleAction


oraclePresent
  :: Party
  -> ContractStream 'V1
  -> [Oracle]
oraclePresent party ContractStreamStart{csCreateStep=CreateStep{createOutput=TransactionScriptOutput{datum=MarloweData{marloweContract}}}} =
  mapMaybe toOracleSymbol
    $ containsOracleAction party marloweContract
oraclePresent party ContractStreamContinued{csContractStep=ApplyTransaction Transaction{output=TransactionOutput{scriptOutput=Just TransactionScriptOutput{datum=MarloweData{marloweContract}}}}} =
  mapMaybe toOracleSymbol
    $ containsOracleAction party marloweContract
oraclePresent _ _ = mempty


oracleReady
  :: Party
  -> ContractStream 'V1
  -> [Oracle]
oracleReady party ContractStreamStart{csCreateStep=CreateStep{createOutput=TransactionScriptOutput{datum=MarloweData{marloweContract}}}} =
  mapMaybe toOracleSymbol
    $ contractReadyForOracle party marloweContract
oracleReady party ContractStreamContinued{csContractStep=ApplyTransaction Transaction{output=TransactionOutput{scriptOutput=Just TransactionScriptOutput{datum=MarloweData{marloweContract}}}}} =
  mapMaybe toOracleSymbol
    $ contractReadyForOracle party marloweContract
oracleReady _ _ = mempty
