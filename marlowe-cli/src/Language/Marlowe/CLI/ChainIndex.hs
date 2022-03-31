-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Chain-index queries.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.ChainIndex (
-- * Queries
  queryApp
, queryPayout
, queryHistory
, queryAddress
, queryTransaction
, queryOutput
) where


import Cardano.Api (AddressAny, AddressInEra, ShelleyEra, TxId, anyAddressInShelleyBasedEra, lovelaceToValue,
                    valueFromList)
import Control.Applicative (liftA2)
import Control.Lens ((^.), (^..))
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.IO (liftCli, maybeWriteJson)
import Language.Marlowe.CLI.Types (CliError (..), OutputQuery (..))
import Language.Marlowe.Client.History (histories)
import Language.Marlowe.Scripts (smallUntypedValidator)
import Language.Marlowe.Semantics (MarloweData (..), MarloweParams (..))
import Ledger (ciTxOutDatum, ciTxOutValue)
import Ledger.Scripts (validatorHash)
import Ledger.Tx.CardanoAPI (fromCardanoAddress, fromCardanoTxId, fromCardanoValue)
import Ledger.Typed.Scripts (validatorAddress, validatorScript)
import Plutus.ChainIndex (Page (..))
import Plutus.ChainIndex.Api (TxoAtAddressRequest (..), UtxoAtAddressRequest (..), page, paget)
import Plutus.ChainIndex.Client (getTx, getTxOut, getTxoSetAtAddress, getUtxoSetAtAddress)
import Plutus.V1.Ledger.Api (Address (..), Credential (..), CurrencySymbol, Datum (..), FromData, TxOutRef, Value,
                             fromBuiltinData, txOutRefId)
import Servant.Client (ClientM)

import qualified Plutus.V1.Ledger.Value as V (flattenValue, geq)


-- | Output of a Marlowe transaction.
data TxOutMarlowe a =
  TxOutMarlowe
  {
    txOutRef     :: TxOutRef  -- ^ The transaction output.
  , txOutValue   :: Value     -- ^ The value output.
  , marloweDatum :: Maybe a   -- ^ The datum, if any.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | Extract Marlowe information from a transaction output.
marloweFromTxOutRef :: FromData a
                    => TxOutRef                  -- ^ The transaction output.
                    -> ClientM (TxOutMarlowe a)  -- ^ Action for extracting the information.
marloweFromTxOutRef txOutRef =
  do
    txOut <- getTxOut txOutRef
    let
      txOutValue = txOut ^. ciTxOutValue
      marloweDatum =
        case txOut ^.. ciTxOutDatum of
          [Right (Datum datum)] -> fromBuiltinData datum
          _                     -> Nothing
    pure TxOutMarlowe{..}


-- | Query transactions at a credential.
queryCredential :: (TxOutRef -> ClientM a)  -- ^ Query relevant information from transaction output.
                -> Bool                     -- ^ Whether to also query spent output.
                -> Credential               -- ^ The payment credential to query.
                -> ClientM [a]              -- ^ Action to query the credential.
queryCredential f spent credential =
  let
    getSet query =
      if spent
        then fmap paget . getTxoSetAtAddress  $ TxoAtAddressRequest  query credential
        else fmap page  . getUtxoSetAtAddress $ UtxoAtAddressRequest query credential
    go thisPageQuery =
      do
        Page{..} <- getSet thisPageQuery
        items <- mapM f pageItems
        (items <>) <$> maybe (pure []) (go . Just) nextPageQuery
  in
    go Nothing


-- | Query Marlowe output of a script.
queryScript :: FromData a
            => Bool                      -- ^ Whether to also query spent output.
            -> Credential                -- ^ The payment credential to query.
            -> ClientM [TxOutMarlowe a]  -- ^ Action for querying the Marlowe information.
queryScript = queryCredential marloweFromTxOutRef


-- | Query state of the Marlowe application script.
queryApp :: MonadError CliError m
         => MonadIO m
         => (forall a. ClientM a -> m a)  -- ^ The chain-index API runner.
         -> MarloweParams                 -- ^ The Marlowe parameters.
         -> Bool                          -- ^ Whether to also query spent output.
         -> Maybe FilePath                -- ^ The output path for the state, if any.
         -> m ()                          -- ^ Action to query the state.
queryApp runApi params spent outputFile =
  do
    let
      credential = ScriptCredential . validatorHash . validatorScript $ smallUntypedValidator params
    result <- runApi $ queryScript spent credential
    maybeWriteJson outputFile (result :: [TxOutMarlowe MarloweData])


-- | Query state of the Marlowe payout script.
queryPayout :: MonadError CliError m
            => MonadIO m
            => (forall a. ClientM a -> m a)  -- ^ The chain-index API runner.
            -> MarloweParams                 -- ^ The Marlowe parameters.
            -> Bool                          -- ^ Whether to also query spent output.
            -> Maybe FilePath                -- ^ The output path for the state, if any.
            -> m ()                          -- ^ Action to query the state.
queryPayout runApi MarloweParams{..} spent outputFile =
  do
    let
      credential = ScriptCredential rolePayoutValidatorHash
    result <- runApi $ queryScript spent credential
    maybeWriteJson outputFile (result :: [TxOutMarlowe CurrencySymbol])


-- | Query the contract history for a Marlowe application script.
queryHistory :: MonadError CliError m
             => MonadIO m
             => (forall b. ClientM b -> m b)  -- ^ The chain-index API runner.
             -> MarloweParams                 -- ^ The Marlowe parameters.
             -> Maybe FilePath                -- ^ The output path for the history, if any.
             -> m ()                          -- ^ Action to query the history.
queryHistory runApi params@MarloweParams{..} outputFile =
  do
    let
      address = validatorAddress $ smallUntypedValidator params
      appCredential = ScriptCredential . validatorHash . validatorScript $ smallUntypedValidator params
      payCredential = ScriptCredential rolePayoutValidatorHash
      query = queryCredential (getTx . txOutRefId) True
    txs <-
      runApi
        $ do
          appTxs <- query appCredential
          payTxs <- query payCredential
          pure . nub $ appTxs <> payTxs
    maybeWriteJson outputFile
      $ histories params address txs


-- | Query the transactions at addresses.
queryAddress :: MonadError CliError m
             => MonadIO m
             => (forall b. ClientM b -> m b)  -- ^ The chain-index API runner.
             -> [AddressAny]                  -- ^ The addresses.
             -> Bool                          -- ^ Whether to also query spent output.
             -> Maybe FilePath                -- ^ The output path for the transactions, if any.
             -> m ()                          -- ^ Action to query the address.
queryAddress runApi addresses spent outputFile =
  do
    let
      toCredential address =
        do
          Address credential _  <-
            liftCli
              . fromCardanoAddress
              $ (anyAddressInShelleyBasedEra :: AddressAny -> AddressInEra ShelleyEra) address
          pure credential
    credentials <- toCredential `mapM` addresses
    txs <-
      runApi
        . fmap (nub . concat)
        $ mapM (queryCredential (getTx . txOutRefId) spent) credentials
    maybeWriteJson outputFile txs


-- | Query the transaction outputs at addresses.
queryOutput :: MonadError CliError m
            => MonadIO m
            => (forall b. ClientM b -> m b)  -- ^ The chain-index API runner.
            -> [AddressAny]                  -- ^ The addresses.
            -> OutputQuery                   -- ^ Filter for the results.
            -> Bool                          -- ^ Whether to also query spent output.
            -> Maybe FilePath                -- ^ The output path for the transactions, if any.
            -> m ()                          -- ^ Action to query the address.
queryOutput runApi addresses query spent outputFile =
  do
    let
      toCredential address =
        do
          Address credential _  <-
            liftCli
              . fromCardanoAddress
              $ (anyAddressInShelleyBasedEra :: AddressAny -> AddressInEra ShelleyEra) address
          pure credential
      query' (_, txOut) =
        let
          value = txOut ^. ciTxOutValue
          count = length $ V.flattenValue value
        in
          case query of
            AllOutput        -> True
            LovelaceOnly{..} -> count == 1 && value `V.geq` fromCardanoValue (lovelaceToValue lovelace)
            AssetOnly{..}    -> count == 2 && value `V.geq` fromCardanoValue (valueFromList [(asset, 1)])
    credentials <- toCredential `mapM` addresses
    txOuts <-
      runApi
        . fmap (nub . concat)
        $ mapM (queryCredential (liftA2 fmap (,) getTxOut) spent) credentials
    maybeWriteJson outputFile
      $ filter query' txOuts


-- | Query the details of transactions.
queryTransaction :: MonadError CliError m
                 => MonadIO m
                 => (forall b. ClientM b -> m b)  -- ^ The chain-index API runner.
                 -> [TxId]                        -- ^ The transaction IDs.
                 -> Maybe FilePath                -- ^ The output path for the transaction, if any.
                 -> m ()                          -- ^ Action to query the transaction.
queryTransaction runApi txIds outputFile =
  do
    txs <- runApi $ mapM (getTx . fromCardanoTxId) txIds
    maybeWriteJson outputFile txs
