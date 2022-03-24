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
  queryApp
, queryPayout
) where


import Control.Lens ((^.), (^..))
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.IO (maybeWriteJson)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.Scripts (smallUntypedValidator)
import Language.Marlowe.Semantics (MarloweData (..), MarloweParams (..))
import Ledger (ciTxOutDatum, ciTxOutValue)
import Ledger.Scripts (validatorHash)
import Ledger.Typed.Scripts (validatorScript)
import Plutus.ChainIndex (Page (..))
import Plutus.ChainIndex.Api (TxoAtAddressRequest (..), UtxoAtAddressRequest (..), page, paget)
import Plutus.ChainIndex.Client (getTxOut, getTxoSetAtAddress, getUtxoSetAtAddress)
import Plutus.V1.Ledger.Api (Credential (..), CurrencySymbol, Datum (..), FromData, TxOutRef, Value, fromBuiltinData)
import Servant.Client (ClientM)


data TxOutMarlowe a =
  TxOutMarlowe
  {
    txOutRef     :: TxOutRef
  , txOutValue   :: Value
  , marloweDatum :: Maybe a
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


marloweFromTxOutRef :: FromData a
                    => TxOutRef
                    -> ClientM (TxOutMarlowe a)
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


queryScript :: FromData a
            => (forall b. ClientM b -> m b)
            -> Credential
            -> Bool
            -> m [TxOutMarlowe a]
queryScript runApi credential spent =
  do
    let
      getSet query =
        if spent
          then fmap paget . getTxoSetAtAddress  $ TxoAtAddressRequest  query credential
          else fmap page  . getUtxoSetAtAddress $ UtxoAtAddressRequest query credential
      go thisPageQuery =
        do
          Page{..} <- getSet thisPageQuery
          items <- mapM marloweFromTxOutRef pageItems
          (items <>) <$> maybe (pure []) (go . Just) nextPageQuery
    runApi (go Nothing)


queryApp :: MonadError CliError m
         => MonadIO m
         => (forall a. ClientM a -> m a)
         -> MarloweParams
         -> Bool
         -> Maybe FilePath
         -> m ()
queryApp runApi params spent outputFile =
  do
    let
      credential = ScriptCredential . validatorHash . validatorScript $ smallUntypedValidator params
    result <- queryScript runApi credential spent
    maybeWriteJson outputFile (result :: [TxOutMarlowe MarloweData])


queryPayout :: MonadError CliError m
            => MonadIO m
            => (forall a. ClientM a -> m a)
            -> MarloweParams
            -> Bool
            -> Maybe FilePath
            -> m ()
queryPayout runApi MarloweParams{..} spent outputFile =
  do
    let
      credential = ScriptCredential rolePayoutValidatorHash
    result <- queryScript runApi credential spent
    maybeWriteJson outputFile (result :: [TxOutMarlowe CurrencySymbol])
