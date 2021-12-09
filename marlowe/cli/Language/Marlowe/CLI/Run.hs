-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Run Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Run (
-- * Computation
  computeMarlowe
) where


import           Cardano.Api                  (SlotNo (..))
import           Control.Monad                (unless, when)
import           Control.Monad.Except         (MonadError, MonadIO, liftIO, throwError)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Language.Marlowe.CLI.Export  (buildDatum)
import           Language.Marlowe.CLI.IO      (decodeFileStrict)
import           Language.Marlowe.CLI.Orphans ()
import           Language.Marlowe.CLI.Types   (CliError (..), DatumInfo (..))
import           Language.Marlowe.Semantics   (Payment (..), TransactionInput (..), TransactionOutput (..),
                                               computeTransaction)
import           Plutus.V1.Ledger.Ada         (adaSymbol, fromValue, getAda)
import           Plutus.V1.Ledger.Value       (Value (..))
import           Prettyprinter.Extras         (Pretty (..))
import           System.IO                    (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy         as LBS (writeFile)
import qualified PlutusTx.AssocMap            as AM (toList)


-- | Compute the next step in a Marlowe contract.
computeMarlowe :: MonadError CliError m
               => MonadIO m
               => FilePath    -- ^ The JSON file containing the contract.
               -> FilePath    -- ^ The JSON file containing the contract's state.
               -> [FilePath]  -- ^ The JSON files containing the contract's inputs.
               -> SlotNo      -- ^ The first valid slot for the transaction.
               -> SlotNo      -- ^ The last valid slot for the transaction.
               -> FilePath    -- ^ The output JSON file with the results of the computation.
               -> Bool        -- ^ Whether to print statistics about the result.
               -> m ()        -- ^ Action to compute the next step in the contract.
computeMarlowe contractFile stateFile inputFiles (SlotNo minimumSlot) (SlotNo maximumSlot) outputFile printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    txInputs <- mapM decodeFileStrict inputFiles
    let
      txInterval = (fromIntegral minimumSlot, fromIntegral maximumSlot)
    case computeTransaction TransactionInput{..} state contract of
      Error message -> throwError . CliError . show $ message
      transactionOutput@TransactionOutput{..}
        -> liftIO
             $ do
               LBS.writeFile outputFile
                 $ encodePretty transactionOutput
               when printStats
                 $ do
                   hPutStrLn stderr ""
                   unless (null txOutWarnings)
                     $ do
                       hPutStrLn stderr "Warnings:"
                       sequence_
                         [
                           hPutStrLn stderr $ "  " <> show warning
                         |
                           warning <- txOutWarnings
                         ]
                   sequence_
                     [
                       do
                         hPutStrLn stderr $ "Payment " <> show (i :: Int)
                         hPutStrLn stderr $ "  Acccount: " <> show accountId
                         hPutStrLn stderr $ "  Payee: " <> show payee
                         hPutStrLn stderr $ "  Ada: " <> show (getAda $ fromValue money)
                         sequence_
                           [
                             hPutStrLn stderr $ "  " <> show (pretty symbol) <> "." <> show (pretty token) <> ": " <> show amount
                           |
                             (symbol, tokenAmounts) <- AM.toList $ getValue money
                           , symbol /= adaSymbol
                           , (token, amount) <- AM.toList tokenAmounts
                           ]
                     |
                       (i, Payment accountId payee money) <- zip [1..] txOutPayments
                     ]
                   hPutStrLn stderr $ "Datum size: " ++ show (diSize $ buildDatum txOutContract txOutState)
