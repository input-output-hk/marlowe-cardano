-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Example contracts.
--
-----------------------------------------------------------------------------


module Language.Marlowe.CLI.Examples (
-- * Contracts
  makeExample
) where


import           Control.Monad.Except            (MonadIO, liftIO)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Language.Marlowe.SemanticsTypes (Contract, State)

import qualified Data.ByteString.Lazy            as LBS (writeFile)


-- | Serialise an example contract to JSON.
makeExample :: MonadIO m
            => FilePath           -- ^ The output JSON file for the Marlowe contract.
            -> FilePath           -- ^ The output JSON file for the Marlowe contract's state.
            -> (Contract, State)  -- ^ The contract and state data.
            -> m ()               -- ^ Action to serialise the Marlowe data.
makeExample contractFile stateFile (marloweContract, marloweState) =
  liftIO
    $ do
      LBS.writeFile contractFile . encodePretty $ marloweContract
      LBS.writeFile stateFile    . encodePretty $ marloweState
