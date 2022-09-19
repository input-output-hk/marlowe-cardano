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




module Language.Marlowe.CLI.Examples
  ( -- * Contracts
    makeExample
  ) where


import Control.Monad.Except (MonadIO, liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy as LBS (writeFile)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State)


-- | Serialise an example contract to JSON.
makeExample :: MonadIO m
            => FilePath           -- ^ The output JSON file for the Marlowe contract.
            -> FilePath           -- ^ The output JSON file for the Marlowe contract's state.
            -> (Contract, State)  -- ^ The contract and state data.
            -> m ()               -- ^ Action to serialise the Marlowe data.
makeExample contractFile stateFile (contract, state) =
  liftIO
    $ do
      LBS.writeFile contractFile . encodePretty $ contract
      LBS.writeFile stateFile    . encodePretty $ state
