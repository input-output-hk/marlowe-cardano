{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.ContractClient where

import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Marlowe.Class (runClientStreaming)
import Data.List (find)
import Data.Map (Map)
import Language.Marlowe.Object.Link (LinkError (TypeMismatch))
import Language.Marlowe.Object.Types (
  Label,
  LabelledObject (..),
  ObjectBundle (..),
  ObjectType (..),
  SomeObjectType (..),
 )
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient (..), hoistMarloweRuntimeClient)
import Language.Marlowe.Protocol.Transfer.Types (ImportError (..))
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Client (importIncremental)
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency)
import qualified Language.Marlowe.Runtime.Contract.Api as Contract
import Network.Protocol.Connection (Connector, runConnector)
import Pipes (MFunctor (..), Pipe, await, yield, (>->))
import Unsafe.Coerce (unsafeCoerce)

newtype ContractClientDependencies m = ContractClientDependencies
  { connector :: Connector MarloweRuntimeClient m
  }

-- | Signature for a delegate that imports a bundle into the runtime.
type ImportBundle m = Label -> Pipe ObjectBundle (Map Label DatumHash) (ExceptT ImportError m) ()

-- | Signature for a delegate that gets a contract from the runtime.
type GetContract m = DatumHash -> m (Maybe ContractWithAdjacency)

-- | Public API of the ContractClient
data ContractClient m = ContractClient
  { importBundle :: ImportBundle m
  , getContract :: GetContract m
  }

contractClient :: (MonadUnliftIO m) => Component m (ContractClientDependencies m) (ContractClient m)
contractClient = arr \ContractClientDependencies{..} ->
  ContractClient
    { importBundle = \main ->
        watchForMain main >-> do
          result <-
            hoist lift $
              runClientStreaming
                hoistMarloweRuntimeClient
                (runConnector connector)
                (RunMarloweTransferClient importIncremental)
          case result of
            Nothing -> pure ()
            Just err -> throwError err
    , getContract = runConnector connector . RunContractQueryClient . Contract.getContract
    }

watchForMain :: (Monad m) => Label -> Pipe ObjectBundle ObjectBundle (ExceptT ImportError m) ()
watchForMain main = do
  ObjectBundle bundle <- await
  yield $ ObjectBundle bundle
  case find ((main ==) . _label) bundle of
    Nothing -> watchForMain main
    Just (LabelledObject _ ContractType _) ->
      yield $ ObjectBundle [] -- send an empty bundle to indicate the end.
    Just (LabelledObject _ t _) ->
      throwError $
        LinkError $
          TypeMismatch
            (UnsafeSomeObjectType $ unsafeCoerce ContractType)
            (UnsafeSomeObjectType $ unsafeCoerce t)
