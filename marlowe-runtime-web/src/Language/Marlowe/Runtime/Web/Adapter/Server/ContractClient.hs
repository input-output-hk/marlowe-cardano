{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Adapter.Server.ContractClient where

import Control.Arrow (arr)
import Control.Concurrent.Component (Component)
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
import Language.Marlowe.Runtime.Client.Transfer (BundlePart (..))
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency)
import qualified Language.Marlowe.Runtime.Contract.Api as Contract
import Network.Protocol.Connection (Connector, runConnector)
import Pipes (Pipe, await, yield, (>->))
import Unsafe.Coerce (unsafeCoerce)

newtype ContractClientDependencies m = ContractClientDependencies
  { connector :: Connector MarloweRuntimeClient m
  }

-- | Signature for a delegate that imports a bundle into the runtime.
type ImportBundle m = Label -> Pipe ObjectBundle (Map Label DatumHash) m (Either ImportError (Map Label DatumHash))

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
        watchForMain main
          >-> runClientStreaming
            hoistMarloweRuntimeClient
            (runConnector connector)
            (RunMarloweTransferClient importIncremental)
    , getContract = runConnector connector . RunContractQueryClient . Contract.getContract
    }

watchForMain :: (Monad m) => Label -> Pipe ObjectBundle BundlePart m (Either ImportError (Map Label DatumHash))
watchForMain main = do
  ObjectBundle bundle <- await
  case find ((main ==) . _label) bundle of
    Nothing -> do
      yield $ IntermediatePart $ ObjectBundle bundle
      watchForMain main
    Just (LabelledObject _ ContractType _) -> do
      yield $ FinalPart $ ObjectBundle bundle
      pure $ Right mempty
    Just (LabelledObject _ t _) ->
      pure $
        Left $
          LinkError $
            TypeMismatch
              (UnsafeSomeObjectType $ unsafeCoerce ContractType)
              (UnsafeSomeObjectType $ unsafeCoerce t)
