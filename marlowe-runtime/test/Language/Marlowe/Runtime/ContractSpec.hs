{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.ContractSpec
  where

import Control.Concurrent.Component
import Control.Monad (join, void)
import Control.Monad.Event.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Default (def)
import Data.List (sort)
import Language.Marlowe (Action(..), Case(..), Contract(..), Observation(..))
import Language.Marlowe.Protocol.Load.Client
  (MarloweLoadClient, hoistMarloweLoadClient, marloweLoadClientPeer, pushContract)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer)
import Language.Marlowe.Runtime.Contract
import Language.Marlowe.Runtime.Contract.Store (hoistContractStore)
import Language.Marlowe.Runtime.Contract.Store.File
import Network.Protocol.Connection
  ( ClientServerPair(clientConnector)
  , SomeClientConnectorTraced
  , SomeConnectionSourceTraced(..)
  , SomeConnectorTraced(SomeConnectorTraced)
  , clientServerPair
  , connectionSource
  )
import Network.Protocol.Driver.Trace (HasSpanContext(..), runConnectorTraced)
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.TypedProtocol (unsafeIntToNat)
import System.IO.LockFile (LockingParameters(..), withLockFile)
import Test.Hspec
import Test.Integration.Workspace (createWorkspace, resolveWorkspacePath)
import UnliftIO (atomically, liftIO, race_)
import UnliftIO.Directory (listDirectory)

spec :: Spec
spec = around setup do
  loadServerSpec

loadServerSpec :: SpecWith MarloweLoadHandle
loadServerSpec = describe "LoadServer" do
  it "Does not commit merkleized contracts to the store" $ runTest do
    Nothing <- runLoadClient $ pushContract $ When
      [ MerkleizedCase (Notify TrueObs) ""
      ]
      0
      Close
    files <- NoopEventT $ listDirectory =<< asks (contractStoreDirectory . contractStoreOptions)
    liftIO $ files `shouldBe` mempty
  it "Does not commit close to the store" $ runTest do
    _ <- runLoadClient $ pushContract Close
    files <- NoopEventT $ listDirectory =<< asks (contractStoreDirectory . contractStoreOptions)
    liftIO $ files `shouldBe` mempty
  it "Commits non-merkleized, non-close contracts to the store" $ runTest do
    let c = Assert FalseObs Close
    Just hash <- runLoadClient $ pushContract c
    storeDir <- NoopEventT $ asks (contractStoreDirectory . contractStoreOptions)
    let lockingParameters = def { sleepBetweenRetries = 1000 } --1 ms
    files <- liftIO $ withLockFile lockingParameters (storeDir <> "/" <> "lockfile") $ listDirectory storeDir
    let expectedFileNames = mappend (read $ show hash) . mappend "." <$> ["adjacency", "closure", "contract"]
    liftIO $ sort (filter (/= "lockfile") files) `shouldBe` sort expectedFileNames

  -- TODO add more tests when the store supports queries (otherwise we're
  -- testing the internals and that's not great).

runLoadClient :: MarloweLoadClient Test a -> Test a
runLoadClient client = NoopEventT $ ReaderT \handle -> runNoopEventT
  case loadConnector handle of
    SomeConnectorTraced _ connector ->
      runConnectorTraced inject connector
        $ hoistMarloweLoadClient (NoopEventT . flip runReaderT handle . runNoopEventT) client

runTest :: Test () -> ActionWith MarloweLoadHandle
runTest = runReaderT . runNoopEventT

setup :: ActionWith MarloweLoadHandle -> IO ()
setup test = runResourceT do
  workspace <- createWorkspace "marlowe-contract-test"
  let
    contractStoreOptions = ContractStoreOptions
      { contractStoreDirectory = resolveWorkspacePath workspace "store"
      , contractStoreStagingDirectory = resolveWorkspacePath workspace "staging"
      }
  contractStore <- liftIO $ createContractStore contractStoreOptions
  liftIO $ join $ atomically do
    loadPair <- clientServerPair marloweLoadServerPeer marloweLoadClientPeer
    pure $ race_
      ( runNoopEventT $ runComponent_ (void contract) ContractDependencies
          { batchSize = unsafeIntToNat 1024
          , contractStore = hoistContractStore liftIO contractStore
          , loadSource = SomeConnectionSourceTraced inject $ connectionSource loadPair
          }
      )
      ( test MarloweLoadHandle
          { contractStoreOptions
          , loadConnector = SomeConnectorTraced inject $ clientConnector loadPair
          }
      )

data TestDependencies = TestDependencies
  { loadDependencies :: ContractDependencies TestRef TestSelector (NoopEventT TestRef TestSelector IO)
  , marloweLoadHandle :: MarloweLoadHandle
  }

data TestSelector f where
  TestSelector :: s f -> TestSelector f

instance Inject s TestSelector where
  inject s withInjField = withInjField (TestSelector s) id

data TestRef = TestRef

instance Semigroup TestRef where
  (<>) = const id

instance Monoid TestRef where
  mempty = TestRef

instance HasSpanContext TestRef where
  context = const $ pure defaultSpanContext
  wrapContext = const TestRef

data MarloweLoadHandle = MarloweLoadHandle
  { contractStoreOptions :: ContractStoreOptions
  , loadConnector :: SomeClientConnectorTraced MarloweLoadClient TestRef TestSelector (NoopEventT TestRef TestSelector IO)
  }

type Test = NoopEventT TestRef TestSelector (ReaderT MarloweLoadHandle IO)
