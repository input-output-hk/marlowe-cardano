{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Integration.Contract
  where

import Cardano.Api.Byron (ScriptData(ScriptDataBytes), hashScriptData)
import Control.Concurrent.Component
import Control.Monad (foldM)
import Control.Monad.Event.Class (Inject(..), NoopEventT(runNoopEventT))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Writer (execWriter, runWriter)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Merkle (deepMerkleize)
import Language.Marlowe.Core.V1.Plate (extractAll)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Client (MarloweLoadClient, marloweLoadClientPeer, pushContract)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Language.Marlowe.Runtime.Contract
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency(adjacency))
import qualified Language.Marlowe.Runtime.Contract.Api as Api
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions(..), createContractStore)
import Network.Protocol.Connection
import Network.Protocol.Driver.Trace (HasSpanContext(..), runSomeConnectorTraced)
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Protocol.Query.Client (QueryClient, queryClientPeer)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.TypedProtocol (unsafeIntToNat)
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Hspec
import Test.Integration.Marlowe (createWorkspace, resolveWorkspacePath)
import UnliftIO (atomically, liftIO, race_)

spec :: Spec
spec = focus $ parallel $ describe "MarloweContract" do
  getContractSpec

getContractSpec :: Spec
getContractSpec = describe "getContract" do
  it "Always finds Close" $ runContractTest do
    let contractHash = hashContract Close
    result <- runQuery $ Api.getContract contractHash
    liftIO $ result `shouldBe` Just Api.ContractWithAdjacency
      { contract = Close
      , contractHash
      , adjacency = mempty
      , closure = Set.singleton contractHash
      }

  it "Fails to find a contract when not in the store" $ runContractTest do
    result <- runQuery $ Api.getContract $ fromCardanoDatumHash $ hashScriptData $ ScriptDataBytes ""
    liftIO $ result `shouldBe` Nothing

  it "Finds a contract when in the store" $ runContractTest do
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract $ testContract 2
    result <- runQuery $ Api.getContract hash
    liftIO $ Api.contractHash <$> result `shouldBe` Just hash

  it "Returns the correct contract for the given hash" $ runContractTest do
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract $ testContract 2
    result <- runQuery $ Api.getContract hash
    liftIO $ hashContract . Api.contract <$> result `shouldBe` Api.contractHash <$> result

  it "Correctly merkleizes the contract" $ runContractTest do
    let initialContract = testContract 2
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract initialContract
    result <- runQuery $ Api.getContract hash
    let expectedContract = fst $ runWriter $ deepMerkleize initialContract
    liftIO $ Api.contract <$> result `shouldBe` Just expectedContract

  it "Returns the correct closure" $ runContractTest do
    let initialContract = testContract 2
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract initialContract
    result <- runQuery $ Api.getContract hash
    let continuations = snd $ runWriter $ deepMerkleize initialContract
    let expectedClosure = Set.insert hash $ Set.map fromPlutusDatumHash $ Map.keysSet continuations
    liftIO $ foldMap Api.closure result `shouldBe` expectedClosure

  it "Returns the correct closure" $ runContractTest do
    let initialContract = testContract 4
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract initialContract
    result <- runQuery $ Api.getContract hash
    let continuations = execWriter $ deepMerkleize initialContract
    let expectedClosure = Set.insert hash $ Set.map fromPlutusDatumHash $ Map.keysSet continuations
    liftIO $ foldMap Api.closure result `shouldBe` expectedClosure

  it "Returns the correct adjacency" $ runContractTest do
    let initialContract = testContract 4
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract initialContract
    result <- expectJust "failed to get contract" $ runQuery $ Api.getContract hash
    let expectedAdjacency = foldMap extractContinuationHash $ extractAll $ Api.contract result
    liftIO $ Api.adjacency result `shouldBe` expectedAdjacency

  it "Is possible to build the correct continuation map with the closure" $ runContractTest do
    let initialContract = testContract 4
    hash <- expectJust "failed to push contract" $ runLoad $ pushContract initialContract
    Api.ContractWithAdjacency{closure} <- expectJust "failed to get contract" $ runQuery $ Api.getContract hash
    let closureWithoutRoot = Set.delete hash closure
    let expectedContinuations = execWriter $ deepMerkleize initialContract
    let
      insertContinuation continuations hash' = do
        Api.ContractWithAdjacency{contractHash, contract = contract'} <-
          expectJust ("failed to get contract " <> show hash') $ runQuery $ Api.getContract hash'
        pure $ Map.insert (toPlutusDatumHash contractHash) contract' continuations
    actualContinuations <- foldM insertContinuation mempty closureWithoutRoot
    liftIO $ actualContinuations `shouldBe` expectedContinuations

extractContinuationHash :: Case Contract -> Set.Set DatumHash
extractContinuationHash = \case
  MerkleizedCase _ hash -> Set.singleton $ fromPlutusDatumHash $ PV2.DatumHash hash
  _ -> mempty

toPlutusDatumHash :: DatumHash -> PV2.DatumHash
toPlutusDatumHash = PV2.DatumHash . PV2.toBuiltin . unDatumHash

fromPlutusDatumHash :: PV2.DatumHash -> DatumHash
fromPlutusDatumHash (PV2.DatumHash hash) = DatumHash $ PV2.fromBuiltin hash

hashContract :: Contract -> DatumHash
hashContract = fromCardanoDatumHash . hashScriptData . toCardanoScriptData . toDatum

testContract :: Int -> Contract
testContract size
  | size <= 0 = Close
  | otherwise = When cases 0 Close
  where
    cases = replicate size $ Case (Notify FalseObs) $ testContract (size - 1)

expectJust :: String -> TestM (Maybe a) -> TestM a
expectJust msg m = m >>= \case
  Nothing -> fail msg
  Just a -> pure a

-- helpers

runLoad :: MarloweLoadClient TestM a -> TestM a
runLoad client = do
  TestHandle {..} <- ask
  runSomeConnectorTraced loadConnector client

runQuery :: QueryClient Api.ContractRequest TestM a -> TestM a
runQuery client = do
  TestHandle {..} <- ask
  runSomeConnectorTraced queryConnector client

-- test plumbing

runContractTest :: TestM () -> IO ()
runContractTest test = runResourceT do
  loadPair <- atomically $ clientServerPair marloweLoadServerPeer marloweLoadClientPeer
  queryPair <- atomically $ clientServerPair queryServerPeer queryClientPeer
  workspace <- createWorkspace "marlowe-contract-test"
  contractStore <- createContractStore ContractStoreOptions
    { contractStoreDirectory = resolveWorkspacePath workspace "contract-store"
    , contractStoreStagingDirectory = resolveWorkspacePath workspace "staging-areas"
    , lockingSleepBetweenRetries = 100_000
    }
  let
    testHandle = TestHandle
      { loadConnector = SomeConnectorTraced inject $ clientConnector loadPair
      , queryConnector = SomeConnectorTraced inject $ clientConnector queryPair
      }
  runNoopEventT $ flip runReaderT testHandle $ race_ test $ runComponent_ (void contract) ContractDependencies
    { batchSize = unsafeIntToNat 10
    , contractStore
    , loadSource = SomeConnectionSourceTraced inject $ connectionSource loadPair
    , querySource = SomeConnectionSourceTraced inject $ connectionSource queryPair
    }

type TestM = ReaderT TestHandle (NoopEventT TestRef AnySelector (ResourceT IO))

data TestHandle = TestHandle
  { loadConnector :: SomeClientConnectorTraced MarloweLoadClient TestRef AnySelector TestM
  , queryConnector :: SomeClientConnectorTraced (QueryClient Api.ContractRequest) TestRef AnySelector TestM
  }

newtype TestRef = TestRef ()
  deriving newtype (Semigroup, Monoid)

instance HasSpanContext TestRef where
  context = const $ pure defaultSpanContext
  wrapContext = const mempty

data AnySelector f where
  AnySelector :: s f -> AnySelector f

instance Inject s AnySelector where
  inject s f = f (AnySelector s) id
