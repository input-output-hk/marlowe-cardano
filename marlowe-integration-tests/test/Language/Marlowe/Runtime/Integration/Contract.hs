{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Integration.Contract where

import Cardano.Api.Byron (ScriptData(ScriptDataBytes), hashScriptData)
import Colog (HasLog(..), LogAction, Message)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Monad (foldM)
import Control.Monad.Event.Class (Inject(..), NoopEventT(runNoopEventT))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Writer (execWriter, runWriter)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Merkle (deepMerkleize)
import Language.Marlowe.Core.V1.Plate (extractAll)
import Language.Marlowe.Core.V1.Semantics (TransactionInput(..), TransactionOutput(..), computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Client (MarloweLoadClient, pushContract, serveMarloweLoadClient)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import qualified Language.Marlowe.Runtime.Contract as Contract
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency(adjacency), merkleizeInputs)
import qualified Language.Marlowe.Runtime.Contract.Api as Api
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions(..), createContractStore)
import Network.Protocol.Connection
import Network.Protocol.Driver.Trace (HasSpanContext(..))
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Protocol.Query.Client (QueryClient, serveQueryClient)
import Network.TypedProtocol (unsafeIntToNat)
import qualified Plutus.V2.Ledger.Api as PV2
import Spec.Marlowe.Semantics.Arbitrary (arbitraryNonnegativeInteger)
import Spec.Marlowe.Semantics.Path (genContractPath, getContract, getInputs)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Integration.Marlowe (createWorkspace, resolveWorkspacePath)
import Test.QuickCheck (Gen, counterexample, forAll)
import UnliftIO (Concurrently(..), atomically, liftIO, race_)

spec :: Spec
spec = parallel $ describe "MarloweContract" do
  getContractSpec
  getMerkleizedInputsSpec

getMerkleizedInputsSpec :: Spec
getMerkleizedInputsSpec = describe "merkleizeInputs" do
  prop "Produces equivalent inputs" \state -> forAll (genTimeInterval state) \interval -> forAll (genContractPath (Environment interval) state) \path ->
    let
      contract = getContract path
      inputs = getInputs [] path
    in counterexample (show inputs)
      $ counterexample (show contract) $ runContractTest do
        hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
        let input = TransactionInput interval $ NormalInput <$> inputs
        input' <- either (fail . show) pure =<< runQuery (merkleizeInputs hash state input)
        Api.ContractWithAdjacency{contract = merkleizedContract} <- expectJust "Failed to get contract" $ runQuery $ Api.getContract hash
        let expected = computeTransaction input state contract
        let
          expected' = case expected of
            TransactionOutput warnings payment state' contract' ->
              TransactionOutput warnings payment state' $ fst $ runWriter $ deepMerkleize contract'
            a -> a
        let actual = computeTransaction input' state merkleizedContract
        liftIO $ actual `shouldBe` expected'

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
  runConnector loadConnector client

runQuery :: QueryClient Api.ContractRequest TestM a -> TestM a
runQuery client = do
  TestHandle {..} <- ask
  runConnector queryConnector client

-- test plumbing

runContractTest :: TestM () -> IO ()
runContractTest test = runResourceT do
  workspace <- createWorkspace "marlowe-contract-test"
  contractStore <- createContractStore ContractStoreOptions
    { contractStoreDirectory = resolveWorkspacePath workspace "contract-store"
    , contractStoreStagingDirectory = resolveWorkspacePath workspace "staging-areas"
    , lockingMicrosecondsBetweenRetries = 100_000
    }
  let
    testComponent = proc contractDeps -> do
      Contract.MarloweContract{..} <- Contract.contract -< contractDeps
      returnA -< TestHandle
        { loadConnector = directConnector serveMarloweLoadClient loadServerSource
        , queryConnector = directConnector serveQueryClient queryServerSource
        , logAction = mempty
        }
  (Concurrently runTestComponent, testHandle) <- atomically $ unComponent testComponent Contract.ContractDependencies
    { batchSize = unsafeIntToNat 10
    , contractStore
    }
  runNoopEventT $ flip runReaderT testHandle $ race_ test runTestComponent

type TestM = ReaderT TestHandle (NoopEventT TestRef AnySelector (ResourceT IO))

data TestHandle = TestHandle
  { loadConnector :: Connector MarloweLoadClient TestM
  , queryConnector :: Connector (QueryClient Api.ContractRequest) TestM
  , logAction :: LogAction TestM Message
  }

instance HasLog TestHandle Message TestM where
  logActionL f TestHandle{..} = (\logAction' -> TestHandle{logAction = logAction', ..}) <$> f logAction

newtype TestRef = TestRef ()
  deriving newtype (Semigroup, Monoid)

instance HasSpanContext TestRef where
  context = const $ pure defaultSpanContext
  wrapContext = const mempty

data AnySelector f where
  AnySelector :: s f -> AnySelector f

instance Inject s AnySelector where
  inject s f = f (AnySelector s) id

genTimeInterval :: State -> Gen TimeInterval
genTimeInterval State{..} = do
  dStart <- arbitraryNonnegativeInteger
  let start = PV2.getPOSIXTime minTime + dStart
  duration <- arbitraryNonnegativeInteger
  pure (PV2.POSIXTime start, PV2.POSIXTime $ start + duration)
