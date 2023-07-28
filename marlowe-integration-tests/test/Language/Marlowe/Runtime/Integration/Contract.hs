{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Integration.Contract where

import Cardano.Api.Byron (ScriptData (ScriptDataBytes), hashScriptData)
import Colog (HasLog (..), LogAction, Message)
import Control.Arrow (Arrow (..), returnA)
import Control.Concurrent.Component
import Control.Monad (foldM, unless)
import Control.Monad.Event.Class (Inject (..), NoopEventT (runNoopEventT))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Writer (execWriter, runWriter)
import Data.ByteString.Base16 (decodeBase16', encodeBase16)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Merkle (deepMerkleize)
import Language.Marlowe.Core.V1.Plate (extractAll)
import Language.Marlowe.Core.V1.Semantics (TransactionInput (..), TransactionOutput (..), computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Object.Link (LinkedObject (LinkedContract), linkBundle', unlink)
import Language.Marlowe.Object.Types (
  Label (..),
  LabelledObject (..),
  ObjectBundle (..),
  ObjectType (..),
  fromCoreContract,
 )
import Language.Marlowe.Protocol.Load.Client (MarloweLoadClient, pushContract, serveMarloweLoadClient)
import Language.Marlowe.Protocol.Transfer.Client (
  MarloweTransferClient (..),
  hoistMarloweTransferClient,
  serveMarloweTransferClient,
 )
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..), toDatum)
import Language.Marlowe.Runtime.Client (
  exportContract,
  exportIncremental,
  importBundle,
  importIncremental,
  runClientStreaming,
 )
import Language.Marlowe.Runtime.Client.Transfer (BundlePart (..))
import qualified Language.Marlowe.Runtime.Contract as Contract
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (adjacency), merkleizeInputs)
import qualified Language.Marlowe.Runtime.Contract.Api as Api
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions (..), createContractStore)
import Network.Protocol.Connection
import Network.Protocol.Driver.Trace (HasSpanContext (..))
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Protocol.Query.Client (QueryClient, serveQueryClient)
import Network.TypedProtocol (unsafeIntToNat)
import Pipes (each, yield, (>->))
import qualified Pipes.Internal as PI
import qualified Pipes.Prelude as P
import qualified Plutus.V2.Ledger.Api as PV2
import Spec.Marlowe.Semantics.Arbitrary (arbitraryNonnegativeInteger)
import Spec.Marlowe.Semantics.Path (genContractPath, getContract, getInputs)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Integration.Marlowe (createWorkspace, resolveWorkspacePath)
import Test.QuickCheck (Gen, chooseInt, counterexample, forAll)
import UnliftIO (
  Concurrently (..),
  atomically,
  concurrently,
  liftIO,
  throwIO,
 )

spec :: Spec
spec = parallel $ describe "MarloweContract" do
  getContractSpec
  getMerkleizedInputsSpec
  transferSpec

getMerkleizedInputsSpec :: Spec
getMerkleizedInputsSpec = describe "merkleizeInputs" do
  prop "Produces equivalent inputs" \state -> forAll (genTimeInterval state) \interval -> forAll (genContractPath (Environment interval) state) \path ->
    let contract = getContract path
        inputs = getInputs [] path
     in counterexample (show inputs) $
          counterexample (show contract) $ runContractTest do
            hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
            let input = TransactionInput interval $ NormalInput <$> inputs
            input' <- either (fail . show) pure =<< runQuery (merkleizeInputs hash state input)
            Api.ContractWithAdjacency{contract = merkleizedContract} <-
              expectJust "Failed to get contract" $ runQuery $ Api.getContract hash
            let expected = computeTransaction input state contract
            let expected' = case expected of
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
    liftIO $
      result
        `shouldBe` Just
          Api.ContractWithAdjacency
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
    let insertContinuation continuations hash' = do
          Api.ContractWithAdjacency{contractHash, contract = contract'} <-
            expectJust ("failed to get contract " <> show hash') $ runQuery $ Api.getContract hash'
          pure $ Map.insert (toPlutusDatumHash contractHash) contract' continuations
    actualContinuations <- foldM insertContinuation mempty closureWithoutRoot
    liftIO $ actualContinuations `shouldBe` expectedContinuations

transferSpec :: Spec
transferSpec = do
  describe "Import" do
    prop "Produces the same results as load" \label contract -> do
      hash <- runContractTest $ expectJust "failed to push contract" $ runLoad $ pushContract contract
      labelMap <-
        runContractTest $
          expectRight "failed to import contract" $
            runTransfer $
              importBundle $
                ObjectBundle [LabelledObject label ContractType $ fromCoreContract contract]
      labelMap `shouldBe` Map.singleton label hash

    prop "All hashes can be found in the store" \contract -> runContractTest do
      labelMap <-
        expectRight "failed to import contract" $ runTransfer $ importBundle $ snd $ unlink $ LinkedContract contract
      for_ (Map.elems labelMap) \hash -> do
        result <- runQuery $ Api.getContract hash
        liftIO $ Api.contractHash <$> result `shouldBe` Just hash

    prop "Produces the correct contract" \contract ->
      let (rootLabel, bundle) = unlink $ LinkedContract contract
       in counterexample ("Root Label: " <> show rootLabel) $
            counterexample ("Bundle: " <> show bundle) $
              runContractTest do
                hashes <- expectRight "failed to import contract" $ runTransfer $ importBundle bundle
                hash <- expectJust "Root hash not found" $ pure $ Map.lookup rootLabel hashes
                result <- runQuery $ Api.getContract hash
                liftIO $ Api.contract <$> result `shouldBe` Api.contract <$> result

    prop "Produces the correct closure" \contract -> do
      let (rootLabel, bundle) = unlink $ LinkedContract contract
       in counterexample ("Root Label: " <> show rootLabel) $
            counterexample ("Bundle: " <> show bundle) $
              runContractTest do
                hashes <- expectRight "failed to import contract" $ runTransfer $ importBundle bundle
                hash <- expectJust "Root hash not found" $ pure $ Map.lookup rootLabel hashes
                result <- runQuery $ Api.getContract hash
                let continuations = snd $ runWriter $ deepMerkleize contract
                let expectedClosure = Set.insert hash $ Set.map fromPlutusDatumHash $ Map.keysSet continuations
                liftIO $ foldMap Api.closure result `shouldBe` expectedClosure

    prop "Is possible to build the correct continuation map with the closure" \contract -> do
      let (rootLabel, bundle) = unlink $ LinkedContract contract
       in counterexample ("Root Label: " <> show rootLabel) $
            counterexample ("Bundle: " <> show bundle) $
              runContractTest do
                hashes <- expectRight "failed to import contract" $ runTransfer $ importBundle bundle
                hash <- expectJust "Root hash not found" $ pure $ Map.lookup rootLabel hashes
                Api.ContractWithAdjacency{closure} <- expectJust "failed to get contract" $ runQuery $ Api.getContract hash
                let closureWithoutRoot = Set.delete hash closure
                let expectedContinuations = execWriter $ deepMerkleize contract
                let insertContinuation continuations hash' = do
                      Api.ContractWithAdjacency{contractHash, contract = contract'} <-
                        expectJust ("failed to get contract " <> show hash') $ runQuery $ Api.getContract hash'
                      pure $ Map.insert (toPlutusDatumHash contractHash) contract' continuations
                actualContinuations <- foldM insertContinuation mempty closureWithoutRoot
                liftIO $ actualContinuations `shouldBe` expectedContinuations

    prop "incremental" \contract ->
      let (_, ObjectBundle bundle) = unlink $ LinkedContract contract
       in forAll (genChunks bundle) \chunks -> do
            expected <-
              runContractTest $ expectRight "failed to import contract" $ runTransfer $ importBundle $ ObjectBundle $ concat chunks
            actual <-
              runContractTest do
                P.fold (<>) mempty id $
                  each (toParts $ ObjectBundle <$> chunks) >-> do
                    result <- runTransferIncremental importIncremental
                    case result of
                      Left err -> throwIO $ userError $ "Failed to import contract incrementally: " <> show err
                      Right ids -> yield ids
            expected `shouldBe` actual

  describe "Export" do
    prop "Labels map to closure" \contract -> runContractTest do
      hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
      ObjectBundle bundle <- expectJust "failed to export contract" $ runTransfer $ exportContract 100 hash
      Api.ContractWithAdjacency{closure} <- expectJust "failed to get contract" $ runQuery $ Api.getContract hash
      liftIO $ Set.fromList (unLabel . _label <$> bundle) `shouldBe` Set.map (encodeBase16 . unDatumHash) closure

    prop "Labels are hashes" \contract -> runContractTest do
      hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
      bundle <- expectJust "failed to export contract" $ runTransfer $ exportContract 100 hash
      let expectOnlyContracts = \case
            LinkedContract c -> pure (LinkedContract c, c)
            a -> fail $ "Unexpected non-contract object in exported bundle " <> show a
      (linked, _) <- expectRight "" $ linkBundle' bundle expectOnlyContracts mempty
      liftIO $ for_ linked \(Label label, c) -> label `shouldBe` encodeBase16 (unDatumHash $ hashContract c)

    prop "import . export" \contract -> do
      ObjectBundle bundle <- runContractTest do
        hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
        expectJust "failed to export contract" $ runTransfer $ exportContract 100 hash
      hashes <- runContractTest $ expectRight "failed to import bundle" $ runTransfer $ importBundle $ ObjectBundle bundle
      hashes
        `shouldBe` Map.fromList
          ((_label &&& DatumHash . fromRight (error "fromRight: left") . decodeBase16' . unLabel . _label) <$> bundle)

    prop "incremental" \contract -> runContractTest do
      hash <- expectJust "failed to push contract" $ runLoad $ pushContract contract
      expected <- expectJust "failed to export contract" $ runTransfer $ exportContract 100 hash
      actual <-
        P.fold (fmap ObjectBundle . on (<>) getObjects) (ObjectBundle []) id do
          success <- runTransferIncremental $ exportIncremental 100 hash
          unless success $ throwIO $ userError "Failed to export contract incrementally"
      liftIO $ expected `shouldBe` actual

toParts :: [ObjectBundle] -> [BundlePart]
toParts = reverse . go . reverse
  where
    go :: [ObjectBundle] -> [BundlePart]
    go [] = []
    go (x : xs) = FinalPart x : (IntermediatePart <$> xs)

genChunks :: [a] -> Gen [[a]]
genChunks [] = pure []
genChunks as = do
  pivot <- chooseInt (1, length as)
  let (chunk, as') = splitAt pivot as
  (chunk :) <$> genChunks as'

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
expectJust msg m =
  m >>= \case
    Nothing -> fail msg
    Just a -> pure a

expectRight :: (Show a) => String -> TestM (Either a b) -> TestM b
expectRight msg m =
  m >>= \case
    Left e -> fail $ msg <> ": " <> show e
    Right a -> pure a

-- helpers

runLoad :: MarloweLoadClient TestM a -> TestM a
runLoad client = do
  TestHandle{..} <- ask
  runConnector loadConnector client

runTransfer :: MarloweTransferClient TestM a -> TestM a
runTransfer client = do
  TestHandle{..} <- ask
  runConnector transferConnector client

runTransferIncremental
  :: forall a' a b' b r
   . MarloweTransferClient (PI.Proxy a' a b' b TestM) r
  -> PI.Proxy a' a b' b TestM r
runTransferIncremental = runClientStreaming hoistMarloweTransferClient runTransfer

runQuery :: QueryClient Api.ContractRequest TestM a -> TestM a
runQuery client = do
  TestHandle{..} <- ask
  runConnector queryConnector client

-- test plumbing

runContractTest :: TestM a -> IO a
runContractTest test = runResourceT do
  workspace <- createWorkspace "marlowe-contract-test"
  contractStore <-
    createContractStore
      ContractStoreOptions
        { contractStoreDirectory = resolveWorkspacePath workspace "contract-store"
        , contractStoreStagingDirectory = resolveWorkspacePath workspace "staging-areas"
        , lockingMicrosecondsBetweenRetries = 100_000
        }
  let testComponent = proc contractDeps -> do
        Contract.MarloweContract{..} <- Contract.contract -< contractDeps
        returnA
          -<
            TestHandle
              { loadConnector = directConnector serveMarloweLoadClient loadServerSource
              , queryConnector = directConnector serveQueryClient queryServerSource
              , transferConnector = directConnector serveMarloweTransferClient transferServerSource
              , logAction = mempty
              }
  (Concurrently runTestComponent, testHandle) <-
    atomically $
      unComponent
        testComponent
        Contract.ContractDependencies
          { batchSize = unsafeIntToNat 10
          , contractStore
          }
  (a, _) <- runNoopEventT $ flip runReaderT testHandle $ concurrently test runTestComponent
  pure a

type TestM = ReaderT TestHandle (NoopEventT TestRef AnySelector (ResourceT IO))

data TestHandle = TestHandle
  { loadConnector :: Connector MarloweLoadClient TestM
  , queryConnector :: Connector (QueryClient Api.ContractRequest) TestM
  , transferConnector :: Connector MarloweTransferClient TestM
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
