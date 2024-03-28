{-# LANGUAGE ExplicitNamespaces #-}

module Language.Marlowe.Runtime.Web.Contract.Source.Server (server) where

import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Language.Marlowe.Core.V1.Merkle (Continuations, deepDemerkleize, demerkleizeContract)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Object.Link (LinkError (DuplicateLabel, TypeMismatch, UnknownSymbol))
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Protocol.Transfer.Types (ImportError (..))
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))

import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.ApiError (badRequest', badRequest'')
import Language.Marlowe.Runtime.Web.Adapter.Server.Monad (
  ServerM,
  getContract,
  importBundle,
 )
import Language.Marlowe.Runtime.Web.Contract.API (
  ContractSourceAPI,
  ContractSourceId (..),
  ContractSourcesAPI,
  PostContractSourceResponse (..),
 )

import Pipes (MFunctor (..), Producer, liftIO, (>->))
import qualified Pipes.Prelude as Pipes
import qualified PlutusLedgerApi.V2 as PV2
import Servant (
  HasServer (ServerT),
  err404,
  throwError,
  type (:<|>) ((:<|>)),
 )

server :: ServerT ContractSourcesAPI ServerM
server =
  post
    :<|> contractSourceServer

contractSourceServer :: ContractSourceId -> ServerT ContractSourceAPI ServerM
contractSourceServer sourceId =
  getOne sourceId
    :<|> getAdjacency sourceId
    :<|> getClosure sourceId

getOne :: ContractSourceId -> Bool -> ServerM Contract
getOne sourceId expand = do
  ContractWithAdjacency{..} <- getContractOrThrow sourceId
  if expand
    then do
      continuations <- loadContinuations closure
      case demerkleizeContract continuations $ deepDemerkleize False contract of
        Left err -> fail err
        Right expanded -> pure expanded
    else pure contract

getAdjacency :: ContractSourceId -> ServerM (ListObject ContractSourceId)
getAdjacency = fmap (coerce . Set.toList . adjacency) . getContractOrThrow

getClosure :: ContractSourceId -> ServerM (ListObject ContractSourceId)
getClosure = fmap (coerce . Set.toList . closure) . getContractOrThrow

loadContinuations :: Set DatumHash -> ServerM Continuations
loadContinuations closure =
  Map.fromDistinctAscList <$> for (Set.toAscList closure) \hash -> do
    ContractWithAdjacency{contract} <- maybe (fail $ "Failed to load continuation: " <> show hash) pure =<< getContract hash
    pure (PV2.DatumHash $ PV2.toBuiltin $ unDatumHash hash, contract)

getContractOrThrow :: ContractSourceId -> ServerM ContractWithAdjacency
getContractOrThrow sourceId = maybe (throwError err404) pure =<< getContract (coerce sourceId)

post :: Label -> Producer ObjectBundle IO () -> ServerM PostContractSourceResponse
post main bundles = do
  (intermediate, result) <- Pipes.fold' (<>) mempty id $ hoist liftIO (Right mempty <$ bundles) >-> importBundle main
  case (intermediate <>) <$> result of
    Left err -> case err of
      ContinuationNotInStore hash ->
        throwError $ badRequest'' "Merkleized continuation not in store." "BadRequest" hash
      LinkError (UnknownSymbol s) ->
        throwError $ badRequest'' "Symbol not defined." "BadRequest" s
      LinkError (DuplicateLabel s) ->
        throwError $ badRequest'' "Duplicate label." "BadRequest" s
      LinkError (TypeMismatch expected actual) ->
        throwError $
          badRequest'' "Type mismatch." "BadRequest" $
            object
              [ "expected" .= expected
              , "actual" .= actual
              ]
    Right ids -> case Map.lookup main ids of
      Nothing -> throwError $ badRequest' "Main contract not defined."
      Just mainId ->
        pure
          PostContractSourceResponse
            { contractSourceId = ContractSourceId $ unDatumHash mainId
            , intermediateIds = ContractSourceId . unDatumHash <$> ids
            }
