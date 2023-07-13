module Language.Marlowe.Runtime.Web.Server.REST.ContractSources where

import Control.Monad.Except (runExceptT)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import qualified Data.Map as Map
import Language.Marlowe.Object.Link (LinkError (DuplicateLabel, TypeMismatch, UnknownSymbol))
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Protocol.Transfer.Types (ImportError (..))
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Web (ContractSourcesAPI)
import Language.Marlowe.Runtime.Web.Server.Monad
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', badRequest'')
import Language.Marlowe.Runtime.Web.Types (ContractSourceId (..), PostContractSourceResponse (..))
import Pipes (MFunctor (..), Producer, liftIO, (>->))
import qualified Pipes.Prelude as Pipes
import Servant

server :: ServerT ContractSourcesAPI ServerM
server = post

post :: Label -> Producer ObjectBundle IO () -> ServerM PostContractSourceResponse
post main bundles = do
  result <- runExceptT $ Pipes.fold (<>) mempty id $ hoist liftIO bundles >-> importBundle main
  case result of
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
