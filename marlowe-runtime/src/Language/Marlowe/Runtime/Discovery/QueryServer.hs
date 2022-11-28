{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Discovery.QueryServer
  where

import Control.Concurrent.Component
import Control.Exception (displayException)
import Data.Aeson (ToJSON, toJSON)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId)
import Language.Marlowe.Runtime.Discovery.Api
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)
import Observe.Event (Event, EventBackend, addField, narrowEventBackend, reference, subEventBackend, withEvent)
import Observe.Event.BackendModification (modifyEventBackend, setInitialCause)
import Observe.Event.DSL (SelectorField(..), SelectorSpec(..))
import Observe.Event.Render.JSON (DefaultRenderFieldJSON(..), DefaultRenderSelectorJSON(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

data PageField a
  = Params a
  | Count Int

instance ToJSON a => DefaultRenderFieldJSON (PageField a) where
  defaultRenderFieldJSON = \case
    Params a -> ("params", toJSON a)
    Count i -> ("count", toJSON i)

data QuerySelector a f where
  Receive :: QuerySelector a (PageField a)
  Next :: QuerySelector a (PageField a)
  Done :: QuerySelector a Void

instance ToJSON a => DefaultRenderSelectorJSON (QuerySelector a) where
  defaultRenderSelectorJSON = \case
    Receive -> ("receive", defaultRenderFieldJSON)
    Next -> ("next", defaultRenderFieldJSON)
    Done -> ("done", defaultRenderFieldJSON)

type QuerySelectorVoid = QuerySelector Void
type QuerySelectorRoleToken = QuerySelector PolicyId

compile $ SelectorSpec "worker"
  [ "get" ≔ Inject ''QuerySelectorVoid
  , ["get", "by", "role"] ≔ Inject ''QuerySelectorRoleToken
  ]

compile $ SelectorSpec ["discovery", "query", "server"]
  [ ["new", "client"] ≔ ''Void
  , "worker" ≔ Inject ''WorkerSelector
  , ["worker", "crashed"] ≔ ''String
  , ["worker", "terminated"] ≔ ''Void
  ]

type RunQueryServer m = RunServer m (QueryServer DiscoveryQuery)

data DiscoveryQueryServerDependencies r = DiscoveryQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , pageSize :: Natural
  , eventBackend :: EventBackend IO r DiscoveryQueryServerSelector
  }

discoveryQueryServer :: Component IO (DiscoveryQueryServerDependencies r) ()
discoveryQueryServer = serverComponentWith
  worker
  (\eventBackend ex -> do
    withEvent eventBackend WorkerCrashed \ev -> addField ev $ displayException ex
  )
  (\eventBackend -> do
    withEvent eventBackend WorkerTerminated $ const $ pure ()
  )
  \DiscoveryQueryServerDependencies{..} -> do
      runQueryServer <- acceptRunQueryServer
      withEvent eventBackend NewClient \ev -> do
        pure
          ( modifyEventBackend (setInitialCause $ reference ev) eventBackend
          , WorkerDependencies
              { runQueryServer
              , getHeaders
              , getHeadersByRoleTokenCurrency
              , pageSize
              , eventBackend = subEventBackend Worker ev
              }
          )

data WorkerDependencies r = WorkerDependencies
  { runQueryServer   :: RunQueryServer IO
  , getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , pageSize :: Natural
  , eventBackend :: EventBackend IO r WorkerSelector
  }

worker :: Component IO (WorkerDependencies r) ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runQueryServer

    server :: QueryServer DiscoveryQuery IO ()
    server = QueryServer $ pure $ ServerStInit \case
      GetContractHeaders ->
        getContractHeadersServer (narrowEventBackend Get eventBackend) pageSize getHeaders
      GetContractHeadersByRoleTokenCurrency policyId ->
        getContractHeadersByRoleTokenCurrencyServer (narrowEventBackend GetByRole eventBackend) policyId getHeadersByRoleTokenCurrency
  run server

getContractHeadersServer
  :: forall r
   . EventBackend IO r (QuerySelector Void)
  -> Natural
  -> IO [ContractHeader]
  -> IO (ServerStNext DiscoveryQuery 'CanReject () Void [ContractHeader] IO ())
getContractHeadersServer eventBackend pageSize getHeaders = do
  withEvent' Receive \ev -> next ev =<< getHeaders
  where
  withEvent' :: QuerySelector Void f -> (Event IO r (QuerySelector Void) f -> IO a) -> IO a
  withEvent' = withEvent eventBackend
  next
    :: Event IO r (QuerySelector Void) (PageField Void)
    -> [ContractHeader]
    -> IO (ServerStNext DiscoveryQuery k () Void [ContractHeader] IO ())
  next ev headers = do
    let (page, page') = splitAt (fromIntegral pageSize) headers
    addField ev $ Count $ length page
    pure case page' of
      [] -> lastPage page
      headers' -> SendMsgNextPage page (Just ()) ServerStPage
        { recvMsgDone = withEvent' Done mempty
        , recvMsgRequestNext = const $ withEvent' Next \ev' -> next ev' headers'
        }
  lastPage
    :: [ContractHeader]
    -> ServerStNext DiscoveryQuery k () Void [ContractHeader] IO ()
  lastPage page = SendMsgNextPage page Nothing ServerStPage
    { recvMsgDone = withEvent' Done mempty
    , recvMsgRequestNext = const $ withEvent' Next $ const $ pure $ lastPage []
    }

getContractHeadersByRoleTokenCurrencyServer
  :: forall r
   . EventBackend IO r (QuerySelector PolicyId)
  -> PolicyId
  -> (PolicyId -> IO [ContractHeader])
  -> IO (ServerStNext DiscoveryQuery 'CanReject Void Void [ContractHeader] IO ())
getContractHeadersByRoleTokenCurrencyServer eventBackend policyId getHeadersByRoleTokenCurrency = do
  let
    withEvent' :: QuerySelector PolicyId f -> (Event IO r (QuerySelector PolicyId) f -> IO a) -> IO a
    withEvent' = withEvent eventBackend
  withEvent' Receive \ev -> do
    addField ev $ Params policyId
    headers <- getHeadersByRoleTokenCurrency policyId
    addField ev $ Count $ length headers
    pure $ SendMsgNextPage headers Nothing ServerStPage
      { recvMsgDone = pure ()
      , recvMsgRequestNext = absurd
      }
