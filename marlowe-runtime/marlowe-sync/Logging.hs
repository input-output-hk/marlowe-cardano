{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , renderRootSelectorOTel
  ) where

import Control.Monad.Event.Class (Inject(..))
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), renderContractId)
import Language.Marlowe.Runtime.Sync.Database
import Network.Protocol.Driver (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace
import Prelude hiding (filter)

data RootSelector f where
  MarloweSyncServer :: TcpServerSelector (Handshake MarloweSync) f -> RootSelector f
  MarloweHeaderSyncServer :: TcpServerSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  MarloweQueryServer :: TcpServerSelector (Handshake MarloweQuery) f -> RootSelector f
  Database :: DatabaseSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject DatabaseSelector RootSelector where
  inject = injectSelector Database

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  MarloweSyncServer sel -> renderTcpServerSelectorOTel sel
  MarloweHeaderSyncServer sel -> renderTcpServerSelectorOTel sel
  MarloweQueryServer sel -> renderTcpServerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel DatabaseSelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  GetTip -> renderQuerySelectorOTel "get_tip" $ const Nothing
  GetTipForContract -> renderQuerySelectorOTel "get_tip_for_contract"
    $ Just . toAttribute . renderContractId
  GetCreateStep -> renderQuerySelectorOTel "get_create_step"
    $ Just . toAttribute . renderContractId
  GetIntersectionForContract -> renderQuerySelectorOTel "get_intersection_for_contract" \case
    GetIntersectionForContractArguments{..} ->
      Just $ toAttribute $ renderContractId contractId : (fromString . show <$> points)
  GetIntersection -> renderQuerySelectorOTel "get_intersection"
    $ Just . toAttribute . fmap (fromString @Text . show)
  GetNextHeaders -> renderQuerySelectorOTel "get_next_headers" $ Just . fromString . show
  GetNextSteps MarloweV1 -> renderQuerySelectorOTel "get_next_steps" \case
    GetNextStepsArguments{..} ->
      Just $ toAttribute
        [ fromString $ show MarloweV1
        , renderContractId contractId
        , fromString $ show fromPoint
        ]
  GetHeaders -> renderQuerySelectorOTel "get_headers" \case
    GetHeadersArguments{..} ->
      Just $ toAttribute
        [ fromString @Text $ show filter
        , fromString $ show range
        ]
  GetContractState -> renderQuerySelectorOTel "get_contract_state"
    $ Just . toAttribute . renderContractId
  GetTransaction -> renderQuerySelectorOTel "get_transaction"
    $ Just . fromString . show
  GetTransactions -> renderQuerySelectorOTel "get_transactions"
    $ Just . toAttribute . renderContractId
  GetWithdrawal -> renderQuerySelectorOTel "get_withdrawal"
    $ Just . fromString . show
  GetWithdrawals -> renderQuerySelectorOTel "get_withdrawals" \case
    GetWithdrawalsArguments{..} ->
      Just $ toAttribute
        [ fromString @Text $ show filter
        , fromString $ show range
        ]
  where
    renderQuerySelectorOTel :: Text -> (p -> Maybe Attribute) -> OTelRendered (QueryField p r)
    renderQuerySelectorOTel queryName renderArguments = OTelRendered
      { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
      , eventKind = Client
      , renderField = \case
          Arguments p -> catMaybes
            [ Just ("db.system", "postgresql")
            , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
            , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
            , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
            , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
            , Just ("net.transport", "ip_tcp")
            , Just ("db.operation", "SELECT")
            , ("db.parameters",) <$> renderArguments p
            ]
          Result _ -> []
      }
