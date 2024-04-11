{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.
module Language.Marlowe.Runtime.Web.API (runtimeApi, RuntimeAPI) where

import Control.Lens (Bifunctor (bimap))
import Control.Monad (guard, replicateM, unless, (<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  eitherDecode,
  encode,
  object,
  withObject,
  (.:),
 )
import Data.Aeson.Types (parseFail)
import Data.Bits (Bits (shiftL), (.|.))
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Functor (void, ($>))
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Version (Version)
import Data.Word (Word8)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema,
  WithRuntimeStatus,
 )

import Language.Marlowe.Runtime.Web.Adapter.Links (
  FromJSONWithLinks (..),
  HasLinkParser (..),
  ToJSONWithLinks (..),
  WithLink (..),
 )
import Language.Marlowe.Runtime.Web.Contract.API (
  ContractHeader (..),
  ContractState (..),
  ContractsAPI,
  GetContractAPI,
  PostContractsResponse,
 )
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  GetTransactionAPI,
  GetTransactionsAPI,
  PostTransactionsResponse,
 )

import Language.Marlowe.Runtime.Web.Payout.API (GetPayoutAPI, PayoutHeader (..), PayoutState (..), PayoutsAPI)

import Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId)
import Language.Marlowe.Runtime.Web.Core.Tip (ChainTip)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TxId (TxId),
  TxOutRef (TxOutRef, txId),
  TxStatus (Confirmed),
 )
import Language.Marlowe.Runtime.Web.Role.API (RoleAPI)
import Language.Marlowe.Runtime.Web.Status (RuntimeStatus (..))
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTx,
  ApplyInputsTxEnvelope (..),
  CardanoTx,
  ContractTx,
  CreateTxEnvelope (..),
  Tx (..),
  TxHeader (..),
  TxJSON,
  WithdrawTx,
  WithdrawTxEnvelope (..),
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  GetWithdrawalAPI,
  PostWithdrawalsResponse,
  WithdrawalHeader (..),
  WithdrawalsAPI,
 )
import Network.Wai (mapResponseHeaders)
import Servant (
  Capture,
  Capture',
  Context ((:.)),
  Description,
  FromHttpApiData (parseUrlPiece),
  Get,
  HasLink (..),
  HasServer (..),
  Header,
  Header',
  Headers,
  IsElem,
  JSON,
  Link,
  MimeRender,
  NoContent,
  Proxy (..),
  Stream,
  Summary,
  ToHttpApiData (toUrlPiece),
  Verb,
  linkURI,
  safeLink,
  type (:<|>),
  type (:>),
 )
import Servant.API (MimeRender (..), MimeUnrender)
import Servant.API.ContentTypes (MimeUnrender (..))
import Servant.Client (HasClient (..))
import Servant.Client.Core (RunClient)
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Pagination (
  AcceptRanges (..),
  ContentRange (ContentRange),
  HasPagination (RangeType),
 )
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.RoutingApplication (RoutingApplication)
import Text.Parsec (char, digit, eof, hexDigit, many1, runParser, string)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

runtimeApi :: Proxy RuntimeAPI
runtimeApi = Proxy

type RuntimeAPI =
  WithRuntimeStatus
    ( "contracts" :> ContractsAPI
        :<|> "withdrawals" :> WithdrawalsAPI
        :<|> "payouts" :> PayoutsAPI
        :<|> RoleAPI
        :<|> "healthcheck"
          :> ( Summary "Test server status"
                :> Description "Check if the server is running and ready to respond to requests."
                :> OperationId "healthcheck"
                :> Get '[JSON] NoContent
             )
    )

-- | Todo : Move these MimeRender and MimeUnrender instances to their appropriate module
-- | For now, they are here because toJSON `WithLink" has a dependency on these RuntimeApi types and runtimeApi`
instance MimeRender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

instance MimeRender (TxJSON ContractTx) (PostContractsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON ContractTx) (PostContractsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

instance MimeRender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

instance (HasServer api ctx) => HasServer (WithRuntimeStatus api) (IO RuntimeStatus ': ctx) where
  type ServerT (WithRuntimeStatus api) m = ServerT api m
  route _ (getStatus :. ctx) =
    fmap addStatusHeaders . route (Proxy @api) ctx
    where
      addStatusHeaders :: RoutingApplication -> RoutingApplication
      addStatusHeaders app req sendRes = app req \case
        Fail err -> sendRes $ Fail err
        FailFatal err -> sendRes $ FailFatal err
        Route res -> do
          status <- getStatus
          sendRes $ Route $ mapResponseHeaders (<> statusHeaders status) res
      statusHeaders RuntimeStatus{..} =
        [ ("X-Node-Tip", encodeUtf8 $ toUrlPiece nodeTip)
        , ("X-Runtime-Chain-Tip", encodeUtf8 $ toUrlPiece runtimeChainTip)
        , ("X-Runtime-Tip", encodeUtf8 $ toUrlPiece runtimeTip)
        , ("X-Runtime-Version", encodeUtf8 $ toUrlPiece runtimeVersion)
        , ("X-Network-Id", encodeUtf8 $ toUrlPiece networkId)
        ]
  hoistServerWithContext _ _ = hoistServerWithContext (Proxy @api) (Proxy @ctx)

type StatusHeaders =
  '[ Header "X-Node-Tip" ChainTip
   , Header "X-Runtime-Chain-Tip" ChainTip
   , Header "X-Runtime-Tip" ChainTip
   , Header "X-Runtime-Version" Version
   , Header "X-Network-Id" NetworkId
   ]

type family AppendStatusHeaders hs where
  AppendStatusHeaders '[] = StatusHeaders
  AppendStatusHeaders (h ': hs) = h ': AppendStatusHeaders hs

type family AddStatusHeaders api where
  AddStatusHeaders (path :> api) = path :> AddStatusHeaders api
  AddStatusHeaders (a :<|> b) = AddStatusHeaders a :<|> AddStatusHeaders b
  AddStatusHeaders (Verb method cTypes status (Headers hs a)) =
    Verb method cTypes status (Headers (AppendStatusHeaders hs) a)
  AddStatusHeaders (Verb method cTypes status a) = Verb method cTypes status (Headers StatusHeaders a)
  AddStatusHeaders (Stream method status framing ct (Headers hs a)) =
    Stream method status framing ct (Headers (AppendStatusHeaders hs) a)
  AddStatusHeaders (Stream cTypes status framing ct a) = Stream cTypes status framing ct (Headers StatusHeaders a)

instance (RunClient m, HasClient m (AddStatusHeaders api)) => HasClient m (WithRuntimeStatus api) where
  type Client m (WithRuntimeStatus api) = Client m (AddStatusHeaders api)
  clientWithRoute m _ = clientWithRoute m $ Proxy @(AddStatusHeaders api)
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @(AddStatusHeaders api)

instance (HasOpenApi (AddStatusHeaders api)) => HasOpenApi (WithRuntimeStatus api) where
  toOpenApi _ = toOpenApi $ Proxy @(AddStatusHeaders api)

instance HasNamedLink ContractHeader RuntimeAPI "contract" where
  type
    Endpoint ContractHeader RuntimeAPI "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink ContractHeader{..} = Just $ mkLink contractId

instance HasNamedLink ContractHeader RuntimeAPI "transactions" where
  type
    Endpoint ContractHeader RuntimeAPI "transactions" =
      "contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI
  namedLink _ _ mkLink ContractHeader{..} = guard (status == Confirmed) $> mkLink contractId

instance HasNamedLink (CreateTxEnvelope tx) RuntimeAPI "contract" where
  type
    Endpoint (CreateTxEnvelope tx) RuntimeAPI "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink CreateTxEnvelope{..} = Just $ mkLink contractId

instance HasNamedLink ContractState RuntimeAPI "transactions" where
  type
    Endpoint ContractState RuntimeAPI "transactions" =
      "contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI
  namedLink _ _ mkLink ContractState{..} = guard (status == Confirmed) $> mkLink contractId

instance HasNamedLink (ApplyInputsTxEnvelope tx) RuntimeAPI "transaction" where
  type
    Endpoint (ApplyInputsTxEnvelope tx) RuntimeAPI "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink ApplyInputsTxEnvelope{..} = Just $ mkLink contractId transactionId

instance HasNamedLink TxHeader RuntimeAPI "transaction" where
  type
    Endpoint TxHeader RuntimeAPI "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink TxHeader{..} = Just $ mkLink contractId transactionId

instance HasNamedLink Tx RuntimeAPI "previous" where
  type
    Endpoint Tx RuntimeAPI "previous" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = guard (inputUtxo /= contractId) $> mkLink contractId (txId inputUtxo)

instance HasNamedLink Tx RuntimeAPI "next" where
  type
    Endpoint Tx RuntimeAPI "next" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = mkLink contractId <$> consumingTx

instance HasNamedLink WithdrawalHeader RuntimeAPI "withdrawal" where
  type
    Endpoint WithdrawalHeader RuntimeAPI "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawalHeader{..} = Just $ mkLink withdrawalId

instance HasNamedLink PayoutHeader RuntimeAPI "payout" where
  type
    Endpoint PayoutHeader RuntimeAPI "payout" =
      "payouts" :> Capture "payoutId" TxOutRef :> GetPayoutAPI
  namedLink _ _ mkLink PayoutHeader{..} = Just $ mkLink payoutId

instance HasNamedLink PayoutState RuntimeAPI "contract" where
  type
    Endpoint PayoutState RuntimeAPI "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink PayoutState{..} = Just $ mkLink contractId

instance HasNamedLink PayoutState RuntimeAPI "transaction" where
  type
    Endpoint PayoutState RuntimeAPI "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink PayoutState{..} = Just $ mkLink contractId $ txId payoutId

instance HasNamedLink PayoutState RuntimeAPI "withdrawal" where
  type
    Endpoint PayoutState RuntimeAPI "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink PayoutState{..} = mkLink <$> withdrawalId

instance HasNamedLink (WithdrawTxEnvelope tx) RuntimeAPI "withdrawal" where
  type
    Endpoint (WithdrawTxEnvelope tx) RuntimeAPI "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawTxEnvelope{..} = Just $ mkLink withdrawalId

class ParseHttpApiData a where
  urlPieceParser :: Parser a

instance ParseHttpApiData TxOutRef where
  urlPieceParser =
    TxOutRef <$> urlPieceParser <*> do
      _ <- string "%23"
      digits <- many1 digit
      case readMaybe digits of
        Just txIx -> pure txIx
        Nothing -> fail "txIx too large"

instance ParseHttpApiData TxId where
  urlPieceParser = do
    let octet :: Parser Word8
        octet = do
          gb <- hexDigit
          lb <- hexDigit
          let gbi = fromIntegral $ digitToInt gb
          let lbi = fromIntegral $ digitToInt lb
          pure $ shiftL gbi 4 .|. lbi
    octets <- replicateM 32 octet
    pure $ TxId $ BS.pack octets

instance (KnownSymbol seg, HasLinkParser endpoint) => HasLinkParser (seg :> endpoint) where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    string (symbolVal $ Proxy @seg) *> linkParser False (Proxy @endpoint)

instance HasLinkParser (Verb m s ct a) where
  linkParser _ _ = eof $> id

instance (HasLinkParser sub) => HasLinkParser (Header' mods sym a :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (Summary summary :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (Description desc :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLink sub) => HasLink (OperationId name :> sub) where
  type MkLink (OperationId name :> sub) link = MkLink sub link
  toLink f _ = toLink f $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (OperationId name :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLink sub) => HasLink (RenameResponseSchema name :> sub) where
  type MkLink (RenameResponseSchema name :> sub) link = MkLink sub link
  toLink f _ = toLink f $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (RenameResponseSchema name :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance
  ( ParseHttpApiData a
  , ToHttpApiData a
  , HasLinkParser sub
  )
  => HasLinkParser (Capture' mods name a :> sub)
  where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    a <- urlPieceParser
    withSubMkLink <- linkParser False $ Proxy @sub
    pure \mkLink -> withSubMkLink $ mkLink a

class (IsElem (Endpoint a api name) api, HasLink (Endpoint a api name)) => HasNamedLink a api (name :: Symbol) where
  type Endpoint a api name :: Type
  namedLink :: Proxy api -> Proxy name -> MkLink (Endpoint a api name) Link -> a -> Maybe Link

instance (HasNamedLink a api name) => HasNamedLink (WithLink name' a) api name where
  type Endpoint (WithLink name' a) api name = Endpoint a api name
  namedLink api' name mkLink = \case
    IncludeLink _ a -> namedLink api' name mkLink a
    OmitLink a -> namedLink api' name mkLink a

instance
  {-# OVERLAPPING #-}
  ( HasNamedLink a RuntimeAPI name
  , ToJSONWithLinks a
  , KnownSymbol name
  )
  => ToJSONWithLinks (WithLink name a)
  where
  toJSONWithLinks (IncludeLink name a) = (maybe links (: links) link, value)
    where
      (links, value) = toJSONWithLinks a
      link = (symbolVal name,) <$> namedLink runtimeApi name (safeLink runtimeApi $ Proxy @(Endpoint a RuntimeAPI name)) a
  toJSONWithLinks (OmitLink a) = toJSONWithLinks a

instance
  {-# OVERLAPPING #-}
  ( HasLinkParser (Endpoint a RuntimeAPI name)
  , FromJSONWithLinks a
  , KnownSymbol name
  )
  => FromJSONWithLinks (WithLink name a)
  where
  fromJSONWithLinks (links, value) = do
    let mUri = lookup (symbolVal $ Proxy @name) links
    case mUri of
      Nothing -> OmitLink <$> fromJSONWithLinks (links, value)
      Just uri -> case runParser (linkParser True (Proxy @(Endpoint a RuntimeAPI name))) () "" uri of
        Right _ -> IncludeLink (Proxy @name) <$> fromJSONWithLinks (links, value)
        Left err -> parseFail $ show err

instance
  ( HasNamedLink a RuntimeAPI name
  , ToJSONWithLinks a
  , KnownSymbol name
  )
  => ToJSON (WithLink name a)
  where
  toJSON = toJSON' . toJSONWithLinks
    where
      toJSON' (links, value) =
        object
          [ "resource" .= value
          , "links" .= object (bimap fromString (toJSON . show . linkURI) <$> links)
          ]

instance
  ( HasLinkParser (Endpoint a RuntimeAPI name)
  , FromJSONWithLinks a
  , KnownSymbol name
  )
  => FromJSON (WithLink name a)
  where
  parseJSON = fromJSONWithLinks <=< parseJSON'
    where
      parseJSON' = withObject "WithLink" \obj -> do
        value <- obj .: "resource"
        links <- Map.toList <$> obj .: "links"
        pure (links, value)

class ContentRangeFromHttpApiData fields resource where
  contentRangeFromHttpApiData :: Text -> Text -> Text -> Either Text (ContentRange fields resource)

instance ContentRangeFromHttpApiData '[] resource where
  contentRangeFromHttpApiData _ _ _ = Left "Invalid content range"

instance
  ( KnownSymbol field
  , ToHttpApiData (RangeType resource field)
  , FromHttpApiData (RangeType resource field)
  , ContentRangeFromHttpApiData fields resource
  )
  => ContentRangeFromHttpApiData (field ': fields) resource
  where
  contentRangeFromHttpApiData field start end
    | field == T.pack (symbolVal $ Proxy @field) =
        ContentRange
          <$> parseUrlPiece start
          <*> parseUrlPiece end
          <*> pure (Proxy @field)
    | otherwise = do
        ContentRange start' end' field' <-
          contentRangeFromHttpApiData @fields @resource field start end
        pure $ ContentRange start' end' field'

instance
  (ContentRangeFromHttpApiData fields resource)
  => FromHttpApiData (ContentRange fields resource)
  where
  parseUrlPiece text = case T.splitOn " " text of
    [field, suffix] -> case T.splitOn ".." suffix of
      [start, end] -> contentRangeFromHttpApiData field start end
      _ -> Left "Invalid content range"
    _ -> Left "Invalid content range"

instance FromHttpApiData (AcceptRanges fields) where
  parseUrlPiece = const $ Right AcceptRanges
