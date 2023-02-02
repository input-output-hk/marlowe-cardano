{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Query.Types
  where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Bifunctor (bimap)
import Data.Binary (Binary(..), getWord8, putWord8)
import GHC.Generics (Generic)
import GHC.Show (showCommaSpace, showSpace)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Codec.Spec (MessageEq(..), ShowProtocol(..))
import Network.Protocol.Driver (MessageToJSON(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))

data MarloweQuery where
  StInit :: MarloweQuery
  StRequest :: a -> MarloweQuery
  StDone :: MarloweQuery

data Request a where
  ReqContractHeaders :: Range ContractId -> Request (Page ContractId ContractHeader)
  ReqBoth :: Request a -> Request b -> Request (a, b)

data SomeRequest where
  SomeRequest :: Request a -> SomeRequest

instance Binary SomeRequest where
  get = do
    tag <- getWord8
    case tag of
      0x00 -> do
        SomeRequest a <- get
        SomeRequest b <- get
        pure $ SomeRequest $ ReqBoth a b
      0x01 -> SomeRequest . ReqContractHeaders <$> get
      _ -> fail "Invalid Request tag"

  put (SomeRequest req) = case req of
    ReqBoth a b -> do
      putWord8 0x00
      put $ SomeRequest a
      put $ SomeRequest b
    ReqContractHeaders range -> do
      putWord8 0x01
      put range

deriving instance Eq (Request a)
deriving instance Show (Request a)
instance ToJSON (Request a) where
  toJSON = \case
    ReqContractHeaders range -> object
      [ "get-contract-headers-for-range" .= range
      ]
    ReqBoth a b -> object
      [ "req-both" .= (toJSON a, toJSON b)
      ]

data StRequest a where
  TokContractHeaders :: StRequest (Page ContractId ContractHeader)
  TokBoth :: StRequest a -> StRequest b -> StRequest (a, b)

deriving instance Show (StRequest a)
deriving instance Eq (StRequest a)

instance Protocol MarloweQuery where
  data Message MarloweQuery st st' where
    MsgRequest :: Request a -> Message MarloweQuery
      'StInit
      ('StRequest a)

    MsgRespond :: a -> Message MarloweQuery
      ('StRequest a)
      'StDone

  data ClientHasAgency ps where
    TokInit :: ClientHasAgency 'StInit

  data ServerHasAgency ps where
    TokRequest :: StRequest a -> ServerHasAgency ('StRequest a)

  data NobodyHasAgency ps where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data Range a = Range
  { rangeStart :: Maybe a
  , rangeOffset :: Int
  , rangeLimit :: Int
  , rangeDirection :: Order
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary)

data Page a b = Page
  { items :: [b]
  , nextRange :: Maybe (Range a)
  , totalCount :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary)

data Order = Ascending | Descending
  deriving stock (Eq, Show, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, Binary)

instance MessageToJSON MarloweQuery where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgRequest req -> object [ "request" .= req ]
    ServerAgency (TokRequest req) -> \case
      MsgRespond a -> object [ "respond" .= responseToJSON req a ]

    where
      responseToJSON :: StRequest a -> a -> Value
      responseToJSON = \case
        TokContractHeaders -> toJSON
        TokBoth a b -> toJSON . bimap (responseToJSON a) (responseToJSON b)

instance ShowProtocol MarloweQuery where
  showsPrecMessage p = fmap (showParen (p >= 11)) . \case
    ClientAgency TokInit -> \case
      MsgRequest req -> (showString "MsgRequest" . showSpace . showsPrec 11 req)
    ServerAgency (TokRequest req) -> \case
      MsgRespond a -> showsPrecResult req 11 a
    where
      showsPrecResult :: StRequest a -> Int -> a -> String -> String
      showsPrecResult = \case
        TokContractHeaders -> showsPrec
        TokBoth ta tb -> \_ (a, b) -> showParen True (showsPrecResult ta 0 a . showCommaSpace . showsPrecResult tb 0 b)
  showsPrecServerHasAgency p (TokRequest req) = showParen (p >= 11) (showString "TokRequest" . showSpace . showsPrec 11 req)
  showsPrecClientHasAgency _ TokInit = showString "TokInit"

instance MessageEq MarloweQuery where
  messageEq = \case
    AnyMessageAndAgency (ClientAgency TokInit) (MsgRequest req) -> \case
      AnyMessageAndAgency (ClientAgency TokInit) (MsgRequest req') -> reqEq req req'
      _ -> False
    AnyMessageAndAgency (ServerAgency (TokRequest req)) (MsgRespond a) -> \case
      AnyMessageAndAgency (ServerAgency (TokRequest req')) (MsgRespond a') -> resultEq req req' a a'
      _ -> False
    where
      reqEq :: Request a -> Request a1 -> Bool
      reqEq (ReqContractHeaders range) (ReqContractHeaders range') = range == range'
      reqEq (ReqContractHeaders _) _ = False
      reqEq (ReqBoth a b) (ReqBoth a' b') = reqEq a a' && reqEq b b'
      reqEq (ReqBoth _ _) _ = False

      resultEq :: StRequest a -> StRequest b -> a -> b -> Bool
      resultEq TokContractHeaders TokContractHeaders = (==)
      resultEq TokContractHeaders _ = const $ const False
      resultEq (TokBoth ta tb) (TokBoth ta' tb') = \(a, b) (a', b') ->
        resultEq ta ta' a a' && resultEq tb tb' b b'
      resultEq (TokBoth _ _) _ = const $ const False
