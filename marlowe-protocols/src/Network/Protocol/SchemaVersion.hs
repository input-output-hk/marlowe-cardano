{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the chain seek protocol.

module Network.Protocol.SchemaVersion
  where

import Data.Binary (Binary(..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Used during handshakes and represents query or command schema
-- versions used by both sides. It is indexed by type which version
-- it represents.
newtype SchemaVersion t = SchemaVersion Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance Binary (SchemaVersion t) where
  put (SchemaVersion v) = put $ T.encodeUtf8 v
  get = do
    bytes <- get
    case T.decodeUtf8' bytes of
      Left err      -> fail $ show err
      Right version -> pure $ SchemaVersion version

