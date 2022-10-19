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


-- | Schema version used for
newtype SchemaVersion = SchemaVersion Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance Binary SchemaVersion where
  put (SchemaVersion v) = put $ T.encodeUtf8 v
  get = do
    bytes <- get
    case T.decodeUtf8' bytes of
      Left err      -> fail $ show err
      Right version -> pure $ SchemaVersion version

