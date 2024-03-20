module Language.Marlowe.Runtime.Web.Adapter.ByteString (
  hasLength,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()

hasLength :: Int -> ByteString -> Either T.Text ByteString
hasLength l bytes
  | BS.length bytes == l = pure bytes
  | otherwise = Left $ "Expected " <> T.pack (show l) <> " bytes"
