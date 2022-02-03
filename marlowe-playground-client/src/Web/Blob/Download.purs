module Web.Blob.Download where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.File.Blob (Blob)

foreign import downloadImpl :: EffectFn2 (Nullable String) Blob Unit

-- | * `FileDownload` strategy enforces full file download.
-- | * `WindowOpen` tries to open the blob in the new
-- | window. If the browser is able to handle a given mime
-- | it will open it up. If the other case browser will download
-- | the blob but its filename will be based on some random
-- | string (internal blob URL).
data HandleMethod = FileDownload String | WindowOpen

download :: HandleMethod -> Blob -> Effect Unit
download handleMethod blob = case handleMethod of
  FileDownload name -> runEffectFn2 downloadImpl (toNullable $ Just name) blob
  WindowOpen -> runEffectFn2 downloadImpl null blob

