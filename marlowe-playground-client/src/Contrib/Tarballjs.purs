module Contrib.Tarballjs where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Maybe (Maybe)
import Data.Undefinable (Undefinable)
import Data.Undefinable as Undefinable
import Effect (Effect)
import Effect.Aff (Aff)
import JS.Object
  ( EffectMth0
  , EffectMth1
  , EffectMth2
  , JSObject
  , runEffectMth1
  , runEffectMth2
  )
import Type.Proxy (Proxy(..))
import Web.File.File (File)

newtype FileName = FileName String

newtype FileContent = FileContent String

type TarWriter = JSObject
  ( addTextFile :: EffectMth2 FileName FileContent Unit
  , download :: EffectMth1 FileName Unit
  , write :: EffectMth0 (Promise Uint8Array)
  )

foreign import tarWriter :: Effect TarWriter

addTextFile :: TarWriter -> FileName -> FileContent -> Effect Unit
addTextFile = runEffectMth2 (Proxy :: Proxy "addTextFile")

download :: TarWriter -> FileName -> Effect Unit
download = runEffectMth1 (Proxy :: Proxy "download")

type TarReader = JSObject
  ( getTextFile :: EffectMth1 FileName (Undefinable String)
  , readFile :: EffectMth1 File (Promise ArrayBuffer)
  )

foreign import tarReader :: Effect TarReader

-- | Reads a given file and updates the state of the reader object as well.
-- | After this call finishes we can safely access pieces of the archive.
readFile :: TarReader -> File -> Aff ArrayBuffer
readFile reader = Promise.toAffE <<< runEffectMth1 (Proxy :: Proxy "readFile")
  reader

getTextFile :: TarReader -> FileName -> Effect (Maybe String)
getTextFile reader = map Undefinable.toMaybe <<< runEffectMth1
  (Proxy :: Proxy "getTextFile")
  reader
