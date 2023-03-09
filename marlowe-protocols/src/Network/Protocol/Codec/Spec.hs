{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Network.Protocol.Codec.Spec
  where

import Data.ByteString.Lazy (ByteString, fromStrict, toChunks)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Show (showSpace)
import Network.Protocol.Codec (BinaryMessage, binaryCodec)
import Network.TypedProtocol (PeerHasAgency, Protocol(..), SomeMessage(..))
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..), Codec(..), PeerHasAgency(..), runDecoder)
import Test.Hspec (Spec, it)
import Test.Hspec.Golden (Golden(..), defaultGolden)
import Test.QuickCheck (Property, Testable(property), counterexample, forAllShrinkShow, infiniteList, oneof)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Property (failed, succeeded)

class ShowProtocol ps where
  showsPrecMessage :: Int -> PeerHasAgency pr st -> Message ps st st' -> ShowS
  showsPrecServerHasAgency :: forall (st :: ps). Int -> ServerHasAgency st -> ShowS
  showsPrecClientHasAgency :: forall (st :: ps). Int -> ClientHasAgency st -> ShowS

class MessageEq ps where
  messageEq :: AnyMessageAndAgency ps -> AnyMessageAndAgency ps -> Bool

class ArbitraryMessage ps where
  arbitraryMessage :: Gen (AnyMessageAndAgency ps)
  shrinkMessage :: PeerHasAgency pr st -> Message ps st st' -> [Message ps st st']

-- | A class for generating variations of a data type. morally, at least one variation of
-- each constructor should be generated.
class Variations a where
  variations :: NonEmpty a

-- | A class for generating variations of protocol messages.
class MessageVariations ps where
  messageVariations :: PeerHasAgency pr (st :: ps) -> NonEmpty (SomeMessage st)
  agencyVariations :: NonEmpty (SomePeerHasAgency ps)

instance MessageVariations ps => Variations (AnyMessageAndAgency ps) where
  variations = do
    SomePeerHasAgency tok <- agencyVariations
    SomeMessage msg <- messageVariations tok
    pure $ AnyMessageAndAgency tok msg

data SomePeerHasAgency ps = forall pr (st :: ps). SomePeerHasAgency (PeerHasAgency pr st)

genByteStringSplits :: ByteString -> Gen [ByteString]
genByteStringSplits bytes = oneof
  [ pure $ fromStrict <$> toChunks bytes
  , chunksByLength bytes <$> infiniteList
  ]
  where
    chunksByLength bs
      | LBS.null bs = const []
      | otherwise = \case
        [] -> [bs]
        len : lens ->
          let
            (chunk, bs') = LBS.splitAt len bs
          in
            chunk : chunksByLength bs' lens

data TextEnvelope = TextEnvelope
  { text :: Text
  , binary :: Text
  } deriving (Eq, Show, Read, Ord)

codecGoldenTests :: forall ps. (MessageVariations ps, ShowProtocol ps, BinaryMessage ps) => Spec
codecGoldenTests = do
  let Codec{..} = binaryCodec @Identity @ps
  for_ variations \(AnyMessageAndAgency tok message) -> do
    let text = T.pack $ showsPrecMessage 0 tok message ""
    let binary = TL.toStrict $ encodeBase16 $ encode tok message
    let output = TextEnvelope{..}
    it (T.unpack text) Golden
      { output
      , encodePretty = show
      , writeToFile = \path -> writeFile path . show
      , readFromFile = fmap read . readFile
      , goldenFile = goldenFile $ defaultGolden (T.unpack text) ""
      , actualFile = actualFile $ defaultGolden (T.unpack text) ""
      , failFirstTime = True
      }

checkPropCodec
  :: forall ps
   . (ArbitraryMessage ps, ShowProtocol ps, MessageEq ps, BinaryMessage ps)
  => Property
checkPropCodec = do
  let Codec{..} = binaryCodec @_ @ps
  forAllShrinkShow (arbitraryMessage @ps) shrinkAnyMessageAndAgency showAnyMessageAndAgency
    \(AnyMessageAndAgency agency msg) -> do
      let bytes = encode agency msg
      bytes' <- oneof [pure [bytes], genByteStringSplits bytes]
      let Identity r = runDecoder bytes' $ runIdentity $ decode agency
      pure case r of
        Left err -> counterexample (show err) failed
        Right (SomeMessage msg')
          | messageEq (AnyMessageAndAgency agency msg) (AnyMessageAndAgency agency msg') -> property succeeded
          | otherwise -> counterexample (showsPrecMessage 0 agency msg' "") failed

showAnyMessageAndAgency :: ShowProtocol ps => AnyMessageAndAgency ps -> String
showAnyMessageAndAgency (AnyMessageAndAgency agency msg) = "" &
  ( showString "AnyMessageAndAgency"
  . showSpace
  . showsPrecAgency 11 agency
  . showSpace
  . showsPrecMessage 11 agency msg
  )

shrinkAnyMessageAndAgency :: ArbitraryMessage ps => AnyMessageAndAgency ps -> [AnyMessageAndAgency ps]
shrinkAnyMessageAndAgency (AnyMessageAndAgency agency msg) =
  [ AnyMessageAndAgency agency msg' | msg' <- shrinkMessage agency msg ]

showsPrecAgency :: forall pr ps (st :: ps). ShowProtocol ps => Int -> PeerHasAgency pr st -> ShowS
showsPrecAgency p = showParen (p >= 11) . \case
  ClientAgency tok ->
    ( showString "ClientAgency"
    . showSpace
    . showsPrecClientHasAgency 11 tok
    )
  ServerAgency tok ->
    ( showString "ServerAgency"
    . showSpace
    . showsPrecServerHasAgency 11 tok
    )
