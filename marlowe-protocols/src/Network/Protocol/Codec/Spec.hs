{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}

module Network.Protocol.Codec.Spec where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, fromStrict, toChunks)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Data (Proxy)
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Internal as Map
import qualified Data.Set.Internal as Set
import Data.Set.NonEmpty.Internal
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time (Day, DiffTime, NominalDiffTime, UTCTime (..), secondsToDiffTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Type.Equality ((:~:) (Refl))
import Data.Version (Version)
import Data.Word
import GHC.Generics
import GHC.Real (Ratio ((:%)))
import GHC.Show (showSpace)
import Network.Protocol.Codec (BinaryMessage, binaryCodec)
import Network.TypedProtocol (PeerHasAgency, Protocol (..), SomeMessage (..))
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..), Codec (..), PeerHasAgency (..), runDecoder)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Golden (defaultGolden)
import Test.QuickCheck (Property, Testable (property), counterexample, forAllShrinkShow, infiniteList, oneof)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Property (failed, succeeded)

class ShowProtocol ps where
  showsPrecMessage :: Int -> PeerHasAgency pr st -> Message ps st st' -> ShowS
  default showsPrecMessage :: (Show (Message ps st st')) => Int -> PeerHasAgency pr st -> Message ps st st' -> ShowS
  showsPrecMessage p _ = showsPrec p

  showsPrecServerHasAgency :: forall (st :: ps). Int -> ServerHasAgency st -> ShowS
  default showsPrecServerHasAgency :: forall (st :: ps). (Show (ServerHasAgency st)) => Int -> ServerHasAgency st -> ShowS
  showsPrecServerHasAgency = showsPrec

  showsPrecClientHasAgency :: forall (st :: ps). Int -> ClientHasAgency st -> ShowS
  default showsPrecClientHasAgency :: forall (st :: ps). (Show (ClientHasAgency st)) => Int -> ClientHasAgency st -> ShowS
  showsPrecClientHasAgency = showsPrec

class TestMessageEquality ps where
  testMessageEquality
    :: PeerHasAgency pr a
    -> PeerHasAgency pr' a'
    -> Message ps a b
    -> Message ps a' b'
    -> Maybe (pr :~: pr', a :~: a', b :~: b')

class MessageEq ps where
  messageEq :: AnyMessageAndAgency ps -> AnyMessageAndAgency ps -> Bool
  default messageEq
    :: ( TestMessageEquality ps
       , forall st st'. Eq (Message ps st st')
       )
    => AnyMessageAndAgency ps
    -> AnyMessageAndAgency ps
    -> Bool
  messageEq (AnyMessageAndAgency tok msg) (AnyMessageAndAgency tok' msg') =
    case testMessageEquality tok tok' msg msg' of
      Nothing -> False
      Just (Refl, Refl, Refl) -> msg == msg'

class ArbitraryMessage ps where
  arbitraryMessage :: Gen (AnyMessageAndAgency ps)
  shrinkMessage :: PeerHasAgency pr st -> Message ps st st' -> [Message ps st st']

-- | A class for generating variations of a data type. morally, at least one variation of
-- each constructor should be generated.
--
-- Warning: do not use the generic implementation for recursive types - it
-- will generate an infinite list.
class Variations a where
  variations :: NonEmpty a
  default variations :: (Generic a, GVariations (Rep a)) => NonEmpty a
  variations = to <$> gVariations

instance Variations Version

instance (Variations a) => Variations [a] where
  variations = [] :| NE.toList (pure <$> variations)

instance (Variations a) => Variations (Set.Set a) where
  variations = Set.Tip :| NE.toList (Set.Bin 1 <$> variations <*> pure Set.Tip <*> pure Set.Tip)

instance (Variations k, Variations a) => Variations (Map.Map k a) where
  variations = Map.Tip :| NE.toList (Map.Bin 1 <$> variations `varyAp` variations <*> pure Map.Tip <*> pure Map.Tip)

instance (Variations a, Ord a) => Variations (NESet a) where
  variations = case NE.sort variations of
    a :| as -> NESet <$> (a :| as) `varyAp` (Set.Tip :| (Set.Bin 1 <$> as <*> pure Set.Tip <*> pure Set.Tip))

instance Variations UTCTime where
  variations = UTCTime <$> variations `varyAp` variations

instance Variations DiffTime where
  variations = secondsToDiffTime <$> variations

instance Variations NominalDiffTime where
  variations = secondsToNominalDiffTime <$> variations

instance (HasResolution a) => Variations (Fixed a) where
  variations = pure 1

instance Variations Day where
  variations = pure $ fromOrdinalDate 2000 1

instance (Variations a, Variations b) => Variations (a, b)

instance (Variations a) => Variations (Maybe a)

instance (Variations a, Variations b) => Variations (Either a b)

instance Variations Bool

instance Variations (Proxy a)

instance Variations ()

instance Variations Word8 where
  variations = pure 1

instance Variations Word16 where
  variations = pure 1

instance Variations Word32 where
  variations = pure 1

instance Variations Word64 where
  variations = pure 1

instance Variations Int8 where
  variations = pure 1

instance Variations Int16 where
  variations = pure 1

instance Variations Int32 where
  variations = pure 1

instance Variations Int64 where
  variations = pure 1

instance Variations Float where
  variations = pure 1

instance Variations Double where
  variations = pure 1

instance Variations Int where
  variations = pure 1

instance (Variations a) => Variations (Ratio a) where
  variations = (:%) <$> variations `varyAp` variations

instance Variations Char where
  variations = pure 'a'

instance Variations Natural where
  variations = pure 1

instance Variations Integer where
  variations = pure 1

instance Variations ByteString where
  variations = mempty :| ["a"]

instance Variations BS.ByteString where
  variations = mempty :| ["a"]

instance Variations Text where
  variations = mempty :| ["a"]

class GVariations f where
  gVariations :: NonEmpty (f a)

instance GVariations U1 where
  gVariations = pure U1

instance (GVariations f, GVariations g) => GVariations (f :+: g) where
  gVariations = (L1 <$> gVariations) <> (R1 <$> gVariations)

instance (GVariations f, GVariations g) => GVariations (f :*: g) where
  gVariations = (:*:) <$> gVariations `varyAp` gVariations

instance (Variations c) => GVariations (K1 i c) where
  gVariations = K1 <$> variations

instance (GVariations f) => GVariations (M1 i t f) where
  gVariations = M1 <$> gVariations

-- | A combinator that can be used like <*> except it doesn't take the
-- cartesian product. Instead, given a list of N elements and a list of M elements,
-- it produces a list of (1 - N + M) elements by applying the head of the first
-- list to the head of the second, then applying the head of the first list to
-- the tail of the second, and applying the tail of the second list to the head
-- of the first. Useful for defining `variations`.
varyAp :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
varyAp (f :| fs) (a :| as) =
  f a
    :| fold
      [ f <$> as
      , ($ a) <$> fs
      ]

infixl 4 `varyAp`

-- | A version of varyAp for optional arguments.
varyAp' :: NonEmpty (Maybe a -> b) -> [a] -> NonEmpty b
varyAp' (f :| fs) as =
  f Nothing
    :| fold
      [ f . Just <$> as
      , case as of
          a : _ -> ($ Just a) <$> fs
          _ -> []
      ]

infixl 4 `varyAp'`

-- | A class for generating variations of protocol messages.
class MessageVariations ps where
  messageVariations :: PeerHasAgency pr (st :: ps) -> NonEmpty (SomeMessage st)
  agencyVariations :: NonEmpty (SomePeerHasAgency ps)

instance (MessageVariations ps) => Variations (AnyMessageAndAgency ps) where
  variations = do
    SomePeerHasAgency tok <- agencyVariations
    SomeMessage msg <- messageVariations tok
    pure $ AnyMessageAndAgency tok msg

data SomePeerHasAgency ps = forall pr (st :: ps). SomePeerHasAgency (PeerHasAgency pr st)

genByteStringSplits :: ByteString -> Gen [ByteString]
genByteStringSplits bytes =
  oneof
    [ pure $ fromStrict <$> toChunks bytes
    , chunksByLength bytes <$> infiniteList
    ]
  where
    chunksByLength bs
      | LBS.null bs = const []
      | otherwise = \case
          [] -> [bs]
          len : lens ->
            let (chunk, bs') = LBS.splitAt len bs
             in chunk : chunksByLength bs' lens

codecGoldenTests :: forall ps. (MessageVariations ps, ShowProtocol ps, BinaryMessage ps) => String -> Spec
codecGoldenTests protocolName = describe "Message Golden Tests" do
  let Codec{..} = binaryCodec @Identity @ps
  it "Matches the golden output" $
    defaultGolden protocolName $
      unlines $
        concatMap (\(a, b) -> [a, b]) $
          sortOn
            fst
            do
              AnyMessageAndAgency tok message <- NE.toList variations
              pure
                ( "Show: " <> showsPrecMessage 0 tok message ""
                , "Binary: " <> TL.unpack (encodeBase16 $ encode tok message)
                )

checkPropCodec
  :: forall ps
   . (ArbitraryMessage ps, ShowProtocol ps, MessageEq ps, BinaryMessage ps)
  => Property
checkPropCodec = do
  let Codec{..} = binaryCodec @_ @ps
  forAllShrinkShow
    (arbitraryMessage @ps)
    shrinkAnyMessageAndAgency
    showAnyMessageAndAgency
    \(AnyMessageAndAgency agency msg) -> do
      let bytes = encode agency msg
      bytes' <- oneof [pure [bytes], genByteStringSplits bytes]
      let Identity r = runDecoder bytes' $ runIdentity $ decode agency
      pure case r of
        Left err -> counterexample (show err) failed
        Right (SomeMessage msg')
          | messageEq (AnyMessageAndAgency agency msg) (AnyMessageAndAgency agency msg') -> property succeeded
          | otherwise -> counterexample (showsPrecMessage 0 agency msg' "") failed

showAnyMessageAndAgency :: (ShowProtocol ps) => AnyMessageAndAgency ps -> String
showAnyMessageAndAgency (AnyMessageAndAgency agency msg) =
  ""
    & ( showString "AnyMessageAndAgency"
          . showSpace
          . showsPrecAgency 11 agency
          . showSpace
          . showsPrecMessage 11 agency msg
      )

shrinkAnyMessageAndAgency :: (ArbitraryMessage ps) => AnyMessageAndAgency ps -> [AnyMessageAndAgency ps]
shrinkAnyMessageAndAgency (AnyMessageAndAgency agency msg) =
  [AnyMessageAndAgency agency msg' | msg' <- shrinkMessage agency msg]

showsPrecAgency :: forall pr ps (st :: ps). (ShowProtocol ps) => Int -> PeerHasAgency pr st -> ShowS
showsPrecAgency p =
  showParen (p >= 11) . \case
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
