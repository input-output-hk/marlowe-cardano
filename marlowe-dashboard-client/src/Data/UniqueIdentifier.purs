module Data.UniqueIdentifier
  ( UniqueIdentifier
  , UUIDExpectation(..)
  , UUIDParseError
  , parse
  , toString
  , fromUUID
  , toUUID
  ) where

import Prologue

import Control.Monad.RWS (RWSResult(..), RWST(..), evalRWST)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Int (hexadecimal, toStringAs)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (charAt)
import Data.UUID.Argonaut (UUID)
import Data.UUID.Argonaut as UUID
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | A structure that encodes the cannonical textual representation of UUIDs as
-- | specified by RCF 4122. Consists of 16 octets represented as 32 hexidecimal
-- | digits displayed in five groups separated by hyphens. Only version 4,
-- | variant 1 UUIDs are supported
newtype UniqueIdentifier = UniqueIdentifier UniqueIdentifier'

derive instance Eq UniqueIdentifier
derive instance Ord UniqueIdentifier
derive newtype instance Bounded UniqueIdentifier
derive newtype instance Enum UniqueIdentifier
derive newtype instance BoundedEnum UniqueIdentifier
derive newtype instance Show UniqueIdentifier

fromUUID :: UUID -> Either UUIDParseError UniqueIdentifier
fromUUID = UUID.toString >>> parse

toUUID :: UniqueIdentifier -> UUID
toUUID = unsafeCoerce <<< toString

toString :: UniqueIdentifier -> String
toString = coerce case _ of
  Nil -> "00000000-0000-0000-0000-000000000000"
  V4 timeLow timeMid timeHi variant clockSeq node -> joinWith "-"
    [ case timeLow of
        TimeLow b3 b2 b1 b0 -> foldMap byteToString [ b3, b2, b1, b0 ]
    , case timeMid of
        TimeMid b1 b0 -> foldMap byteToString [ b1, b0 ]
    , fold
        [ "4"
        , case timeHi of
            TimeHi n b -> nibbleToString n <> byteToString b
        ]
    , fold
        [ variantToString variant
        , case clockSeq of
            ClockSequence n b -> nibbleToString n <> byteToString b
        ]
    , case node of
        Node b5 b4 b3 b2 b1 b0 ->
          foldMap byteToString [ b5, b4, b3, b2, b1, b0 ]
    ]

data UUIDExpectation
  = Hex
  | Hyphen
  | Version
  | Variant
  | Zero

derive instance Eq UUIDExpectation
derive instance Ord UUIDExpectation

type UUIDParseError =
  { position :: Int
  , expected :: UUIDExpectation
  }

parse :: String -> Either UUIDParseError UniqueIdentifier
parse input = fst <$> evalRWST parseRWST input (Tuple true 0)

parseRWST
  :: RWST
       String
       Unit
       (Tuple Boolean Int)
       (Either UUIDParseError)
       UniqueIdentifier
parseRWST = coerce do
  timeLow <- consumeTimeLow
  consumeHyphen
  timeMid <- consumeTimeMid
  consumeHyphen
  v <- consumeVersion
  if v then do
    timeHi <- consumeTimeHi
    consumeHyphen
    variant <- consumeVariant1
    clockSeq <- consumeClockSeq
    consumeHyphen
    node <- consumeNode
    pure $ V4 timeLow timeMid timeHi variant clockSeq node
  else do
    consumeZero
    consumeZero
    consumeZero
    consumeHyphen
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeHyphen
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    consumeZero
    pure Nil
  where
  consumeTimeLow = TimeLow
    <$> consumeByte
    <*> consumeByte
    <*> consumeByte
    <*> consumeByte
  consumeTimeMid = TimeMid <$> consumeByte <*> consumeByte
  consumeTimeHi = TimeHi <$> consumeNibble <*> consumeByte
  consumeClockSeq = ClockSequence <$> consumeNibble <*> consumeByte
  consumeNode = Node
    <$> consumeByte
    <*> consumeByte
    <*> consumeByte
    <*> consumeByte
    <*> consumeByte
    <*> consumeByte
  consumeByte = Byte <$> consumeNibble <*> consumeNibble
  consumeNibble = consume Hex $ coerce <<< case _ of
    '0' -> Just 0x0
    '1' -> Just 0x1
    '2' -> Just 0x2
    '3' -> Just 0x3
    '4' -> Just 0x4
    '5' -> Just 0x5
    '6' -> Just 0x6
    '7' -> Just 0x7
    '8' -> Just 0x8
    '9' -> Just 0x9
    'a' -> Just 0xa
    'b' -> Just 0xb
    'c' -> Just 0xc
    'd' -> Just 0xd
    'e' -> Just 0xe
    'f' -> Just 0xf
    _ -> Nothing
  consumeVariant1 = consume Variant $ coerce <<< case _ of
    '8' -> Just 0x8
    '9' -> Just 0x9
    'a' -> Just 0xa
    'b' -> Just 0xb
    _ -> Nothing
  consumeVersion = consume Version case _ of
    '0' -> Just false
    '4' -> Just true
    _ -> Nothing
  consumeZero = consume Zero case _ of
    '0' -> Just unit
    _ -> Nothing
  consumeHyphen = consume Hyphen case _ of
    '-' -> Just unit
    _ -> Nothing

  consume :: forall a. UUIDExpectation -> (Char -> Maybe a) -> RWST _ _ _ _ a
  consume expected f = RWST \input (Tuple allZero position) ->
    note ({ position, expected }) do
      char <- charAt position input
      RWSResult (Tuple (allZero && char == '0') (position + 1))
        <$> f char
        <*> pure unit

-- | Exported as a newtype so we can derive Generic without exporting the
-- | constructors
data UniqueIdentifier'
  = Nil
  | V4
      TimeLow -- low 32 bits of the time
      TimeMid -- middle 16 bits of the time
      TimeHi -- high 12 bits of the time
      Variant1 -- 2 bit "variant 1" sequence
      ClockSequence -- 12 bit clock sequence
      Node -- 48-bit node id

derive instance Generic UniqueIdentifier' _
derive instance Eq UniqueIdentifier'
derive instance Ord UniqueIdentifier'
instance Bounded UniqueIdentifier' where
  bottom = genericBottom
  top = genericTop

instance Enum UniqueIdentifier' where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum UniqueIdentifier' where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show UniqueIdentifier' where
  show = genericShow

data TimeLow = TimeLow Byte Byte Byte Byte

derive instance Generic TimeLow _
derive instance Eq TimeLow
derive instance Ord TimeLow
instance Bounded TimeLow where
  bottom = genericBottom
  top = genericTop

instance Enum TimeLow where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum TimeLow where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show TimeLow where
  show = genericShow

data TimeMid = TimeMid Byte Byte

derive instance Generic TimeMid _
derive instance Eq TimeMid
derive instance Ord TimeMid
instance Bounded TimeMid where
  bottom = genericBottom
  top = genericTop

instance Enum TimeMid where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum TimeMid where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show TimeMid where
  show = genericShow

data TimeHi = TimeHi Nibble Byte

derive instance Generic TimeHi _
derive instance Eq TimeHi
derive instance Ord TimeHi
instance Bounded TimeHi where
  bottom = genericBottom
  top = genericTop

instance Enum TimeHi where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum TimeHi where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show TimeHi where
  show = genericShow

newtype Variant1 = Variant1 Int

derive instance Generic Variant1 _
derive instance Eq Variant1
derive instance Ord Variant1
instance Bounded Variant1 where
  bottom = Variant1 0x8
  top = Variant1 0xb

instance Enum Variant1 where
  pred (Variant1 n) = toEnum $ n - 1
  succ (Variant1 n) = toEnum $ n + 1

instance BoundedEnum Variant1 where
  cardinality = Cardinality 4
  fromEnum = coerce
  toEnum i
    | i < 0x8 || i > 0xb = Nothing
    | otherwise = Just $ Variant1 i

instance Show Variant1 where
  show = genericShow

variantToString :: Variant1 -> String
variantToString (Variant1 i) = toStringAs hexadecimal i

data ClockSequence = ClockSequence Nibble Byte

derive instance Generic ClockSequence _
derive instance Eq ClockSequence
derive instance Ord ClockSequence
instance Bounded ClockSequence where
  bottom = genericBottom
  top = genericTop

instance Enum ClockSequence where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum ClockSequence where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show ClockSequence where
  show = genericShow

data Node = Node Byte Byte Byte Byte Byte Byte

derive instance Generic Node _
derive instance Eq Node
derive instance Ord Node
instance Bounded Node where
  bottom = genericBottom
  top = genericTop

instance Enum Node where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum Node where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show Node where
  show = genericShow

data Byte = Byte Nibble Nibble

derive instance Generic Byte _
derive instance Eq Byte
derive instance Ord Byte
instance Bounded Byte where
  bottom = genericBottom
  top = genericTop

instance Enum Byte where
  pred = genericPred
  succ = genericSucc

instance BoundedEnum Byte where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show Byte where
  show = genericShow

byteToString :: Byte -> String
byteToString (Byte n1 n0) = foldMap nibbleToString [ n1, n0 ]

newtype Nibble = Nibble Int

derive instance Generic Nibble _
derive instance Eq Nibble
derive instance Ord Nibble
instance Bounded Nibble where
  bottom = Nibble 0
  top = Nibble 15

instance Enum Nibble where
  pred (Nibble n) = toEnum $ n - 1
  succ (Nibble n) = toEnum $ n + 1

instance BoundedEnum Nibble where
  cardinality = Cardinality 16
  fromEnum = coerce
  toEnum i
    | i < 0 || i > 15 = Nothing
    | otherwise = Just $ Nibble i

instance Show Nibble where
  show = genericShow

nibbleToString :: Nibble -> String
nibbleToString (Nibble i) = toStringAs hexadecimal i
