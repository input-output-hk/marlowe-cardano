{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.Common
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Api (ContractId(..))
import Test.QuickCheck hiding (shrinkMap)

instance Arbitrary ContractId where
  arbitrary = ContractId <$> arbitrary

genUTCTime :: Gen UTCTime
genUTCTime = posixSecondsToUTCTime . fromIntegral <$> arbitrary @Word64

shrinkMap :: (a -> [a]) -> Map k a -> [Map k a]
shrinkMap f = fmap Map.fromDistinctAscList . shrinkList (\(k, a) -> (k,) <$> f a) . Map.toAscList

shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrinkTuple shrinkA shrinkB (a, b) = [ (a', b) | a' <- shrinkA a ]
                                  <> [ (a, b') | b' <- shrinkB b ]
