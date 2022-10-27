{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Transaction.Arbitrary
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary(arbitrary), Gen, oneof)

genByteString :: Gen ByteString
genByteString = BS.pack <$> arbitrary

genSet :: Ord a => Gen a -> Gen (Set a)
genSet genMember = do
  oneof
    [ pure mempty
    , do
        member <- genMember
        Set.insert member <$> genSet genMember
    ]

