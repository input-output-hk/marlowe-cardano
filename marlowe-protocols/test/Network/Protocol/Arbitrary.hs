{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol.Arbitrary
  where

import Data.Text (Text)
import qualified Data.Text as T
import Network.Protocol.SchemaVersion (SchemaVersion(SchemaVersion))
import Test.QuickCheck (Arbitrary(arbitrary), Gen)

genText :: Gen Text
genText = T.pack <$> arbitrary

genSchemaVersion :: Gen (SchemaVersion cmd)
genSchemaVersion = SchemaVersion <$> genText

