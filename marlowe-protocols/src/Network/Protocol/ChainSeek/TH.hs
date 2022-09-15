{-# LANGUAGE TemplateHaskell #-}

module Network.Protocol.ChainSeek.TH (mkSchemaVersion) where

import Data.Hashable (Hashable (hash))
import qualified Data.Text as T
import Language.Haskell.TH
import Network.Protocol.ChainSeek.Types (SchemaVersion (..))

-- Template Haskell is used to reliably keep the schema version updated to
-- match the query type.
mkSchemaVersion :: String -> Name -> Q [Dec]
mkSchemaVersion declName queryTypeName = do
  queryTypeInfo <- reify queryTypeName
  -- Expression: SchemaVersion (T.pack "<hash of Move type>")
  let
    body = appE (conE 'SchemaVersion)
      $ parensE
      $ appE (varE 'T.pack)
      $ litE
      $ stringL
      $ "schema_" <> show (hash $ show queryTypeInfo)
  sequence
    [ sigD (mkName declName) $ conT ''SchemaVersion
    , valD (varP (mkName declName)) (normalB body) []
    ]
