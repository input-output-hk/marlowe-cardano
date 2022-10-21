{-# LANGUAGE TemplateHaskell #-}

module Network.Protocol.SchemaVersion.TH
  ( mkSchemaVersion
  ) where

import Data.Hashable (Hashable(hash))
import qualified Data.Text as T
import Language.Haskell.TH
import Network.Protocol.SchemaVersion (SchemaVersion(..))

-- Template Haskell is used to reliably keep the schema version updated to
-- match the query type.
mkSchemaVersion :: String -> Name -> Q [Dec]
mkSchemaVersion declName schemaTypeName = do
  schemaTypeInfo <- reify schemaTypeName
  -- Expression: SomeSchemaVersion (T.pack "<hash of Move type>")
  let
    body = appE (conE 'SchemaVersion)
      $ parensE
      $ appE (varE 'T.pack)
      $ litE
      $ stringL
      $ "schema_" <> show (hash $ show schemaTypeInfo)
  sequence
    [ sigD (mkName declName) (appT (conT ''SchemaVersion) (conT schemaTypeName))
    , valD (varP (mkName declName)) (normalB body) []
    ]
