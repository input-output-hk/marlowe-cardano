module Data.Aeson.Applicative where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (..), Parser, parseFail)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))

-- | A summary of the valid keys for an object's schema.
data Schema
  = -- | A schema in which both sub-schemas must hold.
    And Schema Schema
  | -- | A schema in which one of the sub-schemas must hold.
    Or Schema Schema
  | -- | A schema which the given key must be present.
    Required Key
  | -- | A schema which the given key may be present.
    Optional Key
  | -- | A schema which always holds
    Always
  | -- | A schema which never holds
    Never
  deriving (Show, Eq, Ord)

-- | A parser for JSON objects.
newtype ObjectParser a = ObjectParser
  { runObjectParser :: (Schema, Object -> Maybe (Parser a))
  }

deriving instance Functor ObjectParser

instance Applicative ObjectParser where
  pure a = ObjectParser (Always, const $ Just $ pure a)
  ObjectParser (schemaF, f) <*> ObjectParser (schemaA, a) =
    ObjectParser
      ( schemaF `And` schemaA
      , (liftA2 . liftA2) (<*>) f a
      )

instance Alternative ObjectParser where
  empty = ObjectParser (Never, const Nothing)
  ObjectParser (schemaA, a) <|> ObjectParser (schemaB, b) =
    ObjectParser
      ( schemaA `Or` schemaB
      , \obj -> a obj <|> b obj
      )

-- | fromString = required . fromString (short hand for required)
instance (FromJSON a) => IsString (ObjectParser a) where
  fromString = required . fromString

-- | A parser which requires a key be present in the object and parses its value via a FromJSON instance.
required :: (FromJSON a) => Key -> ObjectParser a
required key = requiredExplicit key parseJSON

-- | A parser which requires a key be present in the object and parses its value via a custom parsing function.
requiredExplicit :: Key -> (Value -> Parser a) -> ObjectParser a
requiredExplicit key parse =
  ObjectParser
    ( Required key
    , \obj -> do
        value <- KM.lookup key obj
        pure $ parse value <?> Key key
    )

-- | A parser which parses the value for a key if it is present in the object or else produces `Nothing`.
optional :: (FromJSON a) => Key -> ObjectParser (Maybe a)
optional key = optionalExplicit key parseJSON

-- | A parser which parses the value for a key if it is present in the object or else produces `Nothing`.
optionalExplicit :: Key -> (Value -> Parser a) -> ObjectParser (Maybe a)
optionalExplicit key parse =
  ObjectParser
    ( Optional key
    , \obj -> Just $ maybe (pure Nothing) (fmap Just . parse) $ KM.lookup key obj
    )

-- | Run an objet parser on a JSON object.
parseObject :: String -> ObjectParser a -> Value -> Parser a
parseObject name (ObjectParser (schema, p)) = withObject name $ fromMaybe failSchema . p
  where
    failSchema = parseFail $ "Object has invalid properties. Valid options are: " <> renderSchema schema

renderSchema :: Schema -> String
renderSchema = \case
  And s1 s2 -> "(" <> intercalate ", " (flattenAnd s1 s2) <> ")"
  Or Always s2 -> renderSchema s2
  Or Never s2 -> renderSchema s2
  Or s1 Always -> renderSchema s1
  Or s1 Never -> renderSchema s1
  Or s1 s2 -> renderSchema s1 <> " | " <> renderSchema s2
  Required key -> show key
  Optional key -> show key <> "?"
  Always -> "anything"
  Never -> "none"

flattenAnd :: Schema -> Schema -> [String]
flattenAnd = \case
  And s1 s2 -> flattenAnd s1 . And s2
  Always -> flip flattenAnd Always
  s ->
    (renderSchema s :) . \case
      And s1 s2 -> flattenAnd s1 s2
      Always -> []
      s' -> [renderSchema s']
