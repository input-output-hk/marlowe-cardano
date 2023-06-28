module Data.Aeson.Applicative where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (..), Parser, parseFail)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))

data Schema
  = And Schema Schema
  | Or Schema Schema
  | Required Key
  | Optional Key
  | Empty
  deriving (Show, Eq, Ord)

newtype ObjectParser a = ObjectParser
  { runObjectParser :: (Schema, Object -> Maybe (Parser a))
  }

deriving instance Functor ObjectParser

instance Applicative ObjectParser where
  pure a = ObjectParser (Empty, const $ Just $ pure a)
  ObjectParser (schemaF, f) <*> ObjectParser (schemaA, a) =
    ObjectParser
      ( schemaF `And` schemaA
      , (liftA2 . liftA2) (<*>) f a
      )

instance Alternative ObjectParser where
  empty = ObjectParser (Empty, const Nothing)
  ObjectParser (schemaA, a) <|> ObjectParser (schemaB, b) =
    ObjectParser
      ( schemaA `Or` schemaB
      , \obj -> a obj <|> b obj
      )

instance (FromJSON a) => IsString (ObjectParser a) where
  fromString = required . fromString

required :: (FromJSON a) => Key -> ObjectParser a
required key = requiredExplicit key parseJSON

requiredExplicit :: Key -> (Value -> Parser a) -> ObjectParser a
requiredExplicit key parse =
  ObjectParser
    ( Required key
    , \obj -> do
        value <- KM.lookup key obj
        pure $ parse value <?> Key key
    )

optional :: (FromJSON a) => Key -> ObjectParser (Maybe a)
optional key = optionalExplicit key parseJSON

optionalExplicit :: Key -> (Value -> Parser a) -> ObjectParser (Maybe a)
optionalExplicit key parse =
  ObjectParser
    ( Optional key
    , \obj -> Just $ maybe (pure Nothing) (fmap Just . parse) $ KM.lookup key obj
    )

parseObject :: String -> ObjectParser a -> Value -> Parser a
parseObject name (ObjectParser (schema, p)) = withObject name $ fromMaybe failSchema . p
  where
    failSchema = parseFail $ "Object has invalid properties. Valid options are: " <> renderSchema schema

renderSchema :: Schema -> String
renderSchema = \case
  And s1 s2 -> "(" <> intercalate ", " (flattenAnd s1 s2) <> ")"
  Or Empty s2 -> renderSchema s2
  Or s1 Empty -> renderSchema s1
  Or s1 s2 -> renderSchema s1 <> " | " <> renderSchema s2
  Required key -> show key
  Optional key -> show key <> "?"
  Empty -> "none"

flattenAnd :: Schema -> Schema -> [String]
flattenAnd = \case
  And s1 s2 -> flattenAnd s1 . And s2
  Empty -> flip flattenAnd Empty
  s ->
    (renderSchema s :) . \case
      And s1 s2 -> flattenAnd s1 s2
      Empty -> []
      s' -> [renderSchema s']
