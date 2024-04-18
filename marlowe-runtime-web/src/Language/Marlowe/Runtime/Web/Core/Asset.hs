module Language.Marlowe.Runtime.Web.Core.Asset (
  Assets (..),
  Tokens (..),
  AssetId (..),
  PolicyId (..),
) where

import Control.DeepSeq (NFData)
import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToParamSchema,
  ToSchema,
  pattern,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

data Assets = Assets
  { lovelace :: Integer
  , tokens :: Tokens
  }
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Tokens = Tokens {unTokens :: Map PolicyId (Map Text Integer)}
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance Semigroup Tokens where
  Tokens a <> Tokens b = Tokens $ Map.unionWith (Map.unionWith (+)) a b

instance Monoid Tokens where
  mempty = Tokens mempty

newtype PolicyId = PolicyId {unPolicyId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, FromHttpApiData, ToJSON, ToJSONKey, FromJSON, FromJSONKey) via Base16

instance NFData PolicyId

instance ToSchema PolicyId where
  declareNamedSchema proxy = pure $ NamedSchema (Just "PolicyId") $ toParamSchema proxy

instance ToParamSchema PolicyId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded minting policy ID for a native Cardano token"
      & pattern ?~ "^[a-fA-F0-9]*$"

data AssetId = AssetId
  { policyId :: PolicyId
  , assetName :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToSchema AssetId
instance FromJSON AssetId
instance ToJSON AssetId

instance ToParamSchema AssetId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description
        ?~ "A minting policy ID and a token name identifying a specific asset type. Encoded as policyId.tokenName."
      & pattern ?~ "^[a-fA-F0-9]*\\..*$"

instance FromHttpApiData AssetId where
  parseUrlPiece piece = case T.breakOn "." piece of
    (_, "") -> Left "Expected ^[a-fA-F0-9]*(\\.).*$"
    (policyId, tokenNameStartingWitPeriodCharacter) -> AssetId <$> parseUrlPiece policyId <*> parseUrlPiece (T.drop 1 tokenNameStartingWitPeriodCharacter)

instance ToHttpApiData AssetId where
  toUrlPiece AssetId{..} = toUrlPiece policyId <> "." <> toUrlPiece assetName
