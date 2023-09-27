module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeadersSpec where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Hasql.Statement (Statement (..))
import Language.Marlowe.Protocol.Query.Types (ContractFilter (..), Order (..), Range (..))
import Language.Marlowe.Runtime.ChainSync.Api (Address (..), AssetId (..), TxId (..), TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweMetadataTag (..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = describe "SQL queries" $ for_ (Set.powerSet $ Set.fromList [minBound .. maxBound]) \modifications -> do
  let name = show $ Set.toList modifications
  let (cFilter, range) = modificationsToContractFilterAndRange modifications
  it ("Matches golden test for " <> name) do
    defaultGolden ("GetHeadersSQL" <> name) $
      unlines $
        catMaybes
          [ do
              startFrom <- rangeStart range
              pure $ showStatement "Delimiter statement" $ delimiterStatement cFilter startFrom
          , Just $ showStatement "Total count statement" $ totalCountStatement cFilter
          , Just $ showStatement "Headers statement" $ headersStatement cFilter range do
              ContractId TxOutRef{..} <- rangeStart range
              pure $ DelimiterRow (unTxId txId) (fromIntegral txIx) 0
          ]

showStatement :: String -> Statement a b -> String
showStatement title (Statement sql _ _ _) = title <> ": " <> T.unpack (decodeUtf8 sql)

modificationsToContractFilterAndRange :: Set QueryModifications -> (ContractFilter, Range ContractId)
modificationsToContractFilterAndRange = foldr modifyContractFilterAndRange defaultContractAndRange

modifyContractFilterAndRange
  :: QueryModifications -> (ContractFilter, Range ContractId) -> (ContractFilter, Range ContractId)
modifyContractFilterAndRange = \case
  RolesCurrency -> first \cFilter -> cFilter{roleCurrencies = Set.singleton ""}
  Tag -> first \cFilter -> cFilter{tags = Set.singleton $ MarloweMetadataTag ""}
  PartyRole -> first \cFilter -> cFilter{partyRoles = Set.singleton $ AssetId "" ""}
  PartyAddress -> first \cFilter -> cFilter{partyAddresses = Set.singleton $ Address ""}
  RangeStart -> fmap \range -> range{rangeStart = Just "#2"}
  RangeAscending -> fmap \range -> range{rangeDirection = Ascending}

defaultContractAndRange :: (ContractFilter, Range ContractId)
defaultContractAndRange =
  ( ContractFilter mempty mempty mempty mempty
  , Range Nothing 0 1 Descending
  )

data QueryModifications
  = RolesCurrency
  | Tag
  | PartyRole
  | PartyAddress
  | RangeStart
  | RangeAscending
  deriving (Eq, Show, Ord, Enum, Bounded)
