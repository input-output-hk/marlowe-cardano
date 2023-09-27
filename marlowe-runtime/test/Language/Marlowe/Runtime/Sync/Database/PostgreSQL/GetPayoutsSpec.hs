module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayoutsSpec where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Hasql.Statement (Statement (..))
import Language.Marlowe.Protocol.Query.Types (Order (..), PayoutFilter (..), Range (..))
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), TxId (..), TxOutRef (..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = describe "SQL queries" $ for_ (Set.filter removeDuplicateWithdrawnSets $ Set.powerSet $ Set.fromList [minBound .. maxBound]) \modifications -> do
  let name = show $ Set.toList modifications
  let (pFilter, range) = modificationsToPayoutFilterAndRange modifications
  it ("Matches golden test for " <> name) do
    defaultGolden ("GetPayoutsSQL" <> name) $
      unlines $
        catMaybes
          [ do
              startFrom <- rangeStart range
              pure $ showStatement "Delimiter statement" $ delimiterStatement pFilter startFrom
          , Just $ showStatement "Total count statement" $ totalCountStatement pFilter
          , Just $ showStatement "Payouts statement" $ payoutsStatement pFilter range do
              TxOutRef{..} <- rangeStart range
              pure $ DelimiterRow (unTxId txId) (fromIntegral txIx) 0
          ]

showStatement :: String -> Statement a b -> String
showStatement title (Statement sql _ _ _) = title <> ": " <> T.unpack (decodeUtf8 sql)

removeDuplicateWithdrawnSets :: Set QueryModifications -> Bool
removeDuplicateWithdrawnSets mods = not $ Set.member Available mods && Set.member Withdrawn mods

modificationsToPayoutFilterAndRange :: Set QueryModifications -> (PayoutFilter, Range TxOutRef)
modificationsToPayoutFilterAndRange = foldr modifyPayoutFilterAndRange defaultPayoutAndRange

modifyPayoutFilterAndRange :: QueryModifications -> (PayoutFilter, Range TxOutRef) -> (PayoutFilter, Range TxOutRef)
modifyPayoutFilterAndRange = \case
  Available -> first \pFilter -> pFilter{isWithdrawn = Just False}
  Withdrawn -> first \pFilter -> pFilter{isWithdrawn = Just True}
  ContractId -> first \pFilter -> pFilter{contractIds = Set.singleton "#1"}
  RoleToken -> first \pFilter -> pFilter{roleTokens = Set.singleton $ AssetId "" ""}
  RangeStart -> fmap \range -> range{rangeStart = Just "#2"}
  RangeAscending -> fmap \range -> range{rangeDirection = Ascending}

defaultPayoutAndRange :: (PayoutFilter, Range TxOutRef)
defaultPayoutAndRange =
  ( PayoutFilter Nothing mempty mempty
  , Range Nothing 0 1 Descending
  )

data QueryModifications
  = Available
  | Withdrawn
  | ContractId
  | RoleToken
  | RangeStart
  | RangeAscending
  deriving (Eq, Show, Ord, Enum, Bounded)
