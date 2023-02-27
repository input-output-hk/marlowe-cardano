module Language.Marlowe.Runtime.Web.Server.Util
  where

import Data.Function (on)
import qualified Data.List as List
import Servant.Pagination (RangeOrder(..))

applyRangeToAscList :: Eq f => (a -> f) -> Maybe f -> Int -> Int -> RangeOrder -> [a] -> Maybe [a]
applyRangeToAscList getField startFrom limit offset order =
  fmap (List.take limit . List.drop offset)
    . case startFrom of
        Nothing -> Just
        Just f -> \as ->
          let
            as' = dropWhile ((/= f) . getField) as
          in
            if null as' then Nothing else Just as'
    . case order of
        RangeDesc -> reverse
        RangeAsc -> id
    . List.nubBy (on (==) getField)
