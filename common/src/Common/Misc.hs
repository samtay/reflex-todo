module Common.Misc where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map    (Map)
import qualified Data.Map    as Map

mapSnoc :: a -> Map Int a -> (Int, Map Int a)
mapSnoc a xs = case Map.maxViewWithKey xs of
  Just ((k, _), _) -> (succ k, Map.insert (succ k) a xs)
  _                -> (0     , Map.singleton 0 a)

intMapSnoc :: a -> IntMap a -> (Int, IntMap a)
intMapSnoc a xs = case IntMap.maxViewWithKey xs of
  Just ((k, _), _) -> (succ k, IntMap.insert (succ k) a xs)
  _                -> (0     , IntMap.singleton 0 a)
