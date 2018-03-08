module Backend.Util where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- | General purpose function to append an element to an IntMap
--
-- In practice, the keys here would likely grow too large. For something
-- more scalable, we'd want to find an efficient way to find unused keys,
-- perhaps keeping an 'MVar [Int]' of available keys in our state, added to
-- when a user disconnects.
mapSnoc :: a -> IntMap a -> (IntMap a, Int) -- ^ The new map and new key
mapSnoc a xs = case IntMap.maxViewWithKey xs of
                 Just ((k, _), _) -> (IntMap.insert (succ k) a xs, succ k)
                 _                -> (IntMap.singleton 1 a, 1)
