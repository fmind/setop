-- |Implementation of Set Operations.

module Setop where

import Protolude
import qualified Data.Set as S

-- |Set construction.
fromList :: Ord a => [a] -> Set a
fromList = S.fromList

-- |Set Union on two sets.
--
-- >>> fromList [0, 1, 2] `union` fromList [0, 2, 4]
-- fromList [0,1,2,4]
-- >>> fromList [0, 2, 4] `union` fromList [0, 1, 2]
-- fromList [0,1,2,4]
union :: Ord a => Set a -> Set a -> Set a
union = S.union

-- |Set Difference on two sets.
--
-- >>> fromList [0, 1, 2] `difference` fromList [0, 2, 4]
-- fromList [1]
-- >>> fromList [0, 2, 4] `difference` fromList [0, 1, 2]
-- fromList [4]
difference :: Ord a => Set a -> Set a -> Set a
difference = S.difference

-- |Set Disjunction on two sets.
--
-- >>> fromList [0, 1, 2] `disjunction` fromList [0, 2, 4]
-- fromList [1,4]
-- >>> fromList [0, 2, 4] `disjunction` fromList [0, 1, 2]
-- fromList [1,4]
disjunction :: Ord a => Set a -> Set a -> Set a
disjunction a b = (a `difference` b) `union` (b `difference` a)

-- |Set Intersection on two sets.
--
-- >>> fromList [0, 1, 2] `intersection` fromList [0, 2, 4]
-- fromList [0,2]
-- >>> fromList [0, 2, 4] `intersection` fromList [0, 1, 2]
-- fromList [0,2]
intersection :: Ord a => Set a -> Set a -> Set a
intersection = S.intersection
