module Utility.Set where

import Data.Set (Set, toList )
import Data.Map (Map, fromListWith)
import Data.Ord

multiPartition f s = fromListWith (flip (++)) $ zip keys vals
    where keys = map f elements
          vals = map return elements
          elements = toList s

andThen :: (Ord a) => (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
comp1 `andThen` comp2 = \a b -> if comp1 a b == EQ then
                                    comp2 a b
                                else
                                    comp1 a b

first (a, b, c) = a
second (a, b, c) = b
third (a, b, c) = c

x = (1, 1, 1)
y = (1, 1, 2)
z = (1, 2, 1)


orderTwo :: (Ord a, Ord b, Ord c) => (a, b, c) -> (a, b, c) -> Ordering
orderTwo = (comparing first) `andThen` (comparing second) `andThen` (comparing third)
