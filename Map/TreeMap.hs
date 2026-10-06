{-# LANGUAGE InstanceSigs #-}
module Map.TreeMap (
  Map, empty, insert, insertOrMerge, fromList, toList, fromListMerge, lookup
) where
import Data.List (intercalate)
import Prelude hiding (lookup)

-- The Map data type to map keys k to values v.
data Map k v 
    = Empty 
    | Branch (Map k v) k v (Map k v)
    deriving Eq


-- Creates an empty Map.
empty :: Ord k => Map k v 
empty = Empty


-- Inserts the given key-value pair into the given Map.
-- Smaller keys go to the left, larger keys to the right.
-- If there is already an entry with the given key, 
-- the original value is replaced with the new value.
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert key val Empty = Branch empty key val empty
insert key val (Branch smaller k v larger) | key < k = Branch (insert key val smaller) k v larger
                                           | key > k = Branch smaller k v (insert key val larger)
                                           | otherwise = Branch smaller key val larger


-- Returns the value stored under the given key k.
-- Spec: get k (insert k v m) == Just v
lookup :: Ord k => k -> Map k v -> Maybe v
lookup _   Empty = Nothing
lookup key (Branch smaller k v larger) | key < k = lookup key smaller
                                       | key > k = lookup key larger
                                       | otherwise = Just v



-- Inserts the given key-value pair into the given Map.
-- If there is already an entry with the given key,
-- the Semigroup of the values is used to combine the existing value
-- with the new value.
insertOrMerge :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insertOrMerge key val Empty = Branch empty key val empty
insertOrMerge key val (Branch smaller k v larger) | key < k = Branch (insertOrMerge key val smaller) k v larger
                                           | key > k = Branch smaller k v (insertOrMerge key val larger)
                                           | otherwise = Branch smaller key (v <> val) larger
  


-- Creates a Map from a list of pairs.
-- Use foldr or foldl and the insert function to implement fromList.
-- When inserting mappings with the same key, the last one should overwrite previous ones.
-- E.g. the Map created using `fromList [("a", 1), ("a", 2)]` should only contain `("a", 2)`.
fromList :: Ord k => [(k,v)] -> Map k v
fromList = foldl (flip (uncurry insert)) empty



-- Creates a Map from a list of pairs.
-- Use foldr and the insertOrMerge function to implement fromListMerge.
fromListMerge :: (Ord k, Semigroup v) => [(k,v)] -> Map k v
fromListMerge = foldr (uncurry insertOrMerge) empty


-- Returns the list of mappings.
toList :: Map k v -> [(k,v)]
toList Empty = []
toList (Branch smaller k v larger) = toList smaller ++ [(k, v)] ++ toList larger


-- Produces a pretty formatted list of mappings:
-- k1 -> v1
-- k2 -> v2
-- ... 
-- Hint: Using toList and Data.List.intercalate can simplify this task.
instance (Show k, Show v) => Show (Map k v) where
  show :: Map k v -> String
  show Empty = []
  show branch = intercalate "\n" $ map (\(k,v) -> show k ++ " -> " ++ show v) (toList branch)


-- Two Maps can be combined if the keys are equatable
-- and if there is a Semigroup instance for the values.
-- For entries with the same keys, the values should be combined using (<>).
-- Again, use the insertOrMerge function.
instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  (<>) :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
  (<>) m1 m2 = foldr (uncurry insertOrMerge) m1 (toList m2)


-- Creates an empty Map (easy). 
instance (Ord k, Semigroup v) => Monoid (Map k v) where
  mempty :: (Ord k, Semigroup v) => Map k v
  mempty = empty
