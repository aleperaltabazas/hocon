module Data.Map
  ( groupBy
  , sortByKey
  , Map
  )
where

import Data.List (sortBy)

type Map k v = [(k, v)]

groupBy :: (Ord k, Eq k) => (v -> k) -> [v] -> Map k [v]
groupBy f vs = sortBy (\(k1, _) (k2, _) -> compare k1 k2) $ go f (vs, [])
 where
  go :: Eq k => (v -> k) -> ([v], Map k [v]) -> Map k [v]
  go _ ([], vs) = vs
  go f (v : vs, acc) =
    let
      append key value (k, v) = if key == k then (k, value : v) else (k, v)
      acc' = if any (\(k', _) -> k' == f v) acc then map (append (f v) v) acc else (f v, [v]) : acc
    in go f (vs, acc')

sortByKey :: (Ord k) => Map k v -> Map k v
sortByKey = sortBy (\a b -> compare (fst a) (fst b))
