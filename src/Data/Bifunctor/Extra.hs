module Data.Bifunctor.Extra
  ( module Data.Bifunctor
  , mapValues
  )
where

import Data.Bifunctor (Bifunctor(..))
import Data.Map (Map)

mapValues :: (b -> c) -> Map a b -> Map a c
mapValues f = map (second f)
