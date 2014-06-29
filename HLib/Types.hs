
module HLib.Types
( Zoldable
, zoldMap
) where

import Data.Monoid

class Zoldable z where
    zoldMap :: (Monoid m) => (z a -> m) -> z a -> m
