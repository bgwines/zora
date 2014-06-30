-- |
-- Module      : Zora.Types
-- Copyright   : (c) Brett Wines 2014
--
-- License     : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- Assorted types and typeclasses
--
module Zora.Types
( Zoldable
, zoldMap
) where

import Data.Monoid

-- | `Zora.Zoldable` is much like `Data.Foldable`, but with a crucial difference:
-- 
-- > foldMap :: (Foldable t, Monoid m) => (  a -> m) -> t a -> m
-- > zoldMap :: (Zoldable t, Monoid m) => (t a -> m) -> t a -> m
-- 
-- It is an augmented form -- @foldMap f t@ is @zoldMap (f . g) t@ where @g :: t a -> a@. With @foldMap@, you lose information that you have at the time of invocation of @f@: the @t a@; the context in which the @a@ is enclosed is discarded. Consider the following situation: you have some tree type, e.g.
-- 
-- > data Tree a = Leaf a | Node a (Tree a) (Tree a)
-- 
-- Suppose you want to get a list of all the nodes in the tree. This is just @zoldMap (\x -> [x]) tree@.
class Zoldable z where
    zoldMap :: (Monoid m) => (z a -> m) -> z a -> m
