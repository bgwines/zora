{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Zora.TreeGraphing
-- Copyright   : (c) Brett Wines 2014
--
-- License     : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- A typeclass with default implementation for graphing trees with <https://hackage.haskell.org/package/graphviz Haskell GraphViz>.
--

module Zora.TreeGraphing
( Graphable
, value
, get_children
, is_empty
, graph
) where

import Zora.Types

import Data.Maybe
import Data.Tuple

import qualified Data.Map as M
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

-- | A typeclass for algebraic data types that are able to be graphed.
-- For these descriptions, assume the following example data type:
-- 
-- > data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
-- 
-- See the supporting file @dot.hs@ for an example of how to graph your data type.
class (Zoldable g) => Graphable g where
	-- | Gets the value contained in a node. For example,
	-- 
	-- > value (Empty) = error "Empty nodes don't contain values."
	-- > value (Leaf x) = x
	-- > value (Node x _ _) = x
	value :: g a -> a
	
	-- | Gets the children of the current node. For example,
	-- 
	-- > get_children (Empty) = error "Empty nodes don't have children."
	-- > get_children (Leaf _) = []
	-- > get_children (Node _ l r) = [l, r]
	get_children :: g a -> [g a]

	-- | Returns whether a node is empty. Sometimes, when declaring algebraic data types, it is desirable to have an "Empty" value constructor. If your data type does not have an "Empty" value constructor, just always return @False@.
	-- 
	-- > is_empty (Empty) = True
	-- > is_empty _ = False
	is_empty :: g a -> Bool

	-- TODO: Multiple trees (e.g. binomial heaps / random access lists)
	-- | Returns a @Graph@ for the given @Graphable@ type. You shouldn't need to override this implementation.
	graph :: forall a. (Show a, Ord a) => g a -> Graph
	graph g = (nodes, edges)
		where
			nodes :: [Node]
			nodes = zip [0..] $ map show' nodes_in_g

			show' :: g a -> Ly.Text
			show' = Ly.pack . show  . value

			nodes_in_g :: [g a]
			nodes_in_g = zoldMap (\a -> [a]) g

			edges :: [Edge]
			edges = concatMap edgeify nodes_in_g

			edgeify :: g a -> [Edge]
			edgeify node =
				catMaybes . map maybe_edge . get_children $ node
				where 
					maybe_edge :: g a -> Maybe Edge
					maybe_edge child = if is_empty child
						then Nothing
						else Just
							( m M.! (show' node)
							, m M.! (show' child)
							, Ly.empty )

					m :: M.Map Ly.Text Label
					m = M.fromList $ map swap nodes
