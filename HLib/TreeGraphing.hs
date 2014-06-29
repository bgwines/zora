{-# LANGUAGE ScopedTypeVariables #-}

module HLib.TreeGraphing
( Graphable
, value
, get_children
, is_empty
, graph
) where

import HLib.Types

import Data.Maybe
import Data.Tuple

import qualified Data.Map as M
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

type Graph = ([Node], [Edge])
type Node = (Int, Ly.Text)
type Edge = (Int, Int, Ly.Text)
type Label = Int

class (Zoldable g) => Graphable g where
	value :: g a -> a
	
	get_children :: g a -> [g a]

	is_empty :: g a -> Bool

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
