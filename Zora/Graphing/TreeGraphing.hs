{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Zora.Graphing.TreeGraphing
-- Copyright   : (c) Brett Wines 2014
--
-- License	   : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- [DEPRECATED; use @Zora.Graphing.DAGGraphing@ instead]
-- A typeclass with default implementation for graphing trees with <https://hackage.haskell.org/package/graphviz Haskell GraphViz>. It is intended to be extremely straightforward to graph your data type; you only need to define three very simple functions (example implementations below).
--

module Zora.Graphing.TreeGraphing {-# DEPRECATED "Use Zora.Graphing.DAGGraphing instead" #-}
( TreeGraphable
, value
, get_children
, is_empty
, graph
) where

import Shelly
import System.Directory (removeFile, getDirectoryContents)
import Control.Exception
import System.IO.Error hiding (catch)

import System.IO.Unsafe

import Data.Maybe
import Data.Tuple

import Data.Monoid

import qualified Data.Map as M
import qualified Data.List as L hiding (zip, map, length, take, drop, takeWhile, last, filter, concatMap)
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

--  hiding (Graph, Edge, Node)
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (value, Label)
import Data.Word

type Label = Int

-- | A typeclass for algebraic data types that are able to be graphed.
--
-- For these descriptions, assume the following example data type:
-- 
-- > data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
-- 
class TreeGraphable g where
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

	-- | Returns whether a node is empty. Sometimes, when declaring algebraic data types, it is desirable to have an \"Empty\" value constructor. If your data type does not have an \"Empty\" value constructor, just always return @False@.
	-- 
	-- > is_empty Empty = True
	-- > is_empty _ = False
	is_empty :: g a -> Bool

zoldMap :: (Monoid m, TreeGraphable g) => (g a -> m) -> g a -> m
zoldMap f node =
	if is_empty node
		then mempty
		else (f node) `mappend` (mconcat . map (zoldMap f) . get_children $ node)

-- | Returns a @String@ to be put into a @.dot@ file for the given @Graphable@ type. You shouldn't need to override this implementation.
as_graph :: forall a g. (Ord a, Show a, TreeGraphable g) => g a -> ([LNode Ly.Text], [LEdge Ly.Text])
as_graph g = (nodes, edges)
	where
		nodes :: [LNode Ly.Text]
		nodes = zip [0..] $ map show' nodes_in_g

		show' :: g a -> Ly.Text
		show' = Ly.pack . show  . value

		nodes_in_g :: [g a]
		nodes_in_g = zoldMap (\a -> [a]) g

		edges :: [LEdge Ly.Text]
		edges = concatMap edgeify nodes_in_g

		edgeify :: g a -> [LEdge Ly.Text]
		edgeify node =
			catMaybes . map maybe_edge . get_children $ node
			where 
				maybe_edge :: g a -> Maybe (LEdge Ly.Text)
				maybe_edge child = if is_empty child
					then Nothing
					else Just
						( m M.! (show' node)
						, m M.! (show' child)
						, Ly.empty )

				m :: M.Map Ly.Text Label
				m = M.fromList $ map swap nodes

-- | Returns a @String@ to be put into a @.dot@ file for the given @Graphable@ type. You won't need to override this implementation.
as_dotfile :: forall a g. (Show a, Ord a, TreeGraphable g) => g a -> String
as_dotfile
	= Ly.unpack
	. printDotGraph
	. graphToDot params
	. mkGraph'
	. as_graph
	where
		mkGraph' :: ([LNode Ly.Text], [LEdge Ly.Text]) -> (Gr Ly.Text Ly.Text)
		mkGraph' (v, e) = mkGraph v e

		params :: GraphvizParams n Ly.Text Ly.Text () Ly.Text
		params = nonClusteredParams { globalAttributes = ga
									, fmtNode = fn
									, fmtEdge = fe }
			where
				fn (_,l) = [textLabel l]
				fe (_,_,l) = [textLabel l]

				ga = [ GraphAttrs [ RankDir	 FromTop
								  , BgColor	 [toWColor White] ]
					 , NodeAttrs	[ shape	 BoxShape
									-- , FillColor (some_color 4)
									-- , style	 filled
									, Width	 0.1
									, Height	0.1 ] ]

-- | Graphs the given @TreeGraphable@ data type. Creates and writes to a file named \"graph.png\", overwriting any existing files with that name. You won't need to override this implementation.
graph :: (Show a, Ord a, TreeGraphable g) => g a -> IO String
graph g =
	let
		outfile :: String
		outfile = "graph-" ++ index ++ ".png"
			where
				index :: String
				index
					= show
					. (+) 1
					. (\s -> read s :: Integer)
					. takeWhile (/= '.')
					. drop 6 -- (length "graph-")
					. last
					$ "graph--1" : graph_files_in_dir

				files_in_dir :: IO [String]
				files_in_dir = getDirectoryContents "." :: IO [String]
				
				graph_files_in_dir :: [String]
				graph_files_in_dir
					= L.sort
					. filter (starts_with "graph-")
					. filter ((==) "graph.png")
					. unsafePerformIO -- TODO: not this
					$ files_in_dir

				starts_with :: String -> String -> Bool
				starts_with prefix str = take (length prefix) str == prefix

		run_dot_cmd :: IO ()
		run_dot_cmd = shelly $ do
			cmd "dot" "-Tpng" "graph.dot" "-ograph.png"--("-o" ++ outfile) -- Can't get this to compile. Not sure why yet. For now, we always write to the same file.

		write_dot_file :: IO ()
		write_dot_file = do
			writeFile "graph.dot" $ as_dotfile g

		remove_dot_file :: IO ()
		remove_dot_file = removeFile "graph.dot" `catch` handleExists
			where
				handleExists e
					| isDoesNotExistError e = return ()
					| otherwise = throwIO e
	in
		do
			write_dot_file
			run_dot_cmd
			remove_dot_file
			return ("Graphed data structure to " ++ "graph.png") -- outfile
