{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Zora.Graphing.DAGGraphing
-- Copyright   : (c) Brett Wines 2014
--
-- License	   : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- A typeclass with default implementation for graphing trees with <https://hackage.haskell.org/package/graphviz Haskell GraphViz>. It is intended to be extremely straightforward to graph your data type; you only need to define three very simple functions (example implementations below).
--

module Zora.Graphing.DAGGraphing
( DAGGraphable
, graph
, expand
, zoldMap
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
import qualified Data.Text as Tx
import qualified Data.Text.Lazy as Ly
import qualified Data.ByteString.Char8 as ByteString

--  hiding (Graph, Edge, Node)
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (show_node, Label)
import Data.Word

type Label = Int

-- | A typeclass for tree-like algebraic data types that are able to be graphed.
--
-- For these descriptions, assume the following example data type:
-- 
-- > data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
-- 
class DAGGraphable g where
	-- | Expands a node into its show_node and children. For example,
	-- 
	-- > expand (Empty) = Nothing
	-- > expand (Leaf x) = Just (x, [])
	-- > expand (Node x l r) = Just (x, [("L child", l), ("R child", r)])
	expand :: g -> Maybe (Maybe String, [(Maybe String, g)])

-- | Returns whether a node is empty. Sometimes, when declaringlgebraic data types, it is desirable to have an \"Empty\" show_node constructor. If your data type does not have an \"Empty\" show_node constructor, just always return @False@.
-- 
-- > is_empty Empty = True
-- > is_empty _ = False
is_empty :: (DAGGraphable g) => g -> Bool
is_empty = isNothing . expand

-- | Gets the show_node contained in a node. For example,
-- 
-- > show_node (Empty) = error "Empty nodes don't contain show_nodes."
-- > show_node (Leaf x) = Just x
-- > show_node (Node x _ _) =
-- >     if x == 0
-- >         then Nothing
-- >         else Just x
show_node :: (DAGGraphable g) => g -> Maybe String
show_node node = if is_empty node
	then error "DAGGraphable implementation error. We shouldn't be calling this function for an empty node."
	else fst . fromJust . expand $ node

-- | Gets the children of the current node together with edge label information. For example,
-- 
-- > get_children (Empty) = error "Empty nodes don't have children."
-- > get_children (Leaf _) = []
-- > get_children (Node _ childrenMap) = map (show . fst) . toList $ childrenMap
get_children :: (DAGGraphable g) => g -> [(Maybe String, g)]
get_children node =
	if is_empty node
		then error "DAGGraphable implementation error. We shouldn't be calling this function for an empty node."
		else snd . fromJust . expand $ node

-- | A default implementation of @zoldMap@ -- being instance of @DAGGraphable@ is enough to make your data type an instance of @Zoldable@ (in @Zora.Types@). You shouldn't need to override this implementation; just define
--
-- > instance Zoldable Tree where
-- > 	zoldMap :: (Monoid m) => (Tree a -> m) -> Tree a -> m
-- > 	zoldMap = G.zoldMap
--
zoldMap :: (Monoid m, DAGGraphable g) => (g -> m) -> g -> m
zoldMap f node =
	if is_empty node
		then mempty
		else (f node) `mappend` (mconcat . map (zoldMap f) . map snd . get_children $ node)

-- | Returns a @String@ to be put into a .dot file for the given @DAGGraphable@ type. You shouldn't need to override this implementation.
as_graph :: forall g. (Eq g, Show g, DAGGraphable g) => g -> ([LNode Ly.Text], [LEdge Ly.Text])
as_graph g = (nodes, edges)
	where
		nodes :: [LNode Ly.Text]
		nodes = zip [0..] $ map show' nodes_in_g

		show' :: g -> Ly.Text
		show' node =
			if isNothing . show_node $ node
				then Ly.empty
				else Ly.pack . fromJust . show_node $ node

		nodes_in_g :: [g]
		nodes_in_g
			= filter (not . is_empty)
			. L.nub
			. zoldMap (\a -> [a])
			$ g

		edges :: [LEdge Ly.Text]
		edges = concatMap edgeify nodes_in_g

		edgeify :: g -> [LEdge Ly.Text]
		edgeify node
			= map make_edge
			. filter (not . is_empty . snd)
			. get_children
			$ node
			where 
				make_edge :: (Maybe String, g) -> LEdge Ly.Text
				make_edge (str, child) =
					( get_label node
					, get_label child
					, if isNothing str then Ly.empty else (Ly.pack . fromJust $ str) )

				get_label :: g -> Label
				get_label node
					= snd
					. head
					. filter ((==) node . fst)
					$ zip nodes_in_g [0..]

-- TODO: Multiple trees (e.g. binomial heaps / random access lists)
-- | Returns a @String@ to be put into a @.dot@ file for the given @DAGGraphable@ type. You won't need to override this implementation.
as_dotfile :: forall g. (Eq g, Show g, DAGGraphable g) => g -> String
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
									, Width  0.1
									, Height 0.1 ] ]

-- | Graphs the given @DAGGraphable@ data type. Output is written to a file named \"graph-i.dot\", where /i/ is the successor of the highest /i/-show_node of all existing \"graph-i.dot\" files in the current directory.You won't need to override this implementation.
graph :: (Eq g, Show g, DAGGraphable g) => g -> IO String
graph g = do
	outfile_name <- calc_outfile_name
	write_dot_file
	run_dot_cmd
	remove_dot_file
	return ("Graphed data structure to " ++ outfile_name)
	where
		calc_outfile_name :: IO String
		calc_outfile_name = do
			index <- calc_index_of_graph_file
			return $ "graph-" ++ index ++ ".png"
			where
				calc_index_of_graph_file :: IO String
				calc_index_of_graph_file = do
					existing_graph_files_in_dir <- calc_existing_graph_files_in_dir
					return
						. show
						. (+) 1
						. (\s -> read s :: Integer)
						. takeWhile ((/=) '.')
						. drop 6 -- length "graph-"
						. last
						. L.sort
						. ((:) "graph--1")
						$ existing_graph_files_in_dir
				
				calc_existing_graph_files_in_dir :: IO [String]
				calc_existing_graph_files_in_dir = do
					directory_contents <- (getDirectoryContents "." :: IO [String])
					return . filter is_graph_file $ directory_contents
					where
						is_graph_file :: String -> Bool
						is_graph_file = starts_with "graph-"

						starts_with :: String -> String -> Bool
						starts_with prefix str = take (length prefix) str == prefix

		run_dot_cmd :: IO ()
		run_dot_cmd = do
			outfile_name <- calc_outfile_name
			shelly $ do cmd "dot" "-Tpng" "graph.dot" (Tx.pack $ "-o" ++ outfile_name)

		write_dot_file :: IO ()
		write_dot_file = do writeFile "graph.dot" $ as_dotfile g

		remove_dot_file :: IO ()
		remove_dot_file = removeFile "graph.dot" `catch` handleExists
			where
				handleExists e = if isDoesNotExistError e
					then return ()
					else throwIO e
