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
-- A typeclass with default implementation for graphing trees with <https://hackage.haskell.org/package/graphviz Haskell GraphViz>. It is intended to be extremely straightforward to graph your data type; you only need to define one simple function (example implementation below).
--

module Zora.Graphing.DAGGraphing
( DAGGraphable
, render
, to_dotfile
, render_dotfile
, expand
) where

import Shelly (shelly, run_, setStdin)
import System.Directory (removeFile, getDirectoryContents)
import Control.Exception
import System.IO.Error hiding (catch)

import Data.Maybe
import Data.String (fromString)
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
class DAGGraphable g where
	-- | Expands a node into its show_node and children. For example, with the following example data type
	--
	-- > data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
	--
	-- , you might have the following definition:
	--
	-- > expand (Empty) = Nothing
	-- > expand (Leaf x) = Just (Just $ show x, [])
	-- > expand (Node x l r) = Just (Just $ show x, [("L child", l), ("R child", r)])
	expand :: g -> Maybe (Maybe String, [(Maybe String, g)])

-- | Returns whether a node is empty. Sometimes, when declaring algebraic data types, it is desirable to have an \"Empty\" show_node constructor. If your data type does not have an \"Empty\" show_node constructor, just always return @False@.
--
-- > is_empty Empty = True
-- > is_empty _ = False
is_empty :: (DAGGraphable g) => g -> Bool
is_empty = isNothing . expand

-- | Gets the contents of a node as a string, if it exists. For example,
--
-- > show_node (Empty) = error "Empty nodes don't contain values."
-- > show_node (Leaf x) = Just x
-- > show_node (Node x _ _) =
-- >     if x == 0
-- >         then Nothing
-- >         else Just x
show_node :: (DAGGraphable g) => g -> Maybe String
show_node node = if is_empty node
	then error "Zora implementation error. We shouldn't be calling this function for an empty node."
	else fst . fromJust . expand $ node

-- | Gets the children of the current node together with edge label information. For example,
--
-- > get_children (Empty) = error "Empty nodes don't have children."
-- > get_children (Leaf _) = []
-- > get_children (Node _ childrenMap) = map (show . fst) . toList $ childrenMap
get_children :: (DAGGraphable g) => g -> [(Maybe String, g)]
get_children node =
	if is_empty node
		then error "Zora implementation error. We shouldn't be calling this function for an empty node."
		else snd . fromJust . expand $ node

-- | Not exposed. Returns a @String@ to be put into a .dot file for the given @DAGGraphable@ type. You shouldn't need to override this implementation.
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
			where
				zoldMap :: (Monoid m, DAGGraphable g) => (g -> m) -> g -> m
				zoldMap f node =
					if is_empty node
						then mempty
						else (f node) `mappend` (mconcat . map (zoldMap f) . map snd . get_children $ node)

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
-- | Returns a @String@ to be put into a <http://en.wikipedia.org/wiki/DOT_(graph_description_language) DOT> file for the given @DAGGraphable@ type.
to_dotfile :: forall g. (Eq g, Show g, DAGGraphable g) => g -> String
to_dotfile
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

-- | Graphs the given string as if it were the contents of a <http://en.wikipedia.org/wiki/DOT_(graph_description_language) DOT> file. Output is written to the specified file. The first parameter is the outfile name, and the second is the contents of the dotfile.
render_dotfile :: String -> String -> IO ()
render_dotfile outfile_name dotfile = shelly $ do
  setStdin (Tx.pack dotfile)
  Shelly.run_ "dot" ["-Tpng", "-o", fromString outfile_name]


-- | Graphs the given @DAGGraphable@ data type to a PDF file. Output is written to the specified file. This is a convenience function that is the composition of @render_dotfile@ and @to_dotfile@.
render :: (Eq g, Show g, DAGGraphable g) => String -> g -> IO ()
render outfile_name = render_dotfile outfile_name . to_dotfile
