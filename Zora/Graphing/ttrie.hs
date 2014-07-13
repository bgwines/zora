{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module TTrie
( empty
, insert
, remove
, contains_contiguous
, contains_noncontiguous
, all_matches
, merge
, is_empty
, fromList
) where

-- works
--     empty
--     insert
--     is_empty
--     fromList
--     contains_contiguous
-- TODO
--     remove
--     contains_noncontiguous (["abc", "ade"] / "adb")
--     all_matches
--     merge


{- TODO -}

import Test.QuickCheck
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Array as A
import Data.Monoid
import Data.Tuple
import Data.Maybe

import Zora.List hiding (merge)
import qualified Zora.Graphing.DAGGraphing as G

import Control.Applicative hiding (empty)

type Index = Int

type ChildrenMap = M.Map Char TTrie
type TransitiveChildrenMap = M.Map Char [TTrie]
type TransitiveChildrenMapUpdateInfo = [(Char, TTrie, TTrie)]

data TTrie = Empty | Node (Maybe String) ChildrenMap TransitiveChildrenMap deriving Show

{-instance (Show a) => Show (TTrie) where
	show :: TTrie -> String
	show Empty = "An empty pairing heap."
	show h = "Heap " ++ (show $ elems h)
-}

instance G.DAGGraphable TTrie where
	expand :: TTrie -> Maybe (Maybe String, [(Maybe String, TTrie)])
	expand Empty = Nothing
	expand (Node word cmap trans_cmap) = Just (label, edges)
		where
			label :: Maybe String
			label = fmap (const "•") word

			edges :: [(Maybe String, TTrie)]
			edges = (++)
				(map (show' "") (M.toList cmap))
				(map (show' "*") (concat' . M.toList $ trans_cmap))

			concat' :: [(Char, [TTrie])] -> [(Char, TTrie)]
			concat' = concatMap (\(ch, ts) -> map (\t -> (ch, t)) $ ts)

			show' :: String -> (Char, b) -> (Maybe String, b)
			show' s (a, b) = (Just (a : s), b)

instance Monoid TTrie where
	mempty :: TTrie
	mempty = Empty

	mappend :: TTrie -> TTrie -> TTrie
	mappend = merge

instance Eq TTrie where
	Empty == Empty = True
	a     == Empty = False
	Empty == b     = False
	(==)
		(Node word  cmap  trans_cmap)
		(Node word' cmap' trans_cmap')
		= and
		[ word == word'
		, cmap == cmap'
		, trans_cmap == trans_cmap' ]

fromList :: [String] -> TTrie
fromList = L.foldl' insert empty . L.nub

empty :: TTrie
empty = Empty

is_empty :: TTrie -> Bool
is_empty Empty = True
is_empty _ = False

merge :: TTrie -> TTrie -> TTrie
merge a Empty = a
merge Empty b = b
{-merge
	t@(Node is_word cmap trans_cmap)
	t'@(Node is_word' cmap' trans_cmap')
	= Node is_word'' cmap'' trans_cmap''
	where
		is_word'' :: Bool
		is_word'' = is_word || is_word'

		cmap'' :: ChildrenMap
		cmap'' = M.unionWith merge cmap cmap'

		trans_cmap'' :: TransitiveChildrenMap
		trans_cmap'' = error "TODO"-}

update_trans_cmap :: TransitiveChildrenMapUpdateInfo -> TransitiveChildrenMap -> TransitiveChildrenMap
update_trans_cmap [] trans_cmap = trans_cmap
update_trans_cmap update_info@((ch, old, new):info') trans_cmap =
	update_trans_cmap info' trans_cmap'
	where
		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = M.insert ch ts' trans_cmap
			where
				ts' :: [TTrie]
				ts' = if is_empty new
					then tail l
					else l
					where l = if isNothing ts
						then [new]
						else new:(L.delete old (fromJust ts)) -- won't crash if not `elem`

				ts :: Maybe [TTrie]
				ts = M.lookup ch trans_cmap

testt = fromList ["abc", "ade", "a"]
testt' = fromList ["abc", "adc", "a"]

-- TODO: point-free form?
insert :: TTrie -> String -> TTrie
insert ttrie str = fst $ insert_rec ttrie str str

insert_rec :: TTrie -> String -> String -> (TTrie, TransitiveChildrenMapUpdateInfo)
insert_rec ttrie "" str = (node, [])
	where
		node :: TTrie
		node = case ttrie of
			Empty -> Node (Just str) M.empty M.empty
			(Node _ cmap trans_cmap) -> Node (Just str) cmap trans_cmap

insert_rec Empty (ch:str') str = (node, update_info)
	where
		node :: TTrie
		node = Node Nothing cmap trans_cmap

		cmap :: ChildrenMap
		cmap = M.insert ch only_child M.empty

		trans_cmap :: TransitiveChildrenMap
		trans_cmap = update_trans_cmap update_info_from_below M.empty 

		_' :: (TTrie, TransitiveChildrenMapUpdateInfo)
		_'@(only_child, update_info_from_below) = insert_rec Empty str' str

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level = (ch, Empty, only_child)

insert_rec t@(Node word cmap trans_cmap) (ch:str') str = (node, update_info)
	where
		node :: TTrie
		node = Node word cmap' trans_cmap'

		cmap' :: ChildrenMap
		cmap' = M.insert ch insertion_child_post cmap

		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = update_trans_cmap update_info_from_below trans_cmap

		insertion_child_pre :: TTrie
		insertion_child_pre = if isNothing child
			then Empty
			else fromJust child
			where
				child :: Maybe TTrie
				child = M.lookup ch cmap

		_' :: (TTrie, TransitiveChildrenMapUpdateInfo)
		_'@(insertion_child_post, update_info_from_below) = insert_rec insertion_child_pre str' str

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level =
					( ch
					, insertion_child_pre
					, insertion_child_post )
		
contains_contiguous :: TTrie -> String -> Bool
contains_contiguous Empty _ = False
contains_contiguous (Node word _ _) "" = not . isNothing $ word
contains_contiguous t@(Node _ cmap _) (ch:str') =
	if isNothing child
		then False
		else contains_contiguous (fromJust child) str'
	where
		child :: Maybe TTrie
		child = M.lookup ch cmap

contains_noncontiguous :: TTrie -> String -> Bool
contains_noncontiguous ttrie str = (all_matches ttrie str) == []

all_matches :: TTrie -> String -> [String]
all_matches ttrie str = all_matches_rec ttrie str ""

-- need to store zipper info in nodes, too?
all_matches_rec :: TTrie -> String -> String -> [String]
all_matches_rec Empty _ so_far = []
all_matches_rec (Node _ _ _) "" so_far = [so_far]
all_matches_rec t@(Node _ cmap trans_cmap) str@(ch:str') so_far =
	if isNothing matching_children
		then []
		else []--or $ contains_contiguous (fromJust child) str'
	where
		matching_children :: Maybe [TTrie]
		matching_children = (:) <$> child <*> trans_children
			where
				child :: Maybe TTrie
				child = M.lookup ch cmap

				trans_children :: Maybe [TTrie]
				trans_children = M.lookup ch trans_cmap

remove :: TTrie -> String -> TTrie
remove t = fst . remove_rec t

remove_rec :: TTrie -> String -> (TTrie, TransitiveChildrenMapUpdateInfo)
remove_rec Empty _ = (Empty, [])
remove_rec (Node _ cmap trans_cmap) "" =
	if M.elems cmap == []
		then (Empty, [])
		else (Node Nothing cmap trans_cmap, [])
remove_rec t@(Node word cmap trans_cmap) str@(ch:str') =
	if isNothing deletion_child_pre
		then (t, [])
		else (t', update_info)
	where
		t' :: TTrie
		t' = if M.size cmap' == 0
			then Empty
			else Node word cmap' trans_cmap'

		deletion_child_pre :: Maybe TTrie
		deletion_child_pre = M.lookup ch cmap

		_' :: (TTrie, TransitiveChildrenMapUpdateInfo)
		_'@(deletion_child_post, update_info_from_below) = remove_rec (fromJust deletion_child_pre) str'

		cmap' :: ChildrenMap
		cmap' = if is_empty deletion_child_post
			then M.delete ch cmap
			else M.insert ch deletion_child_post . M.delete ch $ cmap

		trans_cmap' :: TransitiveChildrenMap
		trans_cmap' = if is_empty deletion_child_post
			then update_trans_cmap [(ch, fromJust deletion_child_pre, Empty)] trans_cmap
			else update_trans_cmap update_info_from_below trans_cmap

		update_info :: TransitiveChildrenMapUpdateInfo
		update_info = update_info_this_level : update_info_from_below
			where
				update_info_this_level :: (Char, TTrie, TTrie)
				update_info_this_level =
					( ch
					, fromJust deletion_child_pre
					, deletion_child_post )

runtests :: IO ()
runtests = quickCheckWith stdArgs { maxSuccess = 3000 } test_Ttrie

test_Ttrie :: [String] -> Bool
test_Ttrie elems = and
	[ all_elems_present ]
	where
		all_elems_present :: Bool
		all_elems_present = all (contains_contiguous t) elems

		t :: TTrie
		t = fromList elems
