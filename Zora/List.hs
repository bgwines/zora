{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Zora.List
-- Copyright   : (c) Brett Wines 2014
--
-- License     : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- Assorted functions on lists.
--

module Zora.List
(
-- * List transformations
  uniqueify
, pairify
, decyclify
, shuffle

-- * Permutations, combinations, and cycles
, powerset
, permutations
, subsets_of_size
, cycles
, has_cycles

-- * Partitioning
, partition_with_block_size
, partition_into_k
, powerpartition

-- * Operations with two lists
, diff_infinite
, merge
, merge_by
, zip_while

-- * Sublists
, remove_at_index
, subseq
, take_while_keep_last
, take_while_and_rest
, subsequences
, contiguous_subsequences

-- * Sorting
, is_sorted
, mergesort

-- * Predicates
, is_palindrome
, contains_duplicates

-- * Assorted functions
, map_keep
, maximum_with_index
, minimum_with_index
, length'
, drop'
, take'
, cons
, snoc
) where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Set as Set

import System.Random

import Data.Monoid
import Data.Maybe

-- ---------------------------------------------------------------------
-- List transformations

-- | /O(n log(n))/ Removes duplicate elements. Like `Data.List.nub`, but for `Ord` types, so it can be faster.
uniqueify :: (Ord a) => [a] -> [a]
uniqueify = Set.elems . Set.fromList

-- | /O(n)/ Zips the list up into pairs. For example,
-- 
-- > pairify [1..6] == [(1,2), (3,4), (5,6)]
-- > pairify [1..5] == [(1,2), (3,4)]
pairify :: [a] -> [(a, a)]
pairify l@(a:b:l') = (a, b) : pairify l'
pairify l = []

-- | /O(l m)/, where /l/ is the cycle length and /m/ is the index of the start of the cycle. If the list contains no cycles, then the runtime is /O(n)/.
decyclify :: (Eq a) => [a] -> [a]
decyclify = fromJust . List.find (not . has_cycles) . iterate decyclify_once

decyclify_once :: (Eq a) => [a] -> [a]
decyclify_once l =
    if isNothing lambda
        then l
        else take' (lambda' + mu'') l
    where
        i       = alg1 (tail l) (tail . tail $ l)
        i'      = fromJust i

        mu      = if (i  == Nothing) then Nothing else alg2 l (drop' i' l)
        mu'     = fromInteger . fromJust $ mu
        mu''    = fromJust mu

        lambda  = if (mu == Nothing) then Nothing else alg3 (drop (mu' + 1) l) (l !! mu')
        lambda' = fromJust lambda

        alg1' :: (Eq a) => Integer -> [a] -> [a] -> Maybe Integer
        alg1' i _ [] = Nothing
        alg1' i [] _ = Nothing
        alg1' i l1 l2 = 
            if (head l1) == (head l2) then
                Just (i + 1) -- + 1 beacuse we start off with l1 = tail l
            else
                if tail l2 == [] then
                    Nothing
                else
                    alg1' (i + 1) (tail l1) (tail . tail $ l2)

        alg2' :: (Eq a) => Integer -> [a] -> [a] -> Maybe Integer
        alg2' mu _  [] = Nothing
        alg2' mu [] _  = Nothing
        alg2' mu l1 l2 = 
            if (head l1) == (head l2) then
                Just mu
            else
                alg2' (mu + 1) (tail l1) (tail l2)

        alg3' :: (Eq a) => Integer -> [a] -> a -> Maybe Integer
        alg3' lambda [] e = Nothing
        alg3' lambda l e =
            if (head l) == e then
                Just lambda
            else
                alg3' (lambda + 1) (tail l) e

        alg1 :: (Eq a) => [a] -> [a] -> Maybe Integer
        alg1 = alg1' 0

        alg2 :: (Eq a) => [a] -> [a] -> Maybe Integer
        alg2 = alg2' 0

        alg3 :: (Eq a) => [a] -> a -> Maybe Integer
        alg3 = alg3' 1

shuffle' :: [a] -> [Integer] -> [a]
shuffle' l indices =
    map fst . List.sortBy (Ord.comparing snd) $ zip l indices

-- | /O(n log(n))/ Shuffles the given list. The second parameter is the seed for the random number generator that backs the shuffle.
shuffle :: forall a. (Eq a) => [a] -> Integer -> [a]
shuffle l seed = shuffle' l randomness
    where
        randomness :: [Integer]
        randomness = prep (length' l) $ random_integers (0, (length' l) - 1) seed

        random_integers :: (Integer, Integer) -> Integer -> [Integer]
        random_integers range = randomRs range . mkStdGen . fromInteger

        prep :: Integer -> [Integer] -> [Integer]
        prep len l' = reverse . take' len $ prep' l' []

        prep' :: [Integer] -> [Integer] -> [Integer]
        prep' [] seen = []
        prep' src seen =
            if head src `elem` seen
                then             prep' (tail src) seen
                else head src : (prep' (tail src) (head src : seen))

-- ---------------------------------------------------------------------
-- Permutations, combinations, and cycles

-- | /O(2^n)/ Computes the powerset of the given list.
powerset :: [a] -> [[a]]
powerset l = powerset_rec l []
    where
        powerset_rec :: [a] -> [a] -> [[a]]
        powerset_rec [] so_far = [so_far]
        powerset_rec src so_far = without ++ with
            where without = powerset_rec (tail src) (so_far)
                  with    = powerset_rec (tail src) ((head src) : so_far)

-- TODO: actually O(n!)?
-- | /O(n!)/ Computes all permutations of the given list.
permutations :: [a] -> [[a]]
permutations l
    | (length l <= 1) = [l]
    | otherwise = 
        let splice_pairs = [(l !! i, remove_at_index (toInteger i) l) | i <- [0..((length l) - 1)]]
        in
            concat [
                [fst splice_pair : recpair | recpair <- permutations $ snd splice_pair]
                | splice_pair <- splice_pairs
            ]

-- | /O(2^k)/ Generates all subsets of the given list of size /k/.
subsets_of_size :: [a] -> Integer -> [[a]]
subsets_of_size l size = subsets_of_size_rec l [] size
    where
        subsets_of_size_rec :: [a] -> [a] -> Integer -> [[a]]
        subsets_of_size_rec src so_far size =
            if size == 0
                then [so_far]
                else if (length src) == 0
                    then []
                    else without ++ with
            where
                without = subsets_of_size_rec (tail src) so_far size
                with    = subsets_of_size_rec (tail src) ((head src) : so_far) (size-1)

{-subsets_of_size_with_replacement_rec :: Integer -> [a] -> [a] -> [[a]]
subsets_of_size_with_replacement_rec size src so_far =
    case size == 0 of
        True  -> [so_far]
        False -> concat [map (e:) rec | e <- src]
        where rec = subsets_of_size_with_replacement_rec (size - 1)

subsets_of_size_with_replacement :: [a] -> Integer -> [[a]]
subsets_of_size_with_replacement l size =
    subsets_of_size_with_replacement_rec size l []-}

-- | /O(n)/ Generates all cycles of a given list. For example,
-- 
-- > cycles [1..3] == [[2,3,1],[3,1,2],[1,2,3]]
cycles :: (Eq a) => [a] -> [[a]]
cycles l = cycles_rec l $ cycle_list l
    where
        cycle_list :: [a] -> [a]
        cycle_list l = (tail l) ++ [head l]

        cycles_rec :: (Eq a) => [a] -> [a] -> [[a]]
        cycles_rec original_l l
            | l == original_l = [l]
            | otherwise = [l] ++ (cycles_rec original_l $ cycle_list l)

-- | /O(l m)/, where /l/ is the cycle length and /m/ is the index of the start of the cycle. If the list contains no cycles, then the runtime is /O(n)/.
has_cycles :: (Eq a) => [a] -> Bool
has_cycles l = (decyclify_once l) /= l

-- ---------------------------------------------------------------------
-- Partitioning

-- | /O(n)/ Partitions the given list into blocks of the specified length. Truncation behaves as follows:
-- 
-- > partition_with_block_size 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
partition_with_block_size :: Int -> [a] -> [[a]]
partition_with_block_size len l =
    if (length l) <= len
        then [l]
        else (take len l) : (partition_with_block_size len (drop len l))

-- | /O(n)/ Partitions the given list into /k/ blocks. Truncation behavior is best described by example:
-- 
-- > partition_into_k  3 [1..9]  == [[1,2,3],[4,5,6],[7,8,9]]
-- > partition_into_k  3 [1..10] == [[1,2,3,4],[5,6,7,8],[9,10]]
-- > partition_into_k  3 [1..11] == [[1,2,3,4],[5,6,7,8],[9,10,11]]
-- > partition_into_k  3 [1..12] == [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
-- > partition_into_k  3 [1..13] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13]]
partition_into_k :: Int -> [a] -> [[a]]
partition_into_k k arr = partition_with_block_size block_size arr
    where
        block_size :: Int
        block_size = if (((length arr) `mod` k) == 0)
            then (length arr) `div` k
            else (length arr) `div` k + 1

-- | /O(B(n))/, where /B(n)/ is the /n/^th <http://en.wikipedia.org/wiki/Bell_number Bell number>. Computes all partitions of the given list. For example,
-- 
-- > powerpartition [1..3] == [[[1],[2],[3]], [[1,2],[3]], [[2],[1,3]], [[1],[2,3]], [[1,2,3]]]
powerpartition :: [a] -> [[[a]]]
powerpartition [] = []
powerpartition l@(x:xs) =
    if length l == 1
        then [[[x]]]
        else concatMap (get_next_partitions x) . powerpartition $ xs
        where
            get_next_partitions :: a -> [[a]] -> [[[a]]]
            get_next_partitions e l = ([e] : l) : (map f indices)
                where
                    f i = (a i) ++ (b i) ++ (c i)
                    
                    a i = ((take' i) l)
                    b i = [e : (l !! (fromInteger i))]
                    c i = (drop' (i+1) l)

                    indices = [0..((length' l) - 1)]

-- ---------------------------------------------------------------------
-- Operations with two lists

-- | Given two infinite sorted lists, generates a list of elements in the first but not the second. Implementation from <http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)>.
diff_infinite :: (Ord a) => [a] -> [a] -> [a]
diff_infinite xs@(x:xt) ys@(y:yt) = 
    case compare x y of
        LT -> x : (diff_infinite xt ys)
        EQ -> diff_infinite xt yt
        GT -> diff_infinite xs yt

-- | /O(max(n, m))/ Merges the two given sorted lists of respective lengths /n/ and /m/. A special case of `merge_by` where the comparison function is `compare`.
merge :: (Ord a) => [a] -> [a] -> [a]
merge = merge_by compare

-- | /O(max(n, m))/ Merges the two given sorted lists of respective lengths /n/ and /m/, comparing elements in between the two lists with the given comparator function.
merge_by :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by cmp as bs
    | length as == 0 = bs
    | length bs == 0 = as
    | otherwise =
        let
            a = head as
            b = head bs
            as' = tail as
            bs' = tail bs
        in
            case cmp a b of
                    LT -> a : merge_by cmp as' bs
                    EQ -> a : merge_by cmp as' bs
                    GT -> b : merge_by cmp as  bs'


-- | /O(min(n, m))/ Zips the two given lists of respective lengths /n/ and /m/ as long as the pairs satisfy the given predicate function.
zip_while :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zip_while f as bs = takeWhile (\(a, b) -> f a b) $ zip as bs

-- ---------------------------------------------------------------------
-- Sublists

-- | /O(n)/ Removes an element at the specified index in the given list.
remove_at_index :: Integer -> [a] -> [a]
remove_at_index i l =
    let a = fst $ splitAt (fromInteger i) l 
        b = snd $ splitAt (fromInteger i) l 
    in  a ++ tail b

-- | /O(n)/ Returns the subsequence of the given length at starting at index /i/ of length /m/. For example,
-- 
-- > subseq 4 5 [1..20] == [5,6,7,8,9]
subseq :: Integer -> Integer -> [a] -> [a]
subseq i len = take (fromInteger len) . drop (fromInteger i)

-- | /(O(n))/ Identical to `takeWhile`, but also contains the first element to satisfy the given predicate function. For example:
-- 
-- > take_while_keep_last (<3) [1..] == [1,2,3]
take_while_keep_last :: (a -> Bool) -> [a] -> [a]
take_while_keep_last f [] = []
take_while_keep_last f (x:xs) =
    if f x
        then [x]
        else x : take_while_keep_last f xs

-- | /(O(n))/ Returns a pair where the first element is identical to what `takeWhile` returns and the second element is the rest of the list
-- 
-- > take_while_keep_last (<3) [1..] == [1,2,3]
take_while_and_rest :: (a -> Bool) -> [a] -> ([a], [a])
take_while_and_rest f [] = ([], [])
take_while_and_rest f l@(x:xs) = if not . f $ x
    then ([], l)
    else (x:(fst rec), snd rec)
    where
        rec = take_while_and_rest f xs

-- | /(O(n^2))/ Returns all contiguous subsequences.
contiguous_subsequences :: [a] -> [[a]]
contiguous_subsequences = (:) [] . concatMap (tail . List.inits) . List.tails

-- | /(O(2^n))/ Returns all subsequences (contiguous and noncontiguous)
subsequences :: [a] -> [[a]]
subsequences = map reverse . powerset


-- ---------------------------------------------------------------------
-- Sorting

-- | /O(n)/ Returns whether the given list is sorted.
is_sorted :: (Ord a) => [a] -> Bool
is_sorted l = and $ zipWith (<=) l (tail l)

-- | /O(n log(n))/ Sorts the given list.
mergesort :: (Ord a) => [a] -> [a]
mergesort l =
    if length l <= 1
        then l
        else merge (mergesort a) (mergesort b)
        where
            (a, b) = splitAt (floor ((fromIntegral $ length l) / 2)) l

-- ---------------------------------------------------------------------
-- Predicates

-- TODO: O(n log(n))
-- | /O(n^2)/ Returns whether the given list is a palindrome.
is_palindrome :: (Eq e) => [e] -> Bool
is_palindrome s =
    if length s <= 1
        then True
        else (head s == last s) && (is_palindrome $ tail . init $ s)

-- TODO: do this more monadically?
-- | /O(n log(n))/ Returns whether the given list contains any element more than once.
contains_duplicates :: forall a . (Ord a) => [a] -> Bool
contains_duplicates l =
    if is_sorted l
        then or $ zipWith (==) l (tail l)
        else isNothing $ foldr insert (Just Set.empty) l
    where
        insert :: a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
        insert a s = if isNothing s
            then Nothing
            else if Set.member a (fromJust s)
                then Nothing
                else Just (Set.insert a (fromJust s))

-- ---------------------------------------------------------------------
-- Assorted functions

-- | /O(n)/ Maps the given function over the list while keeping the original list. For example:
-- 
-- > map_keep chr [97..100] == [(97,'a'),(98,'b'),(99,'c'),(100,'d')]
map_keep :: (a -> b) -> [a] -> [(a, b)]
map_keep f l = zipWith (\a b -> (a, b)) l (map f l)

-- | Like `length`, but returns an integer.
length' :: [a] -> Integer
length' = toInteger . length

-- | Like `drop`, but takes an integer.
drop' :: Integer -> [a] -> [a]
drop' = drop . fromInteger

-- | Like `take`, but takes an integer.
take' :: Integer -> [a] -> [a]
take' = take . fromInteger

-- | List pre-pending.
cons :: a -> [a] -> [a]
cons = (:)

-- | List post-pending.
snoc :: a -> [a] -> [a]
snoc e l = l ++ [e]

-- | /O(n)/ Finds the maximum element of the given list and returns a pair of it and the index at which it occurs (if the maximum element occurs multiple times, behavior is identical to that of `Data.List.maximumBy`). The list must be finite and non-empty.
maximum_with_index :: (Ord a) => [a] -> (a, Integer)
maximum_with_index xs =
    List.maximumBy (Ord.comparing fst) (zip xs [0..])

-- | /O(n)/ Finds the minimum element of the given list and returns a pair of it and the index at which it occurs (if the minimum element occurs multiple times, behavior is identical to that of `Data.List.minimumBy`). The list must be finite and non-empty.
minimum_with_index :: (Ord a) => [a] -> (a, Integer)
minimum_with_index xs =
    List.minimumBy (Ord.comparing fst) (zip xs [0..])
