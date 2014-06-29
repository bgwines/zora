{-# LANGUAGE ScopedTypeVariables #-}

module HLib.List
( decyclify
, diff
, find_guaranteed
, gen_cycles
, gen_perms
, gen_subsets_of_size
, get_splice_pair
, indexify
, is_palindrome
, map_keep
, maximum_with_index
, merge
, mergeBy
, mergesort
, powerpartition
, powerset
, shuffle
, splice_out
, substr
, take_while_keep_last
, uniqueify
, length'
, drop'
, take'
, partition
, partition_with_block_size
, partition_into_k
, takeWhileAndRest
, zipWhile
, contains_duplicates
, sorted_contains_duplicates
, cons
, snoc
, pairify
, is_sorted
, safe_tail
) where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Set as Set

import System.Random

import Data.Monoid
import Data.Maybe

import HLib.Tuple

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
    case compare x y of
        LT -> x : (diff xt ys)
        EQ -> diff xt yt
        GT -> diff xs yt

get_next_partitions :: a -> [[a]] -> [[[a]]]
get_next_partitions e l =
	([e] : l) : (map f indices)
	where
		f i = (a i) ++ (b i) ++ (c i)
		
		a i = ((take' i) l)
		b i = [e : (l !! (fromInteger i))]
		c i = (drop' (i+1) l)

		indices = [0..((length' l) - 1)]

powerpartition :: [a] -> [[[a]]]
powerpartition [] = []
powerpartition l =
	if length l == 1 then
		[[[head l]]]
	else
		let
			rec_partitions = powerpartition (tail l)
		in
			concat . map (get_next_partitions (head l)) $ rec_partitions

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

decyclify :: (Eq a) => [a] -> [a]
decyclify l =
    let
        i       = alg1 (tail l) (tail . tail $ l)
        i'      = fromJust i

        mu      = if (i  == Nothing) then Nothing else alg2 l (drop' i' l)
        mu'     = fromInteger . fromJust $ mu
        mu''    = fromJust mu

        lambda  = if (mu == Nothing) then Nothing else alg3 (drop (mu' + 1) l) (l !! mu')
        lambda' = fromJust lambda
    in
        if lambda /= Nothing then
            take' (lambda' + mu'') l
        else
            l

shuffle' :: [a] -> [Integer] -> [a]
shuffle' l indices =
    map fst . List.sortBy (Ord.comparing snd) $ zip l indices

shuffle :: [a] -> [a]
shuffle l = shuffle' l randomness
    where
        randomness = prep (length l) $ random_integers (0, (length' l) - 1) 280172349

        prep len l = reverse . take len $ prep' l []

        prep' [] seen = []
        prep' src seen =
            case head src `elem` seen of
                True ->              prep' (tail src) seen
                False -> head src : (prep' (tail src) (head src : seen))

pairify :: [a] -> [(a, a)]
pairify l@(a:b:l') = (a, b) : pairify l'
pairify l = []

indexify :: [a] -> [(Integer, a)]
indexify = zipWith (\a b -> (a, b)) [1..]

take_while_keep_last :: (a -> Bool) -> [a] -> [a]
take_while_keep_last f [] = []
take_while_keep_last f l@(e:l') = 
    case f e of
        False -> [e]
        True -> e : take_while_keep_last f l'

uniqueify :: (Ord a) => [a] -> [a]
uniqueify = Set.elems . Set.fromList

take' :: Integer -> [a] -> [a]
take' = take . fromInteger

drop' :: Integer -> [a] -> [a]
drop' = drop . fromInteger

length' :: [a] -> Integer
length' = toInteger . length

map_keep :: (a -> b) -> [a] -> [(a, b)]
map_keep f l = zipWith pair l (map f l)

cycle_list :: [a] -> [a]
cycle_list l = (tail l) ++ [head l]

gen_cycles :: (Eq a) => [a] -> [[a]]
gen_cycles l = gen_cycles_rec l $ cycle_list l

gen_cycles_rec :: (Eq a) => [a] -> [a] -> [[a]]
gen_cycles_rec original_l l
    | l == original_l = [l]
    | otherwise = [l] ++ (gen_cycles_rec original_l $ cycle_list l)

find_guaranteed :: (a -> Bool) -> [a] -> a
find_guaranteed f l = 
    if f $ head l
        then head l
        else find_guaranteed f $ tail l

powerset_rec :: [a] -> [a] -> [[a]]
powerset_rec [] so_far = [so_far]
powerset_rec src so_far = without ++ with
    where without = powerset_rec (tail src) (so_far)
          with = powerset_rec (tail src) ((head src) : so_far)

powerset :: [a] -> [[a]]
powerset l = powerset_rec l []

gen_subsets_of_size_rec :: [a] -> [a] -> Integer -> [[a]]
gen_subsets_of_size_rec src so_far size =
    case size == 0 of
        True  -> [so_far]
        False -> if (length src) == 0
            then []
            else without ++ with
    where without = gen_subsets_of_size_rec (tail src) so_far size
          with  = gen_subsets_of_size_rec (tail src) ((head src) : so_far) (size-1)

gen_subsets_of_size :: [a] -> Integer -> [[a]]
gen_subsets_of_size l size = gen_subsets_of_size_rec l [] size

{-gen_subsets_of_size_with_replacement_rec :: Integer -> [a] -> [a] -> [[a]]
gen_subsets_of_size_with_replacement_rec size src so_far =
    case size == 0 of
        True  -> [so_far]
        False -> concat [map (e:) rec | e <- src]
        where rec = gen_subsets_of_size_with_replacement_rec (size - 1)

gen_subsets_of_size_with_replacement :: [a] -> Integer -> [[a]]
gen_subsets_of_size_with_replacement l size =
    gen_subsets_of_size_with_replacement_rec size l []-}

-- TODO: O(n)
is_palindrome :: (Eq e) => [e] -> Bool
is_palindrome s
    | length s <= 1 = True
    | otherwise =
        (head s == last s)
        &&
        (is_palindrome $ tail $ init s)

get_splice_pair :: Integer -> [a] -> (a, [a])
get_splice_pair i l = (l !! (fromInteger i), splice_out i l)

splice_out :: Integer -> [a] -> [a]
splice_out i l =
    let a = fst $ splitAt (fromInteger i) l 
        b = snd $ splitAt (fromInteger i) l 
    in  a ++ tail b

gen_perms :: [a] -> [[a]]
gen_perms l
    | (length l <= 1) = [l]
    | otherwise = 
        let splice_pairs = [get_splice_pair (toInteger i) l | i <- [0..((length l) - 1)]]
        in
        concat [
            [fst splice_pair : recpair | recpair <- gen_perms $ snd splice_pair]
            | splice_pair <- splice_pairs
        ]

subseq :: Integer -> Integer -> [a] -> [a]
subseq i len s = take (fromInteger len) $ drop (fromInteger i) s

substr :: Integer -> Integer -> String -> String
substr = subseq

maximum_with_index :: (Ord a) => [a] -> (a, Integer)
maximum_with_index xs =
	List.maximumBy (Ord.comparing fst) (zip xs [0..])

mergesort :: (Ord a) => [a] -> [a]
mergesort l
    | length l == 0 || length l == 1 = l
    | otherwise = 
        let a = fst $ splitAt (floor ((fromIntegral $ length l)/2)) l
            b = snd $ splitAt (floor ((fromIntegral $ length l)/2)) l
        in merge (mergesort a) (mergesort b)

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

mergeBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp as bs
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
                    LT -> a : mergeBy cmp as' bs
                    EQ -> a : mergeBy cmp as' bs
                    GT -> b : mergeBy cmp as  bs'

-- TODO: O(n)
partition :: Int -> [a] -> [[a]]
partition len l =
    if (length l) <= len
        then [l]
        else (take len l) : (partition len (drop len l))

partition_with_block_size :: Int -> [a] -> [[a]]
partition_with_block_size = partition

partition_into_k :: Int -> [a] -> [[a]]
partition_into_k k arr = partition_with_block_size block_size arr
    where
        block_size :: Int
        block_size = if (((length arr) `mod` k) == 0)
            then (length arr) `div` k
            else (length arr) `div` k + 1

takeWhileAndRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileAndRest f [] = ([], [])
takeWhileAndRest f l@(x:xs) = if not (f x)
    then ([], l)
    else (x:(fst rec), snd rec)
        where rec = takeWhileAndRest f xs

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zipWhile f as bs = takeWhile (\(a, b) -> f a b) $ zip as bs

-- TODO: do this more monadically?
contains_duplicates :: forall a . (Ord a) => [a] -> Bool
contains_duplicates l = (foldr (insert) (Just Set.empty) l) == Nothing
    where
        insert :: a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
        insert a s = if s == Nothing
            then Nothing
            else if Set.member a (fromJust s)
                then Nothing
                else Just (Set.insert a (fromJust s))

sorted_contains_duplicates :: (Eq a) => [a] -> Bool
sorted_contains_duplicates [] = False
sorted_contains_duplicates l = or $ zipWith (==) l (tail l)

cons :: a -> [a] -> [a]
cons = (:)

snoc :: a -> [a] -> [a]
snoc e l = l ++ [e]

is_sorted :: (Ord a) => [a] -> Bool
is_sorted l = and $ zipWith (<=) l (tail l)

safe_tail :: [a] -> [a]
safe_tail [] = []
safe_tail l = tail l

random_integers :: (Integer, Integer) -> Integer -> [Integer]
random_integers range seed = randomRs range . mkStdGen $ fromInteger seed
