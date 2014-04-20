module HLib
( primes
, is_prime
, coprime
, continued_fraction_sqrt
, continued_fraction_sqrt_infinite
, decyclify
, diff
, divides
, euler_phi
, factorize
, fill_set
, find
, find_guaranteed
, from_just
, fromRealFrac
, gen_cycles
, gen_perms
, gen_subsets_of_size
, get_divisors
, get_num_digits
, get_splice_pair
, indexify
, irr_squares
, is_int
, is_palindrome
, is_power
, map_keep
, maximum_with_index
, merge
, mergesort
, pair
, pairify
, powerpartition
, powerset
, random_integers
, shuffle
, splice_out
, sqrt_convergents
, substr
, take_while_keep_last
, to_int
, tri_area
, tri_area_double
, triple
, uniqueify
, length'
, drop'
, take'
, fst3
, snd3
, trd3
, partition
, takeWhileAndRest
) where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import System.Random

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

get_divisors :: Integer -> [Integer]
get_divisors = init . uniqueify . map product . powerset . factorize

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
        i'      = from_just i

        mu      = if (i  == Nothing) then Nothing else alg2 l (drop' i' l)
        mu'     = fromInteger . from_just $ mu
        mu''    = from_just mu

        lambda  = if (mu == Nothing) then Nothing else alg3 (drop (mu' + 1) l) (l !! mu')
        lambda' = from_just lambda
    in
        if lambda /= Nothing then
            take' (lambda' + mu'') l
        else
            l

random_integers :: (Integer, Integer) -> Integer -> [Integer]
random_integers range seed = randomRs range . mkStdGen $ fromInteger seed

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
            case h src `elem` seen of
                True ->           prep' (t src) seen
                False -> h src : (prep' (t src) (h src : seen))

pairify :: [a] -> [(a, a)]
pairify l = (a, b) : pairify l'
    where
        a = head l
        b = head . tail $ l
        l' = tail . tail $ l

h :: [a] -> a
h = head

t :: [a] -> [a]
t = tail

indexify :: [a] -> [(Integer, a)]
indexify = zipWith (\a b -> (a, b)) [1..]

sqrt_convergents_rec :: (Integer, Integer) -> (Integer, Integer) -> [Integer] -> [(Integer, Integer)]
sqrt_convergents_rec (a'', b'') (a', b') cf =
    (a, b) : sqrt_convergents_rec (a', b') (a, b) cf'
        where
            a = e * a' + a''
            b = e * b' + b''
            e = head cf
            cf' = tail cf

sqrt_convergents :: Integer -> [(Integer, Integer)]
sqrt_convergents n =
    (a0, b0) : (a1, b1) : 
        sqrt_convergents_rec
            (a0, b0)
            (a1, b1)
            (tail . tail $ cf)
    where
        (a0, b0) = (e, 1)
        (a1, b1) = (e * e' + 1, e')
        e = head cf
        e' = head . tail $ cf
        cf = continued_fraction_sqrt_infinite n

irr_squares :: [Integer]
irr_squares = map round $ filter (not . is_int . sqrt) [1..10000] 

next_continued_fraction_sqrt :: (Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer, Integer)
next_continued_fraction_sqrt (d, m, a, a0, n) = (d', m', a', a0, n)
    where
        d' = (n - m'^2) `div` d
        m' = (d * a)  - m
        a' = floor $ (fromIntegral (a0 + m')) / (fromIntegral d')

continued_fraction_sqrt_infinite :: Integer -> [Integer]
continued_fraction_sqrt_infinite n =
        map trd5
            $ iterate next_continued_fraction_sqrt (d0, m0, a0, a0, n)
    where
        m0 = 0
        d0 = 1
        a0 = floor . sqrt . fromInteger $ n
        trd5 (_, _, x, _, _) = x

continued_fraction_sqrt :: Integer -> [Integer]
continued_fraction_sqrt n =
    take_while_keep_last (/= 2*a0) $ continued_fraction_sqrt_infinite n
        where a0 = floor . sqrt . fromInteger $ n

take_while_keep_last f [] = []
take_while_keep_last f l = 
    let
        e = head l
        l' = tail l
    in
        case f e of
            False -> [e]
            True -> e : take_while_keep_last f l'

-- Heron's formula
tri_area :: Integer -> Integer -> Integer -> Double
tri_area a b c = 
    sqrt $ p * (p-a') * (p-b') * (p-c')
        where
            a' = fromInteger a
            b' = fromInteger b
            c' = fromInteger c
            p = (fromInteger (a + b + c)) / 2

tri_area_double :: Double -> Double -> Double -> Double
tri_area_double a b c = 
    sqrt $ p * (p-a) * (p-b) * (p-c)
        where
            p = (a + b + c) / 2

uniqueify :: (Ord a) => [a] -> [a]
uniqueify = Set.elems . Set.fromList

take' :: Integer -> [a] -> [a]
take' = take . fromInteger

drop' :: Integer -> [a] -> [a]
drop' = drop . fromInteger

length' :: [a] -> Integer
length' = toInteger . length

to_int :: Double -> Integer
to_int = (toInteger . round)

triple :: a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)

pair :: a -> b -> (a, b)
pair a b = (a, b)

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

is_power :: Integer -> Integer -> Bool
is_power n e = (round (fromIntegral n ** (1/(fromInteger e))))^e == n

get_num_digits :: Integer -> Integer
get_num_digits n = (1 + (floor $ logBase 10 (fromInteger n)))

divides :: Integer -> Integer -> Bool
divides n x = (n `mod` x) == 0

divides_both :: Integer -> Integer -> Integer -> Bool
divides_both d n m = (n `mod` d == 0) && (m `mod` d == 0)

euler_phi_rec :: Integer -> Integer -> Integer -> Integer
euler_phi_rec n m count
    | (n == m) = count
    | (coprime n m) = euler_phi_rec n (m+1) (count+1)
    | otherwise  = euler_phi_rec n (m+1) count

euler_phi_slow :: Integer -> Integer
euler_phi_slow n = euler_phi_rec n 1 0

euler_phi_medium :: Integer -> Integer
euler_phi_medium n 
    | (n == 2) = 1
    | (even n) = case (even half) of
        True  -> 2 * (euler_phi_medium half)
        False -> 1 * (euler_phi_medium half)
    | (odd n) = euler_phi_slow n
        where half = div n 2

factorize :: Integer -> [Integer]
factorize 0 = []
factorize 1 = []
factorize n = p : factorize (n `div` p)
    where p = find_guaranteed (\p -> (n `mod` p) == 0) primes

euler_phi_for_powers_of_primes :: (Integer, Integer) -> Integer
euler_phi_for_powers_of_primes (p, a) = p^(a-1) * (p-1)

-- ultramegafast
euler_phi :: Integer -> Integer
euler_phi 1 = 0
euler_phi n = product 
    $ map
        euler_phi_for_powers_of_primes
        $ map format $ List.group $ factorize n
    where
        format l = (head l, (toInteger . length) l) 

from_just :: Maybe a -> a
from_just (Just x) = x

fromRealFrac :: Integer -> Double
fromRealFrac = fromRational . toRational

find_guaranteed :: (a -> Bool) -> [a] -> a
find_guaranteed f l = 
    if f $ head l
        then head l
        else find_guaranteed f $ tail l

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f l = case f $ head l of
    True -> Just (head l)
    False -> find f $ tail l

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

fst3 :: (a, a, a) -> a
fst3 (a, b, c) = a

snd3 :: (a, a, a) -> a
snd3 (a, b, c) = b

trd3 :: (a, a, a) -> a
trd3 (a, b, c) = c

fill_set :: [Integer] -> Set.Set Integer
fill_set l = fill_set_rec Set.empty l

fill_set_rec :: Set.Set Integer -> [Integer] -> Set.Set Integer
fill_set_rec s [] = s
fill_set_rec s l = Set.insert (head l) (fill_set_rec s $ tail l)

is_int :: Double -> Bool
is_int x = x == (fromInteger (round x))

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

substr :: Integer -> Integer -> [a] -> [a]
substr i len s = take (fromInteger len) $ drop (fromInteger i) s

maximum_with_index :: (Ord a) => [a] -> (a, Integer)
maximum_with_index xs =
	List.maximumBy (Ord.comparing fst) (zip xs [0..])

--merge :: (Ord a) => [a] -> [a] -> [a]
--merge xs@(x:xt) ys@(y:yt) = 
--  case compare x y of
--  LT -> x : (merge xt ys)
--  EQ -> x : (merge xt yt)
--  GT -> y : (merge xs yt)

mergesort :: (Ord a) => [a] -> [a]
mergesort l
    | length l == 0 || length l == 1 = l
    | otherwise = 
        let a = fst $ splitAt (floor ((fromIntegral $ length l)/2)) l
            b = snd $ splitAt (floor ((fromIntegral $ length l)/2)) l
        in merge (mergesort a) (mergesort b)

merge :: (Ord a) => [a] -> [a] -> [a]
merge a b
	| length a == 0 = b
	| length b == 0 = a
	| otherwise = case (head a < head b) of
	            True  -> head a : merge (tail a) b
	            False -> head b : merge a (tail b)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

merge_infinite :: (Ord a) => [a] -> [a] -> [a]
merge_infinite xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge_infinite xt ys)
    EQ -> x : (merge_infinite xt yt)
    GT -> y : (merge_infinite xs yt)

primes :: [Integer]
nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f $ map g $ tail primes
  where 
    f (x:xt) ys = x : (merge_infinite xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

is_prime_rec :: Integer -> Integer -> Bool
is_prime_rec n k
    | (n <= 1) = False
    | (fromInteger k >= ((fromInteger n) / 2) + 1.0) = True
    | ((n `mod` k) == 0) = False
    | otherwise = is_prime_rec n (k+1)

is_prime :: Integer -> Bool
is_prime n = is_prime_rec n 2

coprime :: Integer -> Integer -> Bool
coprime a b = Nothing == find is_common_divisor [2..(min a b)]
  where is_common_divisor n = (a `mod` n == 0) && (b `mod` n == 0)

partition :: Int -> [a] -> [[a]]
partition len l =
    if (length l) <= len
        then [l]
        else (take len l) : (partition len (drop len l))

takeWhileAndRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileAndRest f [] = ([], [])
takeWhileAndRest f l@(x:xs) = if not (f x)
    then ([], l)
    else (x:(fst rec), snd rec)
        where rec = takeWhileAndRest f xs
