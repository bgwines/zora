-- |
-- Module	  : Zora.Math
-- Copyright   : (c) Brett Wines 2014
--
-- License	 : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- Assorted mathematical functions.
--

module Zora.Math
( -- * Prime numbers and division
  primes
, composites
, prime
, prime_miller_rabin
, coprime
, euler_phi
, factor
, factor_number_is_perfect_square
, divisors
, divisors_number_is_perfect_square
, num_divisors
, num_divisors_of_n_squared_leq_n

-- * Square roots
, irrational_squares
, sqrt_convergents
, continued_fraction_sqrt
, continued_fraction_sqrt_infinite
, square

-- * Modular arithmetic
, add_mod
, sub_mod
, mul_mod
, div_mod
, pow_mod
, multiplicative_inverse

-- * Assorted functions
, fibs
, sqrt_perfect_square
, is_int
, is_power_of_int
, double_to_int
, num_digits
, tri_area
, tri_area_double
, solve_linear_system
) where

import qualified Zora.List as ZList

import qualified Data.List as List

import Data.Maybe

import Control.Applicative

import System.Random

-- ---------------------------------------------------------------------
-- Prime numbers and division

-- | A complete, monotonically increasing, infinite list of primes. Implementation from <http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)>.
primes :: [Integer]
primes = [2, 3, 5] ++ (ZList.diff_infinite [7, 9 ..] composites)

-- | A complete, monotonically increasing, infinite list of composite numbers.
composites :: [Integer]
composites = foldr1 f $ map g $ tail primes
	where
		f (x:xt) ys = x : (merge_infinite xt ys)
		g p = [ n * p | n <- [p, p + 2 ..]]

		merge_infinite :: (Ord a) => [a] -> [a] -> [a]
		merge_infinite xs@(x:xt) ys@(y:yt) = 
			case compare x y of
				LT -> x : (merge_infinite xt ys)
				EQ -> x : (merge_infinite xt yt)
				GT -> y : (merge_infinite xs yt)

random_integers :: (Integer, Integer) -> Integer -> [Integer]
random_integers range seed = randomRs range . mkStdGen $ fromInteger seed

-- | /O(log^3 n)/ Uses the <http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Example Miller-Rabin primality test> to determine primality. Always correctly identifies primes, but may misidentify some composites as primes (for most practical input values, this will not happen (~3 below 10^9? 0 below 10^7.).).
prime_miller_rabin :: Integer -> Bool
prime_miller_rabin n =
	if small_prime_divides || (n == 1)
		then False
		else (n `elem` (take 10 primes)) || all (prime_miller_rabin' n) as
		where
			as :: [Integer]
			as = take 30 $ random_integers (2, n - 2) 2436572

			small_prime_divides :: Bool
			small_prime_divides
				= any (\p -> (n `mod` p) == 0)
				. filter ((/=) n)
				$ take 7 primes

prime_miller_rabin' :: Integer -> Integer -> Bool
prime_miller_rabin' n a =
	if (x == 1) || (x == n - 1)
		then True
		else if powers' == []
			then False
			else head powers' == (n-1)
	where
		powers' = filter (\k -> k == 1 || k == (n - 1)) powers

		powers :: [Integer]
		powers
			= map (\e -> (pow_mod n x e))
			. scanl1 (mul_mod n)
			$ replicate ((fromIntegral s) - 1) 2

		x :: Integer
		x = pow_mod n a d

		s :: Integer
		s = last . takeWhile (\e -> ((n - 1) `mod` (2^e)) == 0) $ [1..]

		d :: Integer
		d = (n - 1) `div` (2^s)

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>. Returns whether the parameter is a prime number.
prime :: Integer -> Bool
prime n = (factor n) == [n]

-- | /O(min(n, m))/ Returns whether the the two parameters are <http://en.wikipedia.org/wiki/Coprime coprime>, that is, whether they share any divisors.
coprime :: Integer -> Integer -> Bool
coprime a b = isNothing .  List.find is_common_divisor $ [2..(min a' b')]
	where
		a' :: Integer
		a' = abs a

		b' :: Integer
		b' = abs b

		is_common_divisor :: Integer -> Bool
		is_common_divisor n = (a `mod` n == 0) && (b `mod` n == 0)

-- | /O(1)/ @phi(p^a)@ for prime @p@ and nonnegative @a@.
euler_phi_for_powers_of_primes :: (Integer, Integer) -> Integer
euler_phi_for_powers_of_primes (p, a) = p^(a-1) * (p-1)

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers).
euler_phi :: Integer -> Integer
euler_phi 1 = 0
euler_phi n = product 
	$ map
		euler_phi_for_powers_of_primes
		$ map format $ List.group $ factor n
	where
		format l = (head l, (toInteger . length) l) 

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>.
factor_number_is_perfect_square :: Integer -> [Integer]
factor_number_is_perfect_square n = ZList.interleave ZList.$$ sqrt_factorization
	where
		sqrt_factorization :: [Integer]
		sqrt_factorization = factor (sqrt_perfect_square n)

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>.
factor :: Integer -> [Integer]
factor = factor' primes

factor' :: [Integer] -> Integer -> [Integer]
factor' _ 0 = []
factor' _ 1 = []
factor' primes' n = (:) p $ factor' primes_rest (n `div` p)
	where
		p :: Integer
		primes_rest :: [Integer]
		(p, primes_rest)
			= fromJust
			. ZList.find_and_rest (\p -> (n `mod` p) == 0)
			$ primes

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>. Essentially, linear in the time it takes to factor the number.
num_divisors :: Integer -> Integer
num_divisors
	= pred
	. product
	. map (succ . snd) -- succ because p_i^0 is a valid choice
	. ZList.elem_counts
	. factor

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>. Essentially, linear in the time it takes to factor the number.
num_divisors_of_n_squared_leq_n :: Integer -> Integer
num_divisors_of_n_squared_leq_n
	= succ
	. (\m -> m `div` 2)
	. product
	. map (succ . ((*) 2) . snd) -- succ because p_i^0 is a valid choice
	. ZList.elem_counts
	. factor

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>. Essentially, linear in the time it takes to factor the number.
divisors_number_is_perfect_square :: Integer -> [Integer]
divisors_number_is_perfect_square = factors_to_divisors . ZList.elem_counts . factor_number_is_perfect_square

-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>. Essentially, linear in the time it takes to factor the number.
divisors :: Integer -> [Integer]
divisors = factors_to_divisors . ZList.elem_counts . factor

factors_to_divisors :: [(Integer, Integer)] -> [Integer]
factors_to_divisors
	= (\l -> if l == [] then [] else init l)
	. List.sort
	. map product
	. map (map (\(p, a) -> p^a))
	. factors_to_divisors_rec
	where
		factors_to_divisors_rec :: [(Integer, Integer)] -> [[(Integer, Integer)]]
		factors_to_divisors_rec = map (filter ((/=) 0 . snd)) . factors_to_divisors_rec'

		factors_to_divisors_rec' :: [(Integer, Integer)] -> [[(Integer, Integer)]]
		factors_to_divisors_rec' [] = []
		factors_to_divisors_rec' ((p, a):[]) = [[(p, a')] | a' <- [0..a]]
		factors_to_divisors_rec' ((p, a):factors) =
			(:) <$> curr_pairs <*> recs
			where
				curr_pairs :: [(Integer, Integer)]
				curr_pairs = [(p, a') | a' <- [0..a]]

				recs :: [[(Integer, Integer)]]
				recs = factors_to_divisors_rec factors

-- ---------------------------------------------------------------------
-- Square roots

sqrt_convergents_rec :: (Integer, Integer) -> (Integer, Integer) -> [Integer] -> [(Integer, Integer)]
sqrt_convergents_rec (a'', b'') (a', b') cf =
	(a, b) : sqrt_convergents_rec (a', b') (a, b) cf'
		where
			a = e * a' + a''
			b = e * b' + b''
			e = head cf
			cf' = tail cf

-- | A list of fractions monotonically increasingly accurately approximating the square root of the parameter, where each fraction is represented as a pair of @(numerator, denominator)@ See <http://en.wikipedia.org/wiki/Convergent_(continued_fraction)>.
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

-- | An infinite list of integers with irrational square roots.
irrational_squares :: [Integer]
irrational_squares = map round $ filter (not . is_int . sqrt) [1..] 

next_continued_fraction_sqrt :: (Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer, Integer)
next_continued_fraction_sqrt (d, m, a, a0, n) = (d', m', a', a0, n)
	where
		d' = (n - m'^2) `div` d
		m' = (d * a)  - m
		a' = floor $ (fromIntegral (a0 + m')) / (fromIntegral d')

-- | An infinite list of the terms of the continued fraction representation of the square root of the given parameter.
continued_fraction_sqrt_infinite :: Integer -> [Integer]
continued_fraction_sqrt_infinite n =
		map trd5
			$ iterate next_continued_fraction_sqrt (d0, m0, a0, a0, n)
	where
		m0 = 0
		d0 = 1
		a0 = floor . sqrt . fromInteger $ n
		trd5 (_, _, x, _, _) = x

-- | /O(k)/ The <http://en.wikipedia.org/wiki/Continued_fraction continued fraction> representation of the square root of the parameter. /k/ is the length of the continued fraction.
continued_fraction_sqrt :: Integer -> [Integer]
continued_fraction_sqrt n
	= ZList.take_while_keep_last (/= (2 * a0))
	. continued_fraction_sqrt_infinite
	$ n
		where
			a0 = floor . sqrt . fromInteger $ n

-- | Determines whether the given integer is a square number.
square :: Integer -> Bool
square = is_int . sqrt . fromIntegral

-- | Takes the square root of a perfect square.
sqrt_perfect_square :: Integer -> Integer
sqrt_perfect_square = toInteger . ceiling . sqrt . fromInteger

-- ---------------------------------------------------------------------
-- Assorted functions

pow :: Integer -> Integer -> (Integer -> Integer -> Integer) -> (Integer -> Integer) -> Integer
pow b 0 _ _ = 1
pow b 1 _ _ = b
pow b_original e_original mul sq = pow' b_original e_original 1
	where
		pow' :: Integer -> Integer -> Integer -> Integer
		pow' b e curr_e
			= if curr_e == e
				then b
				else if (curr_e * 2) <= e
					then pow' (sq b) e (curr_e * 2)
					else b `mul` (pow' b_original (e - curr_e) 1)

-- | /O(log_2 e)/ Raises base /b/ (2nd param) to exponent /e/ (3rd param) mod /m/ (1st param). E.g.:
--
--
--     > pow_mod 13 2 4
--
--     > 3 
pow_mod :: Integer -> Integer -> Integer -> Integer
pow_mod m b e = pow b e (mul_mod m) (square_mod m)
	where
		square_mod :: Integer -> Integer -> Integer
		square_mod m a = (a * a) `mod` m

-- | Multiplies the second parameter by the third, mod the first. E.g.:
--
--
--     > mul_mod 5 2 4
--
--     > 3
mul_mod :: Integer -> Integer -> Integer -> Integer
mul_mod m a b = (a * b) `mod` m

-- | Adds the second parameter by the third, mod the first. E.g.:
--
--
--     > add_mod 5 3 4
--
--     > 2
add_mod :: Integer -> Integer -> Integer -> Integer
add_mod m a b = (a + b) `mod` m

-- | Subtracts the third parameter from the second, mod the first. E.g.:
--
--
--     > sub_mod 5 16 7
--
--     > 4
sub_mod :: Integer -> Integer -> Integer -> Integer
sub_mod m a b = (a - b) `mod` m

-- | Divides the second parameter by the third, mod the first. More explicitly, it multiplies the second by the multiplicative inverse of the third, mod the first. E.g.:
--
--
--     > div_mod 5 16 7
--
--     > Just 3
--
-- Note that only elements coprime to the modulus will have inverses; in cases that do not match this criterion, we return Nothing.
div_mod :: Integer -> Integer -> Integer -> Maybe Integer
div_mod m a b =
	if isJust b'
		then Just (mul_mod m a (fromJust b'))
		else Nothing
		where
			b' :: Maybe Integer
			b' = multiplicative_inverse m b

-- | Like @div_mod@, but with the assurance that the modulus is prime (i.e. denominator will have an inverse). Thus, the returnvalue doesn't need to be wrapped in a @Maybe@.
div_mod_prime :: Integer -> Integer -> Integer -> Integer
div_mod_prime m a b = fromJust (div_mod m a b)

-- | /O(log m)/ Computes the multiplicative inverse of the second parameter, in the group /Z_m/, where /m/ is the first parameter. E.g.:
--
--
--     > multiplicative_inverse 13 6
--
--     > Just 11
--
-- That is, 6 * 11 = 66, and 66 `mod` 13 == 1 . Note that only elements coprime to the modulus will have inverses; in cases that do not match this criterion, we return Nothing.

multiplicative_inverse :: Integer -> Integer -> Maybe Integer
multiplicative_inverse m g =
	if coprime m g
		then Just (pow_mod m g (m - 2))
		else Nothing

-- ---------------------------------------------------------------------
-- Assorted functions

-- | An infinite list of the Fibonacci numbers.
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- | /O(1)/ Area of a triangle, where the parameters are the edge lengths (Heron's formula).
tri_area :: Integer -> Integer -> Integer -> Double
tri_area a b c = 
	sqrt $ p * (p-a') * (p-b') * (p-c')
		where
			a' = fromInteger a
			b' = fromInteger b
			c' = fromInteger c
			p = (fromInteger (a + b + c)) / 2

-- | /O(1)/ Area of a triangle, where the parameters are the edge lengths (Heron's formula).
tri_area_double :: Double -> Double -> Double -> Double
tri_area_double a b c = 
	sqrt $ p * (p-a) * (p-b) * (p-c)
		where
			p :: Double
			p = (a + b + c) / 2

-- | /O(1)/ Calculates whether /n/ is the /e/^th power of any integer, where /n/ is the first parameter and /e/ is the second.
is_power_of_int :: Integer -> Integer -> Bool
is_power_of_int n e = (round (fromIntegral n ** (1/(fromInteger e))))^e == n

-- | /O(log_10(n))/ Calculates the number of digits in an integer. Relies on @logBase@, so gives wrong answer for very large `n`.
num_digits :: Integer -> Integer
num_digits n = (1 + (floor $ logBase 10 (fromInteger n)))

-- | Returns whether a @Double@ value is an integer. For example, @16.0 :: Double@ is an integer, but not @16.1@.
is_int :: Double -> Bool
is_int x = x == (fromInteger . round $ x)

-- | Converts a @Double@ to an @Integer@.
double_to_int :: Double -> Integer
double_to_int = (toInteger . round)

type RowAndRHS = ([Double], Double)
type LinearSystem = [RowAndRHS]
-- | Solves a given system of linear equations. Can be subject to rounding errors. Here's an example:
--
--
--     > solve_linear_system [[2, 3, 4],[6, -3, 9],[2, 0, 1]] [20, -6, 8]
--
--     > [4.999999999999999,6.0,-2.0]
solve_linear_system :: [[Double]] -> [Double] -> [Double]
solve_linear_system a b
	= map (\n -> if n == 0 then 0 else n)
	. solve_row_echelon_system
	. map (ZList.map_fst (map epsilon_round))
	. perform_gaussian_elimination 0
	$ zip a b

epsilon_round :: Double -> Double
epsilon_round n
	= if (abs $ (fromIntegral . round $ n) - n) < epsilon
		then fromIntegral . round $ n
		else n
	where
		epsilon :: Double
		epsilon = 0.0001

(<+>) :: RowAndRHS -> RowAndRHS -> RowAndRHS
a@(a1, a2) <+> b@(b1, b2) = (zipWith (+) a1 b1, a2 + b2)

scale :: Double -> RowAndRHS -> RowAndRHS
scale s (a, b) =
	( map ((*) s) a
	, (*) s b )

solve_row_echelon_system :: LinearSystem -> [Double]
solve_row_echelon_system [] = []
solve_row_echelon_system (curr : rest)
	= curr_solved : rest_solved
	where
		curr_solved :: Double
		curr_solved = solve_row_echelon_system_row curr rest_solved

		rest_solved :: [Double]
		rest_solved = solve_row_echelon_system rest

-- e.g.
--     (row, rhs)    = ([2.0,1.0,-1.0], 8.0)
--     solved_coeffs = [3,-1]
solve_row_echelon_system_row :: RowAndRHS -> [Double] -> Double
solve_row_echelon_system_row (row', rhs) []
	= snd . normalize $ (row', rhs)
solve_row_echelon_system_row (row', rhs) solved_coeffs
	= snd
	. normalize
	$ List.foldl'
		f
		(row, rhs)
		(zip [0..] scalars)
	where
		f :: RowAndRHS -> (Int, Double) -> RowAndRHS
		f (r, x) (i, s)
			= (<+>) (make_row_and_rhs i)
			. scale s
			$ (r, x)

		make_row_and_rhs :: Int -> RowAndRHS
		make_row_and_rhs i =
			((replicate (i+1) 0)
				++ [1]
				++ (replicate (length row - i - 2) 0)
			, solved_coeffs !! i)

		-- parallel to solved_coeffs
		scalars :: [Double]
		scalars
			= (:) (head individual_scalars)
			$ zipWith (/) (tail individual_scalars) individual_scalars
			where
				individual_scalars :: [Double]
				individual_scalars
					= tail
					$ map (\r -> -1 / r) row

		row :: [Double]
		row = dropWhile ((==) 0) row'

perform_gaussian_elimination :: Int -> LinearSystem -> LinearSystem
perform_gaussian_elimination row system
	| ((row + 1) == (length system) - 1) = initial ++ [scale_and_add (last initial) (head system')]
	| otherwise = perform_gaussian_elimination (succ row) (initial ++ rest)
		where
			initial :: LinearSystem
			initial = take (row + 1) system

			system' :: LinearSystem
			system' = drop (row + 1) system

			rest :: LinearSystem
			rest
				= zipWith scale_and_add
					system'
					(repeat $ last initial)

			scale_and_add :: RowAndRHS -> RowAndRHS -> RowAndRHS
			scale_and_add one two = one <+> (scale scalar two)
				where
					scalar :: Double
					scalar = -1 * (((fst one) !! row) / ((fst two) !! row))

normalize :: RowAndRHS -> RowAndRHS
normalize (row, rhs) = (map f row, rhs / scalar)
	where
		f :: Double -> Double
		f n = if n /= 0 then 1 else 0

		scalar :: Double
		scalar = fromJust . List.find ((/=) 0) $ row
