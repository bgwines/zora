-- |
-- Module      : Zora.Math
-- Copyright   : (c) Brett Wines 2014
--
-- License     : BSD-style
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
, coprime
, euler_phi
, factor
, divisors

-- * Square roots
, irrational_squares
, sqrt_convergents
, continued_fraction_sqrt
, continued_fraction_sqrt_infinite

-- * Assorted functions
, is_int
, is_power_of_int
, int_to_double
, num_digits
, tri_area
, tri_area_double
) where

import qualified Data.List as List

import Data.Maybe

import Zora.List

-- ---------------------------------------------------------------------
-- Prime numbers and division

-- | A complete, monotonically increasing, infinite list of primes. Implementation from <http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)>.
primes :: [Integer]
primes = [2, 3, 5] ++ (diff_infinite [7, 9 ..] composites)

-- | A complete, monotonically increa?ing, infinite list of composite numbers.
composites :: [Integer]
composites = foldr1 f $ map g $ tail primes
	where
		f (x:xt) ys = x : (merge_infinite xt ys)
		g p		 = [ n * p | n <- [p, p + 2 ..]]

		merge_infinite :: (Ord a) => [a] -> [a] -> [a]
		merge_infinite xs@(x:xt) ys@(y:yt) = 
			case compare x y of
				LT -> x : (merge_infinite xt ys)
				EQ -> x : (merge_infinite xt yt)
				GT -> y : (merge_infinite xs yt)

prime_rec :: Integer -> Integer -> Bool
prime_rec n k
	| (n <= 1) = False
	| (fromInteger k >= ((fromInteger n) / 2) + 1.0) = True
	| ((n `mod` k) == 0) = False
	| otherwise = prime_rec n (k+1)

-- | /O(n)/ Returns whether the parameter is a prime number.
prime :: Integer -> Bool
prime n = prime_rec n 2

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

-- TODO: don't start over in `primes`
-- | /O(k n log(n)^-1)/, where /k/ is the number of primes dividing /n/ (double-counting for powers). /n log(n)^-1/ is an approximation for <http://en.wikipedia.org/wiki/Prime-counting_function the number of primes below a number>.
factor :: Integer -> [Integer]
factor 0 = []
factor 1 = []
factor n = p : factor (n `div` p)
	where p = fromJust . List.find (\p -> (n `mod` p) == 0) $ primes

-- | /O(4^(k n log(n)^-1))/, where /k/ is the number of primes dividing /n/ (double-counting for powers).
divisors :: Integer -> [Integer]
divisors = init . uniqueify . map product . powerset . factor

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
continued_fraction_sqrt n =
	take_while_keep_last (/= (2 * a0)) . continued_fraction_sqrt_infinite $ n
		where
			a0 = floor . sqrt . fromInteger $ n

-- ---------------------------------------------------------------------
-- Assorted functions

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
			p = (a + b + c) / 2


-- | /O(1)/ Calculates whether /n/ is the /e/^th power of any integer, where /n/ is the first parameter and /e/ is the second.
is_power_of_int :: Integer -> Integer -> Bool
is_power_of_int n e = (round (fromIntegral n ** (1/(fromInteger e))))^e == n

-- | /O(log_10(n))/ Calculates the number of digits in an integer.
num_digits :: Integer -> Integer
num_digits n = (1 + (floor $ logBase 10 (fromInteger n)))

-- | Returns whether a @Double@ value is an integer. For example, @16.0 :: Double@ is an integer, but not @16.1@.
is_int :: Double -> Bool
is_int x = x == (fromInteger (round x))

-- | Converts a @Double@ to an @Integer@.
int_to_double :: Double -> Integer
int_to_double = (toInteger . round)
