
module HLib.Math
( primes
, is_prime
, coprime
, continued_fraction_sqrt
, continued_fraction_sqrt_infinite
, divides
, euler_phi
, factorize
, fromRealFrac
, get_divisors
, get_num_digits
, irr_squares
, is_int
, is_power
, sqrt_convergents
, to_int
, tri_area
, tri_area_double
) where

import qualified Data.List as List

import HLib.List

-- http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)
primes :: [Integer]
nonprimes :: [Integer]
primes = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes)
nonprimes = foldr1 f $ map g $ tail primes
	where
		f (x:xt) ys = x : (merge_infinite xt ys)
		g p		 = [ n * p | n <- [p, p + 2 ..]]

		merge_infinite :: (Ord a) => [a] -> [a] -> [a]
		merge_infinite xs@(x:xt) ys@(y:yt) = 
			case compare x y of
				LT -> x : (merge_infinite xt ys)
				EQ -> x : (merge_infinite xt yt)
				GT -> y : (merge_infinite xs yt)

is_prime_rec :: Integer -> Integer -> Bool
is_prime_rec n k
	| (n <= 1) = False
	| (fromInteger k >= ((fromInteger n) / 2) + 1.0) = True
	| ((n `mod` k) == 0) = False
	| otherwise = is_prime_rec n (k+1)

is_prime :: Integer -> Bool
is_prime n = is_prime_rec n 2

coprime :: Integer -> Integer -> Bool
coprime a b = Nothing == List.find is_common_divisor [2..(min a b)]
	where
		is_common_divisor n = (a `mod` n == 0) && (b `mod` n == 0)

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

fromRealFrac :: Integer -> Double
fromRealFrac = fromRational . toRational

is_int :: Double -> Bool
is_int x = x == (fromInteger (round x))

get_divisors :: Integer -> [Integer]
get_divisors = init . uniqueify . map product . powerset . factorize

to_int :: Double -> Integer
to_int = (toInteger . round)
