
module HLib.Tuple
( triple
, fst3
, snd3
, trd3
, pair
, pairAppend
, pairMap
) where

triple :: a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)

fst3 :: (a, a, a) -> a
fst3 (a, b, c) = a

snd3 :: (a, a, a) -> a
snd3 (a, b, c) = b

trd3 :: (a, a, a) -> a
trd3 (a, b, c) = c

pair :: a -> b -> (a, b)
pair a b = (a, b)

pairAppend :: (a, b) -> c -> (a, b, c)
pairAppend (a, b) c = (a, b, c)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, a') = (f a, f a')
