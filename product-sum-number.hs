{-# LANGUAGE ImplicitParams, TupleSections #-}

import Data.Array
import Data.List

sieve n = go 2 $ listArray (2,n) (repeat True)
  where go x a | x > n = a
               | a!x = go (x+1) (a//[(k*x, False) | k <- [2..div n x]])
               | otherwise = go (x+1) a

lim :: Int
lim = 15000

primes = filter isprime [2..lim]
isprime = (sieve lim!)

a `divides` b = b `mod` a == 0

factor n = go n primes
  where go n (p:ps) | isprime n = [n]
                    | p `divides` n = p : go (div n p) (p:ps)
                    | otherwise = go n ps

factors n = let f:fs = factor n in count 1 f fs
  where count c a [] = [(a,c)]
        count c a (f:fs) | f == a = count (c+1) f fs
                         | otherwise = (a,c) : count 1 f fs

divisors = go . factors
  where go [] = [1]
        go ((p,c):fs) = do
          x <- [p^b | b <- [0..c]]
          d <- go fs
          return (x*d)

products n = go 2 n
  where go l 1 = [[]]
        go l n = do
          a <- filter (>= l) (divisors n)
          as <- go a (div n a)
          return (a:as)

ways n = (\xs -> n - sum xs + length xs) <$> products n

smallest = array (2,12000) $ [(w,n) | n <- [lim,lim-1..2], w <- ways n, 2 <= w && w <= 12000]

main = print . sum . nub . elems $ smallest
