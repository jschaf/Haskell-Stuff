-- Joe Schafer
-- Project Euler #3
-- http://projecteuler.net/index.php?section=problems&id=3

-- The prime factors of 13195 are 5, 7, 13 and 29.  What is the
-- largest prime factor of the number 600851475143?

-- Answer: 6857

import qualified Data.Map as Map

input = 600851475143

-- mapSieve :: [Int] -> [Int]    
mapSieve xs = sieve xs Map.empty
    where
      sieve []     table = []
      sieve (x:xs) table =
          case Map.lookup x table of
            Nothing -> x : sieve xs (Map.insert (x*x) [x] table)
            Just facts -> sieve xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table


primes = mapSieve [2..]         -- Using a wheel wil break this sieve.

factors :: Integer -> [Integer]
factors n = factors' n primes
    where
      factors' n pss@(p:ps)
          | n == 1         = []
          | n `mod` p == 0 = p : factors' (n `div` p) pss
          | otherwise      = factors' n ps

main = putStrLn . show . last . factors $ input
