-- Joe Schafer
-- Project Euler #7
-- http://projecteuler.net/index.php?section=problems&id=7

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
-- can see that the 6th prime is 13.

-- What is the 10001st prime number?

import qualified Data.Map as Map

mapSieve :: [Int] -> [Int]    
mapSieve xs = sieve xs Map.empty
    where
      sieve []     table = []
      sieve (x:xs) table =
          case Map.lookup x table of
            Nothing    -> x : sieve xs (Map.insert (x*x) [x] table)
            Just facts -> sieve xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

primes = mapSieve [2..]         -- Using a wheel wil break this sieve.

main = putStrLn . show $ primes !! 10000