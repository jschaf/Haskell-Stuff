-- Joe Schafer
-- Project Euler #10
-- http://projecteuler.net/index.php?section=problems&id=10

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- 
-- Find the sum of all the primes below two million.

-- Answer: 142913828922

import qualified Data.Map as Map

mapSieve :: [Integer] -> [Integer]    
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

main = putStrLn . show . sum . takeWhile (<2000000) $ primes         