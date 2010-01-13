-- Joe Schafer
-- Project Euler #3
-- http://projecteuler.net/index.php?section=problems&id=3

-- The prime factors of 13195 are 5, 7, 13 and 29.  What is the
-- largest prime factor of the number 600851475143 ?

-- Answer: 6857

--import Data.Edison.Coll.MinHeap as H
-- import Criterion.Main (defaultMain)

import Data.Function (on)

import qualified Data.Map as Map
-- import Data.Edison.Prelude
-- import qualified Data.Edison.Assoc.StandardMap as Map
import qualified Data.Edison.Coll.MinHeap as Min
import qualified Data.Edison.Coll.SplayHeap as Splay
-- import qualified Data.Edison.Coll.LazyPairingHeap as Pair


-- | Get prime candidates of the form 6n +/- 1.  This does not include
-- 2 or 3.
primeCands :: [Int]
primeCands = cand' 5
    where
      cand  n = n : cand' (n + 4)
      cand' n = n : cand  (n + 2)
                
primesTrial = 2 : [x | x <- [3..], isPrime x]
    where
      isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
      factorsToTry x = takeWhile (\p -> p * p <= x) primesTrial

primesTrial' = 2 : [x | x <- 3:primeCands, isPrime x]
    where
      isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
      factorsToTry x = takeWhile (\p -> p * p <= x) primesTrial'
        
primesSieve :: Int -> [Int]
primesSieve = undefined


mapSieve xs = sieve xs Map.empty
    where
      sieve []     table = []
      sieve (x:xs) table =
          case Map.lookup x table of
            Nothing -> x : sieve xs (Map.insert (x*x) [x] table)
            Just facts -> sieve xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

type MinHeap a = Min.Min (Splay.Heap a) a
data Entry = Entry {composite :: Int,
                    factors   :: [Int]}
           deriving (Show)

instance Eq Entry where
    (==) = (==) `on` composite

instance Ord Entry where
    (<=) = (<=) `on` composite

-- type MinHeap k a = Min.Min (Map.FM k a) k
prioritySieve :: [Int] -> [Int]
prioritySieve [] = []
prioritySieve (x:xs) = sieve' xs (insertPrime x xs Min.empty)
    where
      insertPrime p xs table = Min.insert (Entry (p*p) (map (*p) xs)) table
      sieve' :: [Int] -> MinHeap Entry -> [Int]
      sieve' []     table = []
      sieve' (x:xs) table
          | nextComposite <= x = sieve' xs (adjust table)
          | otherwise          = x : sieve' xs (insertPrime x xs table)
          where
            nextComposite = composite (Min.minElem table)
            adjust table
                | n <= x = adjust (Min.insert (Entry n' ns) (Min.deleteMin table))
                | otherwise = table
                where
                  (n, n':ns) = let m = Min.minElem table
                               in (composite m, factors m)

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8
            :6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n + x)

primes = 2 : 3 : 5 : 7 : mapSieve (spin wheel2357 11)

main = putStrLn . show $ sum $ take 100000 primes


customSieve :: [Int] -> [Int]
customSieve [] = []
customSieve (x:xs) = x : sieve xs (insertPrime x xs empty)
    where
      insertPrime p xs table = insert (p*p) (map (*p) xs) table
      sieve [] table = []
      sieve (x:xs) table
          | nextComposite <= x = sieve xs (adjust table)
          | otherwise = x : sieve xs (insertPrime x xs table)
          where
            nextComposite = minKey table
            adjust table
                | n <= x = adjust (deleteMinAndInsert n' ns table)
                | otherwise = table
                where
                  (n, n':ns) = minKeyValue table
       
-- Declare the data type constructors.
data Ord k => PriorityQueue k a = Nil
                                | Branch k a (PriorityQueue k a) (PriorityQueue k a)

-- Declare the exported interface functions.
-- Return an empty priority queue.
empty :: Ord k => PriorityQueue k a
empty = Nil

-- Return the highest-priority key.
minKey :: Ord k => PriorityQueue k a -> k
minKey = fst . minKeyValue

-- Return the highest-priority key plus its associated value.
minKeyValue :: Ord k => PriorityQueue k a -> (k, a)
minKeyValue Nil              = error "empty queue"
minKeyValue (Branch k a _ _) = (k, a)

-- Insert a key/value pair into a queue.
insert :: Ord k => k -> a -> PriorityQueue k a -> PriorityQueue k a
insert k a q = union (singleton k a) q

-- Delete the highest-priority key/value pair and insert a new key/value pair into the queue.
deleteMinAndInsert :: Ord k => k -> a -> PriorityQueue k a -> PriorityQueue k a
deleteMinAndInsert k a Nil              = singleton k a
deleteMinAndInsert k a (Branch _ _ l r) = union (insert k a l) r

-- Declare the private helper functions.
-- Join two queues in sorted order.
union :: Ord k => PriorityQueue k a -> PriorityQueue k a -> PriorityQueue k a
union l Nil = l
union Nil r = r
union l@(Branch kl _ _ _) r@(Branch kr _ _ _)
    | kl <= kr  = link l r
    | otherwise = link r l

-- Join two queues without regard to order.
-- (This is a helper to the union helper.)
link (Branch k a Nil m) r = Branch k a r m
link (Branch k a ll lr) r = Branch k a lr (union ll r)

-- Return a queue with a single item from a key/value pair.
singleton :: Ord k => k -> a -> PriorityQueue k a
singleton k a = Branch k a Nil Nil