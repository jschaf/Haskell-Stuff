-- Joe Schafer
-- Problem D
-- September 2009

import Data.List
import Test.QuickCheck

{-# LANGUAGE GeneralizedNewtypeDeriving #-}    
    
-- | Pass reverse-sorted data into powersum and pick the best option.    
solver :: [Int] -> [Int]
solver xs = [closest, sum xs - closest]
    where goal = sum xs `div` 2
          closest = head . powersum goal . reverse . sort $ xs

-- Copied from
-- http://www.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss `interleave` map (x:) xss
--     where xss = powerset xs

-- | Combine powerset with the problem.  Instead of using lists,
-- perform summation while building the powerset.  Also, drop all
-- elements larger than the goal.  This is valid because there will be
-- a counterpart to the larger item.  When we merge, remove duplicates
-- which will not change the result.
powersum :: Int -> [Int] -> [Int]
powersum _ [] = [0]
powersum goal (x:xs) = zs
    where xss = powersum goal xs
          zs  = dropWhile (> goal) $ map (x+) xss `merge` xss

-- | Merge two sorted lists from largest to smallest, removing
-- duplicates. Modified slightly from:
-- http://stackoverflow.com/questions/941699/merge-sorted-inputs-in-haskell
merge :: (Ord a) => [a] -> [a] -> [a]
[] `merge` ys = ys
xs `merge` [] = xs
(x:xs) `merge` (y:ys) = case x `compare` y of
    LT -> y : (x:xs) `merge` ys
    EQ -> x : xs `merge` ys
    GT -> x : xs `merge` (y:ys)

-- | Print a string that matches the output format.
listString :: Show a => [a] -> String
listString = tail . concatMap ((' ' :) . show)

processData :: [String] -> [String]
processData = map (listString . solver . tail . readInts . words)
              where readInts :: [String] -> [Int]
                    readInts = map read

main :: IO ()
main = readFile "inD.txt" >>=
       (putStr . unlines . processData . init . tail . lines)

-- newtype ProbInt = ProbInt Int
--     deriving (Eq, Ord, Show, Num)
             
-- class Arbitrary ProbInt where
--     arbitrary = choose (1, 5000)

-- prop_ordered :: ProbInt -> [ProbInt] -> Property                
prop_ordered goal xs = ordered (powersum goal xs)
    where
      ordered :: [Int] -> Bool
      ordered []       = True
      ordered [x]      = True
      ordered (x:y:zs) = x >= y && ordered (y:zs)

prop_unique goal xs = nub zs == zs where zs = powersum goal xs

-- Broken
prop_maximum goal xs = length xs > 2 && goal > minimum xs ==> head zs == maximum zs where zs = powersum goal xs