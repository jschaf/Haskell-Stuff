-- Joe Schafer
-- Problem J

import Data.List (nub, maximumBy, sort, groupBy, (\\))
import Data.Function (on)
import Test.QuickCheck
import Data.Word(Word32)

-- | Given a list where the first element is the range of numbers
-- (1..n) and the rest are already picked numbers, return the
-- beginning of the largest open range.  If there is a tie, return the
-- lowest beginning number, hence the call to reverse because maximum
-- returns the last biggest number.
bestPick :: [Int] -> Int
bestPick = longestStart . reverse . groupRanges . choices

-- | Group a sorted list by elements that are in successive order.
-- Each sublist is a continous range of elements.
groupRanges :: (Eq a, Enum a) => [a] -> [[a]]
groupRanges l = foldr step [[last l]] (init l)
    where step a bs'@(b:bs) | succ a == head b = (a:b) : bs
                            | otherwise = [a] : bs'

-- | Given a list consisting of a max number and followed by the
-- numbers already selected, return the unselected numbers.  The
-- unselected numbers are in sorted order because we build the source
-- list as [1..N].
choices :: (Num a, Ord a, Enum a, Eq a) => [a] -> [a]
choices (n:xs) = [1..n] \\ xs

-- | Given a list of lists, return the first element of the longest
-- list.
longestStart :: [[a]] -> a
longestStart = head . maximumBy (compare `on` length)
   
processData :: [String] -> [String]
processData = map (show . bestPick . map read . init . words)

main :: IO ()
main = readFile "inJ.txt" >>=
       (putStr . unlines . processData . init . tail . lines)

tests = [
 (1, [1]),
 (1, [2,2]),
 (2, [100,1]),
 (3, [10,5,2,8]),
 (2, [3,1,3]),
 (5, [5,1,2,3,4]),
 (1, [5,2,3,4,5]),
 (7, [1000,1,6]),
 (1, [7,4]),
 (5, [8,4]),
 (1000, 1000:[1..999]),
 (1, 1000:[2,4..1000])]

testAll = [ans == bestPick input | (ans, input) <- tests]


-- -- Take from:
-- -- http://www.mail-archive.com/haskell-cafe@haskell.org/msg10221.html
-- instance Arbitrary Word32 where
--     arbitrary = do let mx,mn :: Integer
--                        mx = fromIntegral (maxBound :: Word32)
--                        mn = fromIntegral (minBound :: Word32)
--                    c <- choose (mx, mn)
--                    return (fromIntegral c)
-- -- coarbitrary a = error "Not implemented"

-- type Nat = Word32

    
-- prop_groupRangesSum :: [[Nat]] -> Bool
-- prop_groupRangesSum l = actual == expected
--     where g = groupRanges . sort . nub $ l
--           actual = map sum g
--           expected = [(head ks + last ks) * (length ks) `div` 2 | ks <- g]
          