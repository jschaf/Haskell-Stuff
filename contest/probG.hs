-- Joe Schafer
-- Problem G
-- October 2009

import Data.Word(Word32)
import Test.QuickCheck
import Debug.Trace

mpg = 30.0 :: Double
tankSize = 15.0 :: Double

-- | Separate the even and odd indices of a list.
splitOddEven :: [a] -> ([a], [a])
splitOddEven ls = (splitter ls, splitter $ tail ls)
    where splitter [] = []
          splitter [x] = [x]
          splitter (x:y:ys) = x : splitter ys

-- | Given a list of gas prices and distances between gas stations,
-- return the minimum cost in cents to finish the trip.  This function
-- handles the stripped data from the file.
minTripCost :: [Int] -> Int
minTripCost (l:ls) = tripExpense gasLeft prices dists
    where gasLeft = gasAfter tankSize l
          (prices, dists) = splitOddEven ls

-- | Given the gas remaining and distance to travel, return how much
-- gas is left.
gasAfter :: Double -> Int -> Double
gasAfter g d = g - (d' / mpg)
    where d' = fromIntegral d

-- | Given the gas remaining and the gas price in cents, return the
-- cost to fill the tank to capacity in cents (rounded up).
fillCost :: Double -> Int -> Int
fillCost g c = ceiling $ (tankSize - g) * (c' + 0.9) 
    where c' = fromIntegral c


-- | Given the gas remaining, a list of gas prices, and a list of
-- distances, return the minimum cost to complete the trip from this
-- point.
tripExpense :: Double -> [Int] -> [Int] -> Int
tripExpense _ [] _ = 0
tripExpense _ _ [] = 0
tripExpense g (p:ps) (d:ds)
    | gasAfter g d >= 0 = min fillHere skipHere
    | otherwise         = fillHere
    where
      fillHere = fillCost g p + expense (p:ps) (d:ds)
      skipHere = tripExpense (gasAfter g d) ps ds

-- | Given a list of gas prices, and a list of distances, return the
-- minimum cost to complete the trip from this point.  This assumes
-- starting with a full tank of gas.  This function should (hopefully)
-- be memoized.
expense :: [Int] -> [Int] -> Int
expense [] _ = 0
expense _ [] = 0
expense (p:ps) (d:ds) = tripExpense (gasAfter tankSize d) ps ds

processData :: [String] -> [String]
processData = map (show . minTripCost . map read . tail . words)

main :: IO ()
main = readFile "inG.txt" >>=
       (putStr . unlines . processData . init . tail . lines)

trip1 = [400, 279, 100, 283, 300, 262, 100] :: [Int]
trip2 = [250, 323, 200, 239, 100, 335, 123, 399, 257, 346, 193, 405, 1] :: [Int]
trip3 = [250, 222, 150] :: [Int]

debug :: Show a => a -> a
debug x = trace (show x) x

debug' :: Show a => String -> a -> a
debug' s x = trace (s ++ show x) x

-- Take from:
-- http://www.mail-archive.com/haskell-cafe@haskell.org/msg10221.html
instance Arbitrary Word32 where
    arbitrary = do let mx,mn :: Integer
                       mx = fromIntegral (maxBound :: Word32)
                       mn = fromIntegral (minBound :: Word32)
                   c <- choose (mx, mn)
                   return (fromIntegral c)
-- coarbitrary a = error "Not implemented"

type Nat = Word32

prop_zeroCostTrip :: [Nat] -> Bool
prop_zeroCostTrip  = (0<=) . sum