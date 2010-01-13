-- Joe Schafer

import Data.List (sort, permutations, findIndex, nub)
import Data.Maybe (fromJust)

blah = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)  
  

-- Really simple version.  Breaks for long strings
permutNaive :: [String] -> Int
permutNaive (a:b:[]) = abs $ (fromJust (findIndex (==a) ps)) - (fromJust (findIndex (==b) ps))
  where ps = nub . sort . permutations $ a
permutDistance _ = undefined

processData :: [String] -> [String]
processData = undefined -- map (show . permutDistance . words)

main :: IO ()
main = readFile "inK.txt" >>=
       (putStr . unlines . processData . init . tail . lines)
