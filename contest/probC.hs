-- Joe Schafer
-- Problem C
-- September 2009

import Data.List(subsequences, foldl', inits, tails, sort, group, maximumBy)
import Data.Function(on)    

-- | Get a list of overlaps between two strings.
overlaps :: String -> String -> [String]
overlaps s t = words $ zipWith combine s t
    where combine a b | a == b = a
                      | otherwise = ' '

-- | Remove all strings that aren't long enough to be a motif.
motifs :: [String] -> [String]
motifs = filter ((>1) . length)

-- | Get all overlaps of a string with itself.  Compare a string to
-- itself shifted right to every position within the string.
allOverlaps :: String -> [String]
allOverlaps s = concatMap (map reverse) $ foldl' step [] suffixes
    where
      step a b = s' `overlaps` b : a
      s' = reverse s
      -- Drop the empty string, single length string and the complete
      -- string.
      suffixes = map reverse . init . drop 2 . inits $ s

mostCommonMotif :: [String] -> String
mostCommonMotif ls | null ls' = ""
                   | otherwise =  head . maximumBy (compare `on` length) . group .
                                  sort $ ls'
                   where ls' = motifs ls

motifSequences :: String -> [String]
motifSequences [] = []
motifSequences (x:y:ys) = [x] : map (x:) ms ++ ms ++ ns
    where (ms,ns) = span ((==y) . head) . motifSequences $ ys
motifSequences xs = [xs]

solver :: String -> String
solver s | t == "" = "NONE"
         | otherwise = t
         where t = mostCommonMotif . allOverlaps $ s

processData :: [String] -> [String]
processData = map solver


main :: IO ()
main = readFile "inC.txt" >>=
       (putStr . unlines . map solver . init . tail . lines)

tests = [("ABABCABABDABABE", "AB"),
         ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", "AA")]

testall = [solver input == expected | (input, expected) <- tests]

lots = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"