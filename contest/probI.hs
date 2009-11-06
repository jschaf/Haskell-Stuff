-- Joe Schafer
-- Problem I

import Data.Ratio

-- | Given a percentage, return the smallest rational
-- | representation of the percentage with an error of +/- 1%.
minRatio :: Integral a => a -> Rational
-- Sweet, all my work is done for me.
minRatio = flip approxRational (1 % 100) . (%100)
           
leastRespondents :: Integral a => [a] -> Integer
leastRespondents ns = commonDenom
    where
      ratios = map minRatio ns
      denoms = map denominator ratios
      commonDenom = foldr1 lcm denoms

processData :: [String] -> [String]
processData = map (show . leastRespondents . map read . words)

tests = [
 (5, [80]),
 (4, [50,75]),
 (6, [67,50]),
 (8, [87,13,50])]

testAll = [ans == leastRespondents input | (ans, input) <- tests]

main :: IO ()
main = readFile "inI.txt" >>=
       (putStr . unlines . processData . init . tail . lines)
