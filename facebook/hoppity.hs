-- Joe Schafer
-- Facebook Engineering Puzzles
-- Hoppity

import System (getArgs)

answer :: Int -> [String]
answer n = [fizz(i) | i <- [1..n], i `mod` 3 == 0 || i `mod` 5 == 0]
    where fizz j | j `mod` 15 == 0 = "Hop"
                 | j `mod` 5 == 0  = "Hophop"
                 | otherwise       = "Hoppity"

main :: IO ()
main = getArgs >>=
  (readFile . head) >>=
  (putStr . unlines . answer . read . head . lines)

