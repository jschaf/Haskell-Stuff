-- Joe Schafer
-- Problem H
-- October 2009


processData :: [String] -> [String]
processData = map (show . handPoints . map readCard . words)

main :: IO ()
main = readFile "inF.txt" >>=
       (putStr . unlines . processData . init . tail . lines)

card1 = map readCard ["QS", "7S", "3S", "2S", "KH", "QH", "TH", "8H", "6H", "QC",
                      "5C", "4C", "2C"]
card2 = map readCard ["2C", "AS", "6C", "JS", "KH", "4S", "3C", "8C", "4C", "5D",
                      "5C", "7C", "4D"]