-- Joe Schafer
-- Problem C
-- September 2009

type Note is Char


primaryMotif :: [Note] -> [Note]
primaryMotif = 
    
    

processData :: [String] -> [String]
processData = map primaryMotif


main :: IO ()
main = readFile "inD.txt" >>=
       (putStr . unlines . processData . init . tail . lines)
