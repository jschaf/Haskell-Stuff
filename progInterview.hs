import Data.List
import Data.Function    
import qualified Data.List.Key as K

-- http://smerity.com/interviews.html

-- Write a program to reverse the order of all the words in a
-- string. (i.e. "Hello World" => "World Hello")
reverseWordOrder :: String -> String
reverseWordOrder = unwords . map reverse . words . reverse

-- Given a list of words how would you find those which are all
-- anagrams of each other? (i.e., if given "tree", "evil", "live" and
-- "god" return "live" and "evil" as anagrams of each other)
-- anagrams :: [String] -> [[String]]
-- anagrams = map (map snd) . filter ((>1) . length) . groupBy ((==) `on` fst) . sort . (zip =<< map sort)

anagrams' :: [String] -> [[String]]
anagrams' = filter ((> 1) . length) . K.group sort

test = ["tree", "evil", "joe", "live", "god"]                     