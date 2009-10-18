-- Joe Schafer
-- Problem B
-- August 2009

import Data.List(sort, sortBy, group, unfoldr, groupBy)
import Data.Char (isSpace)

data Suit = Alpha | Bravo | Charlie | Delta | Echo | Foxtrot
            deriving (Bounded, Enum, Eq, Ord, Show)

data Face = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
            deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { face :: Face, suit :: Suit }
            deriving (Eq, Show)

card :: Face -> Suit -> Card
card f s = Card {face = f, suit = s}

data Hand = WildRabble | Straight | Flush | ThreeOfAKind | StraightFlush
            deriving (Eq, Show, Ord)

-- Adapted directly from
-- http://quantile95.com/2008/12/31/monte-carlo-poker-odds/
hand :: [Card] -> Hand
hand cs =
    case matches of
      [1,1,1] ->
          case undefined of
            _ | isStraight && isFlush -> StraightFlush
            _ | isFlush               -> Flush
            _ | isStraight            -> Straight
            _ | otherwise             -> WildRabble
      [3]                             -> ThreeOfAKind
      otherwise                       -> WildRabble
    where
      (x:xs) = sort . map (fromEnum . face) $ cs
      (s:ss) = map suit cs
      isStraight = xs == [x+1,x+2]
      isFlush = all (== s) ss
      matches = sort . map length . group $ (x:xs)

compareHands :: [Card] -> [Card] -> Ordering
compareHands a b =
    case order of
      LT -> order
      GT -> order
      EQ -> tiebreaker
    where order = compare (hand a) (hand b)
          tiebreaker = compare (cardSum a) (cardSum b)
          cardSum = sum . map (fromEnum . face)

buildBestHand :: [Card] -> [Card] -> [Card]
buildBestHand as bs = undefined
    where suitOrdered = groupBy suitGrouper . sortBy compareSuits $ bs
          faceOrdered = groupBy faceGrouper . sortBy compareFaces $ bs
                        
          faceGrouper a b = (face a) == (face b)
          suitGrouper a b = (suit a) == (suit b)
                            
          compareSuits a b = case suitOrder of
                               EQ -> compare (face a) (face b)
                               _  -> suitOrder
              where suitOrder = compare (suit a) (suit b)
          compareFaces a b = compare (face a) (face b)
                                
cardFromString :: String -> Card
cardFromString s = card face suit
    -- Assume string length is 2
    where (faceStr, suitStr) = splitAt 1 . trim $ s
                               
          face :: Face
          face = toEnum $ (read faceStr :: Int) - 1
                 
          -- 65 is the ASCII Code for 'A'
          suit :: Suit
          suit = toEnum . (flip (-) 65) . fromEnum . head $ suitStr

processData :: [String] -> [String]
processData = map (solver . map readCard . splitChar ' ' . trim)

readCard :: String -> Card
readCard = cardFromString . trim

solver :: [Card] -> String
solver = undefined

main :: IO ()
main = do
  fileData <- readFile $ "inB.txt"
  putStr . unlines . processData . init . tail . lines $ fileData

-- Taken from
-- http://julipedia.blogspot.com/2006/08/split-function-in-haskell.html
splitChar :: Char -> String -> [String]
splitChar = unfoldr . splitChar'
    where
      splitChar' :: Char -> String -> Maybe (String, String)
      splitChar' c l
          | null l = Nothing
          | otherwise = Just (h, drop 1 t)
          where (h, t) = span (/=c) l

-- Taken from
-- http://www.biglist.com/lists/xsl-list/archives/200112/msg01067.html
trim :: String -> String
trim = applyTwice (reverse . trim1)
    where  trim1 = dropWhile (`elem` delim)
           delim = [' ', '\t', '\n', '\r']
           applyTwice f = f . f
