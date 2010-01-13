-- Joe Schafer
-- Problem B
-- August 2009

import Data.List(sort, sortBy, group, unfoldr, groupBy, maximumBy, nubBy)
import Data.Function(on)

data Suit = Alpha | Bravo | Charlie | Delta | Echo | Foxtrot
            deriving (Bounded, Enum, Eq, Ord, Show)

data Face = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
            deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { face :: Face, suit :: Suit }
            deriving (Eq, Show, Ord)

card :: Face -> Suit -> Card
card f s = Card {face = f, suit = s}

type Hand = [Card]

data HandRank = WildRabble | Straight | Flush | ThreeOfAKind | StraightFlush
               deriving (Eq, Show, Ord)

data GameHands = GameHands {mine :: Hand,
                            opponent :: Hand,
                            available :: Hand}
                 deriving (Show)


gameHands :: [Hand] -> GameHands
gameHands xs | length xs < 3 = undefined
             | otherwise = GameHands {mine      = xs !! 0,
                                      opponent  = xs !! 1,
                                      available = xs !! 2}


-- Adapted directly from
-- http://quantile95.com/2008/12/31/monte-carlo-poker-odds/
hand :: Hand -> HandRank
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

-- Compare two card hands.
compareHands :: Hand -> Hand -> Ordering
compareHands a b | hand a == hand b = (compare `on` cardSum) a b
                 | otherwise        = (compare `on` hand) a b
    where cardSum = sum . map (fromEnum . face)

-- Build the best hand from the input hand.
bestHand :: Hand -> Hand
bestHand xs = head . filter (not . null) $ hands
  where hands = [f(xs) | f <- [straightFlush, flush, straight,
                               threeOfAKind, wildRabble]]

groupSuits :: Hand -> [Hand]
groupSuits = groupBy ((==) `on` suit) . sortBy compareSuits
  where compareSuits a b | suit a == suit b = (compare `on` face) b a
                         | otherwise        = (compare `on` suit) a b

groupFaces :: Hand -> [Hand]
groupFaces = groupBy ((==) `on` face) . reverse . sort

-- Annoyingly common
compFaceHead = compare `on` face . head

safeMaximumBy f xs | null xs = []
                   | otherwise = maximumBy f xs
                                 
straightFlush :: Hand -> Hand
straightFlush xs = safeMaximumBy compFaceHead allStraights
  where
    allStraights = concatMap buildStraights . groupSuits $ xs

-- Get the flush with the highest starting card
flush :: Hand -> Hand
flush xs | null allFlushes = []
         | otherwise = take 3 . safeMaximumBy compFaceHead $ allFlushes
  where allFlushes = map reverse . filter ((>2) . length) . groupSuits $ xs

straight :: Hand -> Hand
straight xs | null ys = []
            | otherwise = head $ ys
  where ys = buildStraights . concat . groupFaces $ xs

threeOfAKind xs | null ys = []
                | otherwise = head ys
  where ys = filter ((>2) . length) . groupBy ((==) `on` face) $ faces
        faces = concat . groupFaces $ xs

wildRabble :: Hand -> Hand
wildRabble xs | length ys < 3 = []
              | otherwise     = ys
  where ys = take 3 . concat . groupFaces $ xs


buildStraights :: Hand -> [Hand]
buildStraights = normalize . foldr step [[]] . nubBy ((==) `on` face)
  where step a bss@(b:bs)
          | null . head $ bss = [[a]]
          | a' == minBound    = [a]:bss
          | pred a' == b'     = (a:b):bs
          | otherwise         = [a]:bss
            where a' = face a
                  b' = face . head $ b
        normalize = map (take 3) . filter ((>2) . length)

-- Given a string like "1A" return the corresponding card type.
cardFromString :: String -> Card
cardFromString s = card faceEnum suitEnum
    -- Assume string length is 2
    where (faceStr, suitStr) = splitAt 1 $ s

          faceEnum :: Face
          faceEnum = toEnum $ (read faceStr :: Int) - 1

          -- 65 is the ASCII Code for 'A'
          suitEnum :: Suit
          suitEnum = toEnum . (flip (-) 65) . fromEnum . head $ suitStr

-- An interface to cardFromString that handles excess whitespace
readCard :: String -> Card
readCard = cardFromString . head . words

readCards :: String -> [Card]
readCards = map readCard . words

processData :: [String] -> [String]
processData = map (solver . gameHands) . groupAt 3 . map readCards

solver :: GameHands -> String
solver gs = case ours `compareHands` theirs of
              GT -> "Yes"
              otherwise -> "No"
  where theirs = bestHand (opponent gs ++ available gs)
        ours   = mine gs

groupAt :: Int -> [a] -> [[a]]
groupAt _ [] = []
groupAt n xs | n <= 0 = [xs]
groupAt n xs = take n xs : (groupAt n . drop n $ xs)

main :: IO ()
main = readFile "inB.txt" >>=
       putStr . unlines . processData . init . tail . lines

prettyCard :: Card -> String
prettyCard c = (show . (+1) . fromEnum $ f) ++ (head . show $ s):[]
  where f = face c
        s = suit c

prettyCards = map prettyCard

cards1 = "7A 7E 7B"
cards2 = "5B"
cards3 = "5E 5C 6B 9A 9B 9C 1B 4D"

test1 = map readCard . words $ "5A 4A 3A 2A 1A"
test2 = map readCard . words $ "1A 5F 5E"         
tests = [
  ("1A 5F 5E", WildRabble),
  ("5A 6A 8B 9B", WildRabble),
  ("5B 6C 3D 4A", Straight),
  ("1D 9D 7A 4A 5D", Flush),
  ("1A 2A 3A 4A 5A", StraightFlush),
  ("4A 1A 3A 5A 6A", StraightFlush)]

testall = [(hand . bestHand $ cards str) == result | (str, result) <- tests]
          where cards = map readCard . words
