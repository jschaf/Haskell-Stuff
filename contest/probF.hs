-- Joe Schafer
-- Problem F
-- October 2009

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Data.List (nub)

data Suit = Club | Diamond | Heart | Spade
            deriving (Enum, Eq, Ord, Show)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace
            deriving (Enum, Eq, Ord, Show)

data Card = Card { face :: Face, suit :: Suit }
            deriving (Eq, Ord, Show)

type Hand = [Card]
type FaceSet = Set.Set Face

card :: Face -> Suit -> Card
card f s = Card {face = f, suit = s}

faceStrings = "23456789TJQKA"
faceTypes = [Two .. Ace]

faceSet = Set.fromDistinctAscList faceTypes            
            
suitStrings = "CDHS"
suitTypes = [Club, Diamond, Heart, Spade]

readCard :: String -> Card
readCard s = card face suit
    where (faceStr, suitStr) = (s !! 0, s !! 1)

          faceList = zip faceStrings faceTypes
          -- XXX using undefined as poor man's exception
          face = fromMaybe undefined . lookup faceStr $ faceList

          suitList = zip suitStrings suitTypes
          suit = fromMaybe undefined . lookup suitStr $ suitList

-- | Calculate how many points a hand is worth.
handPoints :: Hand -> Int
handPoints h = faceScore h + suitScore h - unprotectedScore h

-- | Sum the total points for face cards in the hand.
faceScore :: Hand -> Int
faceScore = sum . map (faceValues . face)
    where faceValues f = fromMaybe 0 . lookup f $ zip [Jack .. Ace] [1..4]

-- | Sum all the voids, singletons, or doubletons in the hand.
suitScore :: Hand -> Int
suitScore = sum . map score . sepSuits
    where score h = fromMaybe 0 . lookup (length h) $ zip [0..2] [3,2,1]

-- | Split a hand into four hands, grouped by suit.
sepSuits :: Hand -> [Hand]
sepSuits h = [f h | f <- map singleSuit suitTypes]
    where singleSuit s = filter ((==) s . suit)

-- | Convert a list of hands into a list of sets of only face values.
suitSets :: Hand -> [FaceSet]
suitSets = map (Set.fromList . map face) . sepSuits

-- | List of cards that are missing from each suit in suitSets.
missingSets :: Hand -> [FaceSet]
missingSets = map (Set.difference faceSet) . suitSets

-- | A list containing all cards that need protection for each
-- | suit.
needProtection :: Hand -> [FaceSet]
needProtection = map (snd . Set.split Ten) . suitSets

-- | The number of points to deduct for unprotected face cards.
unprotectedScore h = sum $ zipWith3 protected (needProtection h) 
                                              (suitSets h)
                                              (missingSets h)

-- | The total number of points to deduct for a suit with
-- | unprotected face cards.
protected :: FaceSet -> FaceSet -> FaceSet -> Int
protected p h m = setSum . Set.map protected' $ p
    where
      setSum = Set.fold (+) 0
      protected' :: Face -> Int
      protected' c | m' > h'   = 1
                   | otherwise = 0
          where
            h' = Set.size . fst $ Set.split c h
            m' = Set.size . snd $ Set.split c m

processData :: [String] -> [String]
processData = map (show . handPoints . map readCard . words)

main :: IO ()
main = readFile "inF.txt" >>=
       (putStr . unlines . processData . init . tail . lines)

-- Testing

instance Arbitrary Face where
    arbitrary = elements faceTypes

instance Arbitrary Suit where
    arbitrary = elements suitTypes

instance Arbitrary Card where
    arbitrary = elements [Card f s | f <- faceTypes, s <- suitTypes]

prop_EmptyScore = handPoints [] == 12

prop_MaxScore h = handPoints (nub h) < 40

prop_MissingComplete h = all ((==) faceSet) split
    where split = zipWith Set.union (suitSets h) (missingSets h)

