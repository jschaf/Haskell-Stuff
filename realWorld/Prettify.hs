module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

line :: Doc
line = Line

double :: Double -> Doc
double d = text (show d)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group Line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where 
      transform = concatMap transformOne
      transformOne d = 
          case d of
            Empty -> ""
            Char c -> [c]
            Text s -> s
            Line -> "\n"
            a `Concat` b -> transform [a,b]
            a `Union` _ -> transform [a]

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
              case d of
                Empty -> best col ds
                Char c -> c : best (col + 1) ds
                Text s -> s ++ best (col + length s) ds
                Line -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b -> nicest col (best col (a:ds))
                                          (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

nest :: Int -> Doc -> Doc
nest lvl x = nest' 0 0 [x]
    where nest' lvl col (d:ds) =
              case d of
                Empty -> nest' lvl col ds
                Char c -> | '{' || '(' || '[' = c : nest' (lvl + 1) (col + 1) ds
                          | otherwise = c : nest lvl col ds
                Text s -> 