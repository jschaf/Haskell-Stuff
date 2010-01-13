-- An implementation of the parser combinator from "higher-Order
-- Function for Parsing" by Graham Hutton

type Parser a b = [a] -> [(b, [a])]

succeed :: b -> Parser a b
succeed v inp = [(v, inp)]

reject :: Parser a b
reject inp = []

satisfy :: (a -> Bool) -> Parser a a
satisfy p [] = reject []
satisfy p (x:xs) | p x = succeed x xs
                 | otherwise = reject xs

literal :: Eq a => a -> Parser a a
literal x = satisfy (==x)

alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp

chain :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `chain` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp,
                                          (v2, out2) <- p2 out1]

using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v, out) <- p inp]

cons = uncurry (:)

many :: Parser a b -> Parser a [b]
many p = ((p `chain` many p) `using` cons) `alt` (succeed [])

some :: Parser a b -> Parser a [b]
some p = (p `chain` many p) `using` cons

number :: Parser Char String
number = some (satisfy digit)
    where digit x = '0' <= x && x <= '9'

word :: Parser Char String
word = some (satisfy letter)
    where letter x = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

string :: Eq a => [a] -> Parser a [a]
string [] = succeed []
string (x:xs) = (literal x `chain` string xs) `using` cons

xchain :: Parser a b -> Parser a c -> Parser a c
p1 `xchain` p2 = (p1 `chain` p2) `using` snd

chainx :: Parser a b -> Parser a c -> Parser a b
p1 `chainx` p2 = (p1 `chain` p2) `using` fst

check :: Parser a b -> c -> Parser a c
p `check` v = p `using` (const v)

data Expr = Number Int | Expr `Add` Expr | Expr `Sub` Expr
                       | Expr `Mul` Expr | Expr `Div` Expr
            deriving (Show)

-- BNF for our grammar
--
-- expn ::= term+term|term-term|term
-- term ::= factor*factor|factor/factor|factor
-- factor ::= digit^(+)|(expn)

expn :: Parser Char Expr
expn = ((term `chain` (literal '+' `xchain` term)) `using` plus)  `alt`
       ((term `chain` (literal '-' `xchain` term)) `using` minus) `alt`
       term

term :: Parser Char Expr
term = ((factor `chain` (literal '*' `xchain` factor)) `using` times)  `alt`
       ((factor `chain` (literal '/' `xchain` factor)) `using` divide) `alt`
       factor

factor :: Parser Char Expr
factor = (number `using` value) `alt`
         (literal '(' `xchain` expn `chainx` literal ')')


value xs = Number (read xs)
plus (x,y) = x `Add` y
minus (x,y) = x `Sub` y
times (x,y) = x `Mul` y
divide (x,y) = x `Div` y