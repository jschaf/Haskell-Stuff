-- Joe Schafer
-- Project Euler #9
-- http://projecteuler.net/index.php?section=problems&id=9


-- A Pythagorean triplet is a set of three natural numbers, a < b < c,
-- for which,
-- 
--          a^2 + b^2 = c^2
-- 
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- 
-- There exists exactly one Pythagorean triplet for which a + b + c =
-- 1000.  Find the product abc.

-- Answer: 31875000

-- http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples

g u = u^2
h v = 2 * v^2
i u v = 2 * u * v

a u v = g u + i u v
b u v = h v + i u v
c u v = g u + h v + i u v

triples m = [[f u v | f <- [a,b,c]] | u <- [1..m], v <- [1..m-1]]

main = putStrLn . show . product . head . filter ((==1000) . sum) $ (triples 15)