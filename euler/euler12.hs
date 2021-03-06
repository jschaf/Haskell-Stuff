-- Joe Schafer
-- Project Euler #12
-- http://projecteuler.net/index.php?section=problems&id=12

-- The sequence of triangle numbers is generated by adding the natural
-- numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6
-- + 7 = 28. The first ten terms would be:
-- 
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- 
-- Let us list the factors of the first seven triangle numbers:
-- 
--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28
-- 
-- We can see that 28 is the first triangle number to have over five
-- divisors.
-- 
-- What is the value of the first triangle number to have over five
-- hundred divisors?

import Data.List (mapAccumL)

triNumbers = scanl1 (+) [1..]


-- From OEIS http://www.research.att.com/~njas/sequences/A000005.
-- d(n) <= 2 sqrt(n) [see Mitrinovich, p. 39, also A046522].
--
-- So n must be at least 62,500.  n is probably even.

lottaDivisors n = undefined
    ts = filter (>62500) triNumbers

main = print $ 10