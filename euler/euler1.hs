-- Joe Schafer
-- Project Euler #1
-- http://projecteuler.net/index.php?section=problems&id=1

-- If we list all the natural numbers below 10 that are multiples of 3 or
-- 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Answer:
-- 233168

main = putStrLn . show $ sum [3,6..999] + sum [5,10..999] - sum [15,30..999]