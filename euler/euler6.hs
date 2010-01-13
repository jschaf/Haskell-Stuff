-- Joe Schafer
-- Project Euler #6
-- http://projecteuler.net/index.php?section=problems&id=6

-- The sum of the squares of the first ten natural numbers is,
-- 
-- 1^2 + 2^2 + ... + 10^2 = 385
-- 
-- The square of the sum of the first ten natural numbers is,
-- 
-- (1 + 2 + ... + 10)^2 = 552 = 3025
--
-- Hence the difference between the sum of the squares of the first
-- ten natural numbers and the square of the sum is 3025 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.

-- Answer: 25164150
answer = a - b
    where a = (sum [1..100])^2
          b = sum [x^2 | x <- [1..100]]

main = putStrLn . show $ answer