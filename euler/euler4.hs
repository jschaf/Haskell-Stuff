-- Joe Schafer
-- Project Euler #4
-- http://projecteuler.net/index.php?section=problems&id=4

-- A palindromic number reads the same both ways. The largest
-- palindrome made from the product of two 2-digit numbers is 9009 =
-- 91 * 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

-- Answer: 906609

import Data.List (unfoldr)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- Digits in reverse order e.g. 124 -> [4,2,1]
digits :: Int -> [Int]
digits n = unfoldr step n
    where
      step b = if b == 0 then Nothing else Just (swap $ quotRem b 10)

isPalindrome :: Int -> Bool
isPalindrome n = ds == reverse ds
    where ds = digits n

main = putStrLn . show . maximum . filter isPalindrome
       $ [x * y | x <- [900 .. 999], y <- [900 ..999]]