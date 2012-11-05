module Euler004 where

-- | Problem 4
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 * 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-- >>> euler004 [99,98..10]
-- 9009
-- >>> euler004 [999,998..100]
-- 906609

euler004 :: [Integer] -> Integer
euler004 range =
  maximum [ x * y | x <- range, y <- range, isPalindrome (x * y) ]
  where isPalindrome x = let y = show x in y == reverse y
