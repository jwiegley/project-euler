module Euler004 where

-- | Problem 4
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 * 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-- >>> euler004 [10..99]
-- 9009
-- >>> euler004 [100..999]
-- 906609

isPalindrome :: Integer -> Bool
isPalindrome n = isPalindrome' $ show n
  where isPalindrome' []     = True
        isPalindrome' [_]    = True
        isPalindrome' [x, y] = x == y
        isPalindrome' str    = head str == last str &&
                               isPalindrome' (init (tail str))

euler004 :: [Integer] -> Integer
euler004 range =
  maximum [ x * y | x <- range, y <- range, isPalindrome (x * y) ]
