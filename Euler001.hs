module Euler001 where

-- | Problem 1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- >>> euler001 10
-- 23
-- >>> euler001 1000
-- 233168

euler001 :: Integer -> Integer
euler001 n = sum $ filter multOf3or5 [1..n-1]
  where multOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0
