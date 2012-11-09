module Euler007 where

import Euler003

-- | Problem 7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6^th prime is 13.
--
-- What is the 10001^st prime number?
--
-- >>> euler007 6
-- 13
-- >>> euler007 10001
-- 104743

euler007 :: Int -> Integer
euler007 n = primes !! (n - 1)
