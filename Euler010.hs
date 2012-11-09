{-# LANGUAGE MagicHash #-}

module Euler010 where

import Euler003

-- | Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
-- >>> euler010 10
-- 17
-- >>> euler010 2000000
-- 142913828922

euler010 :: Integer -> Integer
euler010 n = sum $ takeWhile (<n) primes
