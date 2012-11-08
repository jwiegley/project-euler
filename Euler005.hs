module Euler005 where

import Euler003

-- | Problem 5
--
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
--
-- What is the smallest number that is evenly divisible by all of the numbers
-- from 1 to 20?
--
-- >>> euler005 10
-- 2520
-- >>> euler005 20
-- 232792560

divisibleByAll :: Integer -> Integer -> Bool
divisibleByAll n y = all (y `divisibleBy`) [n,n-1..2]

euler005 :: Integer -> Integer
euler005 n = head $ filter (divisibleByAll n) [n,n+n..]
