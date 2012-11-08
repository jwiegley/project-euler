{-# LANGUAGE MagicHash #-}

module Euler009 where

-- | Problem 9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
--                              a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--
-- >>> euler009 12
-- 60
-- >>> euler009 1000
-- 31875000

euler009 :: Int -> Int
euler009 n = (\(x, y, z) -> x * y * z) $ head $
             [ (a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n],
               a < b && b < c, a^2 + b^2 == c^2, a + b + c == n ]
