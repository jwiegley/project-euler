{-# LANGUAGE BangPatterns #-}

module Euler005 where

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
-- >>> euler005b 10
-- 2520
-- >>> euler005 20
-- 232792560
-- >>> euler005b 20
-- 232792560

euler005 :: Integer -> Integer
euler005 n = head $ filter (divisibleByAll n) [n..]
  where divisibleByAll o y = all (\x -> y `rem` x == 0) [2..o]

------------------------------------------------------------------------

multiples :: Integer -> [Integer]
multiples n = iterate (+n) n

-- NOTE: This function assumes the input lists are sorted and infinite!
lazyIntersect :: Ord a => [a] -> [a] -> [a]
lazyIntersect xs ys =
  let x     = head xs
      cands = takeWhile (<= x) ys
      rest  = lazyIntersect (tail xs) (dropWhile (<= x) ys)
  in if not (null cands) && last cands == x
     then x : rest
     else rest

euler005b :: Integer -> Integer
euler005b n =
  let xs = map multiples [2..n]
  in head $ foldr lazyIntersect (head xs) (tail xs)
