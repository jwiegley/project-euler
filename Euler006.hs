module Euler006 where

-- | Problem 6
--
-- The sum of the squares of the first ten natural numbers is,
--                        1^2 + 2^2 + ... + 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
--                     (1 + 2 + ... + 10)^2 = 55^2 = 3025
--
-- Hence the difference between the sum of the squares of the first ten
-- natural numbers and the square of the sum is 3025 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.
--
-- >>> euler006 10
-- 2640
-- >>> euler006 100
-- 25164150

sumOfSquares :: Int -> Int
sumOfSquares n = sum (map (^2) [1..n])

squareOfSums :: Int -> Int
squareOfSums n = (sum [1..n])^2

euler006 :: Int -> Int
euler006 n = squareOfSums n - sumOfSquares n
