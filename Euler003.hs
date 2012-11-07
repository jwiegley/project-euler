module Euler003 where

import           Data.List
import           Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as V

-- | Problem 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
--
-- >>> euler003 13195
-- [5,7,13,29]
-- >>> V.last $ euler003 600851475143
-- 6857

sieve :: Int -> Vector Bool
sieve n = let vec = V.replicate (n+1) True
          in foldl' (\v x -> v // [ (y, False)
                                  | y <- [x*2,x*3..n]]) vec [2..n]

divisibleBy :: Int -> Int -> Bool
divisibleBy n x = x > 1 && n `mod` x == 0

euler003 :: Int -> Vector Int
euler003 n =
  V.filter (n `divisibleBy`) . V.findIndices id . sieve . limit $ n
  where limit = ceiling . sqrt . fromIntegral
