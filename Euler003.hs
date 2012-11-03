module Euler003 where

import Data.List
import Data.Array.Unboxed

-- | Problem
--
-- >>> euler003 13195
-- [5,7,13,29]
-- >>> euler003 600851475143
-- [71,839,1471,6857]

primes :: Integer -> [Integer]
primes = map fst . filter snd . assocs . sieve

sieve :: Integer -> UArray Integer Bool
sieve n =
  let ary = array (3, n) $ zipWith (,) [3..n] (repeat True)
  in foldl' (\s x ->
              s // unfoldr (\y -> if y > n
                                  then Nothing
                                  else Just ((y, False), y + x))
                           (x + x))
            ary [3..n]

euler003 :: Integer -> [Integer]
euler003 n = filter (\x -> n `rem` x == 0)
             . primes . ceiling . sqrt . fromIntegral $ n
