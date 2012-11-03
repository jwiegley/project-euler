module Euler001 where

-- | 1
--
-- >>> euler001
-- 233168

euler001 :: Integer
euler001 = sum $ filter multOf3or5 [1..999]
  where multOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0
