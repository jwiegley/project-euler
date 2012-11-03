module Euler002 where

-- | Problem
--
-- >>> euler002
-- 4613732

fib :: [Integer]
fib = 1 : 2 : fib' 1 2
  where fib' x y = x + y : fib' y (x + y)

euler002 :: Integer
euler002 = sum $ takeWhile (<4000000) $ filter even $ fib
