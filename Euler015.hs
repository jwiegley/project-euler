module Euler015 where

-- | Problem 15
--
-- Starting in the top left corner of a 2 * 2 grid, there are 6 routes
-- (without backtracking) to the bottom right corner.
--
-- How many routes are there through a 20 * 20 grid?
--
-- >>> euler015 2
-- 6
-- >>> euler015 3
-- 20
-- >>> euler015 4
-- 70
-- (ghci) euler015 20                -- this is just too slow to test
-- 137846528820

euler015 :: Int -> Int
euler015 n = go 0 0
  where go x y
          | x == n && y == n = 1
          | x > n || y > n   = 0
          | otherwise = let l = go x (y + 1)
                            r = go (x + 1) y
                        in seq l $ seq r $ l + r
