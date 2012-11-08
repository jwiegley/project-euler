module Euler020 where

import Euler008

-- | Problem 20
--
-- n! means n * (n 1) * ... * 3 * 2 * 1
--
-- Find the sum of the digits in the number 100!
--
-- >>> euler020 100
-- 648

euler020 n = sum . digits . product $ [1..n]
