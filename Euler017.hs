module Euler017 where
import Data.List

-- | Problem 17
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
-- 20 letters. The use of "and" when writing out numbers is in compliance with
-- British usage.
--
-- >>> euler017 5
-- 19
-- >>> euler017 1000
-- 21124

numberName :: Int -> String
numberName n
  | n == 0 = "zero"

  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 10 = "ten"

  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"

  | n == 20 = "twenty"
  | n == 30 = "thirty"
  | n == 40 = "forty"
  | n == 50 = "fifty"
  | n == 60 = "sixty"
  | n == 70 = "seventy"
  | n == 80 = "eighty"
  | n == 90 = "ninety"

  | n < 100 =
    numberName ((n `div` 10) * 10) ++ "-" ++ numberName (n `mod` 10)

  | n < 1000 =
    let remainder = n `mod` 100
    in numberName (n `div` 100) ++ " hundred" ++
       if remainder > 0
       then " and " ++ numberName remainder
       else ""

  | n < 10000 =
    let remainder = n `mod` 1000
    in numberName (n `div` 1000) ++ " thousand" ++
       if remainder > 0
       then if remainder < 100
            then " and " ++ numberName remainder
            else ", " ++ numberName remainder
       else ""

  | otherwise = error "Not yet implemented"

euler017 :: Int -> Int
euler017 n = foldl' (\acc x -> let y = length (letters x)
                               in seq y $ acc + y) 0 [1..n]
  where letters = filter (not . (`elem` " -")) . numberName
