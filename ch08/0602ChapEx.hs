module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0    = "zero"
  | n == 1    = "one"
  | n == 2    = "two"
  | n == 3    = "three"
  | n == 4    = "four"
  | n == 5    = "five"
  | n == 6    = "six"
  | n == 7    = "seven"
  | n == 8    = "eight"
  | n == 9    = "nine"
  | otherwise = ""

digits :: Int -> [Int]
digits n = digits' n ((length . show $ n) - 1)
  where digits' n 0 = (digitAtP n 0 : [])
        digits' n p = (digitAtP n p : digits' n (p-1))
        digitAtP n p = mod (div n (10^p)) 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
