-- ex08.hs
module Ex08 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
