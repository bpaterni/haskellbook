module Cipher where

import Data.Char

nextCaesarCharWithOffset :: Int -> Char -> Char
nextCaesarCharWithOffset offset x
  | elem x alphaLower = alphaLower !! (rem ((ord x) - (ord 'a') + offset) $ length alphaLower)
  | elem x alphaUpper = alphaUpper !! (rem ((ord x) - (ord 'A') + offset) $ length alphaUpper)
  | otherwise    = x
  where
    alphaLower = ['a'..'z']
    alphaUpper = ['A'..'Z']

caesarWithOffset :: Int -> [Char] -> [Char]
--caesarWithOffset = undefined
caesarWithOffset offset = map (nextCaesarCharWithOffset offset)
--caesar' offset = map . nextCaesarCharWithOffset $ offset

caesar :: String -> String
caesar = undefined

uncaesar :: String -> String
uncaesar = undefined
