module Caesar
  where

import Data.Char

alphaLower = ['a'..'z']
alphaUpper = ['A'..'Z']
alphaLowerUpper = concat [alphaLower, alphaUpper]

nextCaesarCharWithOffset :: Int -> Char -> Char
nextCaesarCharWithOffset offset x
  | elem x alphaLower = alphaLower !! (mod ((ord x) - (ord 'a') + offset) $ length alphaLower)
  | elem x alphaUpper = alphaUpper !! (mod ((ord x) - (ord 'A') + offset) $ length alphaUpper)
  | otherwise    = x

caesarWithOffset :: Int -> [Char] -> [Char]
caesarWithOffset offset = map (nextCaesarCharWithOffset offset)

caesarOffset = 10

caesar :: String -> String
caesar = caesarWithOffset caesarOffset

uncaesar :: String -> String
uncaesar = caesarWithOffset (negate caesarOffset)

caesarMain :: IO ()
caesarMain = do
  plaintext <- getLine
  putStrLn (caesar plaintext)
