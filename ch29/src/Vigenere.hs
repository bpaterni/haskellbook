module Vigenere
  ( vigenere 
  , unvigenere )
    where

import Data.Char

-- Vigenere Cipher

alphaLowerUpper = concat [['a'..'z'],['A'..'Z']]

nextCaesarCharWithOffset :: Int -> Char -> Char
nextCaesarCharWithOffset offset x
  | elem x alphaLower = alphaLower !! (mod ((ord x) - (ord 'a') + offset) $ length alphaLower)
  | elem x alphaUpper = alphaUpper !! (mod ((ord x) - (ord 'A') + offset) $ length alphaUpper)
  | otherwise    = x
  where
    alphaLower = ['a'..'z']
    alphaUpper = ['A'..'Z']

data PlainSecret =
  PlainSecret Char Char
  deriving (Eq, Show)

pairWithSecret :: String -> String -> [PlainSecret]
pairWithSecret secret plaintext = pairWithSecret' 0 plaintext
  where pairWithSecret' _ [] = []
        pairWithSecret' sIdx (c:cs)
          | elem c alphaLowerUpper = (PlainSecret c (secret !! sIdx)) : pairWithSecret' (mod (sIdx+1) (length secret)) cs
          | otherwise = (PlainSecret c c) : pairWithSecret' sIdx cs

vigenere :: String -> String -> String
vigenere secret plaintext = map encode $ pairWithSecret secret plaintext
  where encode (PlainSecret p s)
          | elem p alphaLowerUpper = nextCaesarCharWithOffset (ord s - ord 'A') p
          | otherwise = p

unvigenere :: String -> String -> String
unvigenere secret crypt = map decode $ pairWithSecret secret crypt
  where decode (PlainSecret p s)
          | elem p alphaLowerUpper = nextCaesarCharWithOffset (negate (ord s - ord 'A')) p
          | otherwise = p
