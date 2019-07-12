module QCCiphers
  where

import Caesar
import Test.QuickCheck
import Vigenere

vigenereSecrets = [ "AMD"
                  , "AOL"
                  , "AWS"
                  , "CAPRA"
                  , "LIBRE"
                  , "ALLY"
                  , "GRID"
                  , "HELLO"
                  , "BYE"
                  ]

plaintextStrings = [ "hello world"
                   , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   , "ATTACKATONCE"
                   ]

plaintextStringGen :: Gen String
plaintextStringGen = do
  --a <- arbitrary `suchThat` (\x -> length x > 3 && all (flip elem ['a'..'z']) x)
  --return a
  elements plaintextStrings

secretPlaintextPairGen :: Gen (String, String)
secretPlaintextPairGen = do
  elements [ (x,y) | x <- vigenereSecrets, y <- plaintextStrings ]

prop_caesarEqual :: String -> Bool
prop_caesarEqual x = (==x) (uncaesar . caesar $ x)

prop_vigenereEqual :: Property
prop_vigenereEqual =
  forAll (secretPlaintextPairGen :: Gen (String, String))
  (\(x,y) -> (==y) ((unvigenere x . vigenere x) y))

runQC :: IO ()
runQC = do
  quickCheck prop_caesarEqual
  quickCheck prop_vigenereEqual

main :: IO ()
main = runQC
