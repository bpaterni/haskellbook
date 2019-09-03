{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction    = "1/0" :: String
alsoBad        = "10"  :: String
shouldWork     = "1/2" :: String
shouldAlsoWork = "2/1" :: String

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannout be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
  let virtuousFraction' =
        runParser virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork

onlyIntParser :: Parser Integer
onlyIntParser = do
  i <- integer
  _ <- eof
  return i

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
--main = do
--  let parseFraction' =
--        runParser parseFraction mempty
--  print $ parseFraction' badFraction
--  print $ parseFraction' shouldWork
--  print $ parseFraction' shouldAlsoWork
--  print $ parseFraction' alsoBad
main = testVirtuous
