{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

a = "blah"       :: String
b = "123"        :: String
c = "123blah789" :: String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannout be zero"
    _ -> return (numerator % denominator)

onlyIntParser :: Parser Integer
onlyIntParser = do
  i <- integer
  _ <- eof
  return i

fracsAndInts :: String
fracsAndInts = [r|
123
1/2
10
2/1
|]

type FractionOrInt =
  Either Rational Integer

fracOrIntParser :: Parser FractionOrInt
fracOrIntParser =
  skipMany (oneOf "\n")
  >>
      (Left <$> try virtuousFraction)
  <|> (Right <$> integer)

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = do
  let p f i =
        parseString f mempty i

  --print $ p (some letter) a
  --print $ p integer b

  --print $ p parseNos a
  --print $ p parseNos b

  --print $ p (many parseNos) c
  --print $ p (some parseNos) c

  --print $ p parseNos eitherOr
  --print $ p (some (token parseNos)) eitherOr

  print $ p (some (token fracOrIntParser)) fracsAndInts
