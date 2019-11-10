{-# LANGUAGE OverloadedStrings #-}
module ConfigINI where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Char (isAlpha)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

import           Text.Trifecta

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

bracketPairParser :: Parser a -> Parser a
bracketPairParser p =
  char '[' *> p <* char ']'

headerParser :: Parser Header
headerParser =
  bracketPairParser (Header <$> some letter)

type Name        = String
type Value       = String
type Assignments = Map Name Value

assignmentParser :: Parser (Name, Value)
assignmentParser = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipComments :: Parser ()
skipComments =
  skipMany ( do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments

  h <- headerParser
  skipEOL

  assignments <- some assignmentParser
  return $
    Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return (Config mapOfSections)
