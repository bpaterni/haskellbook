-- Chapter Exercises

{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Data.Bits
import Data.Word
import Text.RawString.QQ
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer { major    :: Major
         , minor    ::  Minor
         , patch    :: Patch
         , release  :: Release
         , metadata :: Metadata
         }
  deriving (Eq, Ord, Show)

verEx01 = "1.0.0-x.7.z.92"
verEx02 = "1.0.0-gamma+002"
verEx03 = "1.0.0-beta+oof.sha.41af286"

numberOrString :: (Monad m, TokenParsing m)
               => m NumberOrString
numberOrString =
  ((NOSI <$> try decimal) <?> "attempted NOS decimal")
  <|> ((NOSS <$> some alphaNum) <?> "attempted NOS alphaNum")

numberOrStringDot :: (Monad m, TokenParsing m)
                  => m NumberOrString
numberOrStringDot = do
  nos <- numberOrString
  _   <- option '.' (char '.')
  return nos

versionCore :: (Monad m, TokenParsing m)
            => m SemVer
versionCore = do
  major   <- decimal
  _       <- char '.'
  minor   <- decimal
  _       <- char '.'
  patch   <- decimal
  return $ SemVer major minor patch [] []

preRelease :: (Monad m, TokenParsing m)
           => m Release
preRelease = some numberOrStringDot

versionCorePreRelease :: (Monad m, TokenParsing m)
                      => m SemVer
versionCorePreRelease = do
  (SemVer maj min p _ _) <- versionCore
  _                      <- char '-'
  pr                     <- preRelease
  return $ (SemVer maj min p pr [])

metadatas :: (Monad m, TokenParsing m)
           => m Metadata
metadatas = some numberOrStringDot

versionCoreBuild :: (Monad m, TokenParsing m)
                 => m SemVer
versionCoreBuild = do
  (SemVer maj min p _ _) <- versionCore
  _                      <- char '+'
  meta                   <- metadatas
  return $ (SemVer maj min p [] meta)

versionCorePreReleaseBuild :: (Monad m, TokenParsing m)
                           => m SemVer
versionCorePreReleaseBuild = do
  (SemVer maj min p _ _) <- versionCore
  _                      <- char '-'
  pr                     <- preRelease
  _                      <- char '+'
  meta                   <- metadatas
  return $ (SemVer maj min p pr meta)

parseSemVer :: Parser SemVer
parseSemVer =
  ( try (versionCore <* eof) <?> "attempted versionCore")
  <|> ( try (versionCorePreRelease <* eof)
        <?> "attempted versionCorePreRelease"
      )
  <|> ( try (versionCoreBuild <* eof)
        <?> "attempted versionCoreBuild"
      )
  <|> ( (versionCorePreReleaseBuild <* eof)
        <?> "attempted versionCorePreReleaseBuild"
      )

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
  d <- some parseDigit
  return $ read d

negBase10Integer :: Parser Integer
negBase10Integer = do
  _ <- char '-'
  d <- base10Integer
  return $ negate d

signedBase10Integer :: Parser Integer
signedBase10Integer =
  ( try negBase10Integer ) <|> base10Integer

-- aka area code
type NumberingPlanArea = String
type Exchange          = String
type LineNumber        = String

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

numberingPlanArea :: (Monad m, TokenParsing m)
                  => m NumberingPlanArea
numberingPlanArea = token _numberingPlanArea
  where _numberingPlanArea = do
          _ <- option '.' (try (token (token (char '1') >> token (char '-'))) <|> try (token (char '(')))
          c1 <- digit
          c2 <- digit
          c3 <- digit
          _ <- option '.' (token (oneOf ")-"))
          return $ [c1,c2,c3]

exchange :: (Monad m, TokenParsing m)
        => m Exchange
exchange = token _exchange
  where _exchange = do
          c1 <- digit
          c2 <- digit
          c3 <- digit
          _ <- option '.' (char '-')
          return [c1,c2,c3]

lineNumber :: (Monad m, TokenParsing m)
           => m LineNumber
lineNumber = token _lineNumber
  where _lineNumber = do
          c1 <- digit
          c2 <- digit
          c3 <- digit
          c4 <- digit
          return [c1,c2,c3,c4]

parsePhone :: Parser PhoneNumber
parsePhone = do
  a <- numberingPlanArea
  e <- exchange
  l <- lineNumber
  return $ PhoneNumber a e l

type Year  = Integer
type Month = Integer
type Day   = Integer

data YMD =
  YMD Year Month Day
  deriving (Eq, Show)

type Hour   = Integer
type Minute = Integer

data Time =
  Time Hour Minute
  deriving (Eq, Show)

data LogEntry =
  LogEntry YMD Time String
  deriving (Eq, Show)

logEx0 = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

logEx1 = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany ( try (do
    _ <- char '-'
    _ <- char '-'
    skipMany (noneOf "\n")
    skipEOL
  ))

parseYMD :: Parser YMD
parseYMD = do
  char '#'
  spaces
  y1 <- digit
  y2 <- digit
  y3 <- digit
  y4 <- digit
  char '-'
  m1 <- digit
  m2 <- digit
  char '-'
  d1 <- digit
  d2 <- digit
  return $ YMD (fromIntegral $ read [y1,y2,y3,y4])
               (fromIntegral $ read [m1,m2])
               (fromIntegral $ read [d1,d2])

parseTime :: Parser Time
parseTime = do
  h1 <- digit
  h2 <- digit
  _  <- char ':'
  m1 <- digit
  m2 <- digit
  return $ Time (fromIntegral $ read [h1,h2]) (fromIntegral $ read [m1,m2])

oneLogEntry :: YMD -> Parser LogEntry
oneLogEntry ymd = do
  spaces
  skipComments
  spaces
  t <- parseTime
  spaces
  s <- many (noneOf "\n")
  skipEOL
  return $ LogEntry ymd t s

oneLogDay :: Parser [LogEntry]
oneLogDay = do
  spaces
  skipComments
  spaces
  ymd <- parseYMD
  spaces
  skipComments
  logEntries <- many (token (oneLogEntry ymd))
  return logEntries

parseLog :: Parser [LogEntry]
parseLog = do
  lss <- many (token oneLogDay)
  return $ concat lss

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseIPAddressOctet :: Parser Integer
parseIPAddressOctet = do
  o <- some digit
  let octet = fromIntegral $ read o
  case octet > 255 of
    True -> fail "octet > 255"
    _ -> return octet

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  o1 <- parseIPAddressOctet
  char '.'
  o2 <- parseIPAddressOctet
  char '.'
  o3 <- parseIPAddressOctet
  char '.'
  o4 <- parseIPAddressOctet
  return ( IPAddress $ fromIntegral ( ( o1 `shift` (24) )
                                    .|.
                                    ( o2 `shift` (16) )
                                    .|.
                                    ( o3 `shift` (8)  )
                                    .|.
                                    ( o4               )
                                  )
         )

main :: IO ()
main = putStrLn "Test suite not yet implemented"
