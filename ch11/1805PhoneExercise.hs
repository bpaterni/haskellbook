import Data.Char
import Data.List

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data PhoneKeyRec =
  PhoneKeyRec { digit :: Digit
              , symbols :: String }
              deriving (Eq, Show)

newtype DaPhone = DaPhone [PhoneKeyRec]
  deriving (Eq, Show)

phone = 
  DaPhone [ PhoneKeyRec { digit = '1', symbols = "1" }
          , PhoneKeyRec { digit = '2', symbols = "ABC2" }
          , PhoneKeyRec { digit = '3', symbols = "DEF3" }
          , PhoneKeyRec { digit = '4', symbols = "GHI4" }
          , PhoneKeyRec { digit = '5', symbols = "JKL5" }
          , PhoneKeyRec { digit = '6', symbols = "MNO6" }
          , PhoneKeyRec { digit = '7', symbols = "PQRS7" }
          , PhoneKeyRec { digit = '8', symbols = "TUV8" }
          , PhoneKeyRec { digit = '9', symbols = "WXYZ9" }
          , PhoneKeyRec { digit = '*', symbols = "^*" }
          , PhoneKeyRec { digit = '0', symbols = "+ _0" }
          , PhoneKeyRec { digit = '#', symbols = ".,#" }
          ]

convo :: [String]
convo = 
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

numKeyPresses :: Char -> String -> Presses
numKeyPresses c syms =
  case index of
    Just i -> i+1
    Nothing -> 0
    where index = elemIndex c syms

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps (DaPhone recs) c = foldr f [] recs
  where f rec acc
          | isUpper c = if elem c (symbols rec)
                           then ('*', 1) : (digit rec, numKeyPresses c (symbols rec)) : acc
                           else acc
          | otherwise = if elem (toUpper c) (symbols rec)
                           then (digit rec, numKeyPresses (toUpper c) (symbols rec)) : acc
                           else acc
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead p = concat . map (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (\(d,p) -> p)

data LetterCountRec =
  LetterCountRec { letter :: Char
                 , count  :: Integer }
                 deriving (Eq, Show)

letterCountsInStr :: String -> [LetterCountRec]
letterCountsInStr = foldr f [] . filter isLetter
  where f c acc
          | elem c (map letter acc) =
            LetterCountRec { letter = c, count = (count (head . filter ((==c) . letter) $ acc)) + 1 } : filter ((/=c) . letter) acc
          | otherwise = LetterCountRec { letter = c, count = 1 } : acc

maxLetterCountInStr :: String -> LetterCountRec
maxLetterCountInStr = head . reverse . sortOn count . letterCountsInStr

mostPopularLetter :: String -> Char
mostPopularLetter = letter . maxLetterCountInStr

costOfMostPopularLetter :: String -> Presses
--costOfMostPopularLetter = undefined
costOfMostPopularLetter xs = sum . map snd . concat . map (reverseTaps phone) . filter (==(mostPopularLetter xs)) $ xs

letterCountsInStrings :: [String] -> [LetterCountRec]
letterCountsInStrings = foldr f [] . concat . map letterCountsInStr
  where f rec acc
          | elem (letter rec) (map letter acc) =
            LetterCountRec { letter = letter rec, count = (count (head . filter ((==letter rec) . letter) $ acc)) + count rec } : filter ((/=(letter rec)) . letter) acc
          | otherwise = LetterCountRec { letter = letter rec, count = count rec } : acc

maxLetterCountInStrings :: [String] -> LetterCountRec
maxLetterCountInStrings = head . reverse . sortOn count . letterCountsInStrings

coolestLtr :: [String] -> Char
coolestLtr = letter . maxLetterCountInStrings

data WordCountRec =
  WordCountRec { word :: String
               , wcount :: Integer }
               deriving (Eq, Show)

wordCountsInString :: String -> [WordCountRec]
wordCountsInString = foldr f [] . words
  where f w acc
          | elem w (map word acc) = 
            WordCountRec { word = w, wcount = (wcount (head . filter ((==w) . word) $ acc)) + 1 } : filter ((/=w) . word) acc
          | otherwise = WordCountRec { word = w, wcount = 1 } : acc

wordCountsInStrings :: [String] -> [WordCountRec]
wordCountsInStrings = foldr f [] . concat . map wordCountsInString
  where f rec acc
          | elem (word rec) (map word acc) =
            WordCountRec { word = word rec, wcount = (wcount (head . filter ((==word rec) . word) $ acc)) + wcount rec } : filter ((/= (word rec)) . word) acc
          | otherwise = WordCountRec { word = word rec, wcount = wcount rec } : acc

maxWordCountInStrings :: [String] -> WordCountRec
maxWordCountInStrings = head . reverse . sortOn wcount . wordCountsInStrings

coolestWord :: [String] -> String
coolestWord = word . maxWordCountInStrings
