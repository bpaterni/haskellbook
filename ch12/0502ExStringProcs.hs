import Data.List

notThe :: String -> Maybe String
notThe w
  | w == "the" = Nothing
  | otherwise  = Just w

replaceThe :: String -> String
replaceThe = concat . intersperse " " . map f . words
  where f = g . notThe
        g x = case x of
                Nothing -> "a"
                Just w  -> w

data WordInfoRec =
  WordInfoRec { word :: String
              , not_the :: Maybe String
              , next_word :: Maybe String }
              deriving (Eq, Show)

recsFromWords :: [String] -> [WordInfoRec]
recsFromWords [] = []
recsFromWords (w:[]) =
  [ WordInfoRec { word = w, not_the = notThe w, next_word = Nothing }]
recsFromWords (w:ws) =
  WordInfoRec { word = w, not_the = notThe w, next_word = Just $ head ws } : recsFromWords ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fromIntegral . length . filter f . recsFromWords . words
  where f WordInfoRec{not_the=Nothing, next_word=Just nw}
          | elem (head nw) "aeiou" = True
          | otherwise = False
        f WordInfoRec{} = False

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter f . map m
  where m c
          | elem c "aeiou" = Just c
          | otherwise      = Nothing
        f (Just c) = True
        f Nothing  = False

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
  | nConsonants s >= nVowels s = Just $ Word' s
  | otherwise = Nothing
  where nConsonants = fromIntegral . length . filter isConsonant
        nVowels     = fromIntegral . length . filter isVowel
        isVowel = (flip elem) vowels
        isConsonant = not . isVowel
