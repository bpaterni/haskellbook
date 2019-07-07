import Data.Char
import Data.List

alphaLowerUpper = concat [['a'..'z'],['A'..'Z']]

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
  | elem x alphaLowerUpper = toUpper x : xs
  | otherwise = x : capitalizeWord xs

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c xs = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/=c) xs)

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . (intersperse ".") . (map capitalizeWord) . splitOn '.'
