module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
  \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c cs = (nextWord : tailWords)
  where nextWord  = takeWhile (/=c) cs
        tailWords = splitOn c $
          dropWhile (==c) (dropWhile (/=c) cs)

myLines :: String -> [String]
myLines = splitOn '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
