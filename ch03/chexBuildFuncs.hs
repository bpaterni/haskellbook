module ChexBuildBuncs where

str = "Curry is awesome"
strBang = appendExclamation str

appendExclamation = (++ "!")

charAt4 = (!! 4)

drop9 = drop 9

charAt2 = (!! 2)

thirdLetter :: String -> Char
thirdLetter = charAt2

letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome!"

rvrs = concat [last, mid, first]
  where last  = (drop 9 str) ++ " "
        mid   = take 3 $ drop 6 str
        first = take 5 str
