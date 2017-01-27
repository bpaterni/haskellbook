module Reverse where

rvrs :: String -> String
rvrs x = concat [last, " ", mid, " ", first]
  where last  = drop 9 x
        mid   = take 2 $ drop 6 x
        first = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
