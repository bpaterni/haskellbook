module Reverse where

-- Given:  "Curry is awesome"
-- Return: "awesome is Curry"
rvrs :: String -> String
rvrs x = first x ++ is x ++ last x
  where first x = drop 9 x
        is    x = take 4 . drop 5 $ x
        last  x = take 5 x

main :: IO ()
main = print . rvrs $ "Curry is awesome"
