module Main where

x = undefined

y = "blah"

main :: IO ()
main = do
  print $ x `seq` snd (x, y)
