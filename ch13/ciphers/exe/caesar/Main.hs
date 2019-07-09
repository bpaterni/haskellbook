module Main where

import Caesar ( caesar )

main :: IO ()
main = do
  plaintext <- getLine
  putStrLn $ caesar plaintext
