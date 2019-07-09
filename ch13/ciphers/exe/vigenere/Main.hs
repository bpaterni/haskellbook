module Main where

import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering
                 , stdout )

import Vigenere ( vigenere )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Vigenere secret: "
  secret <- getLine
  putStr "Message Plaintext: "
  plaintext <- getLine
  putStrLn $ "Encrypted message: " ++ vigenere secret plaintext
