module Main where

import System.Environment (getArgs)
import System.IO

import Control.Monad.Loops (iterateM_)
import Vigenere ( vigenere
                , unvigenere
                )

runVigenere :: (String -> String -> String) -> String -> IO ()
runVigenere f secret =
  interact (unlines . (fmap (f secret)) . lines)

main :: IO ()
main = do
  [secret, mode] <- getArgs
  case mode of
    "-d" -> runVigenere unvigenere secret
    "-e" -> runVigenere vigenere secret
    otherwise -> putStrLn "Unhandled mode!"
