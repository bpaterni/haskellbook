-- Fix the code
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid = elem '!'

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn $ "Good, was very excite: " ++ e

main :: IO ()
main = doExcite
