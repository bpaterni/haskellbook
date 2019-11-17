module Main where

import Control.Exception

noWhammies :: IO (Either SomeException ())
noWhammies = try undefined

megaButtums :: IO (Either SomeException ())
megaButtums = try $ return undefined

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
