module Main where

import Control.Exception

canICatch :: Exception e
          => e
          -> IO (Either SomeException ())
canICatch = try . throwIO

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
