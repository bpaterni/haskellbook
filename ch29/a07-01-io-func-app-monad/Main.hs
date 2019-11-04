-- IO's Functor, Applicative, and Monad
module Main where

import Control.Monad (replicateM, join)
import System.Random (randomIO)

embedInIO = return :: a -> IO a

s = "I'll put in some ingredients"

main :: IO ()
main = do
  putStrLn "Entrypoint not implemented yet"
