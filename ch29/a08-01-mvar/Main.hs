module Main where

import Control.Concurrent

main :: IO ()
main = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero
