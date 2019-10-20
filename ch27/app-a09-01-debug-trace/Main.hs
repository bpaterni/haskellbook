module Main where

import Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd" (1 + 1))
    + twice
      (trace "I got eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne =
        trace "I got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne

bla = Just $ trace "eval'd 1" 1

fm' = fmap ((+1) :: Int -> Int) bla

blah = Just $ trace "eval'd 1" 1

a :: Num a => a
a = 1

concrete :: Int
concrete = 1

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
