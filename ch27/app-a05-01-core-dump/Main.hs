module Main where

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1

s_discrimiatory :: Bool -> Int
s_discrimiatory b =
  let x = undefined
  in case x `seq` b of
       False -> 0
       True -> 1

ex01 = const 1 undefined

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
