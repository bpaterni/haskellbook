module Main where

import Criterion.Main

infixl 9 !?
-- attempt #1
--_      !? n | n < 0 = Nothing
--[]     !? _         = Nothing
--(x:_)  !? 0         = Just x
--(_:xs) !? n         = xs !? (n-1)
-- attempt #2
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
    (\x r k ->
      case k of
        0 -> Just x
        _ -> r (k-1))
    (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998
  ]
