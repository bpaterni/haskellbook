module GlobalLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction = (+ (woot + topLevelValue))
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
