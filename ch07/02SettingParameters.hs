myNum :: Num a => a
myNum = 1

--myVal = myNum
myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

bindExp :: Integer -> String
bindExp x =
  let x = 10; y = 5 in
      "the integer was: " ++ show x
      ++ " and y was: " ++ show y
