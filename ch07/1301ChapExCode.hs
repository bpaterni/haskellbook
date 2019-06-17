tensDigit :: Integral a => a -> a
tensDigit x = d
  --where xLast = x `div` 10
  where xLast = fst . (flip divMod) 10 $ x
        d     = xLast `mod` 10
--tensDigit = fst . (flip divMod) 10

hunsD :: Integral a => a -> a
hunsD x = d
  where xLast = fst . (flip divMod) 100 $ x
        d     = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
--foldBool x y res =
--  case res of
--    False  -> x
--    _      -> y
foldBool x y res
  | res == True = y
  | otherwise   = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
