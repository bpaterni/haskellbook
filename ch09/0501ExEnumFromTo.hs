myEFT :: (Enum a, Ord a) => a -> a -> [a]
myEFT start stop
  | (compare start stop) == GT = []
  | (compare start stop) == EQ = [start]
  | otherwise                = (start : myEFT (succ start) stop)

eftBool :: Bool -> Bool -> [Bool]
--eftBool True _      = []
--eftBool True  True  = [True]
--eftBool False False = [False]
--eftBool start stop  = (start : eftBool (succ start) stop)
eftBool = myEFT

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEFT
