{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

newtype IntString =
  IntString (Int, String)
  deriving (Eq, Show)

instance TooMany IntString where
  tooMany (IntString (n, _)) = n > 5

newtype IntInt =
  IntInt (Int, Int)
  deriving (Eq, Show)

instance TooMany IntInt where
  tooMany (IntInt (n, m)) = (n+m) > 10

--newtype NumAndTooMany =
--  (Num a, TooMany a) => (a, a)
--  deriving (Eq, show)
--newtype AnotherTooMany a =
--  AnotherTooMany (a, a)
--  deriving (Eq, Show, TooMany)

--instance NumAndTooMany where
--  tooMany (NumAndTooMany (m, n)) = (m+n) > 15
instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (t1, t2) = (t1+t2) > 44
