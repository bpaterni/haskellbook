-- 01
data TisAnInteger = TisAn Integer
  deriving Show

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

-- 02
data TwoIntegers =
  Two Integer Integer
  deriving Show

instance Eq TwoIntegers where
  (==) (Two i j) (Two i' j') =
       i == i'
    && j == j'

data TisAString = TisAs String
  deriving Show

instance Eq TisAString where
  (==) (TisAs s) (TisAs s') = s == s'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i)   (TisAnInt i')   = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _              _               = False

data Pair a = Pair a a
  deriving Show

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair v' w') =
       v == v'
    && w == w'

data Tuple a b = Tuple a b
  deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v w) (Tuple v' w') =
       v == v'
    && w == w'

data Which a =
    ThisOne a
  | ThatOne a
  deriving Show

instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne w) (ThatOne w') = w == w'
  (==) _           _            = False

data EitherOr a b =
    Hello a
  | Goodbye b
  deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v)   (Hello v')   = v == v'
  (==) (Goodbye w) (Goodbye w') = w == w'
  (==) _           _            = False
