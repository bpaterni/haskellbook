import Test.QuickCheck

import FunctorQCProps

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d )
         => Arbitrary (Four a b c d) where
           arbitrary = do
             a <- arbitrary
             b <- arbitrary
             c <- arbitrary
             d <- arbitrary
             return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

-- !!! Impossible due to kind of *
data Trivial = Trivial
  deriving (Eq, Show)

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorIdentity :: Three String Char Int -> Bool)
  quickCheck (functorIdentity :: Four [Int] String Char Int -> Bool)
  quickCheck (functorIdentity :: Four' String Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Identity Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Pair Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Two String Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Three String Char Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Four [Int] String Char Int -> Bool)
  quickCheck ((functorCompose (+1) (*2)) :: Four' String Int -> Bool)
