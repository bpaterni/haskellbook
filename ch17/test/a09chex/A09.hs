import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Type []
-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]
--
-- Type IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b
--
-- Type (,) a
-- pure :: Monoid a => b -> (a, b)
-- (<*>) :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
--
-- Type (->) e
-- pure :: a -> b -> a
-- (<*>) :: (e -> a -> b) -> (e -> a) -> e -> b

data Pair a = Pair a a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Eq a
  => EqProp (Pair a) where
    (=-=) = eq

data Two a b = Two a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two a' x) = Two (mappend a a') (f x)

instance (Eq a, Eq b)
  => EqProp (Two a b) where
    (=-=) = eq

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

instance (Monoid a, Monoid b)
  => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a b f <*> Three a' b' x = Three (mappend a a') (mappend b b') (f x)

instance (Eq a, Eq b, Eq c)
  => EqProp (Three a b c) where
    (=-=) = eq

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      b' <- arbitrary
      return $ Three' a b b'

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a)
  => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' a f g <*> Three' a' x y = Three' (mappend a a') (f x) (g y)

instance (Eq a, Eq b)
  => EqProp (Three' a b) where
    (=-=) = eq

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

instance (Monoid a, Monoid b, Monoid c)
  => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    Four a b c f <*> Four a' b' c' x = Four (mappend a a') (mappend b b') (mappend c c') (f x)

instance (Eq a, Eq b, Eq c, Eq d)
  => EqProp (Four a b c d) where
    (=-=) = eq

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

instance Monoid a
  => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' a b c f <*> Four' a' b' c' x = Four' (mappend a a') (mappend b b') (mappend c c') (f x)

instance (Eq a, Eq b)
  => EqProp (Four' a b) where
    (=-=) = eq

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (\x y z -> (x,y,z))

main :: IO ()
main = do
  quickBatch $ applicative (Pair (1,2,3) (4,5,6) :: Pair (Int,Int,Int))
  quickBatch $ applicative (Two [1] (4,5,6) :: Two [Int] (Int, Int, Int))
  quickBatch $ applicative (Three [1] [2] (4,5,6) :: Three [Int] [Int] (Int, Int, Int))
  quickBatch $ applicative (Three' [1] (1,2,3) (4,5,6) :: Three' [Int] (Int, Int, Int))
  quickBatch $ applicative (Four [1] [2] [3] (1,2,3) :: Four [Int] [Int] [Int] (Int, Int, Int))
  quickBatch $ applicative (Four' [1] [2] [3] (1,2,3) :: Four' [Int] (Int, Int, Int))
