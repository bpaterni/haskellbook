import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance Arbitrary a
  => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Constant a b) where
  Constant x <> Constant y = Constant (x <> y)

instance Monoid a => Monoid (Constant a b) where
  mempty = Constant mempty

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a
  => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x `mappend` y)

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

instance Arbitrary a
  => Arbitrary (Constant a b) where
    arbitrary = do
      a <- arbitrary
      return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

data Optional a =
  Nada
  | Yep a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) Nada fx@(Yep _) = fx
  (<>) fx@(Yep _) Nada = fx
  (<>) (Yep x) (Yep y) = Yep (x <> y)

instance Monoid a
  => Monoid (Optional a) where
    mempty = Nada

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a
  => Arbitrary (Optional a) where
    arbitrary = do
      a <- arbitrary
      frequency [ (1, return Nada)
                , (1, return $ Yep a)
                ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys =
    Cons x $ xs <> ys

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

toMyList :: Foldable t => t a -> List a
toMyList = foldr Cons Nil

maxListSize = 10

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList n = do
  xs <- vectorOf n arbitrary
  return $ toMyList xs

instance Arbitrary a
  => Arbitrary (List a) where
    arbitrary = sized go
      where go n = arbitraryList $ mod n maxListSize

instance Eq a => EqProp (List a) where
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

instance (Eq a, Eq b, Eq c)
  => EqProp (Three a b c) where
    (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b)
  => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a b f <*> Three a' b' x = Three (mappend a a') (mappend b b') (f x)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Pair a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Pair a b

instance (Eq a, Eq b)
  => EqProp (Pair a b) where
    (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure x = Pair mempty x
  (Pair x f) <*> (Pair x' y) = Pair (x <> x') (f y)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

data Big a b =
  Big a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Big a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      b' <- arbitrary
      return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = liftA2 (Big a) (f b) (f b')

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Bigger a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      b' <- arbitrary
      b'' <- arbitrary
      return $ Bigger a b b' b''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = liftA3 (Bigger a) (f b) (f b') (f b'')

--data S n a = S (n a) a deriving (Eq, Show)
--
--instance ( Functor n
--         , Arbitrary (n a)
--         , Arbitrary a
--         )
--      => Arbitrary (S n a) where
--        arbitrary =
--          S <$> arbitrary <*> arbitrary
--
--instance ( Applicative n
--         , Testable (n Property)
--         , Eq a
--         , Eq (n a)
--         , EqProp a
--         )
--      => EqProp (S n a) where
--        (=-=) = eq
--
--instance Traversable n
--  => Traversable (S n) where
--    traverse = undefined

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  xs <- treeGen
  ys <- treeGen
  a <- arbitrary
  elements [ Leaf a
           , Empty
           , Node xs a ys
           ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node tl a tr) = Node (fmap f tl) (f a) (fmap f tr)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node tl a tr) = (foldMap f tl) <> f a <> (foldMap f tr)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node tl a tr) = Node <$> traverse f tl <*> f a <*> traverse f tr

main :: IO ()
main = do
  let tIdentity :: Identity (Int, Int, [Int])
      tIdentity = undefined
      tConstant :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      tConstant = undefined
      tOptional :: Optional (Int, Int, [Int])
      tOptional = undefined
      tList     :: List (Int, Int, [Int])
      tList     = undefined
      tThree    :: Three Bool Char (Int, Int, [Int])
      tThree    = undefined
      tPair     :: Pair Char (Int, Int, [Int])
      tPair     = undefined
      tBig      :: Big Char (Int, Int, [Int])
      tBig      = undefined
      tBigger   :: Bigger Char (Int, Int, [Int])
      tBigger   = undefined
      tTree     :: Tree (Int, Int, [Int])
      tTree     = undefined
  quickBatch $ traversable tIdentity
  quickBatch $ traversable tConstant
  quickBatch $ traversable tOptional
  quickBatch $ traversable tList
  quickBatch $ traversable tThree
  quickBatch $ traversable tPair
  quickBatch $ traversable tBig
  quickBatch $ traversable tBigger
  quickBatch $ traversable tTree
