import Control.Monad ( join )
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary a
  => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance (Semigroup b, Semigroup a)
  => Semigroup (BahEither b a) where
    PLeft a <> PLeft a' = PLeft (a <> a')
    PRight b <> PRight b' = PRight (b <> b')
    PRight b <> _ = PRight b
    _ <> PRight b = PRight b

instance (Monoid b, Monoid a)
  => Monoid (BahEither b a) where
    mempty = PLeft mempty
    mappend (PLeft a) (PLeft a') = PLeft (mappend a a')
    mappend (PRight b) (PRight b') = PRight (mappend b b')
    mappend (PRight b) _ = PRight b
    mappend _ (PRight b) = PRight b

instance Functor (BahEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight x) = PRight x

instance (Monoid b) => Applicative (BahEither b) where
  pure = PLeft
  PLeft f <*> PLeft x = PLeft (f x)
  --PRight b <*> PRight b' = PRight (mappend b b')
  PRight x <*> _ = PRight x
  _ <*> PRight x = PRight x

instance (Monoid b) => Monad (BahEither b) where
  return = pure
  PLeft x >>= f = f x
  PRight x >>= _ = PRight x

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (BahEither b a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (2, return $ PLeft a)
                , (1, return $ PRight b)
                ]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
  (=-=) = eq

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

instance Arbitrary a
  => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys =
    Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys =
  Cons x $ xs `append'` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

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

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (\x -> return $ f x)

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
--l2 f ma mb = (ma >>= (\a -> return $ f a)) >>= (\g -> mb >>= (\b -> return $ g b))
-- Much nicer do-syntax
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= (\a -> mf >>= (\f -> return $ f a))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= (\b -> meh as f >>= (\bs -> return $ b:bs))

flipType :: (Monad m) => [m a] -> m [a]
flipType lma = meh lma id

main :: IO ()
main = do
  putStrLn ""
  putStrLn "--- Functor ---"
  quickBatch $ functor (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ functor (PLeft (5, "hi", 6) :: BahEither String (Int, String, Int))
  quickBatch $ functor (Identity (5, "hi", 6) :: Identity (Int, String, Int))
  quickBatch $ functor (Cons (5, "hi", 6) Nil :: List (Int, String, Int))
  putStrLn ""
  putStrLn "--- Applicative ---"
  quickBatch $ applicative (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ applicative (PLeft (5, "hi", 6) :: BahEither String (Int, String, Int))
  quickBatch $ applicative (Identity (5, "hi", 6) :: Identity (Int, String, Int))
  quickBatch $ applicative (Cons (5, "hi", 6) Nil :: List (Int, String, Int))
  putStrLn ""
  putStrLn "--- Monad ---"
  quickBatch $ monad (NopeDotJpg :: Nope (Int, String, Int))
  quickBatch $ monad (PLeft (5, "hi", 6) :: BahEither String (Int, String, Int))
  quickBatch $ monad (Identity (5, "hi", 6) :: Identity (Int, String, Int))
  quickBatch $ monad (Cons (5, 7, 6) Nil :: List (Int, Int, Int))
