import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a
  => Semigroup (ZipList a) where
  (ZipList x) <> (ZipList y) = ZipList (x <> y)

instance Monoid a
  => Monoid (ZipList a) where
    --mempty = ZipList []
    mempty = pure mempty
    mappend = liftA2 mappend

instance Eq a
  => EqProp (ZipList a) where
    (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys =
  Cons x $ xs `append'` ys

fold' :: (a -> b -> b) -> b -> List a -> b
fold' _ b Nil = b
fold' f b (Cons x xs) = f x (fold' f b xs)

concat' :: List (List a) -> List a
concat' = fold' append' Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f = concat' . fmap f

toMyList :: Foldable t => t a -> List a
toMyList = foldr Cons Nil

arbitraryList :: Arbitrary a => Int -> Gen (List a)
arbitraryList n =
  do xs <- vectorOf n arbitrary
     return $ toMyList xs

instance Arbitrary a
  => Arbitrary (List a) where
    arbitrary = sized arbitraryList

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f Nil) <*> xs = f <$> xs
  (Cons f fs) <*> xs = (f <$> xs) `append'` (fs <*> xs)

instance Eq a
  => EqProp (List a) where
    (=-=) = eq

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  (ZipList' xs) <*> (ZipList' ys) =
    ZipList' $ map (\(f, x) -> f x) (zip xs ys)

arbitraryZipList' :: Arbitrary a
                  => Int -> Gen (ZipList' a)
arbitraryZipList' n =
  do xs <- vectorOf n arbitrary
     return $ ZipList' xs

instance Arbitrary a
  => Arbitrary (ZipList' a) where
    arbitrary = sized arbitraryZipList'

data Validation err a =
    Failure' err
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure' err) = Failure' err
  fmap f (Success' x) = Success' (f x)

instance Monoid e
  => Applicative (Validation e) where
    pure x = Success' x
    --(<*>) = undefined
    (Failure' e0) <*> (Failure' e1) = Failure' (e0 <> e1)
    (Failure' e) <*> _ = Failure' e
    _ <*> (Failure' e) = Failure' e
    (Success' f) <*> (Success' x) = Success' (f x)

arbitraryValidation :: (Arbitrary err, Arbitrary a)
                    => Gen (Validation err a)
arbitraryValidation = do
  err <- arbitrary
  a   <- arbitrary
  frequency [ (4, return (Success' a))
            , (1, return (Failure' err))
            ]

instance (Arbitrary err, Arbitrary a)
  => Arbitrary (Validation err a) where
    arbitrary = arbitraryValidation

instance (Eq err, Eq a)
  => EqProp (Validation err a) where
    (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  -- takes forever on the 'composition' law for some reason?
  --quickBatch $ applicative (Cons ('a', 'h', '1') Nil)
  quickBatch $ applicative (ZipList' [(1,2,3)] :: ZipList' (Int,Int,Int))
  quickBatch $ applicative (Success' (1, 2, 3) :: Validation String (Int, Int, Int))
