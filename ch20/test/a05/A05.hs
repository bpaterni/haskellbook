-- Some basic derived operations
import Control.Applicative
import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
--sum' = foldr (+) 0
sum' = getSum . (foldMap Sum)

product' :: (Foldable t, Num a) => t a -> a
--product' = foldr (*) 1
product' = getProduct . (foldMap Product)

elem' :: (Foldable t, Eq a)
      => a -> t a -> Bool
elem' x = getAny . (foldMap (\y -> Any (x == y)))

newtype Minimum a = Minimum { getMinimum :: Maybe a }
  deriving (Eq, Show)

instance Ord a => Semigroup (Minimum a) where
  Minimum a <> Minimum b = Minimum ( (min <$> a <*> b) <|> a <|> b )

instance Ord a => Monoid (Minimum a) where
  mempty = Minimum Nothing

minimum' :: (Foldable t, Ord a)
        => t a -> Maybe a
minimum' = getMinimum . foldMap (Minimum . Just)

newtype Maximum a = Maximum { getMaximum :: Maybe a }
  deriving (Eq, Show)

instance Ord a => Semigroup (Maximum a) where
  Maximum a <> Maximum b = Maximum ( (max <$> a <*> b) <|> a <|> b )

instance Ord a => Monoid (Maximum a) where
  mempty = Maximum Nothing

maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' = getMaximum . foldMap (Maximum . Just)

null' :: (Foldable t) => t a -> Bool
null' = (not . getAny) . foldMap (\_ -> Any True)

length' :: (Foldable t) => t a -> Int
length' = getSum . (foldMap (\_ -> Sum 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x -> [x])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' t = foldMap ((<>) mempty) t

foldMap' :: (Foldable t, Monoid m)
         => (a -> m) -> t a  -> m
foldMap' f = foldr ((<>) . f) mempty

data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two a' x) = Two (mappend a a') (f x)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a)
  => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' a f g <*> Three' a' x y = Three' (mappend a a') (f x) (g y)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a
  => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    Four' a b c f <*> Four' a' b' c' x = Four' (mappend a a') (mappend b b') (mappend c c') (f x)

instance Foldable (Four' a) where
  foldMap f (Four' _ _ _ b) = f b

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
