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

main :: IO ()
main = putStrLn "Test suite not yet implemented"
