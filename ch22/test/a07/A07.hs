-- The Monad of functions
import Control.Applicative (liftA2)

newtype Reader r a =
  Reader { runReader :: r -> a }

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
           humanName :: HumanName
         , dogName :: DogName
         , address :: Address
         } deriving (Eq, Show)

data Dog =
  Dog {
        dogsName :: DogName
      , dogsAddress :: Address
      } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

liftA2' :: Applicative f
        => (a -> b -> c)
        -> f a -> f b -> f c
liftA2' f a a' = f <$> a <*> a'

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) =
    Reader $ (f . ra)

instance Applicative (Reader r) where
  pure a = Reader $ (\_ -> a)
  Reader rab <*> Reader ra =
    --Reader $ (\r -> rab r (ra r))
    Reader $ (rab <*> ra)

instance Monad (Reader r) where
  return = pure
  Reader ra >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor f, Foldable f, Num a) => f a -> (f a, Int)
barPlus r = (foo r, length r)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a)
        -> (a -> r -> b)
        -> (r -> b)
fooBind m k = \r -> k (m r) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader $ \p -> dogName p
  addy <- Reader $ \p -> address p
  return $ Dog name addy
  --Reader (\p -> dogName p) >>=
  --  (\name -> Reader (\p -> address p) >>=
  --    (\addy -> return $ Dog name addy))

main :: IO ()
main = putStrLn "Test suite not yet implemented"
