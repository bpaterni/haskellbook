-- Exercises: Compose Instances

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose fga) =
      Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
  => Applicative (Compose f g) where
    pure = Compose . pure . pure
    Compose fgf <*> Compose fgx =
      Compose $ ((<*>) <$> fgf <*> fgx)

instance (Foldable f, Foldable g)
  => Foldable (Compose f g) where
    foldMap f (Compose ghx) = foldMap (foldMap f) ghx

instance (Traversable f, Traversable g)
  => Traversable (Compose f g) where
    traverse f (Compose ghx) = Compose <$> traverse (traverse f) ghx

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  --bimap f g (Const a) = Const (f a) (g a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a

data Drei a b c = Drei a b c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
  deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right a) = Right (g a)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
