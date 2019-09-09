-- Twinplicative

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure  = Identity
  Identity f <*> Identity x = Identity $ f x

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

v :: Compose []
             Maybe
             (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
