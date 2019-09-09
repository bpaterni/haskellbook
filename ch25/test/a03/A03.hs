-- Two functors sittin' in a tree, L-I-F-T-I-N-G

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose fga) =
      Compose $ (fmap . fmap) f fga

v :: Compose []
             Maybe
             (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
