import Test.QuickCheck

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b)  = Second (f b)

-- Functor laws:
--   identity    - fmap id      = id
--   composition - fmap (p . q) = (fmap p) . (fmap q)

functorIdentity :: (Functor f, Eq (f a))
                => f a
                -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
