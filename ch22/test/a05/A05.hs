
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  --fmap :: (a -> b)
  --     -> Reader r a
  --     -> Reader r b
  --fmap f (Reader ra) =
  --  Reader $ \r -> f (ra r)
  fmap f (Reader ra) =
    Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
-- \r -> f (ra r)
-- \x -> f (g x)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
