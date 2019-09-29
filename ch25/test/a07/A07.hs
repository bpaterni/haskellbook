-- Monad transformers

-- Doing it badly

newtype MaybeIO a =
  MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a =
  MaybeList { runMaybeList :: [Maybe a] }

-- ^^^ but we don't need to resort to this ^^^
-- can get a Monad for two types, as long as we know what one of the
-- types is.

main :: IO ()
main = putStrLn "Test suite not yet implemented"
