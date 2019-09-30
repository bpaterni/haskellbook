-- StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m)
  => Functor (StateT s m) where
    fmap f (StateT sma) =
      StateT $ \s ->
        fmap (\(a, s') -> (f a, s')) $ sma s

instance (Monad m)
  => Applicative (StateT s m) where
    pure x = StateT (\s -> pure (x, s))
    StateT smab <*> StateT sma =
      StateT $ \s -> do
        (a2b, s') <- smab s
        (a, s'')  <- sma s'
        return (a2b a, s'')

instance (Monad m)
  => Monad (StateT s m) where
    return = pure
    --(>>=) :: StateT s m a
    --      -> (a -> StateT s m b)
    --      -> StateT s m b
    StateT sma >>= f =
      StateT $ \s -> do
        (a, s') <- sma s
        runStateT (f a) s

main :: IO ()
main = putStrLn "Test suite not yet implemented"
