-- MaybeT

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
  => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
      MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
  => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (MaybeT fab) <*> (MaybeT mma) =
      MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
  => Monad (MaybeT m) where
    return = pure
    -- (>>=) :: MaybeT m a
    --       -> (a -> MaybeT m b)
    --       -> MaybeT m b
    (MaybeT ma) >>= f =
      MaybeT $ do
        v <- ma
        case v of
          Nothing -> return Nothing
          Just y  -> runMaybeT (f y)


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m)
  => Functor (EitherT e m) where
    fmap f (EitherT ma) =
      EitherT $ (fmap . fmap) f ma

instance (Applicative m)
  => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (EitherT fab) <*> (EitherT mma) =
      EitherT $ (<*>) <$> fab <*> mma

instance (Monad m)
  => Monad (EitherT e m) where
    return = pure
    (EitherT ma) >>= f =
      EitherT $ do
        v <- ma
        case v of
          Left l  -> return (Left l)
          Right y -> runEitherT . f $ y

swapEither :: Either e a
           -> Either a e
--swapEither (Left l) = Right jk
--swapEither (Right ma) = Left ma
swapEither = either Right Left

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT famc fbmc (EitherT amb) = do
  v <- amb
  case v of
    Left l -> famc l
    Right r -> fbmc r

main :: IO ()
main = putStrLn "Test suite not yet implemented"
