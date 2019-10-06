-- MonadIO aka zoom-zoom

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (MonadIO m)
  => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

main :: IO ()
main = putStrLn "Test suite not yet implemented"
