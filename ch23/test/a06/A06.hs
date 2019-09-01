-- Write State for yourself

import Control.Applicative       (liftA3)
import Control.Monad             (replicateM)
import Control.Monad.Trans.State

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi $ \s -> (\(a',s') -> (f a', s')) (g s)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a,s)
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (a', s') = f s
          (a'', s'') = g s'
       in (a' a'', s'')

instance Monad (Moi s) where
  return = pure

  Moi f >>= g = Moi $ \s ->
    let (a, s') = f s
     in runMoi (g a) s'

main :: IO ()
main = putStrLn "Test suite not yet implemented"
