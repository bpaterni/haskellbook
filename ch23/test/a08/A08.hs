-- Chapter exercises

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

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec sa s = snd . runMoi sa $ s

eval :: Moi s a -> s -> a
eval sa s = fst . runMoi sa $ s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s ->
  ((), f s)

main :: IO ()
main = putStrLn "Test suite is not yet implemented"
