import Control.Applicative
import Control.Monad

-- Functor's fmap in terms of Applicative
fmapApp f xs = pure f <*> xs

-- Functor's fmap in terms of Monad
fmapMon f xs = xs >>= return . f

-- implement bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

main :: IO ()
main = putStrLn "Test suite not yet implemented"
