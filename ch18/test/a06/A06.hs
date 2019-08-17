import Control.Monad ( join
                     , (>=>)
                     )

mcomp :: Monad m => (b -> m c)
                 -> (a -> m b)
                 -> a -> m c
--mcomp f g a = f (g a)
--mcomp f g a = join (f <$> (g a))
mcomp f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "

main :: IO ()
main = putStrLn "Test suite not yet implemented"
