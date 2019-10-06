-- Chapter Excercises

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity

rDec :: Num a => Reader a a
--rDec = Reader . ((-)1)
rDec = reader (flip (-) 1)

rShow :: Show a
      => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  liftIO $ putStrLn ("hi: " ++ show r)
  return (r + 1)

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO . putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
