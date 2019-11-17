module Main where

import Control.Exception
import System.Environment (getArgs)

-- Control.Exception
-- try :: Exception e => IO a -> IO (Either e a)

willIFail :: Integer
          -> IO (Either ArithException ())
willIFail denom =
  try $ print $ div 5 denom

onlyReportError :: Show e
                => IO (Either e a)
                -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail = onlyReportError . willIFail

willFail' :: Integer -> IO ()
willFail' denom =
  print (div 5 denom) `catch` handler
    where handler :: ArithException -> IO ()
          handler e = print e

testDiv :: String -> IO ()
testDiv = onlyReportError . willIFail . read

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = do
  args <- getArgs
  mapM_ testDiv args
