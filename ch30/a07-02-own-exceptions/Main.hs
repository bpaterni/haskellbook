module Main where

import Control.Exception

data EATD = 
    NotEven Integer
  | NotDivThree Integer
  deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv :: Integer -> IO Integer
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i        = throwIO (NotEven i)
  | otherwise    = do
    print i
    return i

onlyReportEATD :: Show e
               => IO (Either e a)
               -> IO ()
onlyReportEATD action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

tryEvenAndThreeDiv :: Integer
                   -> IO (Either EATD Integer)
tryEvenAndThreeDiv = try . evenAndThreeDiv

testEvenAndThreeDiv :: Integer -> IO ()
testEvenAndThreeDiv = onlyReportEATD . tryEvenAndThreeDiv

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = mapM_ testEvenAndThreeDiv [1..100]
