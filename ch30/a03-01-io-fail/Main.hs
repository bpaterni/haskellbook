module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)

handler' :: SomeException -> IO ()
handler' (SomeException e) = do
  putStrLn ( "Running main caused an error!\
             \ It was: " ++ show e )
  writeFile "bbb" "hi"

main :: IO ()
main = do
  catch go handler'
    where go = do
            writeFile "aaa" "hi"
            putStrLn "wrote to file"
