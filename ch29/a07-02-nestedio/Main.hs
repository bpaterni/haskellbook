-- IO's Functor, Applicative, and Monad
--   * Nested IO
module Main where

import Data.Time.Calendar
import Data.Time.Clock
import System.Random

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")

main :: IO ()
main = do
  putStrLn "Entrypoint not implemented yet"
