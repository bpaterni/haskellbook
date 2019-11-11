{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Exception
  ( ArithException(..)
  , AsyncException(..) )
import Data.Typeable

-- Good -- using existential quantification
data MyException =
  forall e .
    (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) =
    showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    _ -> Right n

-- Bad -- using a sum type
data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing ->
      case cast e of
        (Just async) -> Async async
        Nothing -> SomethingElse

runDisc n =
  either discriminateError
  (const SomethingElse) (multiError n)

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
