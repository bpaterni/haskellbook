module Main where

import Control.Monad ( forever )
import Data.Char ( isLetter
                 , toLower
                 )
import System.Exit ( exitSuccess )

isPalindrome :: String -> Bool
isPalindrome x = xAug == reverse xAug
  where xAug = map toLower xFiltered
        xFiltered = filter isLetter x

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

main :: IO ()
main = palindrome
