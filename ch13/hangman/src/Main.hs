module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust
                  , fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering
                 , stdout )
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter isGameLength aw)
    where isGameLength w =
           let l = length (w :: String)
            in  l >= minWordLength
             && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
    ++ " Guessed so far: '" ++ guessed ++ "'"

freshPuzzle :: String -> Puzzle
freshPuzzle x =
  Puzzle xLower
         (map (const Nothing) xLower)
         []
           where xLower = map toLower x

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle x _ _) = (flip elem) x . toLower

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ x) = (flip elem) x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discoved guessed) c =
  Puzzle word newFilledInSoFar (cLower:guessed)
    where cLower = toLower c
          zipper cGuess cWord cGuessed =
            if cGuess == cWord
               then Just cWord
               else cGuessed
          newFilledInSoFar =
            zipWith (zipper cLower) word discoved

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 
     then do putStrLn "You lose!"
             putStrLn $
               "The word was: " ++ wordToGuess
             exitSuccess
     else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
     then do putStrLn "You win!"
             exitSuccess
     else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "

  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle word
  runGame puzzle
