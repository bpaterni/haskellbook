module QCHangman
  where

import Data.Maybe
import Hangman
  ( fillInCharacter
  , gameWords
  , handleGuess
  , Puzzle (..)
  , WordList (..)
  )
import Test.QuickCheck

gameWordsToGen :: WordList -> Gen WordList
gameWordsToGen wl = do
  return $ wl

puzzleGen :: Gen Puzzle
puzzleGen = do
  --gws <- arbitrary `suchThat` (flip elem ["hello", "world"])
  --a <- arbitrary `suchThat` (flip elem ["hello", "world"])
  --b <- arbitrary `suchThat` (\x -> length x == length a)
  --return Puzzle{ secret      = a
  --             , discoveries = b
  --             , guesses     = []
  --             , nIncorrect  = 0 }
  oneof [ return Puzzle { secret = "hello"
                        , discoveries = map (const Nothing) "hello"
                        , guesses     = []
                        , nIncorrect  = 0
                        }
        ]

instance Arbitrary Puzzle where
  arbitrary = puzzleGen

puzzleAndValidGuessGen :: Gen (Puzzle, Char)
puzzleAndValidGuessGen = do
  a <- arbitrary
  b <- arbitrary `suchThat` (flip elem (secret a))
  return (a, b)

puzzleAndInvalidGuessGen :: Gen (Puzzle, Char)
puzzleAndInvalidGuessGen = do
  a <- arbitrary
  b <- arbitrary `suchThat` (not . flip elem (secret a))
  return (a, b)

isPuzzleGuessValid :: (Puzzle, Char) -> Bool
isPuzzleGuessValid (p, g) =
  length matchedNewDiscoveries > 0
    where filledPuzzle = fillInCharacter p g
          newDiscoveries = discoveries filledPuzzle
          matchedNewDiscoveries = filter (==g) (catMaybes newDiscoveries)

isPuzzleGuessInvalid :: (Puzzle, Char) -> Bool
isPuzzleGuessInvalid (p, g) =
  length matchedNewDiscoveries == 0
    where filledPuzzle = fillInCharacter p g
          newDiscoveries = discoveries filledPuzzle
          matchedNewDiscoveries = filter (==g) (catMaybes newDiscoveries)

prop_puzzleWithValidGuess :: Property
prop_puzzleWithValidGuess =
  forAll puzzleAndValidGuessGen isPuzzleGuessValid

prop_puzzleWithInvalidGuess :: Property
prop_puzzleWithInvalidGuess =
  forAll puzzleAndInvalidGuessGen isPuzzleGuessInvalid

runQC :: IO ()
runQC = do
  quickCheck prop_puzzleWithValidGuess
  quickCheck prop_puzzleWithInvalidGuess

main :: IO ()
main = runQC
