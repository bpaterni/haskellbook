-- Morra -- P-C, random C
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment (getArgs)
import System.Random (randomRIO)

data Player =
    Human { score :: Integer
          , position :: Integer
          }
  | Computer { score :: Integer
             , position :: Integer
             }
  deriving (Eq)

instance Show Player where
  show (Human { score = s }) =
    mconcat [ "Human: " ++ show s ]
  show (Computer { score = s}) =
    mconcat [ "Computer: " ++ show s ]

playerAddPoint :: Player -> Player
playerAddPoint p = p { score = (score p) + 1 }

data Game =
  Game { player1 :: Player
       , player2 :: Player
       }
  deriving (Eq)

instance Show Game where
  show (Game { player1 = p1, player2 = p2 }) =
    mconcat [ show p1
            , "\n"
            , show p2
            ]

displayInitInfo :: Game -> IO ()
displayInitInfo (Game { player1 = p1, player2 = p2 }) = do
  putStrLn $ mconcat [ "-- P1 ("
                     , case p1 of
                         (Human {}) -> "Human"
                         (Computer {}) -> "Computer"
                     , ") is odds, P2 ("
                     , case p2 of
                         (Human {}) -> "Human"
                         (Computer {}) -> "Computer"
                     , ") is evens."
                     ]

getPlayerGuess :: Player -> IO Integer
getPlayerGuess p@(Human { position = pos}) = do
  putStr $ mconcat [ "Player "
                   , show pos
                   , " (Human) Guess: "
                   ]
  g <- getLine
  case g of
    "1" -> return 1
    "2" -> return 2
    otherwise -> do
      putStrLn "Invalid guess, try again!"
      getPlayerGuess p
getPlayerGuess (Computer {}) = liftIO $ randomRIO (1,2)

--displayRoundResults :: Game
--                    -> Integer
--                    -> Integer
--                    -> IO ()
--displayRoundResults (Game { player1=p1, player2=p2 }) guessP1 guessP2 = do
--  putStrLn $ mconcat [ "Player "
--                     , show $ position p1
--                     , 

nextGame :: Game -> Integer -> Integer -> IO Game
nextGame game@(Game {player1=p1, player2=p2}) guessP1 guessP2 = do
  let sumGuesses = guessP1 + guessP2
      isP1Winner = even sumGuesses
      _nextGame
         | isP1Winner = game { player1 = playerAddPoint p1 }
         | otherwise  = game { player2 = playerAddPoint p2 }
  putStrLn $ mconcat [ show guessP1
                     , " + "
                     , show guessP2
                     , " = "
                     , show sumGuesses
                     , " --> "
                     , case isP1Winner of
                         True -> "even"
                         False -> "odd"
                     , ". P"
                     , case isP1Winner of
                         True -> show $ position p1
                         False -> show $ position p2
                     , " Wins!"
                     ]
  return _nextGame

runGame :: StateT Game IO Game
runGame = do
  game@(Game { player1 = p1, player2 = p2 }) <- get
  guessP1 <- liftIO $ getPlayerGuess p1
  guessP2 <- liftIO $ getPlayerGuess p2
  nextG <- liftIO $ nextGame game guessP1 guessP2
  liftIO $ putStrLn $ show nextG
  case (+) (score . player1 $ nextG) (score . player2 $ nextG) >= 3 of
    False -> do
      nextG' <- liftIO $ execStateT runGame nextG
      return nextG'
    True -> do
      put nextG
      return nextG

displayEndResults :: Game -> IO ()
displayEndResults (Game { player1=p1, player2=p2 }) = do
  let resOrdering = score p1 `compare` score p2
  --putStrLn $ mconcat [ show $ score p1
  --                   , case resOrdering of
  --                       LT -> " < "
  --                       EQ -> " = "
  --                       GT -> " > "
  --                   , show $ score p2
  --                   ]
  putStrLn $ case resOrdering of
               LT -> "Player 2 wins!"
               EQ -> "Tie!"
               GT -> "Player 1 wins!"

playGame :: Game -> IO ()
playGame g = do
  displayInitInfo g
  endGame <- execStateT runGame g
  displayEndResults endGame
  return ()

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = do
  [argNPlayers] <- getArgs
  let nPlayers = read argNPlayers :: Integer
  case nPlayers of
    1 -> playGame $ Game (Human 0 1) (Computer 0 2)
    2 -> playGame $ Game (Human 0 1) (Human 0 2)
    otherwise -> do
      putStrLn $ "Invalid # of players " ++ show nPlayers
