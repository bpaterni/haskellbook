module Main where

-- For time and heap profile:
-- 1. stack build
-- 2. from inside project-name/app compile the program with profiling
-- enabled:
--   stack ghc -- -prof -fprof-auto -rtsopts -O2 Main.hs
-- 3. create heap and time profile: ./Main +RTS -hc -p
-- 4. Turn the heap profile into PS file then PDF:
--   stack exec -- hp2ps -c Main.hp && ps2pdf Main.ps

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

dictWordsFile :: String
dictWordsFile = "/usr/share/dict/words"

dictWords :: IO String
dictWords = SIO.readFile dictWordsFile

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile dictWordsFile

dictWordsTL :: IO TL.Text
dictWordsTL = TLIO.readFile dictWordsFile

main :: IO ()
main = do
  replicateM_ 10 (dictWords >>= print)
  replicateM_ 10 (dictWordsT >>= TIO.putStrLn)
  replicateM_ 10 (dictWordsTL >>= TLIO.putStrLn)
