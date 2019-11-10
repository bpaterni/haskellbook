module Main where

import Control.Monad
import Options.Applicative
import System.Environment (getArgs)
import System.IO
import System.IO.Error

import Vigenere ( vigenere
                , unvigenere
                )

runVigenere :: (String -> String -> String) -> String -> IO ()
runVigenere f secret =
  interact (unlines . (fmap (f secret)) . lines)

runVigenereNoTimeout :: IOWithVigenereOptions -> IO ()
runVigenereNoTimeout
  IOWithVigenereOptions{ secret=secret, isEncode=isEncode } =
    runVigenere func secret
      where func = if isEncode
                      then vigenere
                      else unvigenere

runVigenereWithTimeout :: IOWithVigenereOptions -> IO ()
runVigenereWithTimeout
  opts@IOWithVigenereOptions{ secret=secret
                            , isEncode=isEncode
                            , timeout=timeout } =
    go (if isEncode then vigenere else unvigenere)
      where go func = do
              isInput <- catchIOError
                (liftM Just (hWaitForInput stdin (timeout*1000)))
                (\e -> if isEOFError e then return Nothing else ioError e)
              case isInput of
                Nothing -> return ()
                Just True -> do
                  line <- getLine
                  putStrLn $ func secret line
                  go func
                Just False -> hPutStrLn stderr $ "Timeout exceeded! Exiting..."

runVigenere' :: IOWithVigenereOptions -> IO ()
runVigenere'
  opts@IOWithVigenereOptions{ timeout=timeout }
    | timeout <= 0 = runVigenereNoTimeout opts
    | otherwise    = runVigenereWithTimeout opts
    
data IOWithVigenereOptions = IOWithVigenereOptions
  { secret   :: String
  , isEncode :: Bool
  , timeout  :: Int }
  deriving (Show, Eq)

ioWithVigenereOptions :: Parser IOWithVigenereOptions
ioWithVigenereOptions = IOWithVigenereOptions
  <$> argument str
      ( help "Secret cipher key"
      <> metavar "SECRET" )
  <*> switch
      ( long "encode" <> short 'e' <> help "Encode input?" )
  <*> option auto
      ( long "timeout"
      <> help "Time to wait for additional input"
      <> showDefault
      <> value 0
      <> metavar "INT" )

mainWithOptions :: IOWithVigenereOptions -> IO ()
mainWithOptions = runVigenere'

-- stack runghc a09-01-chapex-io-with-vigenere/Main.hs -- hello -d
main :: IO ()
main = mainWithOptions =<< execParser opts
  where opts = info (ioWithVigenereOptions <**> helper)
          ( fullDesc
          <> header "Decode or encode using Vigenere Cipher" )
