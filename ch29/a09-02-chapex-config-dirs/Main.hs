module Main where

import Control.Monad
import Data.Map (Map, assocs, fromList)
import qualified Data.Map as M
import qualified Options.Applicative as OA
import System.Directory
import System.FilePath
import Text.Trifecta

import ConfigINI

data ConfigDirsOptions = ConfigDirsOptions
  { dir :: String }
  deriving (Show, Eq)

configDirsOptions :: OA.Parser ConfigDirsOptions
configDirsOptions = ConfigDirsOptions
  <$> OA.argument OA.str
      ( OA.help "Directory to load" <> OA.metavar "DIR" )

mapFileContents :: [FilePath] -> IO (Map FilePath String)
mapFileContents fps = do
  fcTups <- mapM
    (\f -> do
      content <- readFile f
      return (f, content))
    fps
  return $ fromList fcTups

mapFileConfigINI :: (Map FilePath String) -> (Map FilePath (Result Config))
mapFileConfigINI = M.map (runParser parseIni mempty)

mainWithOptions :: ConfigDirsOptions -> IO ()
mainWithOptions ConfigDirsOptions{dir=dir} = do
  files <- listDirectory dir
  let inis = map (dir </>) $ filter ("ini" `isExtensionOf`) files
  mapFCs <- mapFileContents inis
  --mapM_ putStrLn inis
  let mapFRs = mapFileConfigINI mapFCs
  mapM_ (\(k, v) -> do
    putStrLn $ " --- " ++ k ++ " --- "
    putStrLn $ show v)
    --(assocs mapFCs)
    (assocs mapFRs)

-- stack runghc a09-02-chapex-config-dirs/Main.hs -- ARGS
main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = mainWithOptions =<< OA.execParser opts
  where opts = OA.info (configDirsOptions OA.<**> OA.helper)
          ( OA.fullDesc <> OA.header "Parse directory of INI files" )
