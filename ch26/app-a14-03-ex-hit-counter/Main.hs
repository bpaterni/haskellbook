-- Hit counter
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config { counts :: IORef (M.Map Text Integer)
         , prefix :: Text
         }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.lookup k m of
    Just a -> (M.adjust (+1) k m, a + 1)
    Nothing -> (M.insert k 1 m, 1)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = prefix config `mappend` TL.pack unprefixed
        ref  = counts config
    (updatedMap, count) <- liftIO $ bumpBoomp key' <$> readIORef ref
    liftIO $ writeIORef ref updatedMap

    html $
      mconcat [ "<h1>Success! Count for '"
              , TL.pack $ unprefixed
              , "' was: "
              , TL.pack $ show count
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  putStrLn $ "Prefix: " ++ prefixArg
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR r = runReaderT r config
  scottyT 3000 runR app
