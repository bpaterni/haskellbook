-- MonadTrans

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
  hiding (get)
import Data.Monoid (mconcat)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    --lift (putStrLn "hello")
    --(ActionT
    --  . (ExceptT . liftM Right)
    --  . liftReaderT
    --  . \m -> StateT (\s -> do
    --                      a <- m
    --                      return (a, s)))
    --  (putStrLn "hello")
    liftIO (putStrLn "hello")
    html $
      mconcat [ "<h1>Scotty, "
              , beam
              , " me up!</h1>"
              ]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
