{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.WebSockets            as WS
import           Server
import           Control.Monad.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text

onMessage :: ReaderT Env IO ()
onMessage = do
    env <- ask
    send (myConnection env) $ show (myID env) ++ " : " ++ show (message env)
    others <- liftIO $ otherConnections env
    forM_ others $ \conn -> send conn $ show (message env)
  where
    send :: WS.Connection -> String -> ReaderT Env IO ()
    send conn txt = do
        conn <- asks myConnection
        liftIO $ WS.sendTextData conn (pack txt)

main :: IO ()
main = runServer defaultServer { Server.onMessage = Main.onMessage }

