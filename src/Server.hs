module Server
    ( defaultServer
    , runServer
    , Env(..)
    , Server(..)
    )
where

import           Data.UUID
import qualified Network.WebSockets            as WS
import qualified Data.Map.Lazy                 as Map
import           Control.Concurrent
import           Control.Monad.Reader
import           System.Random
import           Control.Exception

type AllConnections = Map.Map UUID WS.Connection

data Server = Server{
    hosts :: String,
    port :: Int,
    onMessage :: ReaderT Env IO (),
    onConnect :: WS.Connection -> IO ()
}
data Env = Env{
    otherConnections :: IO AllConnections,
    myID :: UUID,
    myConnection :: WS.Connection,
    message :: WS.DataMessage
}

-- 0.0.0.0:8080
-- nothing to do
defaultServer :: Server
defaultServer = Server { hosts     = "0.0.0.0"
                       , port      = 8080
                       , onMessage = return ()
                       , onConnect = \_ -> return ()
                       }

runServer :: Server -> IO ()
runServer server = do
    mvar <- newMVar Map.empty
    let serverApp :: WS.ServerApp
        serverApp = withConnection mvar $ \(uuid, conn) -> do
            onConnect server conn
            forever $ do
                msg <- WS.receiveDataMessage conn
                let env :: Env
                    env = Env { otherConnections = Map.filterWithKey (\k _ -> k /= uuid) <$> readMVar mvar
                              , myID           = uuid
                              , myConnection   = conn
                              , message        = msg
                              }
                runReaderT (onMessage server) env
    WS.runServer (hosts server) (port server) serverApp

withConnection
    :: MVar AllConnections
    -> ((UUID, WS.Connection) -> IO ())
    -> WS.PendingConnection
    -> IO ()
withConnection mvar act pending = bracket open close act  where
    open :: IO (UUID, WS.Connection)
    open = do
        -- connect
        conn <- WS.acceptRequest pending
        -- 30秒ごとにpingを打って接続を保持
        WS.forkPingThread conn 30
        -- connection一覧に追加
        uuid <- modifyMVar mvar $ addConnection conn
        putStrLn $ "connect from " ++ show uuid
        return (uuid, conn)

    close :: (UUID, WS.Connection) -> IO ()
    close (uuid, _) = do
        -- connection一覧から削除
        modifyMVar_ mvar $ removeConnection uuid
        putStrLn $ "disconnect from " ++ show uuid

    -- connection一覧に追加
    addConnection
        :: WS.Connection -> AllConnections -> IO (AllConnections, UUID)
    addConnection conn allConn = do
        uuid <- randomIO
        return (Map.insert uuid conn allConn, uuid)

    -- connection一覧から削除
    removeConnection :: UUID -> AllConnections -> IO AllConnections
    removeConnection uuid allConn = return $ Map.delete uuid allConn
