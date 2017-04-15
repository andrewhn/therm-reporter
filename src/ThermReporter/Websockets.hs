module ThermReporter.Websockets where

import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Text.Encoding
import Data.Maybe (fromMaybe, fromJust)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forever, forM_, join)
import Control.Exception (bracket, handle)
import qualified Data.Text as T
import qualified Data.ByteString as B

import ThermReporter.Types

disconnect :: WSConnections -> UUID -> WS.ConnectionException -> IO ()
disconnect wss uuid e = do
  modifyMVar_ wss hndl
  where hndl xs = return $ remove uuid xs
        remove clientid = filter ((/= clientid) . connId)

wsApp :: Config -> WS.ServerApp
wsApp cfg pending = do
  probablyUuid <- nextUUID
  let uuid = fromJust probablyUuid
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- save connection
  let wsc = getWSConnections cfg
  modifyMVar_ wsc $ \xs -> return $ WSConnection uuid conn : xs
  handle (disconnect wsc uuid) $ forever (WS.receiveData conn :: IO B.ByteString)

broadcast :: T.Text -> WSConnections -> IO ()
broadcast message wss = do
  clients <- readMVar wss
  forM_ clients (\client -> (conn client) `WS.sendTextData` message)
