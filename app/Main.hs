module Main where

import Network.Wai.Handler.Warp (run, defaultSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Control.Concurrent (newMVar)
import Servant (serve, Proxy(..))
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Database.SQLite.Simple (open)

import ThermReporter.Types
import ThermReporter.Webservice
import ThermReporter.Websockets

main :: IO ()
main = do
  wsState <- newMVar [] :: IO WSConnections
  tCache <- newMVar M.empty :: IO TempCounter
  conn <- open "temperature.db"  -- fixme: clean up on exit
  let config = Config wsState tCache conn
      app = serve (Proxy :: Proxy ServiceAPI) $ apiServer config
      wsServer = wsApp config
      appWithSocket = websocketsOr WS.defaultConnectionOptions wsServer app
  run 5353 appWithSocket
