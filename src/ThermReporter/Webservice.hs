module ThermReporter.Webservice where

import Servant
import Control.Monad.Trans.Except
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode)

import ThermReporter.Types
import ThermReporter.Persist
import ThermReporter.Websockets

type ServiceAPI = DataAPI
             :<|> Raw  -- static index

type DataAPI = "report" :> ReqBody '[JSON] TemperatureReport
                        :> Post '[JSON] TemperatureReport
          :<|> "history" :> Get '[JSON] [HistoryPayload]

type AppM = ReaderT Config Handler

apiServer :: Config -> Server ServiceAPI
apiServer cfg =
  let receive t = do
        wsc <- asks getWSConnections
        tc <- asks getTempCounter
        conn <- asks getDatabaseConn
        liftIO $ do
          writeReading tc t conn
          broadcast (T.pack $ LBS.unpack $ encode t) wsc
        return t
      hist = do
        conn <- asks getDatabaseConn
        h <- liftIO $ getHistory conn
        return h
      dataApi :: ServerT DataAPI AppM  -- needs guidance
      dataApi = receive :<|> hist
      static = serveDirectory "static/"
  in (enter (readerToEither cfg) dataApi) :<|> static

readerToEither :: Config -> AppM :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg
