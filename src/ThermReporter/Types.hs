module ThermReporter.Types where

import qualified Data.Map as M
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar)
import Data.UUID (UUID)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data WSConnection = WSConnection
  { connId :: UUID
  , conn :: WS.Connection
  }

type WSConnections = MVar [WSConnection]

type TempCounter = MVar (M.Map String Int)

data Config = Config
  { getWSConnections :: WSConnections
  , getTempCounter :: TempCounter
  , getDatabaseConn :: Connection
  }

-- incoming from sensors
data TemperatureReport = TemperatureReport
  { n :: String          -- sensor name
  , s :: String          -- site name
  , t :: Double          -- temp reading (celsius)
  , dt :: Maybe Integer  -- timestamp
  } deriving (Show, Generic)

instance FromJSON TemperatureReport
instance ToJSON TemperatureReport
instance ToRow TemperatureReport
  where toRow tr = [toField $ s tr, toField $ n tr, toField $ t tr]
instance FromRow TemperatureReport
  where fromRow = TemperatureReport <$> field <*> field <*> field <*> field

-- outgoing from webservice
data HistoryPayload = HistoryPayload
  { site :: String
  , sensor :: String
  , observations :: [TempObs]
  } deriving (Show, Generic)

instance ToJSON HistoryPayload

data TempObs = TempObs
  { ts :: Maybe Integer
  , c :: Double
  } deriving (Show, Generic)

instance ToJSON TempObs

