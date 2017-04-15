module ThermReporter.Persist where

import Database.SQLite.Simple (Connection, query_, execute)
import qualified Data.Map as M
import Control.Concurrent (modifyMVar_, readMVar)
import Data.Maybe (fromMaybe)
import Data.List (sortBy, groupBy)
import Data.Function (on)

import ThermReporter.Types

writeReading :: TempCounter -> TemperatureReport -> Connection -> IO ()
writeReading tc tr conn = do
  m <- readMVar tc
  let cacheKey = n tr
      numReads = fromMaybe 0 (M.lookup cacheKey m)
  if numReads >= 29  -- write approx every 30 seconds
    then do
      modifyMVar_ tc (return . M.insert cacheKey 0)
      execute conn "INSERT INTO obs(timestamp, site, sensor, temperature) \
                   \VALUES (strftime(\"%s\", CURRENT_TIMESTAMP),?,?,?)" tr
    else modifyMVar_ tc (return . M.insert cacheKey (numReads + 1))

getHistory :: Connection -> IO [HistoryPayload]
getHistory conn =
  let q = "SELECT sensor, site, temperature, timestamp \
          \FROM obs \
          \ORDER BY timestamp \
          \LIMIT 1000"
  in buildPayload <$> query_ conn q
  where
    buildPayload :: [TemperatureReport] -> [HistoryPayload]
    buildPayload = map build . groupBy keyEq . sortBy keyOrd
    keyOrd a b = compare (n a, s a) (n b, s b)
    keyEq a b = keyOrd a b == EQ
    build trs@(tr:_) = HistoryPayload (s tr) (n tr) (fmap makeObs trs)
    makeObs tr = TempObs (dt tr) (t tr)
