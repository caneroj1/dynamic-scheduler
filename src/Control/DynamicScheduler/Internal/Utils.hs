module Control.DynamicScheduler.Internal.Utils
(
  toString
, nextDelay
, tenSeconds
, changeStatus
, setAsync
, unsetAsync
, changeHostThread
) where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.NewTypes
import Data.Text (Text, unpack)
import Data.Time.Clock
import GHC.Conc
import System.Cron
import System.Cron.Types (CronSchedule, serializeCronSchedule)

-- | Utility function to stringify a cron schedule.
toString :: CronSchedule -> String
toString = unpack . serializeCronSchedule

nextSecondsDelay :: UTCTime -> UTCTime -> Int
nextSecondsDelay next = nominalToInt . diffUTCTime next

nominalToInt :: NominalDiffTime -> Int
nominalToInt = round . (scale *) . realToFrac

nextDelay :: CronSchedule -> UTCTime -> Maybe Int
nextDelay cs current = flip nextSecondsDelay current <$> nextMatch cs current

scale :: Double
scale = 1000000

tenSeconds :: Int
tenSeconds = round $ scale * 10

changeStatus :: Status -> ScheduledRunner -> ScheduledRunner
changeStatus s r = r{status = s}

changeHostThread :: ThreadId -> ScheduledRunner -> ScheduledRunner
changeHostThread tId s = s{hostThread = Just tId}

setAsync :: Async () -> ScheduledRunner -> ScheduledRunner
setAsync d s = s{asyncData = Just d}

unsetAsync :: ScheduledRunner -> ScheduledRunner
unsetAsync s = s{asyncData = Nothing}
