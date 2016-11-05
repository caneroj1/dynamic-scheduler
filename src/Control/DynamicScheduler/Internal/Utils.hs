module Control.DynamicScheduler.Internal.Utils
(
  toString
, nextDelay
) where

import Data.Text (Text, unpack)
import Data.Time.Clock
import System.Cron
import System.Cron.Types (CronSchedule, serializeCronSchedule)

-- | Utility function to stringify a cron schedule.
toString :: CronSchedule -> String
toString = unpack . serializeCronSchedule

nextSecondsDelay :: UTCTime -> UTCTime -> Int
nextSecondsDelay next = nominalToInt . diffUTCTime next

nominalToInt :: NominalDiffTime -> Int
nominalToInt = round . (1000000 *) . realToFrac

nextDelay :: CronSchedule -> UTCTime -> Maybe Int
nextDelay cs current = flip nextSecondsDelay current <$> nextMatch cs current
