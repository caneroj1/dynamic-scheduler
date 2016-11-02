module Control.DynamicScheduler.Internal.Utils
(
  toString
) where

import Data.Text (Text, unpack)
import System.Cron.Types (CronSchedule, serializeCronSchedule)

-- | Utility function to stringify a cron schedule.
toString :: CronSchedule -> String
toString = unpack . serializeCronSchedule
