module Control.DynamicScheduler.Internal.Utils
(
  toString
) where

import Data.Text (Text, unpack)
import System.Cron.Types (CronSchedule, serializeCronSchedule)

toString :: CronSchedule -> String
toString = unpack . serializeCronSchedule
