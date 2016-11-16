module Control.DynamicScheduler.Internal.CronUtils
(
  toString
, nextDelay
, tenSeconds
, utcAfterDelay
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
nominalToInt = round . (scale *) . realToFrac

nextDelay :: CronSchedule -> UTCTime -> Maybe Int
nextDelay cs current = flip nextSecondsDelay current <$> nextMatch cs current

utcAfterDelay :: Int -> UTCTime -> UTCTime
utcAfterDelay i t = fromInteger (toSeconds i) `addUTCTime` t

scale :: Double
scale = 1000000

toSeconds :: Int -> Integer
toSeconds i = toInteger i `div` round scale

tenSeconds :: Int
tenSeconds = round $ scale * 10
