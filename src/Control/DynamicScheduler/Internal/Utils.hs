module Control.DynamicScheduler.Internal.Utils
(
  changeStatus
, changeTime
, changeScheduleState
, changeStatusAndNextTime
) where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.Types
import Data.Time.Clock
import GHC.Conc

changeStatus :: Status -> ScheduledRunner -> ScheduledRunner
changeStatus s r = r{runStatus = s}

changeTime :: UTCTime -> ScheduledRunner -> ScheduledRunner
changeTime t r = r{nextTimeToRun = Just t}

changeScheduleState :: ThreadId -> ScheduledRunner -> ScheduledRunner
changeScheduleState tId s = s{scheduleState = Scheduled tId}

changeStatusAndNextTime :: Status
                        -> Maybe UTCTime
                        -> ScheduledRunner
                        -> ScheduledRunner
changeStatusAndNextTime s u r = r{nextTimeToRun = u, runStatus = s}
