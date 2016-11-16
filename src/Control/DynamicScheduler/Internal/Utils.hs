module Control.DynamicScheduler.Internal.Utils
(
  changeStatus
, changeScheduleState
) where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.Types
import GHC.Conc

changeStatus :: Status -> ScheduledRunner -> ScheduledRunner
changeStatus s r = r{runStatus = s}

changeScheduleState :: ThreadId -> ScheduledRunner -> ScheduledRunner
changeScheduleState tId s = s{scheduleState = Scheduled tId}
