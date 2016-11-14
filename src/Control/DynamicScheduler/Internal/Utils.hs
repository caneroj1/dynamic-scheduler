module Control.DynamicScheduler.Internal.Utils
(
  changeStatus
, changeScheduleState
) where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.NewTypes
import GHC.Conc

changeStatus :: Status -> ScheduledRunner -> ScheduledRunner
changeStatus s r = r{runStatus = s}

changeScheduleState :: ThreadId -> ScheduledRunner -> ScheduledRunner
changeScheduleState tId s = s{scheduleState = Scheduled tId}

-- setAsync :: Async () -> ScheduledRunner -> ScheduledRunner
-- setAsync d s = s{asyncData = Just d}
--
-- unsetAsync :: ScheduledRunner -> ScheduledRunner
-- unsetAsync s = s{asyncData = Nothing}
