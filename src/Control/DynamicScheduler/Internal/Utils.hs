module Control.DynamicScheduler.Internal.Utils
(
  changeStatus
, setAsync
, unsetAsync
, changeHostThread
) where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.NewTypes
import GHC.Conc

changeStatus :: Status -> ScheduledRunner -> ScheduledRunner
changeStatus s r = r{status = s}

changeHostThread :: ThreadId -> ScheduledRunner -> ScheduledRunner
changeHostThread tId s = s{hostThread = Just tId}

setAsync :: Async () -> ScheduledRunner -> ScheduledRunner
setAsync d s = s{asyncData = Just d}

unsetAsync :: ScheduledRunner -> ScheduledRunner
unsetAsync s = s{asyncData = Nothing}
