module Control.DynamicScheduler.Internal.ThreadUtils
(
  nextId
, runTask
) where

import Control.Concurrent.Async
import Control.Concurrent.Supply
import Control.DynamicScheduler.Internal.CronUtils
import Control.DynamicScheduler.Internal.Types
import Control.DynamicScheduler.Internal.Utils
import Control.Monad
import Data.Time.Clock
import GHC.Conc

nextId :: TVar Supply -> STM RunnerId
nextId ts = do
  s <- readTVar ts
  let (next, s') = freshId s
  writeTVar ts s'
  return $ Id next

setRunningStatus :: Async () -> ScheduledRunnerTV -> IO ()
setRunningStatus a tv = atomically $
  writeTVar tv =<< updateStatusSTM (Running a) tv

unsetRunningStatus :: ScheduledRunnerTV -> IO ()
unsetRunningStatus tv = atomically $
  writeTVar tv =<< updateStatusSTM Waiting tv

updateStatusSTM :: Status -> ScheduledRunnerTV -> STM ScheduledRunner
updateStatusSTM s tv = changeStatus s <$> readTVar tv

setNextTime :: UTCTime -> ScheduledRunnerTV -> STM ()
setNextTime t tv = writeTVar tv =<< changeTime t <$> readTVar tv

updateStatusAndTime :: Status
                    -> Maybe UTCTime
                    -> ScheduledRunnerTV
                    -> STM ()
updateStatusAndTime s t tv = writeTVar tv =<< updateStatusUTC s t tv

updateStatusUTC :: Status
                -> Maybe UTCTime
                -> ScheduledRunnerTV
                -> STM ScheduledRunner
updateStatusUTC s t tv = changeStatusAndNextTime <$> pure s
                                                 <*> pure t
                                                 <*> readTVar tv

updateScheduleStateSTM :: ScheduledRunnerTV -> ThreadId -> IO ()
updateScheduleStateSTM tv tId = atomically $
  writeTVar tv =<< changeScheduleState tId <$> readTVar tv

runTask :: ScheduledRunnerTV -> IO ()
runTask s = updateScheduleStateSTM s =<< forkIO threadAction
  where
    threadAction = do
      (cs, action) <- atomically $ do
                        r <- runner <$> readTVar s
                        return (schedule r, action r)
      currTime <- getCurrentTime
      handleTimeDelay currTime action $ nextDelay cs currTime

    handleTimeDelay _ _      Nothing  = do
      atomically (updateStatusAndTime NoScheduleMatch Nothing s)
      threadDelay tenSeconds
      threadAction

    handleTimeDelay ct action (Just t) = do
      let nextUTCRunTime = utcAfterDelay t ct
      atomically (setNextTime nextUTCRunTime s)
      threadDelay t
      a <- async action
      setRunningStatus a s
      wait a
      unsetRunningStatus s
      threadAction
