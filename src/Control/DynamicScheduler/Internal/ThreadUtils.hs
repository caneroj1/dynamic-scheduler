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
      handleTimeDelay action $ nextDelay cs currTime

    handleTimeDelay _      Nothing  =
      atomically (updateStatusSTM NoScheduleMatch s) >>
      threadDelay tenSeconds                         >>
      threadAction

    handleTimeDelay action (Just t) = do
      threadDelay t
      a <- async action
      setRunningStatus a s
      wait a
      unsetRunningStatus s
      threadAction
