module Control.DynamicScheduler.Internal.ThreadUtils
(
  nextId
, runTask
) where

import Control.Concurrent.Async
import Control.Concurrent.Supply
import Control.DynamicScheduler.Internal.NewTypes
import Control.DynamicScheduler.Internal.Utils
import Control.Monad
import Data.Time.Clock
import GHC.Conc

nextId :: Scheduler -> (RunnerId, Scheduler)
nextId s =
  let (next, source') = freshId $ source s
    in (Id next, s {source = source'})

setRunningStatus :: Async () -> ScheduledRunnerTV -> IO ()
setRunningStatus a tv = atomically $
  writeTVar tv =<< setAsync a <$> updateStatusSTM Running tv

unsetRunningStatus :: ScheduledRunnerTV -> IO ()
unsetRunningStatus tv = atomically $
  writeTVar tv =<< unsetAsync <$> updateStatusSTM Waiting tv

updateStatusSTM :: Status -> ScheduledRunnerTV -> STM ScheduledRunner
updateStatusSTM s tv = changeStatus s <$> readTVar tv

updateHostThreadSTM :: ScheduledRunnerTV -> ThreadId -> IO ()
updateHostThreadSTM tv tId = atomically $
  writeTVar tv =<< changeHostThread tId <$> readTVar tv

runTask :: ScheduledRunnerTV -> IO ()
runTask s = updateHostThreadSTM s =<< forkIO threadAction
  where
    threadAction = do
      cs <- atomically $ schedule . runner <$> readTVar s
      currTime <- getCurrentTime
      handleTimeDelay $ nextDelay cs currTime

    handleTimeDelay Nothing  = atomically (updateStatusSTM CouldNotSchedule s)
                               >> threadDelay tenSeconds >> threadAction
    handleTimeDelay (Just t) = do
      threadDelay t
      fn <- atomically (action . runner <$> readTVar s)
      d  <- async fn
      setRunningStatus d s
      wait d
      unsetRunningStatus s
      threadAction
