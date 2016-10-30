module Control.DynamicScheduler.SchedulerUtils
(
  startAll
, tryWithExecutor
) where

import Control.Concurrent
import Control.DynamicScheduler.Internal.Utils
import Control.DynamicScheduler.Types
import Control.Monad
import GHC.Conc
import System.Cron.Schedule

startAll :: (Runnable a) => AvailableExecutors -> [a] -> IO [ThreadId]
startAll tv = execSchedule . mapM_ (addMe tv)
  where
    addMe tv runnable =
      addJob (tryWithExecutor tv (run runnable))
        (toString $ schedule runnable)

tryWithExecutor :: AvailableExecutors -> IO () -> IO ()
tryWithExecutor ae fn = do
  scheduled <- scheduleWithExecutor ae
  when scheduled (fn >> unscheduleWithExecutor ae)
  return ()

unscheduleWithExecutor :: AvailableExecutors -> IO ()
unscheduleWithExecutor ae = atomically $ do
  executors <- readTVar ae
  writeTVar ae (returnExecutor executors)

scheduleWithExecutor :: AvailableExecutors -> IO Bool
scheduleWithExecutor ae = atomically $ do
  executors <- readTVar ae
  let canRunTask = canRun executors
  if canRunTask
    then
      writeTVar ae (takeExecutor executors) >> return True
    else
      return False
