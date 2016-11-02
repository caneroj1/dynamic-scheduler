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

-- | Given the number of available executors and a new list of runnable things,
-- start scheduling them.
startAll :: (Runnable a) => AvailableExecutors -> [a] -> IO [ThreadId]
startAll tv = execSchedule . mapM_ (addMe tv)
  where
    addMe tv runnable =
      addJob (tryWithExecutor tv (run runnable))
        (toString $ schedule runnable)

-- | Given the number of available executors and an IO action that should be
-- run, try and grab an executor. If there are no available executors, then
-- the action is not run. If there is an available executor, we take one out
-- of the pool, run the action, and return the executor to the pool when we are
-- finished.
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

canRun :: ExecutorCount -> Bool
canRun (Limited 0) = False
canRun _           = True

takeExecutor :: ExecutorCount -> ExecutorCount
takeExecutor (Limited n) = Limited (n-1)
takeExecutor e           = e

returnExecutor :: ExecutorCount -> ExecutorCount
returnExecutor (Limited n) = Limited (n+1)
returnExecutor e           = e
