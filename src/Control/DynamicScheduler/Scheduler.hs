module Control.DynamicScheduler.Scheduler
(
  runDynamicScheduler
) where

import Control.Concurrent
import Control.DynamicScheduler.Internal.Utils
import Control.DynamicScheduler.SchedulerUtils
import Control.DynamicScheduler.Types
import Control.Monad
import Control.Monad.State.Strict
import GHC.Conc

runDynamicScheduler :: (Runnable a) => Config a -> IO ()
runDynamicScheduler c = do
  tv <- newTVarIO $ executors c
  scheduler tv `evalStateT` mkSchedulerState c

scheduler :: (Runnable a) => AvailableExecutors -> Scheduler a
scheduler tv = do
  runnables <- liftIO =<< gets (source . config)
  threadIds <- liftIO $ startAll tv runnables
  modify' (addThreadIds threadIds)
  liftIO . threadDelay =<< gets (seconds . config)
  scheduler tv
