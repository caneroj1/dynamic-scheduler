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

-- | Given the Config settings, start running the dynamic scheduler. This will
-- create a TVar to hold the number of executors so that we can access them
-- across threads. Then we will start checking for available runnable actions.
-- Whenever we new runnable actions, we schedule them, and the earliest they
-- can run is one minute after we started them.
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

data SchedulerState a =
  SchedulerState {
    threads     :: [ThreadId]
  , config      :: Config a
  }

mkSchedulerState :: Config a -> SchedulerState a
mkSchedulerState = SchedulerState []

addThreadIds :: [ThreadId] -> SchedulerState a -> SchedulerState a
addThreadIds ts st@SchedulerState {threads = oldTs} = st {threads = ts ++ oldTs}

type Scheduler a = StateT (SchedulerState a) IO ()
