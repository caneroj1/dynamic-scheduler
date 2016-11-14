module Control.DynamicScheduler.NewActions
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Supply
import Control.DynamicScheduler.Internal.NewTypes
import Control.DynamicScheduler.Internal.ThreadUtils
import Control.DynamicScheduler.Internal.Utils
import Control.DynamicScheduler.SchedulerUtils
import Control.DynamicScheduler.Types
import Control.Monad
import Control.Monad.State.Strict
import ListT (ListT)
import qualified ListT as L hiding (ListT)
import Data.Text
import Data.Map.Strict (empty)
import Focus
import GHC.Conc
import qualified STMContainers.Map as SMap
import System.Cron

-- | Data type representing responses when executing actions on the @Scheduler@.
data Report = DoesNotExist
            | NotRunning
            | AlreadyScheduled
            | Success
            deriving Show

type Action = IO Report
type StrategyResult = Decision ScheduledRunnerTV
type TaskMapStrategy = Maybe ScheduledRunnerTV -> STM (Action, StrategyResult)

initScheduler :: IO Scheduler
initScheduler = Sch <$> SMap.newIO <*> newSupply

newTask :: Runner -> Scheduler -> IO (RunnerId, Scheduler)
newTask runner scheduler = do
  stv <- newTVarIO $ newScheduledRunner id'
  atomically $ SMap.insert stv id' (taskMap scheduler)
  runTask stv
  return (id', scheduler')
  where
    (id', scheduler') = nextId scheduler
    newScheduledRunner rId = ScheduledRunner runner rId Waiting NotScheduled

cancelTask :: TaskMapStrategy
cancelTask Nothing   = return (return DoesNotExist, Keep)
cancelTask (Just tv) = do
  s <- readTVar tv
  case runStatus s of
    Running a -> updateTVar s >> return (cancel a >> return Success, Keep)
    _         -> return (return NotRunning, Keep)
  where
    updateTVar s = writeTVar tv s{runStatus = Waiting}

deleteTask :: TaskMapStrategy
deleteTask Nothing   = return (return DoesNotExist, Keep)
deleteTask (Just tv) = do
  s <- readTVar tv
  let first  = cancelAction $ runStatus s
      second = unscheduleAction $ scheduleState s
  return (first >> second >> return Success, Remove)
  where
    cancelAction (Running a) = cancel a
    cancelAction _           = return ()
    unscheduleAction (Scheduled t) = killThread t
    unscheduleAction _             = return ()

stopTask :: TaskMapStrategy
stopTask Nothing     = return (return DoesNotExist, Keep)
stopTask m@(Just tv) = do
  (action, _) <- deleteTask m
  updateTVar =<< readTVar tv
  return (action >> return Success, Keep)
  where
    updateTVar s = writeTVar tv s{runStatus = NoScheduleMatch,
                                  scheduleState = NotScheduled}

startTask :: TaskMapStrategy
startTask Nothing   = return (return DoesNotExist, Keep)
startTask (Just tv) = do
  s <- readTVar tv
  case scheduleState s of
    (Scheduled _) -> return (return AlreadyScheduled, Keep)
    NotScheduled  -> return (runTask tv >> return Success, Keep)

renameTask :: Text -> TaskMapStrategy
renameTask _ Nothing   = return (return DoesNotExist, Keep)
renameTask t (Just tv) = do
  s <- readTVar tv
  let r = runner s
  writeTVar tv s{runner = r{name = t}}
  return (return Success, Keep)

rescheduleTask :: CronSchedule -> TaskMapStrategy
rescheduleTask _ Nothing   = return (return DoesNotExist, Keep)
rescheduleTask c (Just tv) = do
  s <- readTVar tv
  let r = runner s
  writeTVar tv s{runner = r{schedule = c}}
  return (return Success, Keep)

stream :: Scheduler -> ListT STM (RunnerId, ScheduledRunner)
stream = L.traverse (\(k, v) -> (,) <$> pure k <*> readTVar v)
          . SMap.stream
          . taskMap
