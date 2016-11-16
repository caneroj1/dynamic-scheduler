module Control.DynamicScheduler.Internal.Strategies
(
  execStrategy
, cancelStrategy
, deleteStrategy
, stopStrategy
, startStrategy
, renameStrategy
, rescheduleStrategy
, Action
, StrategyResult
, Report(..)
)
where

import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.ThreadUtils
import Control.DynamicScheduler.Internal.Types
import Data.Text (Text)
import Focus
import GHC.Conc
import System.Cron (CronSchedule)

-- | Data type representing responses when executing actions on the @Scheduler@.
data Report = DoesNotExist
            | NotRunning
            | AlreadyScheduled
            | Success
            deriving Show

type Action = IO Report
type TraversalDecision = Decision ScheduledRunnerTV
type StrategyResult = STM (Action, TraversalDecision)
type TaskMapStrategy = ScheduledRunnerTV -> StrategyResult

execStrategy :: TaskMapStrategy -> Maybe ScheduledRunnerTV -> StrategyResult
execStrategy _        Nothing   = return (return DoesNotExist, Keep)
execStrategy strategy (Just tv) = strategy tv

cancelStrategy :: TaskMapStrategy
cancelStrategy tv = do
  s <- readTVar tv
  case runStatus s of
    Running a -> updateTVar s >> return (cancel a >> return Success, Keep)
    _         -> return (return NotRunning, Keep)
  where
    updateTVar s = writeTVar tv s{runStatus = Waiting}

deleteStrategy :: TaskMapStrategy
deleteStrategy tv = do
  s <- readTVar tv
  let first  = cancelAction $ runStatus s
      second = unscheduleAction $ scheduleState s
  return (first >> second >> return Success, Remove)
  where
    cancelAction (Running a) = cancel a
    cancelAction _           = return ()
    unscheduleAction (Scheduled t) = killThread t
    unscheduleAction _             = return ()

stopStrategy :: TaskMapStrategy
stopStrategy tv = do
  (action, _) <- deleteStrategy tv
  updateTVar =<< readTVar tv
  return (action >> return Success, Keep)
  where
    updateTVar s = writeTVar tv s{runStatus = NoScheduleMatch,
                                  scheduleState = NotScheduled}

startStrategy :: TaskMapStrategy
startStrategy tv = do
  s <- readTVar tv
  case (scheduleState s, runStatus s) of
    (Scheduled _, _)                -> return (return AlreadyScheduled, Keep)
    (NotScheduled, NoScheduleMatch) ->
      updateTVar s >>
      return (runTask tv >> return Success,Keep)
    _                               -> return (return AlreadyScheduled, Keep)
  where
    updateTVar s = writeTVar tv s{runStatus = Waiting}

renameStrategy :: Text -> TaskMapStrategy
renameStrategy t = modifyRunner (\runner -> runner{name = t})

rescheduleStrategy :: CronSchedule -> TaskMapStrategy
rescheduleStrategy c = modifyRunner (\runner -> runner{schedule = c})

modifyRunner :: (Runner -> Runner) -> TaskMapStrategy
modifyRunner f tv = do
  s <- readTVar tv
  let r = runner s
  writeTVar tv s{runner = f r}
  return (return Success, Keep)
