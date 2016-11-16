module Control.DynamicScheduler.Internal.FocusActions
where

import Control.DynamicScheduler.Internal.Strategies
import Control.DynamicScheduler.Internal.Types
import Data.Text (Text)
import System.Cron (CronSchedule)

cancelTask :: Maybe ScheduledRunnerTV -> StrategyResult
cancelTask = execStrategy cancelStrategy

deleteTask :: Maybe ScheduledRunnerTV -> StrategyResult
deleteTask = execStrategy deleteStrategy

stopTask :: Maybe ScheduledRunnerTV -> StrategyResult
stopTask = execStrategy stopStrategy

startTask :: Maybe ScheduledRunnerTV -> StrategyResult
startTask = execStrategy startStrategy

renameTask :: Text -> Maybe ScheduledRunnerTV -> StrategyResult
renameTask t = execStrategy (renameStrategy t)

rescheduleTask :: CronSchedule -> Maybe ScheduledRunnerTV -> StrategyResult
rescheduleTask c = execStrategy (rescheduleStrategy c)
