module Control.DynamicScheduler.API
where

import Control.Concurrent.Supply
import qualified Control.DynamicScheduler.Internal.FocusActions as Focus
import Control.DynamicScheduler.Internal.NewTypes
import Control.DynamicScheduler.Internal.Strategies
import Control.DynamicScheduler.Internal.ThreadUtils
import Control.Monad
import ListT (ListT)
import qualified ListT as L hiding (ListT)
import Data.Text (Text)
import Data.Map.Strict (empty)
import GHC.Conc
import qualified STMContainers.Map as SMap
import System.Cron (CronSchedule)

type FocusFunc = Maybe ScheduledRunnerTV -> StrategyResult

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

cancelTask :: RunnerId -> Scheduler -> IO Report
cancelTask = withFocus Focus.cancelTask

deleteTask :: RunnerId -> Scheduler -> IO Report
deleteTask = withFocus Focus.deleteTask

stopTask :: RunnerId -> Scheduler -> IO Report
stopTask = withFocus Focus.stopTask

startTask :: RunnerId -> Scheduler -> IO Report
startTask = withFocus Focus.startTask

renameTask :: Text -> RunnerId -> Scheduler -> IO Report
renameTask t = withFocus (Focus.renameTask t)

rescheduleTask :: CronSchedule -> RunnerId -> Scheduler -> IO Report
rescheduleTask c = withFocus (Focus.rescheduleTask c)

stream :: Scheduler -> ListT STM (RunnerId, ScheduledRunner)
stream = L.traverse (\(k, v) -> (,) <$> pure k <*> readTVar v)
          . SMap.stream
          . taskMap

withFocus :: FocusFunc -> RunnerId -> Scheduler -> IO Report
withFocus f rId s =
  join . atomically $ SMap.focus f rId (taskMap s)
