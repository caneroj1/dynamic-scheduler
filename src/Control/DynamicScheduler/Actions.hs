module Control.DynamicScheduler.Actions
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
import Data.Text
import Data.Map.Strict (empty)
import Focus
import GHC.Conc
import qualified STMContainers.Map as SMap

-- | Data type representing responses when executing actions on the @Scheduler@.
data Report = DoesNotExist RunnerId
            | NotRunning   RunnerId
            | Success
            deriving Show

-- | Create a new @Scheduler@ with an empty set of tasks to run.
initScheduler :: IO Scheduler
initScheduler = Sch <$> SMap.newIO <*> newSupply

-- | Adds a new @Runner@ to the @Scheduler@ and starts it immediately.
--  We acquire a new unique @RunnerId@ for the @Runner@ from our supply, and return
--  that, as well as the new @Scheduler@.
addTask :: Runner -> Scheduler -> IO (RunnerId, Scheduler)
addTask runner scheduler = do
  stv <- newTVarIO $ newScheduledRunner id'
  atomically $ SMap.insert stv id' (taskMap scheduler)
  runTask stv
  return (id', scheduler')
  where
    (id', scheduler') = nextId scheduler
    newScheduledRunner rId = ScheduledRunner runner rId Nothing Waiting Nothing

-- | Given a @RunnerId@ and a @Scheduler@, cancel the action associated
--  with that @RunnerId@ if it is running.
cancelTask :: RunnerId -> Scheduler -> IO Report
cancelTask rId scheduler = do
  mbStv <- atomically $ SMap.lookup rId (taskMap scheduler)
  case mbStv of
    Nothing  -> return $ DoesNotExist rId
    Just stv -> cancelScheduledTask rId stv

cancelScheduledTask :: RunnerId -> ScheduledRunnerTV -> IO Report
cancelScheduledTask rId stv = uncurry doCancel =<< toPair stv
  where
    toPair stv = atomically $ do
      s <- readTVar stv
      return (asyncData s, status s)
    doCancel (Just a) Running = wait a >> return Success
    doCancel _        _       = return $ NotRunning rId

-- | Given a @RunnerId@ and a @Scheduler@, delete the task associated
--  with that @RunnerId@ from the list of tasks. If the task is running, it is
--  canceled first.
deleteTask :: RunnerId -> Scheduler -> IO (Report, Scheduler)
deleteTask rId scheduler = do
  stv <- atomically $ SMap.lookup rId (taskMap scheduler)
  killAndDelete rId scheduler stv
--
-- renameTask :: RunnerId -> Scheduler -> Text -> IO ()
-- renameTask rId scheduler name =
--   atomically $
--     SMap.focus adjustFocus rId (taskMap scheduler)
--   where
--     rename s = readTVar s >>= fmap renamed >>= writeTVar s
--       where
--         renamed sch = sch {runner = r {name = name}}
--           where r = runner sch
--     adjustFocus = Focus.adjustM rename

killAndDelete :: RunnerId -> Scheduler -> Maybe ScheduledRunnerTV -> IO (Report, Scheduler)
killAndDelete rId s Nothing   = return (DoesNotExist rId, s)
killAndDelete rId s (Just tv) = do
  cancelTask rId s
  tId <- atomically $ hostThread <$> readTVar tv
  atomically $ SMap.delete rId (taskMap s)
  case tId of
    Nothing  -> return (NotRunning rId, s)
    (Just t) -> killThread t >> return (Success, s)
