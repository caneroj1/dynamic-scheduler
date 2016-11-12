{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.DynamicScheduler
import Control.Monad
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text hiding (Text)
import System.Cron
import System.Cron.Types

data Task = Task
  {
    taskId       :: Int
  , taskName     :: Text
  , taskSchedule :: CronSchedule
  , taskWait     :: Int
  }

instance Ord Task where
  Task{taskId = t1} <= Task{taskId = t2} = t1 <= t2

instance Eq Task where
  Task{taskId = t1} == Task{taskId = t2} = t1 == t2

instance Runnable Task where
  run Task{taskId = tid, taskName = tname, taskWait = twait} = do
    putStrLn $ "Running Task #" ++ show tid ++ ", " ++ show tname
    putStrLn $ "Waiting for " ++ show twait ++ " seconds.."
    threadDelay twait
    putStrLn $ "Task #" ++ show tid ++ " just woke up!"
    putStrLn "Finishing Up!"

  schedule = taskSchedule

toCronSchedule :: Text -> CronSchedule
toCronSchedule t = let (Right cs) = parseCronSchedule t
                      in cs

everyTwoMinutes :: CronSchedule
everyTwoMinutes = toCronSchedule "*/2 * * * *"

everyThreeMinutes :: CronSchedule
everyThreeMinutes = toCronSchedule "*/3 * * * *"

tasks :: [Task]
tasks = [ Task 1 "First Task"     everyMinute 10
        , Task 2 "Second Task"    everyTwoMinutes 2
        , Task 5 "The Third Task" everyThreeMinutes 5
        , Task 11 "Task Four"     everyMinute 1
        , Task 30 "Best Task"     everyThreeMinutes 20 ]

maxTasks = 5

type MutableInt = IORef Int

taskSource :: MutableInt -> IO [Task]
taskSource intRef = do
  i <- readIORef intRef
  let ts = get i
  modifyIORef' intRef (+1)
  return ts
  where
    get i
      | i < maxTasks = [tasks !! i]
      | otherwise    = []

main = do
  let (Just s) = mkSeconds 15
  intRef <- newIORef 0
  runDynamicScheduler $ cfg intRef s
  forever (return ())
  where
    cfg intRef s = Config s (taskSource intRef) (mkExecutorCount 2)
