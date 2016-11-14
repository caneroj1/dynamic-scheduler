{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.DynamicScheduler
import Control.Monad
import qualified ListT as L
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text hiding (Text)
import GHC.Conc
import System.Cron
import System.Cron.Types
--
-- data Task = Task
--   {
--     taskId       :: Int
--   , taskName     :: Text
--   , taskSchedule :: CronSchedule
--   , taskWait     :: Int
--   }
--
-- instance Ord Task where
--   Task{taskId = t1} <= Task{taskId = t2} = t1 <= t2
--
-- instance Eq Task where
--   Task{taskId = t1} == Task{taskId = t2} = t1 == t2
--
-- instance Runnable Task where
--   run Task{taskId = tid, taskName = tname, taskWait = twait} = do
--     putStrLn $ "Running Task #" ++ show tid ++ ", " ++ show tname
--     putStrLn $ "Waiting for " ++ show twait ++ " seconds.."
--     threadDelay twait
--     putStrLn $ "Task #" ++ show tid ++ " just woke up!"
--     putStrLn "Finishing Up!"
--
--   schedule = taskSchedule
--
toCronSchedule :: Text -> CronSchedule
toCronSchedule t = let (Right cs) = parseCronSchedule t
                      in cs

-- everyMinute :: CronSchedule
-- everyMinute = toCronSchedule "*/1 * * * *"

everyTwoMinutes :: CronSchedule
everyTwoMinutes = toCronSchedule "*/2 * * * *"

everyThreeMinutes :: CronSchedule
everyThreeMinutes = toCronSchedule "*/3 * * * *"

-- tasks :: [Task]
-- tasks = [ Task 1 "First Task"     everyMinute 10
--         , Task 2 "Second Task"    everyTwoMinutes 2
--         , Task 5 "The Third Task" everyThreeMinutes 5
--         , Task 11 "Task Four"     everyMinute 1
--         , Task 30 "Best Task"     everyThreeMinutes 20 ]
--
-- maxTasks = 5
--
-- type MutableInt = IORef Int
--
-- taskSource :: MutableInt -> IO [Task]
-- taskSource intRef = do
--   i <- readIORef intRef
--   let ts = get i
--   modifyIORef' intRef (+1)
--   return ts
--   where
--     get i
--       | i < maxTasks = [tasks !! i]
--       | otherwise    = []

msg = putStrLn "Hello!"

main = do
  -- let (Just s) = mkSeconds 15
  -- intRef <- newIORef 0
  -- runDynamicScheduler $ cfg intRef s
  sch <- initScheduler
  (rId, sch2) <- addTask r2 sch
  putStrLn $ "Added New Task: " ++ show rId
  threadDelay 10000000
  (rId2, sch3) <- addTask r1 sch2
  putStrLn $ "Added New Task: " ++ show rId2
  threadDelay 60000000
  (rId3, sch4) <- addTask r3 sch3
  putStrLn $ "Added New Task: " ++ show rId3
  threadDelay 60000000
  printScheduler sch4
  putStrLn "Deleting"
  (r, sch5) <- deleteTask rId sch4
  print r
  printScheduler sch5
  threadDelay 120000000
  (rId4, sch6) <- addTask r4 sch4
  putStrLn $ "Added New Task: " ++ show rId4
  printScheduler sch6
  threadDelay 65000000
  printScheduler sch6
  putStrLn "Cancelling!!"
  print =<< cancelTask rId4 sch6
  printScheduler sch6
  (r, sch7) <- deleteTask rId4 sch6
  print r
  printScheduler sch7
  where
    -- cfg intRef s = Config s (taskSource intRef) (mkExecutorCount 2)
    r1 = Runner everyMinute msg "First Task"
    r2 = Runner everyTwoMinutes (putStrLn "Hello Two!") "Second Task"
    r3 = Runner everyMinute (putStrLn "Hello Three!") "Third Task"
    r4 = Runner everyMinute (threadDelay 120000000 >> putStrLn "bobo") "Last"

printScheduler :: Scheduler -> IO ()
printScheduler s = mapM_ (print . snd) =<< (atomically . L.toList $ stream s)
