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

toCronSchedule :: Text -> CronSchedule
toCronSchedule t = let (Right cs) = parseCronSchedule t
                      in cs

everyTwoMinutes :: CronSchedule
everyTwoMinutes = toCronSchedule "*/2 * * * *"

everyThreeMinutes :: CronSchedule
everyThreeMinutes = toCronSchedule "*/3 * * * *"

msg = putStrLn "Hello!"

main = do
  sch <- initScheduler
  rId <- newTask r2 sch
  putStrLn $ "Added New Task: " ++ show rId
  printScheduler sch
  threadDelay 10000000
  printScheduler sch
  rId2 <- newTask r1 sch
  putStrLn $ "Added New Task: " ++ show rId2
  threadDelay 60000000
  rId3 <- newTask r3 sch
  putStrLn $ "Added New Task: " ++ show rId3
  threadDelay 20000000
  printScheduler sch
  putStrLn "Deleting"
  r <- deleteTask rId sch
  print r
  printScheduler sch
  threadDelay 10000000
  r <- stopTask rId sch
  print r
  printScheduler sch
  putStrLn "Stopping!"
  r <- stopTask rId2 sch
  print r
  printScheduler sch
  threadDelay 120000000
  rId4 <- newTask r4 sch
  putStrLn $ "Added New Task: " ++ show rId4
  printScheduler sch
  threadDelay 65000000
  printScheduler sch
  putStrLn "Cancelling!!"
  print =<< cancelTask rId4 sch
  printScheduler sch
  threadDelay 10000000
  putStrLn "Starting!"
  print =<< startTask rId2 sch
  printScheduler sch
  threadDelay 10000000
  putStrLn "Starting Again"
  print =<< startTask rId2 sch
  printScheduler sch
  threadDelay 10000000
  putStrLn "Renaming!"
  print =<< renameTask "RENAMED!" rId4 sch
  printScheduler sch
  threadDelay 10000000
  putStrLn "Rescheduling - Not Exist"

  -- does not exist
  print =<< rescheduleTask everyMinute rId sch
  printScheduler sch
  threadDelay 10000000
  putStrLn "Rescheduling - Yes"

  -- should work
  print =<< rescheduleTask everyTwoMinutes rId2 sch
  printScheduler sch
  threadDelay 10000000
  where
    r1 = Runner everyMinute msg "First Task"
    r2 = Runner everyTwoMinutes (putStrLn "Hello Two!") "Second Task"
    r3 = Runner everyMinute (putStrLn "Hello Three!") "Third Task"
    r4 = Runner everyMinute (threadDelay 120000000 >> putStrLn "bobo") "Last"

printScheduler :: Scheduler -> IO ()
printScheduler s = mapM_ (print . snd) =<< (atomically . L.toList $ stream s)
