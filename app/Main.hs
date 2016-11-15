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
  (rId, sch2) <- newTask r2 sch
  putStrLn $ "Added New Task: " ++ show rId
  threadDelay 10000000
  (rId2, sch3) <- newTask r1 sch2
  putStrLn $ "Added New Task: " ++ show rId2
  threadDelay 60000000
  (rId3, sch4) <- newTask r3 sch3
  putStrLn $ "Added New Task: " ++ show rId3
  threadDelay 20000000
  printScheduler sch4
  putStrLn "Deleting"
  -- (r, sch5) <- deleteTask rId sch4
  r <- deleteTask rId sch4
  print r
  printScheduler sch4
  threadDelay 10000000
  r <- stopTask rId sch4
  print r
  printScheduler sch4
  r <- stopTask rId2 sch4
  print r
  printScheduler sch4
  -- print r

  -- threadDelay 120000000
  -- (rId4, sch6) <- newTask r4 sch4
  -- putStrLn $ "Added New Task: " ++ show rId4
  -- printScheduler sch6
  -- threadDelay 65000000
  -- printScheduler sch6
  -- putStrLn "Cancelling!!"
  -- print =<< cancelTask rId4 sch6
  -- printScheduler sch6
  -- (r, sch7) <- deleteTask rId4 sch6
  -- print r
  -- printScheduler sch7
  where
    r1 = Runner everyMinute msg "First Task"
    r2 = Runner everyTwoMinutes (putStrLn "Hello Two!") "Second Task"
    r3 = Runner everyMinute (putStrLn "Hello Three!") "Third Task"
    r4 = Runner everyMinute (threadDelay 120000000 >> putStrLn "bobo") "Last"

printScheduler :: Scheduler -> IO ()
printScheduler s = mapM_ (print . snd) =<< (atomically . L.toList $ stream s)
