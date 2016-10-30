{-# LANGUAGE GADTs #-}

module Control.DynamicScheduler.Types
(
  Seconds(unSeconds)
, mkSeconds
, ExecutorCount
, mkExecutorCount
, canRun
, takeExecutor
, returnExecutor
, Runnable(..)
, Config(..)
, seconds
, source
, executors
, SchedulerState(..)
, addThreadIds
, mkSchedulerState
, AvailableExecutors
, Scheduler
) where

import Control.Concurrent (ThreadId)
import Control.Monad.State.Strict (StateT)
import GHC.Conc (TVar)
import System.Cron

newtype Seconds = Seconds { unSeconds :: Int }

mkSeconds :: Int -> Seconds
mkSeconds s
  | s < 0     = error "mkSeconds: unexpected negative number"
  | otherwise = Seconds $ s * oneSecondConv
  where
    oneSecondConv  = 1000000

data ExecutorCount = Unlimited | Limited Integer

mkExecutorCount :: Integer -> ExecutorCount
mkExecutorCount e
  | e <= 0    = Unlimited
  | otherwise = Limited e

canRun :: ExecutorCount -> Bool
canRun (Limited 0) = False
canRun _           = True

takeExecutor :: ExecutorCount -> ExecutorCount
takeExecutor (Limited n) = Limited (n-1)
takeExecutor e           = e

returnExecutor :: ExecutorCount -> ExecutorCount
returnExecutor (Limited n) = Limited (n+1)
returnExecutor e           = e

class Runnable a where
  run      :: a -> IO ()
  schedule :: a -> CronSchedule

data Config a where
  Config :: (Runnable a)
         => Seconds
         -> IO [a]
         -> ExecutorCount
         -> Config a

seconds :: Config a -> Int
seconds (Config s _ _) = unSeconds s

source :: Config a -> IO [a]
source (Config _ src _) = src

executors :: Config a -> ExecutorCount
executors (Config _ _ e) = e

type AvailableExecutors = TVar ExecutorCount

data SchedulerState a =
  SchedulerState {
    threads     :: [ThreadId]
  , config      :: Config a
  }

addThreadIds :: [ThreadId] -> SchedulerState a -> SchedulerState a
addThreadIds ts st@SchedulerState {threads = oldTs} = st {threads = ts ++ oldTs}

mkSchedulerState :: Config a -> SchedulerState a
mkSchedulerState = SchedulerState []

type Scheduler a = StateT (SchedulerState a) IO ()
