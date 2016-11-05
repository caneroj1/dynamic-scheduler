{-# LANGUAGE GADTs #-}

module Control.DynamicScheduler.Types
(
  Seconds
, mkSeconds
, ExecutorCount(..)
, mkExecutorCount
, Runnable(..)
, Config(..)
, seconds
, source
, executors
, AvailableExecutors
, ThreadMap
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.BroadcastChan
import Control.Monad.State.Strict (StateT)
import Data.Map.Strict (Map)
import GHC.Conc (TVar)
import System.Cron

type ThreadMap a = Map a ThreadId

-- | Newtype wrapper around a number of seconds
newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Eq, Show)

-- | Smart constructor for the Seconds type.
-- Control.Concurrent's threadDelay function expects the thread delay in
-- microseconds. You can just pass an integer representing the whole number
-- of seconds to mkSeconds and we will do the conversion for you.
mkSeconds :: Int -> Maybe Seconds
mkSeconds s
  | s < 0     = Nothing
  | otherwise = Just . Seconds $ s * oneSecondConv
  where
    oneSecondConv  = 1000000

-- | Data type representing the max number of scheduled jobs that could
-- be running.
data ExecutorCount = Unlimited | Limited Integer
  deriving (Eq, Show)

-- | Smart constructor for the ExecutorCount data type. A positive number
-- limits the number of executors that number. Anything else gives an
-- unlimited number of executors.
mkExecutorCount :: Integer -> ExecutorCount
mkExecutorCount e
  | e <= 0    = Unlimited
  | otherwise = Limited e

-- | Typeclass for things that can be run in the scheduler. Implement this
-- typeclass for types that you would like to pass to the scheduler
-- and have it manage running them.
class (Ord a) => Runnable a where
  -- | The run function should return some IO action that is run on a schedule.
  run      :: a -> IO ()
  -- | The schedule function should return a CronSchedule that dictates
  -- when the runnable instance should be scheduled.
  schedule :: a -> CronSchedule

-- | The Config data type represents the configuration options that can be
-- passed to the scheduler.
data Config a where
  Config :: (Runnable a)
         => Seconds -- | The number of seconds that should elapse between when
                    -- the scheduler should wake up and check for more runnable
                    -- actions.

         -> IO [a]  -- | A function that is called whenever the scheduler wakes
                    -- up. This function should return a list of whatever
                    -- runnable actions should now be added to the scheduler.

         -> ExecutorCount -- | The max number of executors for the scheduler.
         -> Config a

-- | Get the seconds from the Config.
seconds :: Config a -> Int
seconds (Config s _ _) = unSeconds s

-- | Get the source function from the Config.
source :: Config a -> IO [a]
source (Config _ src _) = src

-- | Get the number of executors from the Config.
executors :: Config a -> ExecutorCount
executors (Config _ _ e) = e

type AvailableExecutors = TVar ExecutorCount

type CommandChannel a = BroadcastChan a Int
