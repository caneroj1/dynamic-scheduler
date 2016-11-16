{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Control.DynamicScheduler.Internal.Types
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DynamicScheduler.Internal.CronUtils
import Control.Concurrent.Supply
import Data.Hashable
import Data.Text (Text)
import Data.Time.Clock
import GHC.Conc
import qualified STMContainers.Map as SMap
import System.Cron

-- | A @Runner@ is a datatype that represents a named action
--  that can be scheduled to run according to its CronSchedule.
data Runner = Runner {
    schedule  :: CronSchedule
  , action    :: IO ()
  , name      :: Text
  }

instance Show Runner where
  show Runner{..} = "[" ++ show name ++ "]: " ++ toString schedule

-- | The Id of a @Runner@. Used a key in the @TaskMap@.
newtype RunnerId = Id {
    unId :: Int
  } deriving (Eq, Ord, Show, Hashable)

data Status = NoScheduleMatch
            | Waiting
            | Running (Async ())

instance Show Status where
  show NoScheduleMatch  = "NoScheduleMatch"
  show Waiting          = "Waiting"
  show (Running a)      = "Running [T #" ++ show (asyncThreadId a) ++ "]"

data State = NotScheduled
           | Scheduled ThreadId
    deriving Show

-- | A @ScheduledRunner@ is a wrapper around a @Runner@.
--  It contains all of the information necessary to manage
--  communication with its scheduler and listener threads, and
--  also is assigned a unique @RunnerId@.
data ScheduledRunner = ScheduledRunner {
    runner        :: Runner
  , runnerId      :: RunnerId
  , runStatus     :: Status
  , scheduleState :: State
  , nextTimeToRun :: Maybe UTCTime
  }

instance Show ScheduledRunner where
  show ScheduledRunner{..} =
    show runner        ++
    ", "               ++
    show runStatus     ++
    ", "               ++
    show scheduleState ++
    ", "               ++
    show nextTimeToRun

type ScheduledRunnerTV = TVar ScheduledRunner

-- | A @TaskMap@ is a type synonym for an STM Map of @RunnerId@ types
--  to their corresponding @ScheduledRunner@ records.
type TaskMap = SMap.Map RunnerId ScheduledRunnerTV

-- | @IdSource@ is a shared Int used for creating new @RunnerId@ types.
type IdSource = TVar Supply

-- | @Scheduler@ is a record that holds the IdSource, as well as
--  the @TaskMap@ for the currently scheduled tasks.
data Scheduler = Sch {
    taskMap :: TaskMap
  , source  :: IdSource
  }
