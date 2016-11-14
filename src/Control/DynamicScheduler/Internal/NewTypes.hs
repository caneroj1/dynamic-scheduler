{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Control.DynamicScheduler.Internal.NewTypes
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Broadcast
import Control.DynamicScheduler.Internal.CronUtils
import Control.Concurrent.Supply
import Data.Hashable
import Data.Text (Text)
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

-- | Newtype wrapper around a ThreadId. A @ListenerId@
--  stores the ThreadId of a thread whose only job is
--  to listen for when its corresponding task should be run.
newtype ListenerId = Listener {
    unListen :: ThreadId
  }

-- | Newtype wrapper around a ThreadId. A @SchedulerId@
--  stores the ThreadId of a thread whose only job is to
--  schedule a task to be run. The thread corresponding to
--  this ThreadId determines how many microseconds it must
--  wait until a CronSchedule is active again, sleeps for that
--  amount of time, and then broadcasts an Awake message to
--  its listener.
newtype SchedulerId = Scheduler {
    unSchedule :: ThreadId
  }

-- | Datatype representing different kinds of messages that can
--  be broadcast.
data Message = Awake | Kill

data Status = CouldNotSchedule
            | Waiting
            | Running
  deriving Show

-- | A @ScheduledRunner@ is a wrapper around a @Runner@.
--  It contains all of the information necessary to manage
--  communication with its scheduler and listener threads, and
--  also is assigned a unique @RunnerId@.
data ScheduledRunner = ScheduledRunner {
    runner        :: Runner
  , runnerId      :: RunnerId
  -- , communicator  :: Broadcast Message
  -- , listenerId    :: ListenerId
  -- , schedulerId   :: SchedulerId
  , asyncData     :: Maybe (Async ())
  , status        :: Status
  , hostThread    :: Maybe ThreadId
  }

instance Show ScheduledRunner where
  show ScheduledRunner{..} =
    show runner ++ ", " ++ show status

type ScheduledRunnerTV = TVar ScheduledRunner

-- | A @TaskMap@ is a type synonym for an STM Map of @RunnerId@ types
--  to their corresponding @ScheduledRunner@ records.
type TaskMap = SMap.Map RunnerId ScheduledRunnerTV

-- | @IdSource@ is a shared Int used for creating new @RunnerId@ types.
type IdSource = Supply

-- | @IdGen@ is a type synonym for something that runs in the STM monad
--  and returns the next available @RunnerId@.
-- type IdGen = STM RunnerId

-- | @Scheduler@ is a record that holds the IdSource, as well as
--  the @TaskMap@ for the currently scheduled tasks.
data Scheduler = Sch {
    taskMap :: TaskMap
  , source  :: IdSource
  }
