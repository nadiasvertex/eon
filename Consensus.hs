{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures, UndecidableInstances  #-}

module Consensus where

import Data.Binary
import Data.Typeable

import Control.Distributed.Process
import Control.Distributed.Process.Node
import qualified Control.Monad.State as ST

import GHC.Generics (Generic)

import Network.Transport.TCP (createTransport, defaultTCPParameters)

data Rpc = AppendEntries LogEntry
         | RequestVote
         deriving (Typeable, Generic)

data Role = Follower
          | Candidate
          | Leader
          deriving (Typeable, Generic)

data Command = BeginTransaction Int
             | RollbackTransaction Int
             | CommitTransaction Int
             deriving (Typeable, Generic)

data LogEntry = LogEntry {
   cmd           :: Command,
   term          :: Int,
   index         :: Int
} deriving (Typeable, Generic)

data Log = Log{
   currentIndex  :: Int,
   entries       :: [LogEntry]
} deriving (Typeable, Generic)

data RoleState = RoleState {
   currentTerm   :: Int,
   currentRole   :: Role,
   currentLeader :: Maybe NodeId
} deriving (Typeable, Generic)

data ClusterState = ClusterState {
   role          :: RoleState,
   logData       :: Log
} deriving (Typeable, Generic)

type TheClusterState = ST.StateT ClusterState

instance Binary Command
instance Binary LogEntry
instance Binary Rpc

start :: ClusterState
start = ClusterState {
            role = RoleState {currentTerm   = 0,
                              currentRole   = Follower,
                              currentLeader = Nothing},
            logData = Log    {currentIndex  = 0,
                              entries       = []}
         }

beginElection :: RoleState -> RoleState
beginElection state =
   RoleState { currentTerm   = currentTerm state + 1,
               currentRole   = Candidate,
               currentLeader = Nothing }

appendEntry :: ClusterState -> LogEntry -> ClusterState
appendEntry (ClusterState r (Log cur_idx old_entries)) entry =
   ClusterState { role    = r,
                  logData = Log { currentIndex = cur_idx+1,
                                  entries = entry:old_entries
                  }
   }

processAppendEntry :: LogEntry -> TheClusterState Process ()
processAppendEntry entry =
   do
      st <- ST.get
      ST.put $ appendEntry st entry

processRpc :: Rpc -> TheClusterState Process ()
processRpc (AppendEntries entry) =
   processAppendEntry entry

processRpc RequestVote = undefined

listenLoop :: TheClusterState Process ()
listenLoop =
   do
      msg <- ST.lift expect
      processRpc msg
      listenLoop

process :: String -> String -> IO ()
process address port =
   do
      Right t <- createTransport address port defaultTCPParameters
      node <- newLocalNode t initRemoteTable

      _ <- forkProcess node $ do
         my_pid <- spawnLocal $ ST.evalStateT listenLoop start
         liftIO $ print my_pid
         return ()
      return ()
