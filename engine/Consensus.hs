{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Consensus where


import           Data.Binary
import           Data.Typeable

import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (forkProcess, initRemoteTable)
import           Control.Exception.Base                             (bracket)

import           GHC.Generics                                       (Generic)

import qualified Control.Monad.State                                as ST
import qualified Control.Monad.Trans.Resource                       as Res
import qualified Database.LevelDB                                   as DB

data Rpc     = AppendEntries LogEntry
 | RequestVote
 deriving (Typeable, Generic)

data Role    = Follower
 | Candidate
 | Leader
 deriving (Typeable, Generic)

data Command = BeginTransaction Int
 | RollbackTransaction Int
 | CommitTransaction Int
 deriving (Typeable, Generic)

data LogEntry     = LogEntry {
   cmd   :: Command,
   term  :: Int,
   index :: Int
} deriving (Typeable, Generic)

data Log          = Log{
   currentIndex :: Int,
   entries      :: [LogEntry]
} deriving (Typeable, Generic)

data RoleState    = RoleState {
   currentTerm   :: Int,
   currentRole   :: Role,
   currentLeader :: Maybe NodeId
} deriving (Typeable, Generic)

data ClusterState = ClusterState {
   role      :: RoleState,
   logData   :: Log,
   peerNodes :: [NodeId]
} deriving (Typeable, Generic)

type TheClusterState = ST.StateT ClusterState

instance Binary Command
instance Binary LogEntry
instance Binary Rpc

-- How many milliseconds to wait while asking for peers
-- to identify themselves.
cluster_announce_wait :: Int
cluster_announce_wait = 250

def :: DB.Options
def = DB.defaultOptions { DB.createIfMissing = True,
                          DB.cacheSize= 2048
}

def_write = DB.defaultWriteOptions

--with_database ::  (f -> DB.DB) -> DB.DB ()
with_database operations =
  bracket Res.createInternalState Res.closeInternalState $ \resource_state -> do
    Res.runInternalState action resource_state
    where action = do
        db <- DB.open "consensus_log.db" def
        operations db

-- Creates a new start state for the cluster.
start :: ClusterState
start = ClusterState {
            role = RoleState {currentTerm   = 0,
                              currentRole   = Follower,
                              currentLeader = Nothing},
            logData = Log    {currentIndex  = 0,
                              entries       = []},
            peerNodes = []
         }

beginElection :: RoleState -> RoleState
beginElection state =
   RoleState { currentTerm   = currentTerm state + 1,
               currentRole   = Candidate,
               currentLeader = Nothing }



appendEntry :: ClusterState -> LogEntry -> ClusterState
appendEntry (ClusterState r (Log cur_idx old_entries) p) entry =
   ClusterState { role    = r,
                  logData = Log { currentIndex = cur_idx+1,
                                  entries = entry:old_entries
                            },
                  peerNodes = p
   }

write_entry  :: ClusterState -> DB.DB -> IO ()
write_entry st db =
  DB.put db def_write "test" "stuff"
  where log_data  = logData st
        index     = currentIndex log_data
        (entry:_) = entries log_data

persistEntry :: ClusterState -> Process ClusterState
persistEntry st =
  do
    --with_database (write_entry st)
    return st

processAppendEntry :: LogEntry -> TheClusterState Process ()
processAppendEntry entry =
   do
      st <- ST.get
      ST.put $ appendEntry st entry


processRpc :: Rpc -> TheClusterState Process ()
processRpc (AppendEntries entry) =
   processAppendEntry entry

processRpc RequestVote = undefined

-- Check and see if we have an RPC. If so, process it.
check_rpc            :: Maybe Rpc -> TheClusterState Process ()
check_rpc Nothing    = return ()
check_rpc (Just rpc) = processRpc rpc

listen_loop         :: Backend -> TheClusterState Process ()
listen_loop backend =
   do
     evaluate_peers backend
     msg <- ST.lift $ expectTimeout 500
     check_rpc msg
     listen_loop backend

evaluate_peers :: Backend -> TheClusterState Process ()
evaluate_peers backend =
  do
   -- First evaluate presence on the peer network.
   peers <- liftIO $ findPeers backend cluster_announce_wait

   -- Dump the list
   liftIO $ print peers

   -- Get the current cluster state and update it's peer information.
   cluster_state <- ST.get
   ST.put $ update_peers cluster_state peers

update_peers :: ClusterState -> [NodeId] -> ClusterState
update_peers cluster_state new_peers =
  ClusterState r l new_peers
  where r = role cluster_state
        l = logData cluster_state

process :: String -> String -> IO ()
process address port =
   do
      backend <- initializeBackend address port initRemoteTable
      node    <- newLocalNode backend

      _ <- forkProcess node $ do
         my_pid <- spawnLocal $ ST.evalStateT (listen_loop backend) start
         liftIO $ print my_pid
         return ()
      return ()
