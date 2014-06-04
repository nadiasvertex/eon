{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

module Compute.RowStore where

-- System modules
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (release)
import Data.Binary
import Data.Monoid
import Data.Typeable
import Database.LevelDB
import Generics.Deriving
import Network.Transport.TCP        (createTransport, defaultTCPParameters)

-- Eon modules
import Compute

data QueryMsg = QueryMsg {
   query  :: Query,
   sender :: ProcessId
} deriving (Typeable, Generic)

instance Binary QueryMsg

handleQuery :: QueryMsg -> Process ()
handleQuery msg =
   reply "received"
   where
      reply = send (sender msg)


initDatabase :: IO DB
initDatabase = runResourceT $
    --log_debug "reading database metadata"
   db <- open "metadata.db"
           defaultOptions { createIfMissing = True }

   table_names <- get db def "table_names"

   return db


dataProcessor :: IO ()
dataProcessor =
   do
      --log_debug "binding network transport"
      Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
      node            <- newLocalNode transport initRemoteTable

      db              <- initDatabase

      forkProcess node $
         do
            receiveWait [match handleQuery]
            return ()

      return ()

   where
      log_debug msg = say ("debug:" `mappend` msg)
