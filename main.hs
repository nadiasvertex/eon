{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- System modules
import System.Environment
import System.Exit
import Control.Concurrent
import Control.Monad
import Data.List
import HFlags

-- Eon modules
import Compute
import Compute.RowStore
import QueryParser

import qualified Consensus as C

defineFlag "l:listen" "127.0.0.1:10001" "The address:port to bind to for the consensus network."
defineFlag "v:version" False "eon data sever 0.1"

-- Take an ip:port string and split it into two pieces.
get_address_args    :: String -> (String, String)
get_address_args arg = (addr, drop 1 port)
                   where (addr,port) = break (==':') arg

version = print "eon data server 0.1" >> exit
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

runLoop :: IO ()
runLoop = do
  threadDelay 1000000
  runLoop  
  
main :: IO ()
main = do
  s <- $initHFlags "eon v0.1"
  when (flags_version) version
  uncurry C.process $ get_address_args flags_listen
  runLoop
  --print (parseString (head args))
