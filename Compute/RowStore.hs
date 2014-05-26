module Compute.RowStore where

import Control.Concurrent.CHP
import Control.Monad

import qualified Control.Concurrent.CHP.Common as CHP

import Compute

data EvaluateMsg = EvaluateMsg {
   out      :: Chanout String,
   query    :: Query,
   database :: Database
}

evaluate :: Query

{-
   Evaluates the predicate portion of queries written into
   this channel. Also processes join conditions. When a
   query is written in it, should also be accompanied by
   an output channel. Tuples which match the predicate are
   written to the output channel for further processing.
                                                            -}

processQuery :: Shared Chanin EvaluateMsg -> CHP ()
processQuery input = forever (
   do
      msg <- claim input readChannel
      writeChannel (out msg) "test"
   )
