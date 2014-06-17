module Compute.Query where

{-

    This module takes care of strategic processing of queries. We leave the tactical processing to lower-level components.
In particular, this module will distribute a query across the cluster. It is important to note that this component
does not figure out the tactical part of joins either.

    While it does retrieve the current cluster configuration from other components, it does not get directly involved
in node to node conversations. Each node that processes the query is provided with the current hash layout of nodes.
This allows the node to perform row-level fetches for join conditions.

-}
