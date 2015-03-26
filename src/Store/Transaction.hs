module Transaction where

-- System modules
import           Data.Int            (Int64)

-- Library modules
import qualified Store.RowColumn             as RC

type TransactionId = Int64

data Transaction = Transaction {
  txid :: TransactionId, -- ^ The id of this transaction
  rows :: RC.RowColumn   -- ^ The rows inserted or updated by this transaction
}
