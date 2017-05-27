
module MVTO.Transaction where

import           Control.Lens

data Status
  = Running
  | Waiting
  | Committed
  | Rollbacked
  deriving (Eq, Ord, Show, Read, Enum)

makePrisms ''Status

newtype Transaction
  = Transaction
  { _tId :: Int
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''Transaction

