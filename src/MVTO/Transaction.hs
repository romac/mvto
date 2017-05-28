{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData             #-}

module MVTO.Transaction where

import           Protolude

import           Data.Coerce  (coerce)

import           Data.Set     (Set)
import qualified Data.Set     as Set

import           Control.Lens

import           MVTO.Lens
import           MVTO.Version

data Status
  = Running
  | Waiting
  | Committed
  | Rollbacked
  deriving (Eq, Ord, Show, Read, Enum)

running :: Prism' Status Status
running = prism identity $ \s -> case s of
  Running -> Right s
  Waiting -> Right s
  _       -> Left s

makePrisms ''Status

data TSTransaction

newtype TransactionId = TransactionId (TS TSTransaction)
  deriving (Eq, Ord, Show, Read)

idToRTS :: TransactionId -> RTS
idToRTS = coerce

idToWTS :: TransactionId -> WTS
idToWTS = coerce

data Transaction = Transaction
  { _transactionId          :: TransactionId
  , _transactionStatus      :: Status
  , _transactionDeps        :: Set Transaction
  , _transactionRevDeps     :: Set Transaction
  , _transactionOldVersions :: Set Version
  , _transactionWrites      :: [Version]
  } deriving (Eq, Ord, Show, Read)

makeClassy ''Transaction

makeFields ''Transaction

isRunning :: Transaction -> Bool
isRunning t = is running (t^.status)

hasEnded :: Transaction -> Bool
hasEnded = not . isRunning

newTransaction :: TransactionId -> Status -> Transaction
newTransaction id status = Transaction
  { _transactionId          = id
  , _transactionStatus      = status
  , _transactionDeps        = Set.empty
  , _transactionRevDeps     = Set.empty
  , _transactionOldVersions = Set.empty
  , _transactionWrites      = []
  }

