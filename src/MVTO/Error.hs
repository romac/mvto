
module MVTO.Error where

import           Protolude

import           MVTO.Transaction
import           MVTO.Version

data MVTOError
  = KeyAlreadyExists Key
  | KeyDoesNotExists Key
  | TransactionDoesNotExists Int
  | TransactionTooOld Transaction RTS WTS
  | TransactionNotRunning Transaction
  | NoVersionWithKeyBefore Key TransactionId
  deriving (Eq, Ord, Show, Read)

