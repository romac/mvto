{-# LANGUAGE StrictData #-}

module MVTO.MVTOState where

import           Protolude

import           Data.Map.Strict  (Map)
import           Data.Set         (Set)

import           Control.Lens

import           MVTO.Transaction
import           MVTO.Version

data MVTOState
  = MVTOState
  { _lastTransactionId :: TransactionId
  , _transactions      :: Set Transaction
  , _versions          :: Map Key [Version]
  }
  deriving (Eq, Ord, Show, Read)

makeClassy ''MVTOState

