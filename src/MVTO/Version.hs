{-# LANGUAGE StrictData #-}

module MVTO.Version where

import           Protolude

import           Control.Lens

data TSRead
data TSWrite

newtype TS a =
  TS Int
  deriving (Eq, Ord, Show, Read)

type RTS = TS TSRead
type WTS = TS TSWrite

newtype Key =
  Key Int
  deriving (Eq, Ord, Show, Read)

data Version = Version
  { _vKey   :: Key
  , _vValue :: Int
  , _vRTS   :: RTS
  , _vWTS   :: WTS
  } deriving (Eq, Ord, Show, Read)

makeLenses ''Version
