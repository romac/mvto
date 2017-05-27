
module MVTO.Version where

import           Control.Lens

import Data.GADT.Compare
import Data.GADT.Show
import Data.Dependent.Sum

data TSRead
data TSWrite

newtype TS a
  = TS Int
  deriving (Eq, Ord, Show, Read)

type RTS = TS TSRead
type WTS = TS TSWrite

newtype Key a
  = Key Int
  deriving (Eq, Ord, Show, Read, Functor)

data Version a
  = Version
  { _vKey   :: Key a
  , _vValue :: a
  , _vRTS   :: RTS
  , _vWTS   :: WTS
  }
  deriving (Eq, Ord, Show, Read, Functor)

makeLenses ''Version

