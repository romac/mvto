
module MVTO.MVTOState where

import           GHC.Exts           (IsList)

import           Data.Set           (Set)
import qualified Data.Set           as Set

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import           Control.Lens

import           MVTO.Transaction
import           MVTO.Version

newtype GList f a = GList [f a]

data MVTOState
  = MVTOState
  { _newestXactId :: Int
  , _xacts        :: Set Transaction
  , _versions     :: DMap Key (GList Version)
  , _deps         :: Map Transaction Transaction
  , _revDeps      :: Map Transaction Transaction
  , _oldVersions  :: DMap Transaction Version
  }

makeLenses ''MVTOState

