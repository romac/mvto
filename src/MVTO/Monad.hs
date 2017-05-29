{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MVTO.Monad where

import           Protolude           hiding ((&))

import           Control.Monad       (unless, when)
import           Control.Monad.Fresh (FreshT, MonadFresh, fresh)
-- import           Control.Monad.Trans.Class

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           Control.Lens

import           MVTO.Error
import           MVTO.MVTOState
import           MVTO.Transaction
import           MVTO.Version

newtype MVTOT m a =
  MVTOT (ExceptT MVTOError
          (StateT MVTOState
            (FreshT Int m))
              a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState MVTOState
    , MonadFresh Int
    , MonadError MVTOError
    ) -- , MonadTrans)

type MVTO = MVTOT Identity

freshId :: MonadFresh Int m => m TransactionId
freshId = TransactionId . TS <$> fresh

beginTransaction :: MVTO Transaction
beginTransaction = do
  id <- freshId
  let xact = newTransaction id Running
  lastTransactionId .= id
  transactions %= Set.insert xact
  pure xact

insert :: Transaction -> Key -> Int -> MVTO ()
insert xact key value = do
  checkRunning xact
  exists <- versions `uses` Map.member key
  when exists $ do
    rollback xact
    throwError (KeyAlreadyExists key)

  let v = Version key value (idToRTS (xact^.id)) (idToWTS (xact^.id))
  versions . at key .= Just [v]

read :: Transaction -> Key -> MVTO Int
read = undefined

write :: Transaction -> Key -> Int -> MVTO ()
write = undefined

commit :: Transaction -> MVTO ()
commit = undefined

rollback :: Transaction -> MVTO ()
rollback = undefined

checkExists :: Transaction -> MVTO ()
checkExists xact = do
  xacts <- use transactions
  unless (Set.member xact xacts) $
    throwError (TransactionNotRunning xact)

checkRunning :: Transaction -> MVTO ()
checkRunning xact = do
  checkExists xact
  unless (isRunning xact) $
    throwError (TransactionNotRunning xact)

