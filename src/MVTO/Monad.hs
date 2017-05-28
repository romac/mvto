{-# LANGUAGE TypeApplications #-}

module MVTO.Monad where

import           Protolude                     hiding (State, get, put, state, throwError, (&))

import           Control.Monad                 (unless, when)

import           Control.Monad.Freer           (Eff, Member)
import           Control.Monad.Freer.Exception (Exc, throwError)
import           Control.Monad.Freer.Fresh     (Fresh, fresh)
import           Control.Monad.Freer.State     (State, get, put)

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Control.Lens

import           MVTO.Error
import           MVTO.Lens
import           MVTO.MVTOState
import           MVTO.Transaction
import           MVTO.Version

type MVTO = Eff '[State MVTOState, Exc MVTOError, Fresh]

getState :: MVTO MVTOState
getState = get

putState :: MVTOState -> MVTO ()
putState = put

freshId :: Member Fresh r => Eff r TransactionId
freshId = TransactionId . TS <$> fresh

beginTransaction :: MVTO Transaction
beginTransaction = do
  id <- freshId
  let xact = newTransaction id Running
  state <- getState
  put $ state & lastTransactionId .~ id
              & transactions %~ Set.insert xact
  pure xact

insert :: Transaction -> Key -> Int -> MVTO ()
insert xact key value = do
  checkRunning xact
  exists <- Map.member key <$> getState $. versions
  when exists $ do
    rollback xact
    throwError (KeyAlreadyExists key)

  let v = Version key value (idToRTS (xact^.id)) (idToWTS (xact^.id))
  state <- getState
  let vs = at key .~ Just [v] $ state^.versions
  putState $ state & versions .~ vs

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
  xacts <- getState $. transactions
  unless (Set.member xact xacts) (throwError (TransactionNotRunning xact))

checkRunning :: Transaction -> MVTO ()
checkRunning xact = do
  checkExists xact
  unless (isRunning xact) (throwError (TransactionNotRunning xact))

