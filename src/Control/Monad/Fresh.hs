{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Fresh
  ( FreshT(..)
  , Fresh
  , runFreshT
  , runFresh
  , MonadFresh
  , fresh
  ) where

import           Protolude

import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict (WriterT)
import           Control.Monad.RWS.Strict (RWST)

newtype FreshT n m a = FreshT (StateT n m a)
  deriving (Functor, Applicative, Monad, MonadState n, MonadTrans)

type Fresh n = FreshT n Identity

runFreshT :: (Monad m, Num n) => FreshT n m a -> m a
runFreshT (FreshT st) = evalStateT st (fromInteger 0)

runFresh :: Num n => Fresh n a -> a
runFresh = runIdentity . runFreshT

class (Num n, Monad m) => MonadFresh n m where
  fresh :: m n

instance (Num n, Monad m) => MonadFresh n (FreshT n m) where
  fresh = do
    n <- get
    put (n + 1)
    pure n

instance MonadFresh n m => MonadFresh n (StateT s m) where
  fresh = lift fresh

instance MonadFresh n m => MonadFresh n (ExceptT e m) where
  fresh = lift fresh

instance MonadFresh n m => MonadFresh n (ReaderT r m) where
  fresh = lift fresh

instance (Monoid w, MonadFresh n m) => MonadFresh n (WriterT w m) where
  fresh = lift fresh

instance (Monoid w, MonadFresh n m) => MonadFresh n (RWST r w s m) where
  fresh = lift fresh

