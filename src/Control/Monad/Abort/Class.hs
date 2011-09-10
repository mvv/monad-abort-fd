{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Abort.Class (
    MonadAbort(..),
    MonadRecover(..),
    onError,
    onError_,
    ignore
  ) where

import Prelude hiding (catch)
import Data.Monoid
import Control.Exception (SomeException, throwIO, catch)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Control.Monad.RWS (MonadRWS)
import qualified Control.Monad.RWS.Lazy as L
import qualified Control.Monad.RWS.Strict as S
import Control.Monad.Trans.Abort (AbortT(..))
import qualified Control.Monad.Trans.Abort as A

class Monad μ ⇒ MonadAbort e μ | μ → e where
  abort ∷ e → μ α

class MonadAbort e μ ⇒ MonadRecover e μ | μ → e where
  recover ∷ μ α → (e → μ α) → μ α

onError ∷ MonadRecover e μ ⇒ μ α → (e → μ β) → μ α
onError m h = recover m (\e → h e >> abort e)

onError_ ∷ MonadRecover e μ ⇒ μ α → μ β → μ α
onError_ m = onError m . const

ignore ∷ MonadRecover e μ ⇒ μ α → μ ()
ignore m = recover (m >> return ()) (const $ return ())

instance Monad μ ⇒ MonadAbort e (AbortT e μ) where
  abort = A.abort

instance Monad μ ⇒ MonadRecover e (AbortT e μ) where
  recover = A.recover

instance MonadAbort SomeException IO where
  abort = throwIO

instance MonadRecover SomeException IO where
  recover = catch

instance Monad μ ⇒ MonadError e (AbortT e μ) where
  throwError = A.abort
  catchError = A.recover

instance MonadCont μ ⇒ MonadCont (AbortT e μ) where
  callCC k = AbortT $ callCC $ \f → runAbortT $ k (lift . f . Right) 

instance MonadReader r μ ⇒ MonadReader r (AbortT e μ) where
  ask = lift ask
  local f = AbortT . local f . runAbortT

instance MonadState s μ ⇒ MonadState s (AbortT e μ) where
  get = lift get
  put = lift . put

instance MonadWriter w μ ⇒ MonadWriter w (AbortT e μ) where
  tell = lift . tell
  listen m = AbortT $ do
    (lr, w) ← listen $ runAbortT m
    return $! fmap (, w) lr
  pass m = AbortT $ pass $ do
    lr ← runAbortT m
    return $! either ((, id) . Left) (\(r, f) → (Right r, f)) lr

instance MonadRWS r w s μ ⇒ MonadRWS r w s (AbortT e μ)

instance MonadAbort e μ ⇒ MonadAbort e (IdentityT μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (IdentityT μ) where
  recover m h = IdentityT $ runIdentityT m `recover` (runIdentityT . h)

instance MonadAbort e μ ⇒ MonadAbort e (ContT r μ) where
  abort = lift . abort

instance MonadAbort e μ ⇒ MonadAbort e (MaybeT μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (MaybeT μ) where
  recover m h = MaybeT $ runMaybeT m `recover` (runMaybeT . h)

instance MonadAbort e μ ⇒ MonadAbort e (ListT μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (ListT μ) where
  recover m h = ListT $ runListT m `recover` (runListT . h)

instance MonadAbort e μ ⇒ MonadAbort e (ReaderT r μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (ReaderT r μ) where
  recover m h = ReaderT $ \r → runReaderT m r `recover` ((`runReaderT` r) . h)

instance MonadAbort e μ ⇒ MonadAbort e (L.StateT s μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (L.StateT s μ) where
  recover m h = L.StateT $ \s →
    L.runStateT m s `recover` ((`L.runStateT` s) . h)

instance MonadAbort e μ ⇒ MonadAbort e (S.StateT s μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (S.StateT s μ) where
  recover m h = S.StateT $ \s →
    S.runStateT m s `recover` ((`S.runStateT` s) . h)

instance (MonadAbort e μ, Monoid w) ⇒ MonadAbort e (L.WriterT w μ) where
  abort = lift . abort

instance (MonadRecover e μ, Monoid w) ⇒ MonadRecover e (L.WriterT w μ) where
  recover m h = L.WriterT $ L.runWriterT m `recover` (L.runWriterT . h)

instance (MonadAbort e μ, Monoid w) ⇒ MonadAbort e (S.WriterT w μ) where
  abort = lift . abort

instance (MonadRecover e μ, Monoid w) ⇒ MonadRecover e (S.WriterT w μ) where
  recover m h = S.WriterT $ S.runWriterT m `recover` (S.runWriterT . h)

instance (MonadAbort e μ, Monoid w) ⇒ MonadAbort e (L.RWST r w s μ) where
  abort = lift . abort

instance (MonadRecover e μ, Monoid w) ⇒ MonadRecover e (L.RWST r w s μ) where
  recover m h = L.RWST $ \r s →
    L.runRWST m r s `recover` (\e → L.runRWST (h e) r s)

instance (MonadAbort e μ, Monoid w) ⇒ MonadAbort e (S.RWST r w s μ) where
  abort = lift . abort

instance (MonadRecover e μ, Monoid w) ⇒ MonadRecover e (S.RWST r w s μ) where
  recover m h = S.RWST $ \r s →
    S.runRWST m r s `recover` (\e → S.runRWST (h e) r s)

