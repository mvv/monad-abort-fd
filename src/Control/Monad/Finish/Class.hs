{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Monad type class for short-cicuiting computation.
module Control.Monad.Finish.Class
  ( MonadFinish(..)
  ) where

import Data.Monoid (Monoid)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Except
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
import Control.Monad.Abort
import Control.Monad.Trans.Finish (FinishT(..))
import qualified Control.Monad.Trans.Finish as F

-- | Class of monads that support short-circuiting.
class Monad μ ⇒ MonadFinish f μ | μ → f where
  -- | Short-circuit the computation with the provided value.
  --
  -- @
  --     finish f >>= rest = finish f
  -- @
  finish ∷ f → μ α

instance Monad μ ⇒ MonadFinish f (FinishT f μ) where
  finish = F.finish

instance MonadCont μ ⇒ MonadCont (FinishT f μ) where
  callCC k = FinishT $ callCC $ \f → runFinishT $ k (lift . f . Right) 

instance MonadError e μ ⇒ MonadError e (FinishT f μ) where
  throwError = lift . throwError
  catchError m h = FinishT $ catchError (runFinishT m) (runFinishT . h)

instance MonadAbort e μ ⇒ MonadAbort e (FinishT f μ) where
  abort = lift . abort

instance MonadRecover e μ ⇒ MonadRecover e (FinishT f μ) where
  recover m h = FinishT $ recover (runFinishT m) (runFinishT . h)

instance MonadReader r μ ⇒ MonadReader r (FinishT f μ) where
  ask = lift ask
  local f = FinishT . local f . runFinishT

instance MonadState s μ ⇒ MonadState s (FinishT f μ) where
  get = lift get
  put = lift . put

instance MonadWriter w μ ⇒ MonadWriter w (FinishT f μ) where
  tell = lift . tell
  listen m = FinishT $ do
    (lr, w) ← listen $ runFinishT m
    return $! fmap (, w) lr
  pass m = FinishT $ pass $ do
    lr ← runFinishT m
    return $! either ((, id) . Left) (\(r, f) → (Right r, f)) lr

instance MonadRWS r w s μ ⇒ MonadRWS r w s (FinishT f μ)

instance MonadFinish f μ ⇒ MonadFinish f (IdentityT μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (ContT r μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (MaybeT μ) where
  finish = lift . finish

instance (MonadFinish f μ, Error e) ⇒ MonadFinish f (ErrorT e μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (ExceptT e μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (AbortT e μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (ListT μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (ReaderT r μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (L.StateT s μ) where
  finish = lift . finish

instance MonadFinish f μ ⇒ MonadFinish f (S.StateT s μ) where
  finish = lift . finish

instance (MonadFinish f μ, Monoid w) ⇒ MonadFinish f (L.WriterT w μ) where
  finish = lift . finish

instance (MonadFinish f μ, Monoid w) ⇒ MonadFinish f (S.WriterT w μ) where
  finish = lift . finish

instance (MonadFinish f μ, Monoid w) ⇒ MonadFinish f (L.RWST r w s μ) where
  finish = lift . finish

instance (MonadFinish f μ, Monoid w) ⇒ MonadFinish f (S.RWST r w s μ) where
  finish = lift . finish
