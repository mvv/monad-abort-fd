{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Exception (
    exception,
    evaluate,
    throw,
    catch,
    catchJust,
    handle,
    handleJust,
    Handler(..),
    catches,
    try,
    tryJust,
    onException,
    onExceptions,
    MonadFinally(..),
    onEscape,
    MonadMask(..),
    mask,
    mask_,
    uninterruptibleMask,
    uninterruptibleMask_,
    bracket,
    bracket_,
    bracketOnEscape,
    bracketOnError,
    module Control.Exception
  ) where

import Prelude hiding (catch)
import Data.Monoid
import Data.Default
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Abort
import Control.Monad.Trans.Finish
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Control
import Control.Exception hiding (
  evaluate, throw, throwIO, catch, catchJust, handle, handleJust,
  Handler(..), catches, try, tryJust, finally, onException,
  block, unblock, blocked, getMaskingState, mask, mask_, uninterruptibleMask,
  uninterruptibleMask_, bracket, bracket_, bracketOnError)
import qualified Control.Exception as E
import GHC.Base (maskAsyncExceptions#, maskUninterruptible#,
                 unmaskAsyncExceptions#)
import GHC.IO (IO(..))

exception ∷ Exception e ⇒ e → α
exception = E.throw

evaluate ∷ MonadBase μ IO ⇒ α → μ α
evaluate = liftBase . E.evaluate

throw ∷ (MonadAbort SomeException μ, Exception e) ⇒ e → μ α
throw = abort . toException

catch ∷ (MonadRecover SomeException μ, Exception e) ⇒ μ α → (e → μ α) → μ α
catch m h = recover m $ \e → maybe (throw e) h (fromException e)

catchJust ∷ (MonadRecover SomeException μ, Exception e)
          ⇒ (e → Maybe β) → μ α → (β → μ α) → μ α
catchJust f m h = catch m $ \e → maybe (throw e) h $ f e

handle ∷ (MonadRecover SomeException μ, Exception e) ⇒ (e → μ α) → μ α → μ α
handle = flip catch

handleJust ∷ (MonadRecover SomeException μ, Exception e)
           ⇒ (e → Maybe β) → (β → μ α) → μ α → μ α
handleJust = flip . catchJust

data Handler μ α = ∀ e . Exception e ⇒ Handler (e → μ α)

catches ∷ MonadRecover SomeException μ ⇒ μ α → [Handler μ α] → μ α
catches m = recover m . hl
  where hl [] e = abort e
        hl (Handler h : hs) e = maybe (hl hs e) h $ fromException e

try ∷ (MonadRecover SomeException μ, Exception e) ⇒ μ α → μ (Either e α)
try m = catch (m >>= return . Right) (return . Left)

tryJust ∷ (MonadRecover SomeException μ, Exception e)
        ⇒ (e → Maybe β) → μ α → μ (Either β α)
tryJust f m = catch (m >>= return . Right) $ \e →
                maybe (throw e) (return . Left) $ f e

onException ∷ (MonadRecover SomeException μ, Exception e)
            ⇒ μ α → (e → μ β) → μ α
onException m h = catch m (\e → h e >> throw e)

onExceptions ∷ MonadRecover SomeException μ
             ⇒ μ α → [Handler μ β] → μ α
onExceptions m = recover m . hl
  where hl [] e = abort e
        hl (Handler h : hs) e =
          maybe (hl hs e) (\e' → h e' >> abort e) $ fromException e

class (Applicative μ, Monad μ) ⇒ MonadFinally μ where
  finally' ∷ μ α → (Maybe α → μ β) → μ (α, β)
  finally  ∷ μ α → μ β → μ α
  finally m = fmap fst . finally' m . const

instance MonadFinally Identity where
  finally' m f = do
    a ← m
    return (a, runIdentity $ f $ Just a)

instance MonadFinally IO where
  finally' m f = E.mask $ \restore → do
    a ← restore m `E.onException` f Nothing
    b ← f $ Just a
    return (a, b)

instance MonadFinally μ ⇒ MonadFinally (MaybeT μ) where
  finally' m f = MaybeT $ do
    ~(mr, fr) ← finally' (runMaybeT m) $ \mbr →
      runMaybeT $ f $ case mbr of
        Just (Just a) → Just a
        _             → Nothing
    return $ (,) <$> mr <*> fr

instance MonadFinally μ ⇒ MonadFinally (ListT μ) where
  finally' m f = ListT $ do
    ~(mrs, frss) ← finally' (runListT m) $ \mbr → case mbr of
      Just rs@(_ : _) → forM rs $ runListT . f . Just
      _ → fmap pure $ runListT $ f Nothing
    return $ zip mrs frss >>= \(mr, frs) → zip (repeat mr) frs

instance MonadFinally μ ⇒ MonadFinally (AbortT e μ) where
  finally' m f = AbortT $ do
    ~(mr, fr) ← finally' (runAbortT m) $ \mbr →
      runAbortT $ f $ case mbr of
        Just (Right a) → Just a
        _              → Nothing
    return $ (,) <$> mr <*> fr

instance MonadFinally μ ⇒ MonadFinally (FinishT β μ) where
  finally' m f = FinishT $ do
    ~(mr, fr) ← finally' (runFinishT m) $ \mbr →
      runFinishT $ f $ case mbr of
        Just (Right a) → Just a
        _              → Nothing
    return $ (,) <$> mr <*> fr

instance (MonadFinally μ, Error e) ⇒ MonadFinally (ErrorT e μ) where
  finally' m f = ErrorT $ do
    ~(mr, fr) ← finally' (runErrorT m) $ \mbr →
      runErrorT $ f $ case mbr of
        Just (Right a) → Just a
        _              → Nothing
    return $ (,) <$> mr <*> fr

instance MonadFinally μ ⇒ MonadFinally (ReaderT r μ) where
  finally' m f = ReaderT $ \r →
    finally' (runReaderT m r) ((`runReaderT` r) . f)

instance MonadFinally μ ⇒ MonadFinally (L.StateT s μ) where
  finally' m f = L.StateT $ \s → do
    ~(~(mr, _), ~(fr, s'')) ← finally' (L.runStateT m s) $ \mbr → do
      let ~(a, s') = case mbr of
             Just ~(x, t) → (Just x, t)
             Nothing      → (Nothing, s)
      L.runStateT (f a) s'
    return ((mr, fr), s'')

instance MonadFinally μ ⇒ MonadFinally (S.StateT s μ) where
  finally' m f = S.StateT $ \s → do
    ((mr, _), (fr, s'')) ← finally' (S.runStateT m s) $ \mbr → case mbr of
      Just (a, s') → S.runStateT (f $ Just a) s'
      Nothing      → S.runStateT (f Nothing) s
    return ((mr, fr), s'')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (L.WriterT w μ) where
  finally' m f = L.WriterT $ do
    ~(~(mr, w), ~(fr, w')) ← finally' (L.runWriterT m) $
      L.runWriterT . f . fmap fst
    return ((mr, fr), w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (S.WriterT w μ) where
  finally' m f = S.WriterT $ do
    ((mr, w), (fr, w')) ← finally' (S.runWriterT m) $ \mbr → case mbr of
      Just (a, _) → S.runWriterT $ f $ Just a
      Nothing     → S.runWriterT $ f Nothing
    return ((mr, fr), w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (L.RWST r w s μ) where
  finally' m f = L.RWST $ \r s → do
    ~(~(mr, _, w), ~(fr, s'', w')) ← finally' (L.runRWST m r s) $ \mbr → do
      let ~(a, s') = case mbr of
             Just ~(x, t, _) → (Just x, t)
             Nothing         → (Nothing, s)
      L.runRWST (f a) r s'
    return ((mr, fr), s'', w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (S.RWST r w s μ) where
  finally' m f = S.RWST $ \r s → do
    ((mr, _, w), (fr, s'', w')) ← finally' (S.runRWST m r s) $ \mbr →
      case mbr of
        Just (a, s', _) → S.runRWST (f $ Just a) r s'
        Nothing         → S.runRWST (f Nothing) r s
    return ((mr, fr), s'', w `mappend` w')

onEscape ∷ MonadFinally μ ⇒ μ α → μ β → μ α
onEscape m f = fmap fst $ finally' m $ maybe (() <$ f) (const $ return ())

deriving instance Ord MaskingState
deriving instance Enum MaskingState
deriving instance Bounded MaskingState

instance Default MaskingState where
  def = MaskedInterruptible

class (Applicative μ, Monad μ, Ord m, Bounded m, Default m)
      ⇒ MonadMask m μ | μ → m where
  getMaskingState ∷ μ m
  setMaskingState ∷ m → μ α → μ α

instance MonadMask () Identity where
  getMaskingState = return ()
  setMaskingState = const id

instance MonadMask MaskingState IO where
  getMaskingState = E.getMaskingState
  setMaskingState Unmasked (IO io) = IO $ unmaskAsyncExceptions# io
  setMaskingState MaskedInterruptible (IO io) = IO $ maskAsyncExceptions# io
  setMaskingState MaskedUninterruptible (IO io) = IO $ maskUninterruptible# io

liftSetMaskingState ∷ (MonadTransControl t, MonadMask m μ, Monad (t μ))
                    ⇒ m → t μ α → t μ α
liftSetMaskingState ms m = control $ \run → setMaskingState ms (run m)
{-# INLINE liftSetMaskingState #-}

instance MonadMask m μ ⇒ MonadMask m (MaybeT μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (ListT μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (AbortT e μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (FinishT β μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance (MonadMask m μ, Error e) ⇒ MonadMask m (ErrorT e μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (ReaderT r μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (L.StateT s μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance MonadMask m μ ⇒ MonadMask m (S.StateT s μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance (MonadMask m μ, Monoid w) ⇒ MonadMask m (L.WriterT w μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance (MonadMask m μ, Monoid w) ⇒ MonadMask m (S.WriterT w μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance (MonadMask m μ, Monoid w) ⇒ MonadMask m (L.RWST r w s μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

instance (MonadMask m μ, Monoid w) ⇒ MonadMask m (S.RWST r w s μ) where
  getMaskingState = lift getMaskingState
  setMaskingState = liftSetMaskingState

withMaskingState ∷ MonadMask m μ
                 ⇒ m → ((∀ η β . MonadMask m η ⇒ η β → η β) → μ α) → μ α
withMaskingState ms' m = do
  ms ← getMaskingState
  if ms' > ms
    then setMaskingState ms' $ m $ setMaskingState ms
    else m id

withMaskingState_ ∷ MonadMask m μ ⇒ m → μ α → μ α
withMaskingState_ m = withMaskingState m . const

mask ∷ MonadMask m μ ⇒ ((∀ η β . MonadMask m η ⇒ η β → η β) → μ α) → μ α
mask = withMaskingState def
 
mask_ ∷ MonadMask m μ ⇒ μ α → μ α
mask_ = withMaskingState_ def

uninterruptibleMask ∷ MonadMask MaskingState μ
                    ⇒ ((∀ η β . MonadMask MaskingState η ⇒ η β → η β) → μ α)
                    → μ α
uninterruptibleMask = withMaskingState MaskedUninterruptible

uninterruptibleMask_ ∷ MonadMask MaskingState μ ⇒ μ α → μ α
uninterruptibleMask_ = withMaskingState_ MaskedUninterruptible

bracket ∷ (MonadFinally μ, MonadMask m μ)
        ⇒ μ α → (α → μ β) → (α → μ γ) → μ γ
bracket acq release m = mask $ \restore → do
  a ← acq
  finally (restore $ m a) (release a)

bracket_ ∷ (MonadFinally μ, MonadMask m μ) ⇒ μ α → μ β → μ γ → μ γ
bracket_ acq release m = bracket acq (const release) (const m)

bracketOnEscape ∷ (MonadFinally μ, MonadMask m μ)
                ⇒ μ α → (α → μ β) → (α → μ γ) → μ γ
bracketOnEscape acq release m = mask $ \restore → do
  a ← acq
  restore (m a) `onEscape` release a

bracketOnError ∷ (MonadRecover e μ, MonadMask m μ)
               ⇒ μ α → (α → μ β) → (α → μ γ) → μ γ
bracketOnError acq release m = mask $ \restore → do
  a ← acq
  r ← restore (m a) `onError_` release a
  r <$ release a

