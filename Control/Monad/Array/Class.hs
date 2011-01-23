{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Array.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, MPTCs
--
----------------------------------------------------------------------------
module Control.Monad.Array.Class
  ( MonadArray(..)
  , MonadArrayTrans(..)
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (liftM)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Data.Array.Base
import Data.Array.IO
import Data.Array.ST
import Data.Monoid

class Monad m => MonadArray m where
  data Arr m :: * -> * -> *
  getBoundsM       :: Ix i => Arr m i e -> m (i, i)
  getNumElementsM  :: Ix i => Arr m i e -> m Int
  newArrayM        :: Ix i => (i, i) -> e -> m (Arr m i e)
  newArrayM_       :: Ix i => (i, i) -> m (Arr m i e)
  unsafeNewArrayM_ :: Ix i => (i, i) -> m (Arr m i e)
  unsafeReadM      :: Ix i => Arr m i e -> Int -> m e
  unsafeWriteM     :: Ix i => Arr m i e -> Int -> e -> m ()

instance MonadArray m => MArray (Arr m) e m where
  getBounds = getBoundsM
  getNumElements = getNumElementsM
  newArray = newArrayM
  unsafeNewArray_ = unsafeNewArrayM_
  newArray_ = newArrayM_
  unsafeRead = unsafeReadM
  unsafeWrite = unsafeWriteM

class MonadArrayTrans t where
  liftArr  :: MonadArray m => Arr m i e -> Arr (t m) i e
  lowerArr :: MonadArray m => Arr (t m) i e -> Arr m i e

instance MonadArray IO where
  newtype Arr IO i e = ArrIO { runArrIO :: IOArray i e } 

  getBoundsM                 = getBounds . runArrIO 
  getNumElementsM            = getNumElements . runArrIO
  newArrayM bs e             = ArrIO <$> newArray bs e
  newArrayM_ bs              = ArrIO <$> newArray_ bs
  unsafeNewArrayM_ bs        = ArrIO <$> unsafeNewArray_ bs
  unsafeReadM (ArrIO a) i    = unsafeRead a i
  unsafeWriteM (ArrIO a) i e = unsafeWrite a i e

instance MonadArray (ST s) where
  newtype Arr (ST s) i e = ArrST { runArrST :: STArray s i e } 

  getBoundsM                 = getBounds . runArrST 
  getNumElementsM            = getNumElements . runArrST
  newArrayM bs e             = ArrST <$> newArray bs e
  newArrayM_ bs              = ArrST <$> newArray_ bs
  unsafeNewArrayM_ bs        = ArrST <$> unsafeNewArray_ bs
  unsafeReadM (ArrST a) i    = unsafeRead a i
  unsafeWriteM (ArrST a) i e = unsafeWrite a i e

instance MonadArray STM where
  newtype Arr STM i e = ArrSTM { runArrSTM :: TArray i e }

  getBoundsM                  = getBounds . runArrSTM
  getNumElementsM             = getNumElements . runArrSTM
  newArrayM bs e              = ArrSTM <$> newArray bs e
  newArrayM_ bs               = ArrSTM <$> newArray_ bs
  unsafeNewArrayM_ bs         = ArrSTM <$> unsafeNewArray_ bs
  unsafeReadM (ArrSTM a) i    = unsafeRead a i
  unsafeWriteM (ArrSTM a) i e = unsafeWrite a i e

instance MonadArray m => MonadArray (ReaderT r m) where
  newtype Arr (ReaderT r m) i e = ArrReaderT { runArrReaderT :: Arr m i e }

  getBoundsM                      = lift . getBounds . runArrReaderT
  getNumElementsM                 = lift . getNumElements . runArrReaderT
  newArrayM bs e                  = lift $ ArrReaderT `liftM` newArray bs e
  newArrayM_ bs                   = lift $ ArrReaderT `liftM` newArray_ bs
  unsafeNewArrayM_ bs             = lift $ ArrReaderT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrReaderT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrReaderT a) i e = lift $ unsafeWrite a i e

instance MonadArrayTrans (ReaderT e) where
  liftArr = ArrReaderT
  lowerArr = runArrReaderT

instance (MonadArray m, Monoid w) => MonadArray (Strict.WriterT w m) where
  newtype Arr (Strict.WriterT w m) i e = ArrStrictWriterT { runArrStrictWriterT :: Arr m i e }

  getBoundsM                            = lift . getBounds . runArrStrictWriterT
  getNumElementsM                       = lift . getNumElements . runArrStrictWriterT
  newArrayM bs e                        = lift $ ArrStrictWriterT `liftM` newArray bs e
  newArrayM_ bs                         = lift $ ArrStrictWriterT `liftM` newArray_ bs
  unsafeNewArrayM_ bs                   = lift $ ArrStrictWriterT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrStrictWriterT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrStrictWriterT a) i e = lift $ unsafeWrite a i e

instance Monoid w => MonadArrayTrans (Strict.WriterT w) where
  liftArr = ArrStrictWriterT
  lowerArr = runArrStrictWriterT

instance (MonadArray m, Monoid w) => MonadArray (Lazy.WriterT w m) where
  newtype Arr (Lazy.WriterT w m) i e = ArrLazyWriterT { runArrLazyWriterT :: Arr m i e }

  getBoundsM                          = lift . getBounds . runArrLazyWriterT
  getNumElementsM                     = lift . getNumElements . runArrLazyWriterT
  newArrayM bs e                      = lift $ ArrLazyWriterT `liftM` newArray bs e
  newArrayM_ bs                       = lift $ ArrLazyWriterT `liftM` newArray_ bs
  unsafeNewArrayM_ bs                 = lift $ ArrLazyWriterT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrLazyWriterT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrLazyWriterT a) i e = lift $ unsafeWrite a i e

instance Monoid w => MonadArrayTrans (Lazy.WriterT w) where
  liftArr = ArrLazyWriterT
  lowerArr = runArrLazyWriterT

instance (MonadArray m, Monoid w) => MonadArray (Strict.RWST r w s m) where
  newtype Arr (Strict.RWST r w s m) i e = ArrStrictRWST { runArrStrictRWST :: Arr m i e }

  getBoundsM                         = lift . getBounds . runArrStrictRWST
  getNumElementsM                    = lift . getNumElements . runArrStrictRWST
  newArrayM bs e                     = lift $ ArrStrictRWST `liftM` newArray bs e
  newArrayM_ bs                      = lift $ ArrStrictRWST `liftM` newArray_ bs
  unsafeNewArrayM_ bs                = lift $ ArrStrictRWST `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrStrictRWST a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrStrictRWST a) i e = lift $ unsafeWrite a i e

instance Monoid w => MonadArrayTrans (Strict.RWST r w s) where
  liftArr = ArrStrictRWST
  lowerArr = runArrStrictRWST

instance (MonadArray m, Monoid w) => MonadArray (Lazy.RWST r w s m) where
  newtype Arr (Lazy.RWST r w s m) i e = ArrLazyRWST { runArrLazyRWST :: Arr m i e }

  getBoundsM                       = lift . getBounds . runArrLazyRWST
  getNumElementsM                  = lift . getNumElements . runArrLazyRWST
  newArrayM bs e                   = lift $ ArrLazyRWST `liftM` newArray bs e
  newArrayM_ bs                    = lift $ ArrLazyRWST `liftM` newArray_ bs
  unsafeNewArrayM_ bs              = lift $ ArrLazyRWST `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrLazyRWST a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrLazyRWST a) i e = lift $ unsafeWrite a i e

instance Monoid w => MonadArrayTrans (Lazy.RWST r w s) where
  liftArr = ArrLazyRWST
  lowerArr = runArrLazyRWST

instance MonadArray m => MonadArray (Strict.StateT s m) where
  newtype Arr (Strict.StateT s m) i e = ArrStrictStateT { runArrStrictStateT :: Arr m i e }

  getBoundsM                           = lift . getBounds . runArrStrictStateT
  getNumElementsM                      = lift . getNumElements . runArrStrictStateT
  newArrayM bs e                       = lift $ ArrStrictStateT `liftM` newArray bs e
  newArrayM_ bs                        = lift $ ArrStrictStateT `liftM` newArray_ bs
  unsafeNewArrayM_ bs                  = lift $ ArrStrictStateT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrStrictStateT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrStrictStateT a) i e = lift $ unsafeWrite a i e

instance MonadArrayTrans (Strict.StateT s) where
  liftArr = ArrStrictStateT
  lowerArr = runArrStrictStateT

instance MonadArray m => MonadArray (Lazy.StateT s m) where
  newtype Arr (Lazy.StateT s m) i e = ArrLazyStateT { runArrLazyStateT :: Arr m i e }

  getBoundsM                         = lift . getBounds . runArrLazyStateT
  getNumElementsM                    = lift . getNumElements . runArrLazyStateT
  newArrayM bs e                     = lift $ ArrLazyStateT `liftM` newArray bs e
  newArrayM_ bs                      = lift $ ArrLazyStateT `liftM` newArray_ bs
  unsafeNewArrayM_ bs                = lift $ ArrLazyStateT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrLazyStateT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrLazyStateT a) i e = lift $ unsafeWrite a i e

instance MonadArrayTrans (Lazy.StateT s) where
  liftArr = ArrLazyStateT
  lowerArr = runArrLazyStateT

instance MonadArray m => MonadArray (MaybeT m) where
  newtype Arr (MaybeT m) i e = ArrMaybeT { runArrMaybeT :: Arr m i e }

  getBoundsM                     = lift . getBounds . runArrMaybeT
  getNumElementsM                = lift . getNumElements . runArrMaybeT
  newArrayM bs e                 = lift $ ArrMaybeT `liftM` newArray bs e
  newArrayM_ bs                  = lift $ ArrMaybeT `liftM` newArray_ bs
  unsafeNewArrayM_ bs            = lift $ ArrMaybeT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrMaybeT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrMaybeT a) i e = lift $ unsafeWrite a i e

instance MonadArrayTrans MaybeT where
  liftArr = ArrMaybeT
  lowerArr = runArrMaybeT

instance MonadArray m => MonadArray (IdentityT m) where
  newtype Arr (IdentityT m) i e = ArrIdentityT { runArrIdentityT :: Arr m i e }

  getBoundsM                        = lift . getBounds . runArrIdentityT
  getNumElementsM                   = lift . getNumElements . runArrIdentityT
  newArrayM bs e                    = lift $ ArrIdentityT `liftM` newArray bs e
  newArrayM_ bs                     = lift $ ArrIdentityT `liftM` newArray_ bs
  unsafeNewArrayM_ bs               = lift $ ArrIdentityT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrIdentityT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrIdentityT a) i e = lift $ unsafeWrite a i e

instance MonadArrayTrans IdentityT where
  liftArr = ArrIdentityT
  lowerArr = runArrIdentityT

instance (MonadArray m, Error x) => MonadArray (ErrorT x m) where
  newtype Arr (ErrorT x m) i e = ArrErrorT { runArrErrorT :: Arr m i e }

  getBoundsM                     = lift . getBounds . runArrErrorT
  getNumElementsM                = lift . getNumElements . runArrErrorT
  newArrayM bs e                 = lift $ ArrErrorT `liftM` newArray bs e
  newArrayM_ bs                  = lift $ ArrErrorT `liftM` newArray_ bs
  unsafeNewArrayM_ bs            = lift $ ArrErrorT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrErrorT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrErrorT a) i e = lift $ unsafeWrite a i e

instance Error x => MonadArrayTrans (ErrorT x) where
  liftArr = ArrErrorT
  lowerArr = runArrErrorT
