{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
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
  , MonadUArray(..)
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (liftM)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Array.Base
import Data.Array.IO
import Data.Array.ST
import Data.Array.MArray.Extras
import Foreign.Ptr
import Foreign.StablePtr
import Data.Int
import Data.Word

-- | Arr m serves as a canonical choice of boxed MArray
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

instance (MonadTrans t, Monad (t m), MonadArray m) => MonadArray (t m) where
  newtype Arr (t m) i e = ArrT { runArrT :: Arr m i e } 

  getBoundsM                = lift . getBounds . runArrT
  getNumElementsM           = lift . getNumElements . runArrT
  newArrayM bs e            = lift $ ArrT `liftM` newArray bs e
  newArrayM_ bs             = lift $ ArrT `liftM` newArray_ bs
  unsafeNewArrayM_ bs       = lift $ ArrT `liftM` unsafeNewArray_ bs
  unsafeReadM (ArrT a) i    = lift $ unsafeRead a i
  unsafeWriteM (ArrT a) i e = lift $ unsafeWrite a i e

-- | UArr m provides unboxed arrays, and can be used on the primitive data types:
-- 
-- 'Bool', 'Char', 'Int', 'Word', 'Double', 'Float', 'Int8', 'Int16', 'Int32', 'Int64', 'Word8',
-- 'Word16', 'Word32', and 'Word64'
-- 
-- It can be used via 'MArray1' to store values of types @'StablePtr' a@, @'FunPtr' a@ and @'Ptr a'@ as well.

class 
  ( MonadArray m 
  , MArray (UArr m) Bool m
  , MArray (UArr m) Char m
  , MArray (UArr m) Int  m
  , MArray (UArr m) Word m
  , MArray (UArr m) Double m
  , MArray (UArr m) Float m
  , MArray (UArr m) Int8 m
  , MArray (UArr m) Int16 m
  , MArray (UArr m) Int32 m
  , MArray (UArr m) Int64 m
  , MArray (UArr m) Word8 m
  , MArray (UArr m) Word16 m
  , MArray (UArr m) Word32 m
  , MArray (UArr m) Word64 m
  , MArray1 (UArr m) StablePtr m
  , MArray1 (UArr m) FunPtr m
  , MArray1 (UArr m) Ptr m 
  ) => MonadUArray m where
  data UArr m :: * -> * -> *

instance MArray IOUArray e IO => MArray (UArr IO) e IO where
  getBounds                  = getBounds . runUArrIO
  getNumElements             = getNumElements . runUArrIO
  newArray bs e              = UArrIO <$> newArray bs e
  newArray_ bs               = UArrIO <$> newArray_ bs
  unsafeNewArray_ bs         = UArrIO <$> unsafeNewArray_ bs
  unsafeRead (UArrIO a) i    = unsafeRead a i
  unsafeWrite (UArrIO a) i e = unsafeWrite a i e

instance MArray1 IOUArray e IO => MArray1 (UArr IO) e IO where
  getBounds1                  = getBounds1 . runUArrIO
  getNumElements1             = getNumElements1 . runUArrIO
  newArray1 bs e              = UArrIO <$> newArray1 bs e
  newArray1_ bs               = UArrIO <$> newArray1_ bs
  unsafeNewArray1_ bs         = UArrIO <$> unsafeNewArray1_ bs
  unsafeRead1 (UArrIO a) i    = unsafeRead1 a i
  unsafeWrite1 (UArrIO a) i e = unsafeWrite1 a i e
  
instance MonadUArray IO where
  newtype UArr IO i e = UArrIO { runUArrIO :: IOUArray i e } 

instance MArray (STUArray s) e (ST s) => MArray (UArr (ST s)) e (ST s) where
  getBounds                  = getBounds . runUArrST
  getNumElements             = getNumElements . runUArrST
  newArray bs e              = UArrST <$> newArray bs e
  newArray_ bs               = UArrST <$> newArray_ bs
  unsafeNewArray_ bs         = UArrST <$> unsafeNewArray_ bs
  unsafeRead (UArrST a) i    = unsafeRead a i
  unsafeWrite (UArrST a) i e = unsafeWrite a i e

instance MArray1 (STUArray s) e (ST s) => MArray1 (UArr (ST s)) e (ST s) where
  getBounds1                  = getBounds1 . runUArrST
  getNumElements1             = getNumElements1 . runUArrST
  newArray1 bs e              = UArrST <$> newArray1 bs e
  newArray1_ bs               = UArrST <$> newArray1_ bs
  unsafeNewArray1_ bs         = UArrST <$> unsafeNewArray1_ bs
  unsafeRead1 (UArrST a) i    = unsafeRead1 a i
  unsafeWrite1 (UArrST a) i e = unsafeWrite1 a i e
  
instance MonadUArray (ST s) where
  newtype UArr (ST s) i e = UArrST { runUArrST :: STUArray s i e } 
  
instance (MonadTrans t, Monad (t m), MonadUArray m, MArray (UArr m) e m) => MArray (UArr (t m)) e (t m) where
  getBounds                  = lift . getBounds . runUArrT
  getNumElements             = lift . getNumElements . runUArrT
  newArray bs e              = lift $ UArrT `liftM` newArray bs e
  newArray_ bs               = lift $ UArrT `liftM` newArray_ bs
  unsafeNewArray_ bs         = lift $ UArrT `liftM` unsafeNewArray_ bs
  unsafeRead (UArrT a) i     = lift $ unsafeRead a i
  unsafeWrite (UArrT a) i e  = lift $ unsafeWrite a i e

instance (MonadTrans t, Monad (t m), MonadUArray m, MArray1 (UArr m) f m) => MArray1 (UArr (t m)) f (t m) where
  getBounds1                  = lift . getBounds1 . runUArrT
  getNumElements1             = lift . getNumElements1 . runUArrT
  newArray1 bs e              = lift $ UArrT `liftM` newArray1 bs e
  newArray1_ bs               = lift $ UArrT `liftM` newArray1_ bs
  unsafeNewArray1_ bs         = lift $ UArrT `liftM` unsafeNewArray1_ bs
  unsafeRead1 (UArrT a) i     = lift $ unsafeRead1 a i
  unsafeWrite1 (UArrT a) i e  = lift $ unsafeWrite1 a i e
  
instance (MonadTrans t, Monad (t m), MonadUArray m) => MonadUArray (t m) where
  newtype UArr (t m) i e = UArrT { runUArrT :: UArr m i e } 
