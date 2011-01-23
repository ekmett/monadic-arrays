{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.MArray.Extras
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  type families, MPTCs
--
-- Provides missing MArray instances for monad transformers and a higher
-- order MArray class.
----------------------------------------------------------------------------
module Data.Array.MArray.Extras
  ( MArray1(..)
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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.List
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
import Foreign.Ptr
import Foreign.StablePtr

-- We could fix a missing fundep in the class hierarchy by adding:
-- class MArray1 a f m | a f -> m where
class Monad m => MArray1 a f m where
  getBounds1       :: Ix i => a i (f e) -> m (i, i)
  getNumElements1  :: Ix i => a i (f e) -> m Int
  newArray1        :: Ix i => (i, i) -> f e -> m (a i (f e))
  newArray1_       :: Ix i => (i, i) -> m (a i (f e))
  unsafeNewArray1_ :: Ix i => (i, i) -> m (a i (f e))
  unsafeRead1      :: Ix i => a i (f e) -> Int -> m (f e)
  unsafeWrite1     :: Ix i => a i (f e) -> Int -> f e -> m ()

instance MArray1 IOUArray Ptr IO where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite

instance MArray1 IOUArray StablePtr IO where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite

instance MArray1 IOUArray FunPtr IO where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite

instance MArray1 (STUArray s) Ptr (ST s) where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite

instance MArray1 (STUArray s) StablePtr (ST s) where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite

instance MArray1 (STUArray s) FunPtr (ST s) where
  getBounds1 = getBounds
  getNumElements1 = getNumElements
  newArray1 = newArray
  newArray1_ = newArray_
  unsafeNewArray1_ = unsafeNewArray_
  unsafeRead1 = unsafeRead
  unsafeWrite1 = unsafeWrite


-- Monad transformer instances

instance MArray1 a e m => MArray1 a e (ContT r m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance (Error x, MArray1 a e m) => MArray1 a e (ErrorT x m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (ListT m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (MaybeT m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (IdentityT m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (ReaderT r m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance (MArray1 a e m, Monoid w) => MArray1 a e (Strict.WriterT w m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance (MArray1 a e m, Monoid w) => MArray1 a e (Lazy.WriterT w m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance (MArray1 a e m, Monoid w) => MArray1 a e (Strict.RWST r w s m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance (MArray1 a e m, Monoid w) => MArray1 a e (Lazy.RWST r w s m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (Strict.StateT s m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

instance MArray1 a e m => MArray1 a e (Lazy.StateT s m) where
  getBounds1 = lift . getBounds1
  getNumElements1 = lift . getNumElements1
  newArray1 i e = lift $ newArray1 i e
  newArray1_ = lift . newArray1_
  unsafeNewArray1_ = lift . unsafeNewArray1_
  unsafeRead1 a i = lift $ unsafeRead1 a i
  unsafeWrite1 a i e = lift $ unsafeWrite1 a i e

