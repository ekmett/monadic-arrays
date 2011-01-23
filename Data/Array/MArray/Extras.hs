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
-- A higher-order MArray class.
----------------------------------------------------------------------------
module Data.Array.MArray.Extras
  ( MArray1(..)
  ) where

import Control.Monad.ST
import Data.Array.Base
import Data.Array.IO
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

