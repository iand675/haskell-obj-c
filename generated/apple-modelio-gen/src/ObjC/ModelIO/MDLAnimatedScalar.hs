{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUTO-GENERATED FROM CodeGen.h
--
-- Generated bindings for @MDLAnimatedScalar@.
module ObjC.ModelIO.MDLAnimatedScalar
  ( MDLAnimatedScalar
  , IsMDLAnimatedScalar(..)
  , setFloat_atTime
  , setDouble_atTime
  , floatAtTime
  , doubleAtTime
  , resetWithFloatArray_atTimes_count
  , resetWithDoubleArray_atTimes_count
  , getFloatArray_maxCount
  , getDoubleArray_maxCount
  , doubleAtTimeSelector
  , floatAtTimeSelector
  , getDoubleArray_maxCountSelector
  , getFloatArray_maxCountSelector
  , resetWithDoubleArray_atTimes_countSelector
  , resetWithFloatArray_atTimes_countSelector
  , setDouble_atTimeSelector
  , setFloat_atTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setFloat:atTime:@
setFloat_atTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CFloat -> CDouble -> IO ()
setFloat_atTime mdlAnimatedScalar value time =
  sendMessage mdlAnimatedScalar setFloat_atTimeSelector value time

-- | @- setDouble:atTime:@
setDouble_atTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> CDouble -> IO ()
setDouble_atTime mdlAnimatedScalar value time =
  sendMessage mdlAnimatedScalar setDouble_atTimeSelector value time

-- | @- floatAtTime:@
floatAtTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> IO CFloat
floatAtTime mdlAnimatedScalar time =
  sendMessage mdlAnimatedScalar floatAtTimeSelector time

-- | @- doubleAtTime:@
doubleAtTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> IO CDouble
doubleAtTime mdlAnimatedScalar time =
  sendMessage mdlAnimatedScalar doubleAtTimeSelector time

-- | @- resetWithFloatArray:atTimes:count:@
resetWithFloatArray_atTimes_count :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Const (Ptr CFloat) -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithFloatArray_atTimes_count mdlAnimatedScalar valuesArray timesArray count =
  sendMessage mdlAnimatedScalar resetWithFloatArray_atTimes_countSelector valuesArray timesArray count

-- | @- resetWithDoubleArray:atTimes:count:@
resetWithDoubleArray_atTimes_count :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Const (Ptr CDouble) -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithDoubleArray_atTimes_count mdlAnimatedScalar valuesArray timesArray count =
  sendMessage mdlAnimatedScalar resetWithDoubleArray_atTimes_countSelector valuesArray timesArray count

-- | @- getFloatArray:maxCount:@
getFloatArray_maxCount :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Ptr CFloat -> CULong -> IO CULong
getFloatArray_maxCount mdlAnimatedScalar valuesArray maxCount =
  sendMessage mdlAnimatedScalar getFloatArray_maxCountSelector valuesArray maxCount

-- | @- getDoubleArray:maxCount:@
getDoubleArray_maxCount :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Ptr CDouble -> CULong -> IO CULong
getDoubleArray_maxCount mdlAnimatedScalar valuesArray maxCount =
  sendMessage mdlAnimatedScalar getDoubleArray_maxCountSelector valuesArray maxCount

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFloat:atTime:@
setFloat_atTimeSelector :: Selector '[CFloat, CDouble] ()
setFloat_atTimeSelector = mkSelector "setFloat:atTime:"

-- | @Selector@ for @setDouble:atTime:@
setDouble_atTimeSelector :: Selector '[CDouble, CDouble] ()
setDouble_atTimeSelector = mkSelector "setDouble:atTime:"

-- | @Selector@ for @floatAtTime:@
floatAtTimeSelector :: Selector '[CDouble] CFloat
floatAtTimeSelector = mkSelector "floatAtTime:"

-- | @Selector@ for @doubleAtTime:@
doubleAtTimeSelector :: Selector '[CDouble] CDouble
doubleAtTimeSelector = mkSelector "doubleAtTime:"

-- | @Selector@ for @resetWithFloatArray:atTimes:count:@
resetWithFloatArray_atTimes_countSelector :: Selector '[Const (Ptr CFloat), Const (Ptr CDouble), CULong] ()
resetWithFloatArray_atTimes_countSelector = mkSelector "resetWithFloatArray:atTimes:count:"

-- | @Selector@ for @resetWithDoubleArray:atTimes:count:@
resetWithDoubleArray_atTimes_countSelector :: Selector '[Const (Ptr CDouble), Const (Ptr CDouble), CULong] ()
resetWithDoubleArray_atTimes_countSelector = mkSelector "resetWithDoubleArray:atTimes:count:"

-- | @Selector@ for @getFloatArray:maxCount:@
getFloatArray_maxCountSelector :: Selector '[Ptr CFloat, CULong] CULong
getFloatArray_maxCountSelector = mkSelector "getFloatArray:maxCount:"

-- | @Selector@ for @getDoubleArray:maxCount:@
getDoubleArray_maxCountSelector :: Selector '[Ptr CDouble, CULong] CULong
getDoubleArray_maxCountSelector = mkSelector "getDoubleArray:maxCount:"

