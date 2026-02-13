{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUTO-GENERATED FROM CodeGenArray.h
--
-- Generated bindings for @MDLAnimatedScalarArray@.
module ObjC.ModelIO.MDLAnimatedScalarArray
  ( MDLAnimatedScalarArray
  , IsMDLAnimatedScalarArray(..)
  , initWithElementCount
  , setFloatArray_count_atTime
  , setDoubleArray_count_atTime
  , getFloatArray_maxCount_atTime
  , getDoubleArray_maxCount_atTime
  , resetWithFloatArray_count_atTimes_count
  , resetWithDoubleArray_count_atTimes_count
  , getFloatArray_maxCount
  , getDoubleArray_maxCount
  , elementCount
  , elementCountSelector
  , getDoubleArray_maxCountSelector
  , getDoubleArray_maxCount_atTimeSelector
  , getFloatArray_maxCountSelector
  , getFloatArray_maxCount_atTimeSelector
  , initWithElementCountSelector
  , resetWithDoubleArray_count_atTimes_countSelector
  , resetWithFloatArray_count_atTimes_countSelector
  , setDoubleArray_count_atTimeSelector
  , setFloatArray_count_atTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> CULong -> IO RawId
initWithElementCount mdlAnimatedScalarArray arrayElementCount =
  sendOwnedMessage mdlAnimatedScalarArray initWithElementCountSelector arrayElementCount

-- | @- setFloatArray:count:atTime:@
setFloatArray_count_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CFloat) -> CULong -> CDouble -> IO ()
setFloatArray_count_atTime mdlAnimatedScalarArray array count time =
  sendMessage mdlAnimatedScalarArray setFloatArray_count_atTimeSelector array count time

-- | @- setDoubleArray:count:atTime:@
setDoubleArray_count_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CDouble) -> CULong -> CDouble -> IO ()
setDoubleArray_count_atTime mdlAnimatedScalarArray array count time =
  sendMessage mdlAnimatedScalarArray setDoubleArray_count_atTimeSelector array count time

-- | @- getFloatArray:maxCount:atTime:@
getFloatArray_maxCount_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CFloat -> CULong -> CDouble -> IO CULong
getFloatArray_maxCount_atTime mdlAnimatedScalarArray array maxCount time =
  sendMessage mdlAnimatedScalarArray getFloatArray_maxCount_atTimeSelector array maxCount time

-- | @- getDoubleArray:maxCount:atTime:@
getDoubleArray_maxCount_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CDouble -> CULong -> CDouble -> IO CULong
getDoubleArray_maxCount_atTime mdlAnimatedScalarArray array maxCount time =
  sendMessage mdlAnimatedScalarArray getDoubleArray_maxCount_atTimeSelector array maxCount time

-- | @- resetWithFloatArray:count:atTimes:count:@
resetWithFloatArray_count_atTimes_count :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CFloat) -> CULong -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithFloatArray_count_atTimes_count mdlAnimatedScalarArray valuesArray valuesCount timesArray timesCount =
  sendMessage mdlAnimatedScalarArray resetWithFloatArray_count_atTimes_countSelector valuesArray valuesCount timesArray timesCount

-- | @- resetWithDoubleArray:count:atTimes:count:@
resetWithDoubleArray_count_atTimes_count :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CDouble) -> CULong -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithDoubleArray_count_atTimes_count mdlAnimatedScalarArray valuesArray valuesCount timesArray timesCount =
  sendMessage mdlAnimatedScalarArray resetWithDoubleArray_count_atTimes_countSelector valuesArray valuesCount timesArray timesCount

-- | @- getFloatArray:maxCount:@
getFloatArray_maxCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CFloat -> CULong -> IO CULong
getFloatArray_maxCount mdlAnimatedScalarArray valuesArray maxCount =
  sendMessage mdlAnimatedScalarArray getFloatArray_maxCountSelector valuesArray maxCount

-- | @- getDoubleArray:maxCount:@
getDoubleArray_maxCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CDouble -> CULong -> IO CULong
getDoubleArray_maxCount mdlAnimatedScalarArray valuesArray maxCount =
  sendMessage mdlAnimatedScalarArray getDoubleArray_maxCountSelector valuesArray maxCount

-- | @- elementCount@
elementCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> IO CULong
elementCount mdlAnimatedScalarArray =
  sendMessage mdlAnimatedScalarArray elementCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector '[CULong] RawId
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @setFloatArray:count:atTime:@
setFloatArray_count_atTimeSelector :: Selector '[Const (Ptr CFloat), CULong, CDouble] ()
setFloatArray_count_atTimeSelector = mkSelector "setFloatArray:count:atTime:"

-- | @Selector@ for @setDoubleArray:count:atTime:@
setDoubleArray_count_atTimeSelector :: Selector '[Const (Ptr CDouble), CULong, CDouble] ()
setDoubleArray_count_atTimeSelector = mkSelector "setDoubleArray:count:atTime:"

-- | @Selector@ for @getFloatArray:maxCount:atTime:@
getFloatArray_maxCount_atTimeSelector :: Selector '[Ptr CFloat, CULong, CDouble] CULong
getFloatArray_maxCount_atTimeSelector = mkSelector "getFloatArray:maxCount:atTime:"

-- | @Selector@ for @getDoubleArray:maxCount:atTime:@
getDoubleArray_maxCount_atTimeSelector :: Selector '[Ptr CDouble, CULong, CDouble] CULong
getDoubleArray_maxCount_atTimeSelector = mkSelector "getDoubleArray:maxCount:atTime:"

-- | @Selector@ for @resetWithFloatArray:count:atTimes:count:@
resetWithFloatArray_count_atTimes_countSelector :: Selector '[Const (Ptr CFloat), CULong, Const (Ptr CDouble), CULong] ()
resetWithFloatArray_count_atTimes_countSelector = mkSelector "resetWithFloatArray:count:atTimes:count:"

-- | @Selector@ for @resetWithDoubleArray:count:atTimes:count:@
resetWithDoubleArray_count_atTimes_countSelector :: Selector '[Const (Ptr CDouble), CULong, Const (Ptr CDouble), CULong] ()
resetWithDoubleArray_count_atTimes_countSelector = mkSelector "resetWithDoubleArray:count:atTimes:count:"

-- | @Selector@ for @getFloatArray:maxCount:@
getFloatArray_maxCountSelector :: Selector '[Ptr CFloat, CULong] CULong
getFloatArray_maxCountSelector = mkSelector "getFloatArray:maxCount:"

-- | @Selector@ for @getDoubleArray:maxCount:@
getDoubleArray_maxCountSelector :: Selector '[Ptr CDouble, CULong] CULong
getDoubleArray_maxCountSelector = mkSelector "getDoubleArray:maxCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector '[] CULong
elementCountSelector = mkSelector "elementCount"

