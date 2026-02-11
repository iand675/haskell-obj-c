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
  , initWithElementCountSelector
  , setFloatArray_count_atTimeSelector
  , setDoubleArray_count_atTimeSelector
  , getFloatArray_maxCount_atTimeSelector
  , getDoubleArray_maxCount_atTimeSelector
  , resetWithFloatArray_count_atTimes_countSelector
  , resetWithDoubleArray_count_atTimes_countSelector
  , getFloatArray_maxCountSelector
  , getDoubleArray_maxCountSelector
  , elementCountSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithElementCount:@
initWithElementCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> CULong -> IO RawId
initWithElementCount mdlAnimatedScalarArray  arrayElementCount =
  fmap (RawId . castPtr) $ sendMsg mdlAnimatedScalarArray (mkSelector "initWithElementCount:") (retPtr retVoid) [argCULong (fromIntegral arrayElementCount)]

-- | @- setFloatArray:count:atTime:@
setFloatArray_count_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CFloat) -> CULong -> CDouble -> IO ()
setFloatArray_count_atTime mdlAnimatedScalarArray  array count time =
  sendMsg mdlAnimatedScalarArray (mkSelector "setFloatArray:count:atTime:") retVoid [argPtr (unConst array), argCULong (fromIntegral count), argCDouble (fromIntegral time)]

-- | @- setDoubleArray:count:atTime:@
setDoubleArray_count_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CDouble) -> CULong -> CDouble -> IO ()
setDoubleArray_count_atTime mdlAnimatedScalarArray  array count time =
  sendMsg mdlAnimatedScalarArray (mkSelector "setDoubleArray:count:atTime:") retVoid [argPtr (unConst array), argCULong (fromIntegral count), argCDouble (fromIntegral time)]

-- | @- getFloatArray:maxCount:atTime:@
getFloatArray_maxCount_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CFloat -> CULong -> CDouble -> IO CULong
getFloatArray_maxCount_atTime mdlAnimatedScalarArray  array maxCount time =
  sendMsg mdlAnimatedScalarArray (mkSelector "getFloatArray:maxCount:atTime:") retCULong [argPtr array, argCULong (fromIntegral maxCount), argCDouble (fromIntegral time)]

-- | @- getDoubleArray:maxCount:atTime:@
getDoubleArray_maxCount_atTime :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CDouble -> CULong -> CDouble -> IO CULong
getDoubleArray_maxCount_atTime mdlAnimatedScalarArray  array maxCount time =
  sendMsg mdlAnimatedScalarArray (mkSelector "getDoubleArray:maxCount:atTime:") retCULong [argPtr array, argCULong (fromIntegral maxCount), argCDouble (fromIntegral time)]

-- | @- resetWithFloatArray:count:atTimes:count:@
resetWithFloatArray_count_atTimes_count :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CFloat) -> CULong -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithFloatArray_count_atTimes_count mdlAnimatedScalarArray  valuesArray valuesCount timesArray timesCount =
  sendMsg mdlAnimatedScalarArray (mkSelector "resetWithFloatArray:count:atTimes:count:") retVoid [argPtr (unConst valuesArray), argCULong (fromIntegral valuesCount), argPtr (unConst timesArray), argCULong (fromIntegral timesCount)]

-- | @- resetWithDoubleArray:count:atTimes:count:@
resetWithDoubleArray_count_atTimes_count :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Const (Ptr CDouble) -> CULong -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithDoubleArray_count_atTimes_count mdlAnimatedScalarArray  valuesArray valuesCount timesArray timesCount =
  sendMsg mdlAnimatedScalarArray (mkSelector "resetWithDoubleArray:count:atTimes:count:") retVoid [argPtr (unConst valuesArray), argCULong (fromIntegral valuesCount), argPtr (unConst timesArray), argCULong (fromIntegral timesCount)]

-- | @- getFloatArray:maxCount:@
getFloatArray_maxCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CFloat -> CULong -> IO CULong
getFloatArray_maxCount mdlAnimatedScalarArray  valuesArray maxCount =
  sendMsg mdlAnimatedScalarArray (mkSelector "getFloatArray:maxCount:") retCULong [argPtr valuesArray, argCULong (fromIntegral maxCount)]

-- | @- getDoubleArray:maxCount:@
getDoubleArray_maxCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> Ptr CDouble -> CULong -> IO CULong
getDoubleArray_maxCount mdlAnimatedScalarArray  valuesArray maxCount =
  sendMsg mdlAnimatedScalarArray (mkSelector "getDoubleArray:maxCount:") retCULong [argPtr valuesArray, argCULong (fromIntegral maxCount)]

-- | @- elementCount@
elementCount :: IsMDLAnimatedScalarArray mdlAnimatedScalarArray => mdlAnimatedScalarArray -> IO CULong
elementCount mdlAnimatedScalarArray  =
  sendMsg mdlAnimatedScalarArray (mkSelector "elementCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithElementCount:@
initWithElementCountSelector :: Selector
initWithElementCountSelector = mkSelector "initWithElementCount:"

-- | @Selector@ for @setFloatArray:count:atTime:@
setFloatArray_count_atTimeSelector :: Selector
setFloatArray_count_atTimeSelector = mkSelector "setFloatArray:count:atTime:"

-- | @Selector@ for @setDoubleArray:count:atTime:@
setDoubleArray_count_atTimeSelector :: Selector
setDoubleArray_count_atTimeSelector = mkSelector "setDoubleArray:count:atTime:"

-- | @Selector@ for @getFloatArray:maxCount:atTime:@
getFloatArray_maxCount_atTimeSelector :: Selector
getFloatArray_maxCount_atTimeSelector = mkSelector "getFloatArray:maxCount:atTime:"

-- | @Selector@ for @getDoubleArray:maxCount:atTime:@
getDoubleArray_maxCount_atTimeSelector :: Selector
getDoubleArray_maxCount_atTimeSelector = mkSelector "getDoubleArray:maxCount:atTime:"

-- | @Selector@ for @resetWithFloatArray:count:atTimes:count:@
resetWithFloatArray_count_atTimes_countSelector :: Selector
resetWithFloatArray_count_atTimes_countSelector = mkSelector "resetWithFloatArray:count:atTimes:count:"

-- | @Selector@ for @resetWithDoubleArray:count:atTimes:count:@
resetWithDoubleArray_count_atTimes_countSelector :: Selector
resetWithDoubleArray_count_atTimes_countSelector = mkSelector "resetWithDoubleArray:count:atTimes:count:"

-- | @Selector@ for @getFloatArray:maxCount:@
getFloatArray_maxCountSelector :: Selector
getFloatArray_maxCountSelector = mkSelector "getFloatArray:maxCount:"

-- | @Selector@ for @getDoubleArray:maxCount:@
getDoubleArray_maxCountSelector :: Selector
getDoubleArray_maxCountSelector = mkSelector "getDoubleArray:maxCount:"

-- | @Selector@ for @elementCount@
elementCountSelector :: Selector
elementCountSelector = mkSelector "elementCount"

