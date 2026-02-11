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
  , setFloat_atTimeSelector
  , setDouble_atTimeSelector
  , floatAtTimeSelector
  , doubleAtTimeSelector
  , resetWithFloatArray_atTimes_countSelector
  , resetWithDoubleArray_atTimes_countSelector
  , getFloatArray_maxCountSelector
  , getDoubleArray_maxCountSelector


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

-- | @- setFloat:atTime:@
setFloat_atTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CFloat -> CDouble -> IO ()
setFloat_atTime mdlAnimatedScalar  value time =
  sendMsg mdlAnimatedScalar (mkSelector "setFloat:atTime:") retVoid [argCFloat (fromIntegral value), argCDouble (fromIntegral time)]

-- | @- setDouble:atTime:@
setDouble_atTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> CDouble -> IO ()
setDouble_atTime mdlAnimatedScalar  value time =
  sendMsg mdlAnimatedScalar (mkSelector "setDouble:atTime:") retVoid [argCDouble (fromIntegral value), argCDouble (fromIntegral time)]

-- | @- floatAtTime:@
floatAtTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> IO CFloat
floatAtTime mdlAnimatedScalar  time =
  sendMsg mdlAnimatedScalar (mkSelector "floatAtTime:") retCFloat [argCDouble (fromIntegral time)]

-- | @- doubleAtTime:@
doubleAtTime :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> CDouble -> IO CDouble
doubleAtTime mdlAnimatedScalar  time =
  sendMsg mdlAnimatedScalar (mkSelector "doubleAtTime:") retCDouble [argCDouble (fromIntegral time)]

-- | @- resetWithFloatArray:atTimes:count:@
resetWithFloatArray_atTimes_count :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Const (Ptr CFloat) -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithFloatArray_atTimes_count mdlAnimatedScalar  valuesArray timesArray count =
  sendMsg mdlAnimatedScalar (mkSelector "resetWithFloatArray:atTimes:count:") retVoid [argPtr (unConst valuesArray), argPtr (unConst timesArray), argCULong (fromIntegral count)]

-- | @- resetWithDoubleArray:atTimes:count:@
resetWithDoubleArray_atTimes_count :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Const (Ptr CDouble) -> Const (Ptr CDouble) -> CULong -> IO ()
resetWithDoubleArray_atTimes_count mdlAnimatedScalar  valuesArray timesArray count =
  sendMsg mdlAnimatedScalar (mkSelector "resetWithDoubleArray:atTimes:count:") retVoid [argPtr (unConst valuesArray), argPtr (unConst timesArray), argCULong (fromIntegral count)]

-- | @- getFloatArray:maxCount:@
getFloatArray_maxCount :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Ptr CFloat -> CULong -> IO CULong
getFloatArray_maxCount mdlAnimatedScalar  valuesArray maxCount =
  sendMsg mdlAnimatedScalar (mkSelector "getFloatArray:maxCount:") retCULong [argPtr valuesArray, argCULong (fromIntegral maxCount)]

-- | @- getDoubleArray:maxCount:@
getDoubleArray_maxCount :: IsMDLAnimatedScalar mdlAnimatedScalar => mdlAnimatedScalar -> Ptr CDouble -> CULong -> IO CULong
getDoubleArray_maxCount mdlAnimatedScalar  valuesArray maxCount =
  sendMsg mdlAnimatedScalar (mkSelector "getDoubleArray:maxCount:") retCULong [argPtr valuesArray, argCULong (fromIntegral maxCount)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFloat:atTime:@
setFloat_atTimeSelector :: Selector
setFloat_atTimeSelector = mkSelector "setFloat:atTime:"

-- | @Selector@ for @setDouble:atTime:@
setDouble_atTimeSelector :: Selector
setDouble_atTimeSelector = mkSelector "setDouble:atTime:"

-- | @Selector@ for @floatAtTime:@
floatAtTimeSelector :: Selector
floatAtTimeSelector = mkSelector "floatAtTime:"

-- | @Selector@ for @doubleAtTime:@
doubleAtTimeSelector :: Selector
doubleAtTimeSelector = mkSelector "doubleAtTime:"

-- | @Selector@ for @resetWithFloatArray:atTimes:count:@
resetWithFloatArray_atTimes_countSelector :: Selector
resetWithFloatArray_atTimes_countSelector = mkSelector "resetWithFloatArray:atTimes:count:"

-- | @Selector@ for @resetWithDoubleArray:atTimes:count:@
resetWithDoubleArray_atTimes_countSelector :: Selector
resetWithDoubleArray_atTimes_countSelector = mkSelector "resetWithDoubleArray:atTimes:count:"

-- | @Selector@ for @getFloatArray:maxCount:@
getFloatArray_maxCountSelector :: Selector
getFloatArray_maxCountSelector = mkSelector "getFloatArray:maxCount:"

-- | @Selector@ for @getDoubleArray:maxCount:@
getDoubleArray_maxCountSelector :: Selector
getDoubleArray_maxCountSelector = mkSelector "getDoubleArray:maxCount:"

