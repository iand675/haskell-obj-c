{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimatedValue@.
module ObjC.ModelIO.MDLAnimatedValue
  ( MDLAnimatedValue
  , IsMDLAnimatedValue(..)
  , isAnimated
  , clear
  , getTimes_maxCount
  , precision
  , timeSampleCount
  , minimumTime
  , maximumTime
  , interpolation
  , setInterpolation
  , keyTimes
  , isAnimatedSelector
  , clearSelector
  , getTimes_maxCountSelector
  , precisionSelector
  , timeSampleCountSelector
  , minimumTimeSelector
  , maximumTimeSelector
  , interpolationSelector
  , setInterpolationSelector
  , keyTimesSelector

  -- * Enum types
  , MDLAnimatedValueInterpolation(MDLAnimatedValueInterpolation)
  , pattern MDLAnimatedValueInterpolationConstant
  , pattern MDLAnimatedValueInterpolationLinear
  , MDLDataPrecision(MDLDataPrecision)
  , pattern MDLDataPrecisionUndefined
  , pattern MDLDataPrecisionFloat
  , pattern MDLDataPrecisionDouble

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
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- isAnimated@
isAnimated :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO Bool
isAnimated mdlAnimatedValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlAnimatedValue (mkSelector "isAnimated") retCULong []

-- | @- clear@
clear :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO ()
clear mdlAnimatedValue  =
  sendMsg mdlAnimatedValue (mkSelector "clear") retVoid []

-- | @- getTimes:maxCount:@
getTimes_maxCount :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> Ptr CDouble -> CULong -> IO CULong
getTimes_maxCount mdlAnimatedValue  timesArray maxCount =
  sendMsg mdlAnimatedValue (mkSelector "getTimes:maxCount:") retCULong [argPtr timesArray, argCULong (fromIntegral maxCount)]

-- | @- precision@
precision :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO MDLDataPrecision
precision mdlAnimatedValue  =
  fmap (coerce :: CULong -> MDLDataPrecision) $ sendMsg mdlAnimatedValue (mkSelector "precision") retCULong []

-- | @- timeSampleCount@
timeSampleCount :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CULong
timeSampleCount mdlAnimatedValue  =
  sendMsg mdlAnimatedValue (mkSelector "timeSampleCount") retCULong []

-- | @- minimumTime@
minimumTime :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CDouble
minimumTime mdlAnimatedValue  =
  sendMsg mdlAnimatedValue (mkSelector "minimumTime") retCDouble []

-- | @- maximumTime@
maximumTime :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CDouble
maximumTime mdlAnimatedValue  =
  sendMsg mdlAnimatedValue (mkSelector "maximumTime") retCDouble []

-- | @- interpolation@
interpolation :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO MDLAnimatedValueInterpolation
interpolation mdlAnimatedValue  =
  fmap (coerce :: CULong -> MDLAnimatedValueInterpolation) $ sendMsg mdlAnimatedValue (mkSelector "interpolation") retCULong []

-- | @- setInterpolation:@
setInterpolation :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> MDLAnimatedValueInterpolation -> IO ()
setInterpolation mdlAnimatedValue  value =
  sendMsg mdlAnimatedValue (mkSelector "setInterpolation:") retVoid [argCULong (coerce value)]

-- | @- keyTimes@
keyTimes :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO (Id NSArray)
keyTimes mdlAnimatedValue  =
  sendMsg mdlAnimatedValue (mkSelector "keyTimes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAnimated@
isAnimatedSelector :: Selector
isAnimatedSelector = mkSelector "isAnimated"

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @getTimes:maxCount:@
getTimes_maxCountSelector :: Selector
getTimes_maxCountSelector = mkSelector "getTimes:maxCount:"

-- | @Selector@ for @precision@
precisionSelector :: Selector
precisionSelector = mkSelector "precision"

-- | @Selector@ for @timeSampleCount@
timeSampleCountSelector :: Selector
timeSampleCountSelector = mkSelector "timeSampleCount"

-- | @Selector@ for @minimumTime@
minimumTimeSelector :: Selector
minimumTimeSelector = mkSelector "minimumTime"

-- | @Selector@ for @maximumTime@
maximumTimeSelector :: Selector
maximumTimeSelector = mkSelector "maximumTime"

-- | @Selector@ for @interpolation@
interpolationSelector :: Selector
interpolationSelector = mkSelector "interpolation"

-- | @Selector@ for @setInterpolation:@
setInterpolationSelector :: Selector
setInterpolationSelector = mkSelector "setInterpolation:"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector
keyTimesSelector = mkSelector "keyTimes"

