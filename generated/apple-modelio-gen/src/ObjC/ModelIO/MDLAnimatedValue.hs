{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , clearSelector
  , getTimes_maxCountSelector
  , interpolationSelector
  , isAnimatedSelector
  , keyTimesSelector
  , maximumTimeSelector
  , minimumTimeSelector
  , precisionSelector
  , setInterpolationSelector
  , timeSampleCountSelector

  -- * Enum types
  , MDLAnimatedValueInterpolation(MDLAnimatedValueInterpolation)
  , pattern MDLAnimatedValueInterpolationConstant
  , pattern MDLAnimatedValueInterpolationLinear
  , MDLDataPrecision(MDLDataPrecision)
  , pattern MDLDataPrecisionUndefined
  , pattern MDLDataPrecisionFloat
  , pattern MDLDataPrecisionDouble

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- isAnimated@
isAnimated :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO Bool
isAnimated mdlAnimatedValue =
  sendMessage mdlAnimatedValue isAnimatedSelector

-- | @- clear@
clear :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO ()
clear mdlAnimatedValue =
  sendMessage mdlAnimatedValue clearSelector

-- | @- getTimes:maxCount:@
getTimes_maxCount :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> Ptr CDouble -> CULong -> IO CULong
getTimes_maxCount mdlAnimatedValue timesArray maxCount =
  sendMessage mdlAnimatedValue getTimes_maxCountSelector timesArray maxCount

-- | @- precision@
precision :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO MDLDataPrecision
precision mdlAnimatedValue =
  sendMessage mdlAnimatedValue precisionSelector

-- | @- timeSampleCount@
timeSampleCount :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CULong
timeSampleCount mdlAnimatedValue =
  sendMessage mdlAnimatedValue timeSampleCountSelector

-- | @- minimumTime@
minimumTime :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CDouble
minimumTime mdlAnimatedValue =
  sendMessage mdlAnimatedValue minimumTimeSelector

-- | @- maximumTime@
maximumTime :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO CDouble
maximumTime mdlAnimatedValue =
  sendMessage mdlAnimatedValue maximumTimeSelector

-- | @- interpolation@
interpolation :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO MDLAnimatedValueInterpolation
interpolation mdlAnimatedValue =
  sendMessage mdlAnimatedValue interpolationSelector

-- | @- setInterpolation:@
setInterpolation :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> MDLAnimatedValueInterpolation -> IO ()
setInterpolation mdlAnimatedValue value =
  sendMessage mdlAnimatedValue setInterpolationSelector value

-- | @- keyTimes@
keyTimes :: IsMDLAnimatedValue mdlAnimatedValue => mdlAnimatedValue -> IO (Id NSArray)
keyTimes mdlAnimatedValue =
  sendMessage mdlAnimatedValue keyTimesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAnimated@
isAnimatedSelector :: Selector '[] Bool
isAnimatedSelector = mkSelector "isAnimated"

-- | @Selector@ for @clear@
clearSelector :: Selector '[] ()
clearSelector = mkSelector "clear"

-- | @Selector@ for @getTimes:maxCount:@
getTimes_maxCountSelector :: Selector '[Ptr CDouble, CULong] CULong
getTimes_maxCountSelector = mkSelector "getTimes:maxCount:"

-- | @Selector@ for @precision@
precisionSelector :: Selector '[] MDLDataPrecision
precisionSelector = mkSelector "precision"

-- | @Selector@ for @timeSampleCount@
timeSampleCountSelector :: Selector '[] CULong
timeSampleCountSelector = mkSelector "timeSampleCount"

-- | @Selector@ for @minimumTime@
minimumTimeSelector :: Selector '[] CDouble
minimumTimeSelector = mkSelector "minimumTime"

-- | @Selector@ for @maximumTime@
maximumTimeSelector :: Selector '[] CDouble
maximumTimeSelector = mkSelector "maximumTime"

-- | @Selector@ for @interpolation@
interpolationSelector :: Selector '[] MDLAnimatedValueInterpolation
interpolationSelector = mkSelector "interpolation"

-- | @Selector@ for @setInterpolation:@
setInterpolationSelector :: Selector '[MDLAnimatedValueInterpolation] ()
setInterpolationSelector = mkSelector "setInterpolation:"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector '[] (Id NSArray)
keyTimesSelector = mkSelector "keyTimes"

