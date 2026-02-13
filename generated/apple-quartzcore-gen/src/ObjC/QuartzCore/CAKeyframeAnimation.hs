{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | General keyframe animation class. *
--
-- Generated bindings for @CAKeyframeAnimation@.
module ObjC.QuartzCore.CAKeyframeAnimation
  ( CAKeyframeAnimation
  , IsCAKeyframeAnimation(..)
  , values
  , setValues
  , path
  , setPath
  , keyTimes
  , setKeyTimes
  , timingFunctions
  , setTimingFunctions
  , calculationMode
  , setCalculationMode
  , tensionValues
  , setTensionValues
  , continuityValues
  , setContinuityValues
  , biasValues
  , setBiasValues
  , rotationMode
  , setRotationMode
  , biasValuesSelector
  , calculationModeSelector
  , continuityValuesSelector
  , keyTimesSelector
  , pathSelector
  , rotationModeSelector
  , setBiasValuesSelector
  , setCalculationModeSelector
  , setContinuityValuesSelector
  , setKeyTimesSelector
  , setPathSelector
  , setRotationModeSelector
  , setTensionValuesSelector
  , setTimingFunctionsSelector
  , setValuesSelector
  , tensionValuesSelector
  , timingFunctionsSelector
  , valuesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- values@
values :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
values caKeyframeAnimation =
  sendMessage caKeyframeAnimation valuesSelector

-- | @- setValues:@
setValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setValues caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setValuesSelector (toNSArray value)

-- | @- path@
path :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO RawId
path caKeyframeAnimation =
  sendMessage caKeyframeAnimation pathSelector

-- | @- setPath:@
setPath :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> RawId -> IO ()
setPath caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setPathSelector value

-- | @- keyTimes@
keyTimes :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
keyTimes caKeyframeAnimation =
  sendMessage caKeyframeAnimation keyTimesSelector

-- | @- setKeyTimes:@
setKeyTimes :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setKeyTimes caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setKeyTimesSelector (toNSArray value)

-- | @- timingFunctions@
timingFunctions :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
timingFunctions caKeyframeAnimation =
  sendMessage caKeyframeAnimation timingFunctionsSelector

-- | @- setTimingFunctions:@
setTimingFunctions :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setTimingFunctions caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setTimingFunctionsSelector (toNSArray value)

-- | @- calculationMode@
calculationMode :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSString)
calculationMode caKeyframeAnimation =
  sendMessage caKeyframeAnimation calculationModeSelector

-- | @- setCalculationMode:@
setCalculationMode :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSString value) => caKeyframeAnimation -> value -> IO ()
setCalculationMode caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setCalculationModeSelector (toNSString value)

-- | @- tensionValues@
tensionValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
tensionValues caKeyframeAnimation =
  sendMessage caKeyframeAnimation tensionValuesSelector

-- | @- setTensionValues:@
setTensionValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setTensionValues caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setTensionValuesSelector (toNSArray value)

-- | @- continuityValues@
continuityValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
continuityValues caKeyframeAnimation =
  sendMessage caKeyframeAnimation continuityValuesSelector

-- | @- setContinuityValues:@
setContinuityValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setContinuityValues caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setContinuityValuesSelector (toNSArray value)

-- | @- biasValues@
biasValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
biasValues caKeyframeAnimation =
  sendMessage caKeyframeAnimation biasValuesSelector

-- | @- setBiasValues:@
setBiasValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setBiasValues caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setBiasValuesSelector (toNSArray value)

-- | @- rotationMode@
rotationMode :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSString)
rotationMode caKeyframeAnimation =
  sendMessage caKeyframeAnimation rotationModeSelector

-- | @- setRotationMode:@
setRotationMode :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSString value) => caKeyframeAnimation -> value -> IO ()
setRotationMode caKeyframeAnimation value =
  sendMessage caKeyframeAnimation setRotationModeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @values@
valuesSelector :: Selector '[] (Id NSArray)
valuesSelector = mkSelector "values"

-- | @Selector@ for @setValues:@
setValuesSelector :: Selector '[Id NSArray] ()
setValuesSelector = mkSelector "setValues:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] RawId
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[RawId] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector '[] (Id NSArray)
keyTimesSelector = mkSelector "keyTimes"

-- | @Selector@ for @setKeyTimes:@
setKeyTimesSelector :: Selector '[Id NSArray] ()
setKeyTimesSelector = mkSelector "setKeyTimes:"

-- | @Selector@ for @timingFunctions@
timingFunctionsSelector :: Selector '[] (Id NSArray)
timingFunctionsSelector = mkSelector "timingFunctions"

-- | @Selector@ for @setTimingFunctions:@
setTimingFunctionsSelector :: Selector '[Id NSArray] ()
setTimingFunctionsSelector = mkSelector "setTimingFunctions:"

-- | @Selector@ for @calculationMode@
calculationModeSelector :: Selector '[] (Id NSString)
calculationModeSelector = mkSelector "calculationMode"

-- | @Selector@ for @setCalculationMode:@
setCalculationModeSelector :: Selector '[Id NSString] ()
setCalculationModeSelector = mkSelector "setCalculationMode:"

-- | @Selector@ for @tensionValues@
tensionValuesSelector :: Selector '[] (Id NSArray)
tensionValuesSelector = mkSelector "tensionValues"

-- | @Selector@ for @setTensionValues:@
setTensionValuesSelector :: Selector '[Id NSArray] ()
setTensionValuesSelector = mkSelector "setTensionValues:"

-- | @Selector@ for @continuityValues@
continuityValuesSelector :: Selector '[] (Id NSArray)
continuityValuesSelector = mkSelector "continuityValues"

-- | @Selector@ for @setContinuityValues:@
setContinuityValuesSelector :: Selector '[Id NSArray] ()
setContinuityValuesSelector = mkSelector "setContinuityValues:"

-- | @Selector@ for @biasValues@
biasValuesSelector :: Selector '[] (Id NSArray)
biasValuesSelector = mkSelector "biasValues"

-- | @Selector@ for @setBiasValues:@
setBiasValuesSelector :: Selector '[Id NSArray] ()
setBiasValuesSelector = mkSelector "setBiasValues:"

-- | @Selector@ for @rotationMode@
rotationModeSelector :: Selector '[] (Id NSString)
rotationModeSelector = mkSelector "rotationMode"

-- | @Selector@ for @setRotationMode:@
setRotationModeSelector :: Selector '[Id NSString] ()
setRotationModeSelector = mkSelector "setRotationMode:"

