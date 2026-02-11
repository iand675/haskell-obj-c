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
  , valuesSelector
  , setValuesSelector
  , pathSelector
  , setPathSelector
  , keyTimesSelector
  , setKeyTimesSelector
  , timingFunctionsSelector
  , setTimingFunctionsSelector
  , calculationModeSelector
  , setCalculationModeSelector
  , tensionValuesSelector
  , setTensionValuesSelector
  , continuityValuesSelector
  , setContinuityValuesSelector
  , biasValuesSelector
  , setBiasValuesSelector
  , rotationModeSelector
  , setRotationModeSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- values@
values :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
values caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "values") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValues:@
setValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setValues caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- path@
path :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO RawId
path caKeyframeAnimation  =
  fmap (RawId . castPtr) $ sendMsg caKeyframeAnimation (mkSelector "path") (retPtr retVoid) []

-- | @- setPath:@
setPath :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> RawId -> IO ()
setPath caKeyframeAnimation  value =
  sendMsg caKeyframeAnimation (mkSelector "setPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- keyTimes@
keyTimes :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
keyTimes caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "keyTimes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyTimes:@
setKeyTimes :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setKeyTimes caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setKeyTimes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timingFunctions@
timingFunctions :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
timingFunctions caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "timingFunctions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimingFunctions:@
setTimingFunctions :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setTimingFunctions caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setTimingFunctions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- calculationMode@
calculationMode :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSString)
calculationMode caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "calculationMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCalculationMode:@
setCalculationMode :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSString value) => caKeyframeAnimation -> value -> IO ()
setCalculationMode caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setCalculationMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tensionValues@
tensionValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
tensionValues caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "tensionValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTensionValues:@
setTensionValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setTensionValues caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setTensionValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- continuityValues@
continuityValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
continuityValues caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "continuityValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContinuityValues:@
setContinuityValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setContinuityValues caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setContinuityValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- biasValues@
biasValues :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSArray)
biasValues caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "biasValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBiasValues:@
setBiasValues :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSArray value) => caKeyframeAnimation -> value -> IO ()
setBiasValues caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setBiasValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rotationMode@
rotationMode :: IsCAKeyframeAnimation caKeyframeAnimation => caKeyframeAnimation -> IO (Id NSString)
rotationMode caKeyframeAnimation  =
  sendMsg caKeyframeAnimation (mkSelector "rotationMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRotationMode:@
setRotationMode :: (IsCAKeyframeAnimation caKeyframeAnimation, IsNSString value) => caKeyframeAnimation -> value -> IO ()
setRotationMode caKeyframeAnimation  value =
withObjCPtr value $ \raw_value ->
    sendMsg caKeyframeAnimation (mkSelector "setRotationMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @values@
valuesSelector :: Selector
valuesSelector = mkSelector "values"

-- | @Selector@ for @setValues:@
setValuesSelector :: Selector
setValuesSelector = mkSelector "setValues:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector
keyTimesSelector = mkSelector "keyTimes"

-- | @Selector@ for @setKeyTimes:@
setKeyTimesSelector :: Selector
setKeyTimesSelector = mkSelector "setKeyTimes:"

-- | @Selector@ for @timingFunctions@
timingFunctionsSelector :: Selector
timingFunctionsSelector = mkSelector "timingFunctions"

-- | @Selector@ for @setTimingFunctions:@
setTimingFunctionsSelector :: Selector
setTimingFunctionsSelector = mkSelector "setTimingFunctions:"

-- | @Selector@ for @calculationMode@
calculationModeSelector :: Selector
calculationModeSelector = mkSelector "calculationMode"

-- | @Selector@ for @setCalculationMode:@
setCalculationModeSelector :: Selector
setCalculationModeSelector = mkSelector "setCalculationMode:"

-- | @Selector@ for @tensionValues@
tensionValuesSelector :: Selector
tensionValuesSelector = mkSelector "tensionValues"

-- | @Selector@ for @setTensionValues:@
setTensionValuesSelector :: Selector
setTensionValuesSelector = mkSelector "setTensionValues:"

-- | @Selector@ for @continuityValues@
continuityValuesSelector :: Selector
continuityValuesSelector = mkSelector "continuityValues"

-- | @Selector@ for @setContinuityValues:@
setContinuityValuesSelector :: Selector
setContinuityValuesSelector = mkSelector "setContinuityValues:"

-- | @Selector@ for @biasValues@
biasValuesSelector :: Selector
biasValuesSelector = mkSelector "biasValues"

-- | @Selector@ for @setBiasValues:@
setBiasValuesSelector :: Selector
setBiasValuesSelector = mkSelector "setBiasValues:"

-- | @Selector@ for @rotationMode@
rotationModeSelector :: Selector
rotationModeSelector = mkSelector "rotationMode"

-- | @Selector@ for @setRotationMode:@
setRotationModeSelector :: Selector
setRotationModeSelector = mkSelector "setRotationMode:"

