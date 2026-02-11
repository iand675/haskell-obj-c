{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageArithmetic
--
-- This depends on Metal.framework.
--
-- This filter takes two source images, a primary source image and a secondary source image,              and outputs a single destination image. It applies an element-wise arithmetic operator to              each pixel in a primary source image and a corresponding pixel in a secondary source image              over a specified region.
--
-- The supported arithmetic operators are the following:              - Addition              - Subtraction              - Multiplication              - Division
--
-- This filter takes additional parameters: primaryScale, secondaryScale, and bias. The default              value for primaryScale and secondaryScale is 1.0f. The default value for bias is 0.0f. This              filter applies primaryScale, secondaryScale, and bias to the primary source pixel (x) and              secondary source pixel (y) in the following way:              - Addition:         result = ((primaryScale * x) + (secondaryScale * y)) + bias              - Subtraction:      result = ((primaryScale * x) - (secondaryScale * y)) + bias              - Multiplicaton:    result = ((primaryScale * x) * (secondaryScale * y)) + bias              - Division:         result = ((primaryScale * x) / (secondaryScale * y)) + bias
--
-- To clamp the result of an arithmetic operation, where              result = clamp(result, minimumValue, maximumValue),              set the minimumValue and maximumValue appropriately. The default value of minimumValue              is -FLT_MAX. The default value of maximumValue is FLT_MAX.
--
-- This filter also takes the following additional parameters:              - primaryStrideInPixels              - secondaryStrideInPixels              These parameters can be used to control broadcasting for the data stored in the primary and              secondary source images. For example, setting all strides for the primary source image to 0              will result in the primarySource image being treated as a scalar value. The only supported              values are 0 or 1. The default value of these parameters is 1.
--
-- This filter accepts uint and int data in addition to unorm and floating-point data.
--
-- You must use one of the sub-classes of MPSImageArithmetic.
--
-- Generated bindings for @MPSImageArithmetic@.
module ObjC.MetalPerformanceShaders.MPSImageArithmetic
  ( MPSImageArithmetic
  , IsMPSImageArithmetic(..)
  , initWithDevice
  , primaryScale
  , setPrimaryScale
  , secondaryScale
  , setSecondaryScale
  , bias
  , setBias
  , minimumValue
  , setMinimumValue
  , maximumValue
  , setMaximumValue
  , initWithDeviceSelector
  , primaryScaleSelector
  , setPrimaryScaleSelector
  , secondaryScaleSelector
  , setSecondaryScaleSelector
  , biasSelector
  , setBiasSelector
  , minimumValueSelector
  , setMinimumValueSelector
  , maximumValueSelector
  , setMaximumValueSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> RawId -> IO (Id MPSImageArithmetic)
initWithDevice mpsImageArithmetic  device =
  sendMsg mpsImageArithmetic (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- primaryScale@
primaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
primaryScale mpsImageArithmetic  =
  sendMsg mpsImageArithmetic (mkSelector "primaryScale") retCFloat []

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setPrimaryScale mpsImageArithmetic  value =
  sendMsg mpsImageArithmetic (mkSelector "setPrimaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- secondaryScale@
secondaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
secondaryScale mpsImageArithmetic  =
  sendMsg mpsImageArithmetic (mkSelector "secondaryScale") retCFloat []

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setSecondaryScale mpsImageArithmetic  value =
  sendMsg mpsImageArithmetic (mkSelector "setSecondaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- bias@
bias :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
bias mpsImageArithmetic  =
  sendMsg mpsImageArithmetic (mkSelector "bias") retCFloat []

-- | @- setBias:@
setBias :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setBias mpsImageArithmetic  value =
  sendMsg mpsImageArithmetic (mkSelector "setBias:") retVoid [argCFloat (fromIntegral value)]

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
minimumValue mpsImageArithmetic  =
  sendMsg mpsImageArithmetic (mkSelector "minimumValue") retCFloat []

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- setMinimumValue:@
setMinimumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setMinimumValue mpsImageArithmetic  value =
  sendMsg mpsImageArithmetic (mkSelector "setMinimumValue:") retVoid [argCFloat (fromIntegral value)]

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
maximumValue mpsImageArithmetic  =
  sendMsg mpsImageArithmetic (mkSelector "maximumValue") retCFloat []

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- setMaximumValue:@
setMaximumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setMaximumValue mpsImageArithmetic  value =
  sendMsg mpsImageArithmetic (mkSelector "setMaximumValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @primaryScale@
primaryScaleSelector :: Selector
primaryScaleSelector = mkSelector "primaryScale"

-- | @Selector@ for @setPrimaryScale:@
setPrimaryScaleSelector :: Selector
setPrimaryScaleSelector = mkSelector "setPrimaryScale:"

-- | @Selector@ for @secondaryScale@
secondaryScaleSelector :: Selector
secondaryScaleSelector = mkSelector "secondaryScale"

-- | @Selector@ for @setSecondaryScale:@
setSecondaryScaleSelector :: Selector
setSecondaryScaleSelector = mkSelector "setSecondaryScale:"

-- | @Selector@ for @bias@
biasSelector :: Selector
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector
setBiasSelector = mkSelector "setBias:"

-- | @Selector@ for @minimumValue@
minimumValueSelector :: Selector
minimumValueSelector = mkSelector "minimumValue"

-- | @Selector@ for @setMinimumValue:@
setMinimumValueSelector :: Selector
setMinimumValueSelector = mkSelector "setMinimumValue:"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @setMaximumValue:@
setMaximumValueSelector :: Selector
setMaximumValueSelector = mkSelector "setMaximumValue:"

