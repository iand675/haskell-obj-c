{-# LANGUAGE DataKinds #-}
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
  , biasSelector
  , initWithDeviceSelector
  , maximumValueSelector
  , minimumValueSelector
  , primaryScaleSelector
  , secondaryScaleSelector
  , setBiasSelector
  , setMaximumValueSelector
  , setMinimumValueSelector
  , setPrimaryScaleSelector
  , setSecondaryScaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> RawId -> IO (Id MPSImageArithmetic)
initWithDevice mpsImageArithmetic device =
  sendOwnedMessage mpsImageArithmetic initWithDeviceSelector device

-- | @- primaryScale@
primaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
primaryScale mpsImageArithmetic =
  sendMessage mpsImageArithmetic primaryScaleSelector

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setPrimaryScale mpsImageArithmetic value =
  sendMessage mpsImageArithmetic setPrimaryScaleSelector value

-- | @- secondaryScale@
secondaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
secondaryScale mpsImageArithmetic =
  sendMessage mpsImageArithmetic secondaryScaleSelector

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setSecondaryScale mpsImageArithmetic value =
  sendMessage mpsImageArithmetic setSecondaryScaleSelector value

-- | @- bias@
bias :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
bias mpsImageArithmetic =
  sendMessage mpsImageArithmetic biasSelector

-- | @- setBias:@
setBias :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setBias mpsImageArithmetic value =
  sendMessage mpsImageArithmetic setBiasSelector value

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
minimumValue mpsImageArithmetic =
  sendMessage mpsImageArithmetic minimumValueSelector

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- setMinimumValue:@
setMinimumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setMinimumValue mpsImageArithmetic value =
  sendMessage mpsImageArithmetic setMinimumValueSelector value

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> IO CFloat
maximumValue mpsImageArithmetic =
  sendMessage mpsImageArithmetic maximumValueSelector

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- setMaximumValue:@
setMaximumValue :: IsMPSImageArithmetic mpsImageArithmetic => mpsImageArithmetic -> CFloat -> IO ()
setMaximumValue mpsImageArithmetic value =
  sendMessage mpsImageArithmetic setMaximumValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageArithmetic)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @primaryScale@
primaryScaleSelector :: Selector '[] CFloat
primaryScaleSelector = mkSelector "primaryScale"

-- | @Selector@ for @setPrimaryScale:@
setPrimaryScaleSelector :: Selector '[CFloat] ()
setPrimaryScaleSelector = mkSelector "setPrimaryScale:"

-- | @Selector@ for @secondaryScale@
secondaryScaleSelector :: Selector '[] CFloat
secondaryScaleSelector = mkSelector "secondaryScale"

-- | @Selector@ for @setSecondaryScale:@
setSecondaryScaleSelector :: Selector '[CFloat] ()
setSecondaryScaleSelector = mkSelector "setSecondaryScale:"

-- | @Selector@ for @bias@
biasSelector :: Selector '[] CFloat
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector '[CFloat] ()
setBiasSelector = mkSelector "setBias:"

-- | @Selector@ for @minimumValue@
minimumValueSelector :: Selector '[] CFloat
minimumValueSelector = mkSelector "minimumValue"

-- | @Selector@ for @setMinimumValue:@
setMinimumValueSelector :: Selector '[CFloat] ()
setMinimumValueSelector = mkSelector "setMinimumValue:"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector '[] CFloat
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @setMaximumValue:@
setMaximumValueSelector :: Selector '[CFloat] ()
setMaximumValueSelector = mkSelector "setMaximumValue:"

