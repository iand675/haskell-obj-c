{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNArithmetic
--
-- This depends on Metal.framework
--
-- The MPSCNNArithmetic filter takes two source images, a primary source image and a              secondary source image, and outputs a single destination image. It applies an              element-wise arithmetic operator to each pixel in a primary source image and a              corresponding pixel in a secondary source image over a specified region.
--
-- The supported arithmetic operators are the following:              - Addition              - Subtraction              - Multiplication              - Division              - Comparison
--
-- This filter takes additional parameters: primaryScale, secondaryScale, and bias. The default              value for primaryScale and secondaryScale is 1.0f. The default value for bias is 0.0f. This              filter applies primaryScale, secondaryScale, and bias to the primary source pixel (x) and              secondary source pixel (y) in the following way:              - Addition:         result = ((primaryScale * x) + (secondaryScale * y)) + bias              - Subtraction:      result = ((primaryScale * x) - (secondaryScale * y)) + bias              - Multiplicaton:    result = ((primaryScale * x) * (secondaryScale * y)) + bias              - Division:         result = ((primaryScale * x) / (secondaryScale * y)) + bias              - Comparison:       Unused.
--
-- To clamp the result of an arithmetic operation, where              result = clamp(result, minimumValue, maximumValue),              set the minimumValue and maximumValue appropriately. The default value of minimumValue              is -FLT_MAX. The default value of maximumValue is FLT_MAX.
--
-- This filter also takes the following additional parameters:              - primaryStrideInPixelsX, primaryStrideInPixelsY, primaryStrideInFeatureChannels              - secondaryStrideInPixelsX, secondaryStrideInPixelsY, secondaryStrideInFeatureChannels              These parameters can be used to control broadcasting for the data stored in the primary and              secondary source images. For example, setting all strides for the primary source image to 0              will result in the primarySource image being treated as a scalar value. The only supported              values are 0 or 1. The default value of these parameters is 1.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- You must use one of the sub-classes of MPSImageArithmetic.
--
-- Generated bindings for @MPSCNNArithmetic@.
module ObjC.MetalPerformanceShaders.MPSCNNArithmetic
  ( MPSCNNArithmetic
  , IsMPSCNNArithmetic(..)
  , initWithDevice
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImage
  , primaryScale
  , setPrimaryScale
  , secondaryScale
  , setSecondaryScale
  , bias
  , setBias
  , primaryStrideInFeatureChannels
  , setPrimaryStrideInFeatureChannels
  , secondaryStrideInFeatureChannels
  , setSecondaryStrideInFeatureChannels
  , minimumValue
  , setMinimumValue
  , maximumValue
  , setMaximumValue
  , initWithDeviceSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector
  , primaryScaleSelector
  , setPrimaryScaleSelector
  , secondaryScaleSelector
  , setSecondaryScaleSelector
  , biasSelector
  , setBiasSelector
  , primaryStrideInFeatureChannelsSelector
  , setPrimaryStrideInFeatureChannelsSelector
  , secondaryStrideInFeatureChannelsSelector
  , setSecondaryStrideInFeatureChannelsSelector
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
initWithDevice :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> RawId -> IO (Id MPSCNNArithmetic)
initWithDevice mpscnnArithmetic  device =
  sendMsg mpscnnArithmetic (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode call that operates on a state for later consumption by a gradient kernel in training
--
-- This is the older style of encode which reads the offset, doesn't change it,                  and ignores the padding method.
--
-- @commandBuffer@ — The command buffer
--
-- @primaryImage@ — A MPSImage to use as the source images for the filter.
--
-- @secondaryImage@ — A MPSImage to use as the source images for the filter.
--
-- @destinationState@ — MPSCNNArithmeticGradientState to be consumed by the gradient layer
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage                                      may not alias primarySourceImage or secondarySourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImage :: (IsMPSCNNArithmetic mpscnnArithmetic, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSCNNArithmeticGradientState destinationState, IsMPSImage destinationImage) => mpscnnArithmetic -> RawId -> primaryImage -> secondaryImage -> destinationState -> destinationImage -> IO ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImage mpscnnArithmetic  commandBuffer primaryImage secondaryImage destinationState destinationImage =
withObjCPtr primaryImage $ \raw_primaryImage ->
  withObjCPtr secondaryImage $ \raw_secondaryImage ->
    withObjCPtr destinationState $ \raw_destinationState ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnArithmetic (mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_primaryImage :: Ptr ()), argPtr (castPtr raw_secondaryImage :: Ptr ()), argPtr (castPtr raw_destinationState :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- primaryScale@
primaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
primaryScale mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "primaryScale") retCFloat []

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setPrimaryScale mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setPrimaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- secondaryScale@
secondaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
secondaryScale mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "secondaryScale") retCFloat []

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setSecondaryScale mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setSecondaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- bias@
bias :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
bias mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "bias") retCFloat []

-- | @- setBias:@
setBias :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setBias mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setBias:") retVoid [argCFloat (fromIntegral value)]

-- | primaryStrideInPixels
--
-- The primarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- primaryStrideInFeatureChannels@
primaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CULong
primaryStrideInFeatureChannels mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "primaryStrideInFeatureChannels") retCULong []

-- | primaryStrideInPixels
--
-- The primarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CULong -> IO ()
setPrimaryStrideInFeatureChannels mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setPrimaryStrideInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CULong
secondaryStrideInFeatureChannels mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "secondaryStrideInFeatureChannels") retCULong []

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setSecondaryStrideInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
minimumValue mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "minimumValue") retCFloat []

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- setMinimumValue:@
setMinimumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setMinimumValue mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setMinimumValue:") retVoid [argCFloat (fromIntegral value)]

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
maximumValue mpscnnArithmetic  =
  sendMsg mpscnnArithmetic (mkSelector "maximumValue") retCFloat []

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- setMaximumValue:@
setMaximumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setMaximumValue mpscnnArithmetic  value =
  sendMsg mpscnnArithmetic (mkSelector "setMaximumValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector :: Selector
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:"

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

-- | @Selector@ for @primaryStrideInFeatureChannels@
primaryStrideInFeatureChannelsSelector :: Selector
primaryStrideInFeatureChannelsSelector = mkSelector "primaryStrideInFeatureChannels"

-- | @Selector@ for @setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannelsSelector :: Selector
setPrimaryStrideInFeatureChannelsSelector = mkSelector "setPrimaryStrideInFeatureChannels:"

-- | @Selector@ for @secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannelsSelector :: Selector
secondaryStrideInFeatureChannelsSelector = mkSelector "secondaryStrideInFeatureChannels"

-- | @Selector@ for @setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannelsSelector :: Selector
setSecondaryStrideInFeatureChannelsSelector = mkSelector "setSecondaryStrideInFeatureChannels:"

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

