{-# LANGUAGE DataKinds #-}
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
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImages
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
  , biasSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImagesSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector
  , initWithDeviceSelector
  , maximumValueSelector
  , minimumValueSelector
  , primaryScaleSelector
  , primaryStrideInFeatureChannelsSelector
  , secondaryScaleSelector
  , secondaryStrideInFeatureChannelsSelector
  , setBiasSelector
  , setMaximumValueSelector
  , setMinimumValueSelector
  , setPrimaryScaleSelector
  , setPrimaryStrideInFeatureChannelsSelector
  , setSecondaryScaleSelector
  , setSecondaryStrideInFeatureChannelsSelector


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
initWithDevice :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> RawId -> IO (Id MPSCNNArithmetic)
initWithDevice mpscnnArithmetic device =
  sendOwnedMessage mpscnnArithmetic initWithDeviceSelector device

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
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImage mpscnnArithmetic commandBuffer primaryImage secondaryImage destinationState destinationImage =
  sendMessage mpscnnArithmetic encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage) (toMPSCNNArithmeticGradientState destinationState) (toMPSImage destinationImage)

-- | Encode call that operates on a state for later consumption by a gradient kernel in training
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method. Multiple images are processed concurrently.              All images must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryImages@ — An array of MPSImage objects containing the primary source images.
--
-- @secondaryImages@ — An array MPSImage objects containing the secondary source images.
--
-- @destinationStates@ — An array of MPSCNNArithmeticGradientStateBatch to be consumed by the gradient layer
--
-- @destinationImages@ — An array of MPSImage objects to contain the result images.                                    destinationImages may not alias primarySourceImages or secondarySourceImages                                    in any manner.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImages :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImages mpscnnArithmetic commandBuffer primaryImages secondaryImages destinationStates destinationImages =
  sendMessage mpscnnArithmetic encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImagesSelector commandBuffer primaryImages secondaryImages destinationStates destinationImages

-- | @- primaryScale@
primaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
primaryScale mpscnnArithmetic =
  sendMessage mpscnnArithmetic primaryScaleSelector

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setPrimaryScale mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setPrimaryScaleSelector value

-- | @- secondaryScale@
secondaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
secondaryScale mpscnnArithmetic =
  sendMessage mpscnnArithmetic secondaryScaleSelector

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setSecondaryScale mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setSecondaryScaleSelector value

-- | @- bias@
bias :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
bias mpscnnArithmetic =
  sendMessage mpscnnArithmetic biasSelector

-- | @- setBias:@
setBias :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setBias mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setBiasSelector value

-- | primaryStrideInPixels
--
-- The primarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- primaryStrideInFeatureChannels@
primaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CULong
primaryStrideInFeatureChannels mpscnnArithmetic =
  sendMessage mpscnnArithmetic primaryStrideInFeatureChannelsSelector

-- | primaryStrideInPixels
--
-- The primarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CULong -> IO ()
setPrimaryStrideInFeatureChannels mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setPrimaryStrideInFeatureChannelsSelector value

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CULong
secondaryStrideInFeatureChannels mpscnnArithmetic =
  sendMessage mpscnnArithmetic secondaryStrideInFeatureChannelsSelector

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setSecondaryStrideInFeatureChannelsSelector value

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
minimumValue mpscnnArithmetic =
  sendMessage mpscnnArithmetic minimumValueSelector

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- setMinimumValue:@
setMinimumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setMinimumValue mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setMinimumValueSelector value

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> IO CFloat
maximumValue mpscnnArithmetic =
  sendMessage mpscnnArithmetic maximumValueSelector

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- setMaximumValue:@
setMaximumValue :: IsMPSCNNArithmetic mpscnnArithmetic => mpscnnArithmetic -> CFloat -> IO ()
setMaximumValue mpscnnArithmetic value =
  sendMessage mpscnnArithmetic setMaximumValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNArithmetic)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSCNNArithmeticGradientState, Id MPSImage] ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationImages:"

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

-- | @Selector@ for @primaryStrideInFeatureChannels@
primaryStrideInFeatureChannelsSelector :: Selector '[] CULong
primaryStrideInFeatureChannelsSelector = mkSelector "primaryStrideInFeatureChannels"

-- | @Selector@ for @setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannelsSelector :: Selector '[CULong] ()
setPrimaryStrideInFeatureChannelsSelector = mkSelector "setPrimaryStrideInFeatureChannels:"

-- | @Selector@ for @secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannelsSelector :: Selector '[] CULong
secondaryStrideInFeatureChannelsSelector = mkSelector "secondaryStrideInFeatureChannels"

-- | @Selector@ for @setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannelsSelector :: Selector '[CULong] ()
setSecondaryStrideInFeatureChannelsSelector = mkSelector "setSecondaryStrideInFeatureChannels:"

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

