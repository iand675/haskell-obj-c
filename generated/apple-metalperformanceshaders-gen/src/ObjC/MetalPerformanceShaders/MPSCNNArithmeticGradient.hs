{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNArithmeticGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNArithmeticGradient filter is the backward filter for the MPSCNNArithmetic              forward filter.
--
-- The forward filter takes two inputs, primary and secondary source images, and produces              a single output image. Thus, going backwards requires two separate filters (one for              the primary source image and one for the secondary source image) that take multiple              inputs and produce a single output. The secondarySourceFilter property is used to              indicate whether the filter is operating on the primary or secondary source image from              the forward pass.
--
-- All the arithmetic gradient filters require the following inputs: gradient image from              the previous layer (going backwards) and all the applicable input source images from              the forward pass.
--
-- The forward filter takes the following additional parameters:              - primaryStrideInPixelsX, primaryStrideInPixelsY, primaryStrideInFeatureChannels              - secondaryStrideInPixelsX, secondaryStrideInPixelsY, secondaryStrideInFeatureChannels              These parameters can be used in the forward filter to control broadcasting for the data              stored in the primary and secondary source images. For example, setting all strides for              the primary source image to 0 will result in the primarySource image being treated as a              single pixel. The only supported values are 0 or 1. The default value of these parameters              is 1.
--
-- The first input to the backward filter is the gradient image from the previous layer              (going backwards), so there are no broadcasting parameters for this input. For the              backward filter, the broadcasting parameters for the second input must match the              broadcasting parameters set for the same image in the forward filter.
--
-- In the backward pass, broadcasting results in a reduction operation (sum) across all of the              applicable broadcasting dimensions (rows, columns, feature channels, or any combination              thereof) to produce the destination image of the size that matches the primary/secondary              input images used in the forward pass.
--
-- In the case of no broadcasting, the following arithmetic gradient operations are copy              operations (that can be optimized away by the graph interface):              - Add (primarySource, secondarySource)              - Subtract (primarySource)
--
-- Similarly to the forward filter, this backward filter takes additional parameters:              primaryScale, secondaryScale, and bias. The default value for primaryScale and secondaryScale              is 1.0f. The default value for bias is 0.0f. This filter applies primaryScale to the primary              source image, applies the secondaryScale to the secondary source image, where appropriate,              and applies bias to the result, i.e.:              result = ((primaryScale * x) [insert operation] (secondaryScale * y)) + bias.
--
-- The subtraction gradient filter for the secondary source image requires that the primaryScale              property is set to -1.0f (for x - y, d/dy(x - y) = -1).
--
-- In the forward filter, there is support for clamping the result of the available operations,              where result = clamp(result, minimumValue, maximumValue). The clamp backward operation is              not supported in the arithmetic gradient filters. If you require this functionality, it can              be implemented by performing a clamp backward operation before calling the arithmetic gradient              filters. You would need to apply the following function on the incomping gradient input image:              f(x) = ((minimumValue < x) && (x < maximumValue)) ? 1 : 0, where x is the original result              (before clamping) of the forward arithmetic filter.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- You must use one of the sub-classes of MPSImageArithmeticGradient.
--
-- Generated bindings for @MPSCNNArithmeticGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNArithmeticGradient
  ( MPSCNNArithmeticGradient
  , IsMPSCNNArithmeticGradient(..)
  , initWithDevice
  , initWithDevice_isSecondarySourceFilter
  , primaryScale
  , setPrimaryScale
  , secondaryScale
  , setSecondaryScale
  , bias
  , setBias
  , secondaryStrideInFeatureChannels
  , setSecondaryStrideInFeatureChannels
  , minimumValue
  , setMinimumValue
  , maximumValue
  , setMaximumValue
  , isSecondarySourceFilter
  , biasSelector
  , initWithDeviceSelector
  , initWithDevice_isSecondarySourceFilterSelector
  , isSecondarySourceFilterSelector
  , maximumValueSelector
  , minimumValueSelector
  , primaryScaleSelector
  , secondaryScaleSelector
  , secondaryStrideInFeatureChannelsSelector
  , setBiasSelector
  , setMaximumValueSelector
  , setMinimumValueSelector
  , setPrimaryScaleSelector
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
initWithDevice :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> RawId -> IO (Id MPSCNNArithmeticGradient)
initWithDevice mpscnnArithmeticGradient device =
  sendOwnedMessage mpscnnArithmeticGradient initWithDeviceSelector device

-- | @- initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilter :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> RawId -> Bool -> IO (Id MPSCNNArithmeticGradient)
initWithDevice_isSecondarySourceFilter mpscnnArithmeticGradient device isSecondarySourceFilter =
  sendOwnedMessage mpscnnArithmeticGradient initWithDevice_isSecondarySourceFilterSelector device isSecondarySourceFilter

-- | @- primaryScale@
primaryScale :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CFloat
primaryScale mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient primaryScaleSelector

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CFloat -> IO ()
setPrimaryScale mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setPrimaryScaleSelector value

-- | @- secondaryScale@
secondaryScale :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CFloat
secondaryScale mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient secondaryScaleSelector

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CFloat -> IO ()
setSecondaryScale mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setSecondaryScaleSelector value

-- | @- bias@
bias :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CFloat
bias mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient biasSelector

-- | @- setBias:@
setBias :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CFloat -> IO ()
setBias mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setBiasSelector value

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CULong
secondaryStrideInFeatureChannels mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient secondaryStrideInFeatureChannelsSelector

-- | secondaryStrideInPixels
--
-- The secondarySource stride in the feature channel dimension. The only supported values are 0 or 1.              The default value for each dimension is 1.
--
-- ObjC selector: @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setSecondaryStrideInFeatureChannelsSelector value

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CFloat
minimumValue mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient minimumValueSelector

-- | minimumValue
--
-- minimumValue is to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of minimumValue is -FLT_MAX.
--
-- ObjC selector: @- setMinimumValue:@
setMinimumValue :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CFloat -> IO ()
setMinimumValue mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setMinimumValueSelector value

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO CFloat
maximumValue mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient maximumValueSelector

-- | maximumValue
--
-- maximumValue is used to clamp the result of an arithmetic operation:              result = clamp(result, minimumValue, maximumValue).              The default value of maximumValue is FLT_MAX.
--
-- ObjC selector: @- setMaximumValue:@
setMaximumValue :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> CFloat -> IO ()
setMaximumValue mpscnnArithmeticGradient value =
  sendMessage mpscnnArithmeticGradient setMaximumValueSelector value

-- | isSecondarySourceFilter
--
-- The isSecondarySourceFilter property is used to indicate whether the arithmetic gradient              filter is operating on the primary or secondary source image from the forward pass.
--
-- ObjC selector: @- isSecondarySourceFilter@
isSecondarySourceFilter :: IsMPSCNNArithmeticGradient mpscnnArithmeticGradient => mpscnnArithmeticGradient -> IO Bool
isSecondarySourceFilter mpscnnArithmeticGradient =
  sendMessage mpscnnArithmeticGradient isSecondarySourceFilterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNArithmeticGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilterSelector :: Selector '[RawId, Bool] (Id MPSCNNArithmeticGradient)
initWithDevice_isSecondarySourceFilterSelector = mkSelector "initWithDevice:isSecondarySourceFilter:"

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

-- | @Selector@ for @isSecondarySourceFilter@
isSecondarySourceFilterSelector :: Selector '[] Bool
isSecondarySourceFilterSelector = mkSelector "isSecondarySourceFilter"

