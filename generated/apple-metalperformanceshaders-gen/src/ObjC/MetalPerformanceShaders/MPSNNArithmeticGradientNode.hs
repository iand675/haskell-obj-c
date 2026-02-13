{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNArithmeticGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNArithmeticGradientNode
  ( MPSNNArithmeticGradientNode
  , IsMPSNNArithmeticGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter
  , initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter
  , initWithGradientImages_forwardFilter_isSecondarySourceFilter
  , primaryScale
  , setPrimaryScale
  , secondaryScale
  , setSecondaryScale
  , bias
  , setBias
  , secondaryStrideInPixelsX
  , setSecondaryStrideInPixelsX
  , secondaryStrideInPixelsY
  , setSecondaryStrideInPixelsY
  , secondaryStrideInFeatureChannels
  , setSecondaryStrideInFeatureChannels
  , minimumValue
  , setMinimumValue
  , maximumValue
  , setMaximumValue
  , isSecondarySourceFilter
  , biasSelector
  , initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector
  , initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector
  , isSecondarySourceFilterSelector
  , maximumValueSelector
  , minimumValueSelector
  , nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector
  , primaryScaleSelector
  , secondaryScaleSelector
  , secondaryStrideInFeatureChannelsSelector
  , secondaryStrideInPixelsXSelector
  , secondaryStrideInPixelsYSelector
  , setBiasSelector
  , setMaximumValueSelector
  , setMinimumValueSelector
  , setPrimaryScaleSelector
  , setSecondaryScaleSelector
  , setSecondaryStrideInFeatureChannelsSelector
  , setSecondaryStrideInPixelsXSelector
  , setSecondaryStrideInPixelsYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | create a new arithmetic gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodesWithSources:]              for an easier way to do this.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The source input image from the forward pass (primary or secondary).
--
-- @gradientState@ — The gradient state produced by the concatenation filter, consumed by this filter.
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNBinaryGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> Bool -> IO (Id MPSNNArithmeticGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter sourceGradient sourceImage gradientState isSecondarySourceFilter =
  do
    cls' <- getRequiredClass "MPSNNArithmeticGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNBinaryGradientStateNode gradientState) isSecondarySourceFilter

-- | create a new arithmetic gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodesWithSources:]              for an easier way to do this.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The source input image from the forward pass (primary or secondary).
--
-- @gradientState@ — The gradient state produced by the concatenation filter, consumed by this filter.
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter :: (IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNBinaryGradientStateNode gradientState) => mpsnnArithmeticGradientNode -> sourceGradient -> sourceImage -> gradientState -> Bool -> IO (Id MPSNNArithmeticGradientNode)
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter mpsnnArithmeticGradientNode sourceGradient sourceImage gradientState isSecondarySourceFilter =
  sendOwnedMessage mpsnnArithmeticGradientNode initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNBinaryGradientStateNode gradientState) isSecondarySourceFilter

-- | create a new arithmetic gradient node
--
-- See also -[MPSCNNNeuronNode gradientFilterNodesWithSources:]              for an easier way to do this.
--
-- @gradientImages@ — The input gradient from the 'downstream' gradient filter and the source input image                                      from the forward pass (primary or secondary).
--
-- @filter@ — The matching filter node from the forward pass.
--
-- @isSecondarySourceFilter@ — The isSecondarySourceFilter property is used to indicate whether the arithmetic                                      gradient filter is operating on the primary or secondary source image from the                                      forward pass.
--
-- ObjC selector: @- initWithGradientImages:forwardFilter:isSecondarySourceFilter:@
initWithGradientImages_forwardFilter_isSecondarySourceFilter :: (IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode, IsNSArray gradientImages, IsMPSNNFilterNode filter_) => mpsnnArithmeticGradientNode -> gradientImages -> filter_ -> Bool -> IO (Id MPSNNArithmeticGradientNode)
initWithGradientImages_forwardFilter_isSecondarySourceFilter mpsnnArithmeticGradientNode gradientImages filter_ isSecondarySourceFilter =
  sendOwnedMessage mpsnnArithmeticGradientNode initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector (toNSArray gradientImages) (toMPSNNFilterNode filter_) isSecondarySourceFilter

-- | @- primaryScale@
primaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
primaryScale mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode primaryScaleSelector

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setPrimaryScale mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setPrimaryScaleSelector value

-- | @- secondaryScale@
secondaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
secondaryScale mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode secondaryScaleSelector

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setSecondaryScale mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setSecondaryScaleSelector value

-- | @- bias@
bias :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
bias mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode biasSelector

-- | @- setBias:@
setBias :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setBias mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setBiasSelector value

-- | @- secondaryStrideInPixelsX@
secondaryStrideInPixelsX :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInPixelsX mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode secondaryStrideInPixelsXSelector

-- | @- setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsX :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInPixelsX mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setSecondaryStrideInPixelsXSelector value

-- | @- secondaryStrideInPixelsY@
secondaryStrideInPixelsY :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInPixelsY mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode secondaryStrideInPixelsYSelector

-- | @- setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsY :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInPixelsY mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setSecondaryStrideInPixelsYSelector value

-- | @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInFeatureChannels mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode secondaryStrideInFeatureChannelsSelector

-- | @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setSecondaryStrideInFeatureChannelsSelector value

-- | @- minimumValue@
minimumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
minimumValue mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode minimumValueSelector

-- | @- setMinimumValue:@
setMinimumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setMinimumValue mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setMinimumValueSelector value

-- | @- maximumValue@
maximumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
maximumValue mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode maximumValueSelector

-- | @- setMaximumValue:@
setMaximumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setMaximumValue mpsnnArithmeticGradientNode value =
  sendMessage mpsnnArithmeticGradientNode setMaximumValueSelector value

-- | @- isSecondarySourceFilter@
isSecondarySourceFilter :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO Bool
isSecondarySourceFilter mpsnnArithmeticGradientNode =
  sendMessage mpsnnArithmeticGradientNode isSecondarySourceFilterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNBinaryGradientStateNode, Bool] (Id MPSNNArithmeticGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNBinaryGradientStateNode, Bool] (Id MPSNNArithmeticGradientNode)
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:"

-- | @Selector@ for @initWithGradientImages:forwardFilter:isSecondarySourceFilter:@
initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector :: Selector '[Id NSArray, Id MPSNNFilterNode, Bool] (Id MPSNNArithmeticGradientNode)
initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector = mkSelector "initWithGradientImages:forwardFilter:isSecondarySourceFilter:"

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

-- | @Selector@ for @secondaryStrideInPixelsX@
secondaryStrideInPixelsXSelector :: Selector '[] CULong
secondaryStrideInPixelsXSelector = mkSelector "secondaryStrideInPixelsX"

-- | @Selector@ for @setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsXSelector :: Selector '[CULong] ()
setSecondaryStrideInPixelsXSelector = mkSelector "setSecondaryStrideInPixelsX:"

-- | @Selector@ for @secondaryStrideInPixelsY@
secondaryStrideInPixelsYSelector :: Selector '[] CULong
secondaryStrideInPixelsYSelector = mkSelector "secondaryStrideInPixelsY"

-- | @Selector@ for @setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsYSelector :: Selector '[CULong] ()
setSecondaryStrideInPixelsYSelector = mkSelector "setSecondaryStrideInPixelsY:"

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

