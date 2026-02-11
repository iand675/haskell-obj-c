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
  , nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector
  , initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector
  , initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector
  , primaryScaleSelector
  , setPrimaryScaleSelector
  , secondaryScaleSelector
  , setSecondaryScaleSelector
  , biasSelector
  , setBiasSelector
  , secondaryStrideInPixelsXSelector
  , setSecondaryStrideInPixelsXSelector
  , secondaryStrideInPixelsYSelector
  , setSecondaryStrideInPixelsYSelector
  , secondaryStrideInFeatureChannelsSelector
  , setSecondaryStrideInFeatureChannelsSelector
  , minimumValueSelector
  , setMinimumValueSelector
  , maximumValueSelector
  , setMaximumValueSelector
  , isSecondarySourceFilterSelector


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
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (if isSecondarySourceFilter then 1 else 0)] >>= retainedObject . castPtr

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
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilter mpsnnArithmeticGradientNode  sourceGradient sourceImage gradientState isSecondarySourceFilter =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpsnnArithmeticGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (if isSecondarySourceFilter then 1 else 0)] >>= ownedObject . castPtr

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
initWithGradientImages_forwardFilter_isSecondarySourceFilter mpsnnArithmeticGradientNode  gradientImages filter_ isSecondarySourceFilter =
withObjCPtr gradientImages $ \raw_gradientImages ->
  withObjCPtr filter_ $ \raw_filter_ ->
      sendMsg mpsnnArithmeticGradientNode (mkSelector "initWithGradientImages:forwardFilter:isSecondarySourceFilter:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ()), argPtr (castPtr raw_filter_ :: Ptr ()), argCULong (if isSecondarySourceFilter then 1 else 0)] >>= ownedObject . castPtr

-- | @- primaryScale@
primaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
primaryScale mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "primaryScale") retCFloat []

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setPrimaryScale mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setPrimaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- secondaryScale@
secondaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
secondaryScale mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "secondaryScale") retCFloat []

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setSecondaryScale mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setSecondaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- bias@
bias :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
bias mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "bias") retCFloat []

-- | @- setBias:@
setBias :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setBias mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setBias:") retVoid [argCFloat (fromIntegral value)]

-- | @- secondaryStrideInPixelsX@
secondaryStrideInPixelsX :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInPixelsX mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "secondaryStrideInPixelsX") retCULong []

-- | @- setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsX :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInPixelsX mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setSecondaryStrideInPixelsX:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryStrideInPixelsY@
secondaryStrideInPixelsY :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInPixelsY mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "secondaryStrideInPixelsY") retCULong []

-- | @- setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsY :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInPixelsY mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setSecondaryStrideInPixelsY:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CULong
secondaryStrideInFeatureChannels mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "secondaryStrideInFeatureChannels") retCULong []

-- | @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setSecondaryStrideInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumValue@
minimumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
minimumValue mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "minimumValue") retCFloat []

-- | @- setMinimumValue:@
setMinimumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setMinimumValue mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setMinimumValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- maximumValue@
maximumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO CFloat
maximumValue mpsnnArithmeticGradientNode  =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "maximumValue") retCFloat []

-- | @- setMaximumValue:@
setMaximumValue :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> CFloat -> IO ()
setMaximumValue mpsnnArithmeticGradientNode  value =
  sendMsg mpsnnArithmeticGradientNode (mkSelector "setMaximumValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- isSecondarySourceFilter@
isSecondarySourceFilter :: IsMPSNNArithmeticGradientNode mpsnnArithmeticGradientNode => mpsnnArithmeticGradientNode -> IO Bool
isSecondarySourceFilter mpsnnArithmeticGradientNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnArithmeticGradientNode (mkSelector "isSecondarySourceFilter") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:@
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_isSecondarySourceFilterSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:isSecondarySourceFilter:"

-- | @Selector@ for @initWithGradientImages:forwardFilter:isSecondarySourceFilter:@
initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector :: Selector
initWithGradientImages_forwardFilter_isSecondarySourceFilterSelector = mkSelector "initWithGradientImages:forwardFilter:isSecondarySourceFilter:"

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

-- | @Selector@ for @secondaryStrideInPixelsX@
secondaryStrideInPixelsXSelector :: Selector
secondaryStrideInPixelsXSelector = mkSelector "secondaryStrideInPixelsX"

-- | @Selector@ for @setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsXSelector :: Selector
setSecondaryStrideInPixelsXSelector = mkSelector "setSecondaryStrideInPixelsX:"

-- | @Selector@ for @secondaryStrideInPixelsY@
secondaryStrideInPixelsYSelector :: Selector
secondaryStrideInPixelsYSelector = mkSelector "secondaryStrideInPixelsY"

-- | @Selector@ for @setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsYSelector :: Selector
setSecondaryStrideInPixelsYSelector = mkSelector "setSecondaryStrideInPixelsY:"

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

-- | @Selector@ for @isSecondarySourceFilter@
isSecondarySourceFilterSelector :: Selector
isSecondarySourceFilterSelector = mkSelector "isSecondarySourceFilter"

