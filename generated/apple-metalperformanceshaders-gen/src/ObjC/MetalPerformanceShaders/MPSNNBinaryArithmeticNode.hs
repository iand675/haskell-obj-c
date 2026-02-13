{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | virtual base class for basic arithmetic nodes
--
-- Generated bindings for @MPSNNBinaryArithmeticNode@.
module ObjC.MetalPerformanceShaders.MPSNNBinaryArithmeticNode
  ( MPSNNBinaryArithmeticNode
  , IsMPSNNBinaryArithmeticNode(..)
  , nodeWithSources
  , nodeWithLeftSource_rightSource
  , initWithSources
  , initWithLeftSource_rightSource
  , gradientClass
  , gradientFilterWithSources
  , gradientFiltersWithSources
  , primaryScale
  , setPrimaryScale
  , secondaryScale
  , setSecondaryScale
  , bias
  , setBias
  , primaryStrideInPixelsX
  , setPrimaryStrideInPixelsX
  , primaryStrideInPixelsY
  , setPrimaryStrideInPixelsY
  , primaryStrideInFeatureChannels
  , setPrimaryStrideInFeatureChannels
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
  , biasSelector
  , gradientClassSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourcesSelector
  , initWithLeftSource_rightSourceSelector
  , initWithSourcesSelector
  , maximumValueSelector
  , minimumValueSelector
  , nodeWithLeftSource_rightSourceSelector
  , nodeWithSourcesSelector
  , primaryScaleSelector
  , primaryStrideInFeatureChannelsSelector
  , primaryStrideInPixelsXSelector
  , primaryStrideInPixelsYSelector
  , secondaryScaleSelector
  , secondaryStrideInFeatureChannelsSelector
  , secondaryStrideInPixelsXSelector
  , secondaryStrideInPixelsYSelector
  , setBiasSelector
  , setMaximumValueSelector
  , setMinimumValueSelector
  , setPrimaryScaleSelector
  , setPrimaryStrideInFeatureChannelsSelector
  , setPrimaryStrideInPixelsXSelector
  , setPrimaryStrideInPixelsYSelector
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

-- | create an autoreleased arithemtic node with an array of sources
--
-- @sourceNodes@ — A valid NSArray containing two sources
--
-- ObjC selector: @+ nodeWithSources:@
nodeWithSources :: IsNSArray sourceNodes => sourceNodes -> IO (Id MPSNNBinaryArithmeticNode)
nodeWithSources sourceNodes =
  do
    cls' <- getRequiredClass "MPSNNBinaryArithmeticNode"
    sendClassMessage cls' nodeWithSourcesSelector (toNSArray sourceNodes)

-- | create an autoreleased arithemtic node with two sources
--
-- @left@ — the left operand
--
-- @right@ — the right operand
--
-- ObjC selector: @+ nodeWithLeftSource:rightSource:@
nodeWithLeftSource_rightSource :: (IsMPSNNImageNode left, IsMPSNNImageNode right) => left -> right -> IO (Id MPSNNBinaryArithmeticNode)
nodeWithLeftSource_rightSource left right =
  do
    cls' <- getRequiredClass "MPSNNBinaryArithmeticNode"
    sendClassMessage cls' nodeWithLeftSource_rightSourceSelector (toMPSNNImageNode left) (toMPSNNImageNode right)

-- | init an arithemtic node with an array of sources
--
-- @sourceNodes@ — A valid NSArray containing two sources
--
-- ObjC selector: @- initWithSources:@
initWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray sourceNodes) => mpsnnBinaryArithmeticNode -> sourceNodes -> IO (Id MPSNNBinaryArithmeticNode)
initWithSources mpsnnBinaryArithmeticNode sourceNodes =
  sendOwnedMessage mpsnnBinaryArithmeticNode initWithSourcesSelector (toNSArray sourceNodes)

-- | init an arithemtic node with two sources
--
-- @left@ — the left operand
--
-- @right@ — the right operand
--
-- ObjC selector: @- initWithLeftSource:rightSource:@
initWithLeftSource_rightSource :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsMPSNNImageNode left, IsMPSNNImageNode right) => mpsnnBinaryArithmeticNode -> left -> right -> IO (Id MPSNNBinaryArithmeticNode)
initWithLeftSource_rightSource mpsnnBinaryArithmeticNode left right =
  sendOwnedMessage mpsnnBinaryArithmeticNode initWithLeftSource_rightSourceSelector (toMPSNNImageNode left) (toMPSNNImageNode right)

-- | @- gradientClass@
gradientClass :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO Class
gradientClass mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode gradientClassSelector

-- | @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray gradientImages) => mpsnnBinaryArithmeticNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnBinaryArithmeticNode gradientImages =
  sendMessage mpsnnBinaryArithmeticNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- | create new arithmetic gradient nodes
--
-- Create two new arithmetic gradient nodes - one that computes the gradient for the primary  source image and one that computes the gradient for the secondary sourcefrom the inference pass.
--
-- ObjC selector: @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray gradientImages) => mpsnnBinaryArithmeticNode -> gradientImages -> IO (Id NSArray)
gradientFiltersWithSources mpsnnBinaryArithmeticNode gradientImages =
  sendMessage mpsnnBinaryArithmeticNode gradientFiltersWithSourcesSelector (toNSArray gradientImages)

-- | @- primaryScale@
primaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
primaryScale mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode primaryScaleSelector

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setPrimaryScale mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setPrimaryScaleSelector value

-- | @- secondaryScale@
secondaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
secondaryScale mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode secondaryScaleSelector

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setSecondaryScale mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setSecondaryScaleSelector value

-- | @- bias@
bias :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
bias mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode biasSelector

-- | @- setBias:@
setBias :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setBias mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setBiasSelector value

-- | @- primaryStrideInPixelsX@
primaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInPixelsX mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode primaryStrideInPixelsXSelector

-- | @- setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInPixelsX mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setPrimaryStrideInPixelsXSelector value

-- | @- primaryStrideInPixelsY@
primaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInPixelsY mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode primaryStrideInPixelsYSelector

-- | @- setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInPixelsY mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setPrimaryStrideInPixelsYSelector value

-- | @- primaryStrideInFeatureChannels@
primaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInFeatureChannels mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode primaryStrideInFeatureChannelsSelector

-- | @- setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInFeatureChannels mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setPrimaryStrideInFeatureChannelsSelector value

-- | @- secondaryStrideInPixelsX@
secondaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInPixelsX mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode secondaryStrideInPixelsXSelector

-- | @- setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInPixelsX mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setSecondaryStrideInPixelsXSelector value

-- | @- secondaryStrideInPixelsY@
secondaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInPixelsY mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode secondaryStrideInPixelsYSelector

-- | @- setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInPixelsY mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setSecondaryStrideInPixelsYSelector value

-- | @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInFeatureChannels mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode secondaryStrideInFeatureChannelsSelector

-- | @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setSecondaryStrideInFeatureChannelsSelector value

-- | @- minimumValue@
minimumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
minimumValue mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode minimumValueSelector

-- | @- setMinimumValue:@
setMinimumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setMinimumValue mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setMinimumValueSelector value

-- | @- maximumValue@
maximumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
maximumValue mpsnnBinaryArithmeticNode =
  sendMessage mpsnnBinaryArithmeticNode maximumValueSelector

-- | @- setMaximumValue:@
setMaximumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setMaximumValue mpsnnBinaryArithmeticNode value =
  sendMessage mpsnnBinaryArithmeticNode setMaximumValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSources:@
nodeWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNBinaryArithmeticNode)
nodeWithSourcesSelector = mkSelector "nodeWithSources:"

-- | @Selector@ for @nodeWithLeftSource:rightSource:@
nodeWithLeftSource_rightSourceSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode] (Id MPSNNBinaryArithmeticNode)
nodeWithLeftSource_rightSourceSelector = mkSelector "nodeWithLeftSource:rightSource:"

-- | @Selector@ for @initWithSources:@
initWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNBinaryArithmeticNode)
initWithSourcesSelector = mkSelector "initWithSources:"

-- | @Selector@ for @initWithLeftSource:rightSource:@
initWithLeftSource_rightSourceSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode] (Id MPSNNBinaryArithmeticNode)
initWithLeftSource_rightSourceSelector = mkSelector "initWithLeftSource:rightSource:"

-- | @Selector@ for @gradientClass@
gradientClassSelector :: Selector '[] Class
gradientClassSelector = mkSelector "gradientClass"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector '[Id NSArray] (Id NSArray)
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

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

-- | @Selector@ for @primaryStrideInPixelsX@
primaryStrideInPixelsXSelector :: Selector '[] CULong
primaryStrideInPixelsXSelector = mkSelector "primaryStrideInPixelsX"

-- | @Selector@ for @setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsXSelector :: Selector '[CULong] ()
setPrimaryStrideInPixelsXSelector = mkSelector "setPrimaryStrideInPixelsX:"

-- | @Selector@ for @primaryStrideInPixelsY@
primaryStrideInPixelsYSelector :: Selector '[] CULong
primaryStrideInPixelsYSelector = mkSelector "primaryStrideInPixelsY"

-- | @Selector@ for @setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsYSelector :: Selector '[CULong] ()
setPrimaryStrideInPixelsYSelector = mkSelector "setPrimaryStrideInPixelsY:"

-- | @Selector@ for @primaryStrideInFeatureChannels@
primaryStrideInFeatureChannelsSelector :: Selector '[] CULong
primaryStrideInFeatureChannelsSelector = mkSelector "primaryStrideInFeatureChannels"

-- | @Selector@ for @setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannelsSelector :: Selector '[CULong] ()
setPrimaryStrideInFeatureChannelsSelector = mkSelector "setPrimaryStrideInFeatureChannels:"

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

