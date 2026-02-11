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
  , nodeWithSourcesSelector
  , nodeWithLeftSource_rightSourceSelector
  , initWithSourcesSelector
  , initWithLeftSource_rightSourceSelector
  , gradientClassSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourcesSelector
  , primaryScaleSelector
  , setPrimaryScaleSelector
  , secondaryScaleSelector
  , setSecondaryScaleSelector
  , biasSelector
  , setBiasSelector
  , primaryStrideInPixelsXSelector
  , setPrimaryStrideInPixelsXSelector
  , primaryStrideInPixelsYSelector
  , setPrimaryStrideInPixelsYSelector
  , primaryStrideInFeatureChannelsSelector
  , setPrimaryStrideInFeatureChannelsSelector
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

-- | create an autoreleased arithemtic node with an array of sources
--
-- @sourceNodes@ — A valid NSArray containing two sources
--
-- ObjC selector: @+ nodeWithSources:@
nodeWithSources :: IsNSArray sourceNodes => sourceNodes -> IO (Id MPSNNBinaryArithmeticNode)
nodeWithSources sourceNodes =
  do
    cls' <- getRequiredClass "MPSNNBinaryArithmeticNode"
    withObjCPtr sourceNodes $ \raw_sourceNodes ->
      sendClassMsg cls' (mkSelector "nodeWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr left $ \raw_left ->
      withObjCPtr right $ \raw_right ->
        sendClassMsg cls' (mkSelector "nodeWithLeftSource:rightSource:") (retPtr retVoid) [argPtr (castPtr raw_left :: Ptr ()), argPtr (castPtr raw_right :: Ptr ())] >>= retainedObject . castPtr

-- | init an arithemtic node with an array of sources
--
-- @sourceNodes@ — A valid NSArray containing two sources
--
-- ObjC selector: @- initWithSources:@
initWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray sourceNodes) => mpsnnBinaryArithmeticNode -> sourceNodes -> IO (Id MPSNNBinaryArithmeticNode)
initWithSources mpsnnBinaryArithmeticNode  sourceNodes =
withObjCPtr sourceNodes $ \raw_sourceNodes ->
    sendMsg mpsnnBinaryArithmeticNode (mkSelector "initWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ())] >>= ownedObject . castPtr

-- | init an arithemtic node with two sources
--
-- @left@ — the left operand
--
-- @right@ — the right operand
--
-- ObjC selector: @- initWithLeftSource:rightSource:@
initWithLeftSource_rightSource :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsMPSNNImageNode left, IsMPSNNImageNode right) => mpsnnBinaryArithmeticNode -> left -> right -> IO (Id MPSNNBinaryArithmeticNode)
initWithLeftSource_rightSource mpsnnBinaryArithmeticNode  left right =
withObjCPtr left $ \raw_left ->
  withObjCPtr right $ \raw_right ->
      sendMsg mpsnnBinaryArithmeticNode (mkSelector "initWithLeftSource:rightSource:") (retPtr retVoid) [argPtr (castPtr raw_left :: Ptr ()), argPtr (castPtr raw_right :: Ptr ())] >>= ownedObject . castPtr

-- | @- gradientClass@
gradientClass :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO Class
gradientClass mpsnnBinaryArithmeticNode  =
  fmap (Class . castPtr) $ sendMsg mpsnnBinaryArithmeticNode (mkSelector "gradientClass") (retPtr retVoid) []

-- | @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray gradientImages) => mpsnnBinaryArithmeticNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnBinaryArithmeticNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpsnnBinaryArithmeticNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | create new arithmetic gradient nodes
--
-- Create two new arithmetic gradient nodes - one that computes the gradient for the primary  source image and one that computes the gradient for the secondary sourcefrom the inference pass.
--
-- ObjC selector: @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode, IsNSArray gradientImages) => mpsnnBinaryArithmeticNode -> gradientImages -> IO (Id NSArray)
gradientFiltersWithSources mpsnnBinaryArithmeticNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpsnnBinaryArithmeticNode (mkSelector "gradientFiltersWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | @- primaryScale@
primaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
primaryScale mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "primaryScale") retCFloat []

-- | @- setPrimaryScale:@
setPrimaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setPrimaryScale mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setPrimaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- secondaryScale@
secondaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
secondaryScale mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "secondaryScale") retCFloat []

-- | @- setSecondaryScale:@
setSecondaryScale :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setSecondaryScale mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setSecondaryScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- bias@
bias :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
bias mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "bias") retCFloat []

-- | @- setBias:@
setBias :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setBias mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setBias:") retVoid [argCFloat (fromIntegral value)]

-- | @- primaryStrideInPixelsX@
primaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInPixelsX mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "primaryStrideInPixelsX") retCULong []

-- | @- setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInPixelsX mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setPrimaryStrideInPixelsX:") retVoid [argCULong (fromIntegral value)]

-- | @- primaryStrideInPixelsY@
primaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInPixelsY mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "primaryStrideInPixelsY") retCULong []

-- | @- setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInPixelsY mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setPrimaryStrideInPixelsY:") retVoid [argCULong (fromIntegral value)]

-- | @- primaryStrideInFeatureChannels@
primaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
primaryStrideInFeatureChannels mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "primaryStrideInFeatureChannels") retCULong []

-- | @- setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setPrimaryStrideInFeatureChannels mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setPrimaryStrideInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryStrideInPixelsX@
secondaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInPixelsX mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "secondaryStrideInPixelsX") retCULong []

-- | @- setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsX :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInPixelsX mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setSecondaryStrideInPixelsX:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryStrideInPixelsY@
secondaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInPixelsY mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "secondaryStrideInPixelsY") retCULong []

-- | @- setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsY :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInPixelsY mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setSecondaryStrideInPixelsY:") retVoid [argCULong (fromIntegral value)]

-- | @- secondaryStrideInFeatureChannels@
secondaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CULong
secondaryStrideInFeatureChannels mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "secondaryStrideInFeatureChannels") retCULong []

-- | @- setSecondaryStrideInFeatureChannels:@
setSecondaryStrideInFeatureChannels :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CULong -> IO ()
setSecondaryStrideInFeatureChannels mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setSecondaryStrideInFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | @- minimumValue@
minimumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
minimumValue mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "minimumValue") retCFloat []

-- | @- setMinimumValue:@
setMinimumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setMinimumValue mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setMinimumValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- maximumValue@
maximumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> IO CFloat
maximumValue mpsnnBinaryArithmeticNode  =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "maximumValue") retCFloat []

-- | @- setMaximumValue:@
setMaximumValue :: IsMPSNNBinaryArithmeticNode mpsnnBinaryArithmeticNode => mpsnnBinaryArithmeticNode -> CFloat -> IO ()
setMaximumValue mpsnnBinaryArithmeticNode  value =
  sendMsg mpsnnBinaryArithmeticNode (mkSelector "setMaximumValue:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSources:@
nodeWithSourcesSelector :: Selector
nodeWithSourcesSelector = mkSelector "nodeWithSources:"

-- | @Selector@ for @nodeWithLeftSource:rightSource:@
nodeWithLeftSource_rightSourceSelector :: Selector
nodeWithLeftSource_rightSourceSelector = mkSelector "nodeWithLeftSource:rightSource:"

-- | @Selector@ for @initWithSources:@
initWithSourcesSelector :: Selector
initWithSourcesSelector = mkSelector "initWithSources:"

-- | @Selector@ for @initWithLeftSource:rightSource:@
initWithLeftSource_rightSourceSelector :: Selector
initWithLeftSource_rightSourceSelector = mkSelector "initWithLeftSource:rightSource:"

-- | @Selector@ for @gradientClass@
gradientClassSelector :: Selector
gradientClassSelector = mkSelector "gradientClass"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

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

-- | @Selector@ for @primaryStrideInPixelsX@
primaryStrideInPixelsXSelector :: Selector
primaryStrideInPixelsXSelector = mkSelector "primaryStrideInPixelsX"

-- | @Selector@ for @setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsXSelector :: Selector
setPrimaryStrideInPixelsXSelector = mkSelector "setPrimaryStrideInPixelsX:"

-- | @Selector@ for @primaryStrideInPixelsY@
primaryStrideInPixelsYSelector :: Selector
primaryStrideInPixelsYSelector = mkSelector "primaryStrideInPixelsY"

-- | @Selector@ for @setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsYSelector :: Selector
setPrimaryStrideInPixelsYSelector = mkSelector "setPrimaryStrideInPixelsY:"

-- | @Selector@ for @primaryStrideInFeatureChannels@
primaryStrideInFeatureChannelsSelector :: Selector
primaryStrideInFeatureChannelsSelector = mkSelector "primaryStrideInFeatureChannels"

-- | @Selector@ for @setPrimaryStrideInFeatureChannels:@
setPrimaryStrideInFeatureChannelsSelector :: Selector
setPrimaryStrideInFeatureChannelsSelector = mkSelector "setPrimaryStrideInFeatureChannels:"

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

