{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNYOLOLossNode
--
-- This node calculates loss information during training               typically immediately after the inference portion               of network evaluation is performed. The result image               of the loss operations is typically the first gradient               image to be comsumed by the gradient passes that work               their way back up the graph. In addition, the node will               update the loss image in the MPSNNLabels with the               desired estimate of correctness.
--
-- Generated bindings for @MPSCNNYOLOLossNode@.
module ObjC.MetalPerformanceShaders.MPSCNNYOLOLossNode
  ( MPSCNNYOLOLossNode
  , IsMPSCNNYOLOLossNode(..)
  , nodeWithSource_lossDescriptor
  , initWithSource_lossDescriptor
  , gradientFilterWithSources
  , inputLabels
  , gradientFilterWithSourcesSelector
  , initWithSource_lossDescriptorSelector
  , inputLabelsSelector
  , nodeWithSource_lossDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:lossDescriptor:@
nodeWithSource_lossDescriptor :: (IsMPSNNImageNode source, IsMPSCNNYOLOLossDescriptor descriptor) => source -> descriptor -> IO (Id MPSCNNYOLOLossNode)
nodeWithSource_lossDescriptor source descriptor =
  do
    cls' <- getRequiredClass "MPSCNNYOLOLossNode"
    sendClassMessage cls' nodeWithSource_lossDescriptorSelector (toMPSNNImageNode source) (toMPSCNNYOLOLossDescriptor descriptor)

-- | @- initWithSource:lossDescriptor:@
initWithSource_lossDescriptor :: (IsMPSCNNYOLOLossNode mpscnnyoloLossNode, IsMPSNNImageNode source, IsMPSCNNYOLOLossDescriptor descriptor) => mpscnnyoloLossNode -> source -> descriptor -> IO (Id MPSCNNYOLOLossNode)
initWithSource_lossDescriptor mpscnnyoloLossNode source descriptor =
  sendOwnedMessage mpscnnyoloLossNode initWithSource_lossDescriptorSelector (toMPSNNImageNode source) (toMPSCNNYOLOLossDescriptor descriptor)

-- | The loss filter is its own gradient filter and doesn't provide a corresponding gradient node.
--
-- The image returned by the loss filter is the gradient image to be consumed by              the gradient filters corresponding to preceeding inference nodes.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSCNNYOLOLossNode mpscnnyoloLossNode, IsNSArray gradientImages) => mpscnnyoloLossNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpscnnyoloLossNode gradientImages =
  sendMessage mpscnnyoloLossNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- | Get the input node for labes and weights, for example to set the handle
--
-- ObjC selector: @- inputLabels@
inputLabels :: IsMPSCNNYOLOLossNode mpscnnyoloLossNode => mpscnnyoloLossNode -> IO (Id MPSNNLabelsNode)
inputLabels mpscnnyoloLossNode =
  sendMessage mpscnnyoloLossNode inputLabelsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:lossDescriptor:@
nodeWithSource_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNYOLOLossDescriptor] (Id MPSCNNYOLOLossNode)
nodeWithSource_lossDescriptorSelector = mkSelector "nodeWithSource:lossDescriptor:"

-- | @Selector@ for @initWithSource:lossDescriptor:@
initWithSource_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNYOLOLossDescriptor] (Id MPSCNNYOLOLossNode)
initWithSource_lossDescriptorSelector = mkSelector "initWithSource:lossDescriptor:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @inputLabels@
inputLabelsSelector :: Selector '[] (Id MPSNNLabelsNode)
inputLabelsSelector = mkSelector "inputLabels"

