{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLossNode
--
-- This node calculates loss information during training               typically immediately after the inference portion               of network evaluation is performed. The result image               of the loss operations is typically the first gradient               image to be comsumed by the gradient passes that work               their way back up the graph. In addition, the node will               update the loss image in the MPSNNLabels with the               desired estimate of correctness.
--
-- Generated bindings for @MPSCNNLossNode@.
module ObjC.MetalPerformanceShaders.MPSCNNLossNode
  ( MPSCNNLossNode
  , IsMPSCNNLossNode(..)
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
nodeWithSource_lossDescriptor :: (IsMPSNNImageNode source, IsMPSCNNLossDescriptor descriptor) => source -> descriptor -> IO (Id MPSCNNLossNode)
nodeWithSource_lossDescriptor source descriptor =
  do
    cls' <- getRequiredClass "MPSCNNLossNode"
    sendClassMessage cls' nodeWithSource_lossDescriptorSelector (toMPSNNImageNode source) (toMPSCNNLossDescriptor descriptor)

-- | @- initWithSource:lossDescriptor:@
initWithSource_lossDescriptor :: (IsMPSCNNLossNode mpscnnLossNode, IsMPSNNImageNode source, IsMPSCNNLossDescriptor descriptor) => mpscnnLossNode -> source -> descriptor -> IO (Id MPSCNNLossNode)
initWithSource_lossDescriptor mpscnnLossNode source descriptor =
  sendOwnedMessage mpscnnLossNode initWithSource_lossDescriptorSelector (toMPSNNImageNode source) (toMPSCNNLossDescriptor descriptor)

-- | The loss filter is its own gradient filter and doesn't provide a corresponding gradient node.
--
-- The image returned by the loss filter is the gradient image to be consumed by              the gradient filters corresponding to preceeding inference nodes.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSCNNLossNode mpscnnLossNode, IsNSArray gradientImages) => mpscnnLossNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpscnnLossNode gradientImages =
  sendMessage mpscnnLossNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- | Get the input node for labes and weights, for example to set the handle
--
-- ObjC selector: @- inputLabels@
inputLabels :: IsMPSCNNLossNode mpscnnLossNode => mpscnnLossNode -> IO (Id MPSNNLabelsNode)
inputLabels mpscnnLossNode =
  sendMessage mpscnnLossNode inputLabelsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:lossDescriptor:@
nodeWithSource_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSCNNLossNode)
nodeWithSource_lossDescriptorSelector = mkSelector "nodeWithSource:lossDescriptor:"

-- | @Selector@ for @initWithSource:lossDescriptor:@
initWithSource_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSCNNLossNode)
initWithSource_lossDescriptorSelector = mkSelector "initWithSource:lossDescriptor:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @inputLabels@
inputLabelsSelector :: Selector '[] (Id MPSNNLabelsNode)
inputLabelsSelector = mkSelector "inputLabels"

