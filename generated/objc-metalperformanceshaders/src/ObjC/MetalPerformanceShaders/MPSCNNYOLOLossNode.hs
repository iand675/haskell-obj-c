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
  , nodeWithSource_lossDescriptorSelector
  , initWithSource_lossDescriptorSelector
  , gradientFilterWithSourcesSelector
  , inputLabelsSelector


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

-- | @+ nodeWithSource:lossDescriptor:@
nodeWithSource_lossDescriptor :: (IsMPSNNImageNode source, IsMPSCNNYOLOLossDescriptor descriptor) => source -> descriptor -> IO (Id MPSCNNYOLOLossNode)
nodeWithSource_lossDescriptor source descriptor =
  do
    cls' <- getRequiredClass "MPSCNNYOLOLossNode"
    withObjCPtr source $ \raw_source ->
      withObjCPtr descriptor $ \raw_descriptor ->
        sendClassMsg cls' (mkSelector "nodeWithSource:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:lossDescriptor:@
initWithSource_lossDescriptor :: (IsMPSCNNYOLOLossNode mpscnnyoloLossNode, IsMPSNNImageNode source, IsMPSCNNYOLOLossDescriptor descriptor) => mpscnnyoloLossNode -> source -> descriptor -> IO (Id MPSCNNYOLOLossNode)
initWithSource_lossDescriptor mpscnnyoloLossNode  source descriptor =
withObjCPtr source $ \raw_source ->
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpscnnyoloLossNode (mkSelector "initWithSource:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | The loss filter is its own gradient filter and doesn't provide a corresponding gradient node.
--
-- The image returned by the loss filter is the gradient image to be consumed by              the gradient filters corresponding to preceeding inference nodes.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSCNNYOLOLossNode mpscnnyoloLossNode, IsNSArray gradientImages) => mpscnnyoloLossNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpscnnyoloLossNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpscnnyoloLossNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | Get the input node for labes and weights, for example to set the handle
--
-- ObjC selector: @- inputLabels@
inputLabels :: IsMPSCNNYOLOLossNode mpscnnyoloLossNode => mpscnnyoloLossNode -> IO (Id MPSNNLabelsNode)
inputLabels mpscnnyoloLossNode  =
  sendMsg mpscnnyoloLossNode (mkSelector "inputLabels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:lossDescriptor:@
nodeWithSource_lossDescriptorSelector :: Selector
nodeWithSource_lossDescriptorSelector = mkSelector "nodeWithSource:lossDescriptor:"

-- | @Selector@ for @initWithSource:lossDescriptor:@
initWithSource_lossDescriptorSelector :: Selector
initWithSource_lossDescriptorSelector = mkSelector "initWithSource:lossDescriptor:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @inputLabels@
inputLabelsSelector :: Selector
inputLabelsSelector = mkSelector "inputLabels"

