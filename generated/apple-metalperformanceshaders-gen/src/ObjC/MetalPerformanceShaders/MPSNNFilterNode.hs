{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNFilterNode
--
-- A placeholder node denoting a neural network filter stage
--
-- There are as many MPSNNFilterNode subclasses as there are              MPS neural network filter objects. Make one of those.               This class defines an polymorphic interface for them.
--
-- Generated bindings for @MPSNNFilterNode@.
module ObjC.MetalPerformanceShaders.MPSNNFilterNode
  ( MPSNNFilterNode
  , IsMPSNNFilterNode(..)
  , init_
  , gradientFilterWithSource
  , gradientFilterWithSources
  , gradientFiltersWithSources
  , gradientFiltersWithSource
  , trainingGraphWithSourceGradient_nodeHandler
  , resultImage
  , resultState
  , resultStates
  , paddingPolicy
  , setPaddingPolicy
  , label
  , setLabel
  , gradientFilterWithSourceSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourceSelector
  , gradientFiltersWithSourcesSelector
  , initSelector
  , labelSelector
  , paddingPolicySelector
  , resultImageSelector
  , resultStateSelector
  , resultStatesSelector
  , setLabelSelector
  , setPaddingPolicySelector
  , trainingGraphWithSourceGradient_nodeHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNFilterNode)
init_ mpsnnFilterNode =
  sendOwnedMessage mpsnnFilterNode initSelector

-- | Return the gradient (backwards) version of this filter.
--
-- The backwards training version of the filter will be returned.              The non-gradient image and state arguments for the filter are automatically              obtained from the target.
--
-- @gradientImage@ — The gradient images corresponding with the resultImage                        of the target
--
-- ObjC selector: @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNFilterNode mpsnnFilterNode, IsMPSNNImageNode gradientImage) => mpsnnFilterNode -> gradientImage -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSource mpsnnFilterNode gradientImage =
  sendMessage mpsnnFilterNode gradientFilterWithSourceSelector (toMPSNNImageNode gradientImage)

-- | Return the gradient (backwards) version of this filter.
--
-- The backwards training version of the filter will be returned.              The non-gradient image and state arguments for the filter are automatically              obtained from the target.
--
-- @gradientImages@ — The gradient images corresponding with the resultImage                        of the target
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSArray gradientImages) => mpsnnFilterNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnFilterNode gradientImages =
  sendMessage mpsnnFilterNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- | Return multiple gradient versions of the filter
--
-- MPSNNFilters that consume multiple inputs generally result in                  multiple conjugate filters for the gradient computation at                  the end of training. For example, a single concatenation operation                  that concatenates multple images will result in an array of slice                  operators that carve out subsections of the input gradient image.
--
-- ObjC selector: @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSArray gradientImages) => mpsnnFilterNode -> gradientImages -> IO (Id NSArray)
gradientFiltersWithSources mpsnnFilterNode gradientImages =
  sendMessage mpsnnFilterNode gradientFiltersWithSourcesSelector (toNSArray gradientImages)

-- | Return multiple gradient versions of the filter
--
-- MPSNNFilters that consume multiple inputs generally result in                  multiple conjugate filters for the gradient computation at                  the end of training. For example, a single concatenation operation                  that concatenates multple images will result in an array of slice                  operators that carve out subsections of the input gradient image.
--
-- ObjC selector: @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNFilterNode mpsnnFilterNode, IsMPSNNImageNode gradientImage) => mpsnnFilterNode -> gradientImage -> IO (Id NSArray)
gradientFiltersWithSource mpsnnFilterNode gradientImage =
  sendMessage mpsnnFilterNode gradientFiltersWithSourceSelector (toMPSNNImageNode gradientImage)

-- | Build training graph from inference graph
--
-- This method will iteratively build the training portion of a graph based                  on an inference graph. Self should be the last node in the                  inference graph. It is typically a loss layer, but can be anything.                  Typically, the "inference graph" used here is the desired inference                  graph with a dropout node and a loss layer node appended.
--
-- The nodes that are created will have default properties. In certain cases,                  these may not be appropriate (e.g. if you want to do CPU based updates                  of convolution weights instead of default GPU updates.) In such cases, your                  application should use the nodeHandler to configure the new nodes as they are                  created.
--
-- BUG: This method can not follow links to regions of the graph that are                  connected to the rest of the graph solely via MPSNNStateNodes. A gradient                  image input is required to construct a MPSNNGradientFilterNode from a                  inference filter node.
--
-- @gradientImage@ — The input gradient image for the first gradient                                  node in the training section of the graph. If nil,                                  self.resultImage is used. This results in a standard monolithic                                  training graph. If the graph is instead divided into multiple                                  subgraphs (potentially to allow for your custom code to appear                                  inbetween MPSNNGraph segments) a new MPSImageNode*                                  may be substituted.
--
-- @nodeHandler@ — An optional block to allow for customization of gradient                                  nodes and intermediate images as the graph is constructed.                                  It may also be used to prune braches of the developing                                  training graph. If nil, the default handler is used. It builds                                  the full graph, and assigns any inferenceNodeSources[i].handle                                  to their gradient counterparts.
--
-- Returns: The list of new MPSNNFilterNode training graph termini. These MPSNNFilterNodes                  are not necessarily all MPSNNGradientFilterNodes. To build a full list of nodes                  created, use a custom nodeHandler. If no nodes are created nil is returned.
--
-- ObjC selector: @- trainingGraphWithSourceGradient:nodeHandler:@
trainingGraphWithSourceGradient_nodeHandler :: (IsMPSNNFilterNode mpsnnFilterNode, IsMPSNNImageNode gradientImage) => mpsnnFilterNode -> gradientImage -> Ptr () -> IO (Id NSArray)
trainingGraphWithSourceGradient_nodeHandler mpsnnFilterNode gradientImage nodeHandler =
  sendMessage mpsnnFilterNode trainingGraphWithSourceGradient_nodeHandlerSelector (toMPSNNImageNode gradientImage) nodeHandler

-- | Get the node representing the image result of the filter
--
-- Except where otherwise noted, the precision used for the               result image (see format property) is copied from the precision               from the first input image node.
--
-- ObjC selector: @- resultImage@
resultImage :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNImageNode)
resultImage mpsnnFilterNode =
  sendMessage mpsnnFilterNode resultImageSelector

-- | convenience method for resultStates[0]
--
-- If resultStates is nil, returns nil
--
-- ObjC selector: @- resultState@
resultState :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNStateNode)
resultState mpsnnFilterNode =
  sendMessage mpsnnFilterNode resultStateSelector

-- | Get the node representing the state result of the filter
--
-- If more than one, see description of subclass for ordering.
--
-- ObjC selector: @- resultStates@
resultStates :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id NSArray)
resultStates mpsnnFilterNode =
  sendMessage mpsnnFilterNode resultStatesSelector

-- | The padding method used for the filter node
--
-- The padding policy configures how the filter centers              the region of interest in the source image. It principally              is responsible for setting the MPSCNNKernel.offset and              the size of the image produced, and sometimes will also              configure .sourceFeatureChannelOffset, .sourceFeatureChannelMaxCount,              and .edgeMode.  It is permitted to set any other filter properties              as needed using a custom padding policy. The default padding              policy varies per filter to conform to consensus expectation for              the behavior of that filter.  In some cases, pre-made padding              policies are provided to match the behavior of common neural              networking frameworks with particularly complex or unexpected              behavior for specific nodes. See MPSNNDefaultPadding class methods              in MPSNeuralNetworkTypes.h for more.
--
-- BUG: MPS doesn't provide a good way to reset the MPSKernel properties              in the context of a MPSNNGraph after the kernel is finished encoding.              These values carry on to the next time the graph is used. Consequently,              if your custom padding policy modifies the property as a function of the              previous value, e.g.:
--
-- kernel.someProperty += 2;
--
-- then the second time the graph runs, the property may have an inconsistent              value, leading to unexpected behavior. The default padding computation              runs before the custom padding method to provide it with a sense of              what is expected for the default configuration and will reinitialize the value              in the case of the .offset. However, that computation usually doesn't reset              other properties. In such cases, the custom padding policy may need to keep              a record of the original value to enable consistent behavior.
--
-- ObjC selector: @- paddingPolicy@
paddingPolicy :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO RawId
paddingPolicy mpsnnFilterNode =
  sendMessage mpsnnFilterNode paddingPolicySelector

-- | The padding method used for the filter node
--
-- The padding policy configures how the filter centers              the region of interest in the source image. It principally              is responsible for setting the MPSCNNKernel.offset and              the size of the image produced, and sometimes will also              configure .sourceFeatureChannelOffset, .sourceFeatureChannelMaxCount,              and .edgeMode.  It is permitted to set any other filter properties              as needed using a custom padding policy. The default padding              policy varies per filter to conform to consensus expectation for              the behavior of that filter.  In some cases, pre-made padding              policies are provided to match the behavior of common neural              networking frameworks with particularly complex or unexpected              behavior for specific nodes. See MPSNNDefaultPadding class methods              in MPSNeuralNetworkTypes.h for more.
--
-- BUG: MPS doesn't provide a good way to reset the MPSKernel properties              in the context of a MPSNNGraph after the kernel is finished encoding.              These values carry on to the next time the graph is used. Consequently,              if your custom padding policy modifies the property as a function of the              previous value, e.g.:
--
-- kernel.someProperty += 2;
--
-- then the second time the graph runs, the property may have an inconsistent              value, leading to unexpected behavior. The default padding computation              runs before the custom padding method to provide it with a sense of              what is expected for the default configuration and will reinitialize the value              in the case of the .offset. However, that computation usually doesn't reset              other properties. In such cases, the custom padding policy may need to keep              a record of the original value to enable consistent behavior.
--
-- ObjC selector: @- setPaddingPolicy:@
setPaddingPolicy :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> RawId -> IO ()
setPaddingPolicy mpsnnFilterNode value =
  sendMessage mpsnnFilterNode setPaddingPolicySelector value

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id NSString)
label mpsnnFilterNode =
  sendMessage mpsnnFilterNode labelSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSString value) => mpsnnFilterNode -> value -> IO ()
setLabel mpsnnFilterNode value =
  sendMessage mpsnnFilterNode setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNNFilterNode)
initSelector = mkSelector "init"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNGradientFilterNode)
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector '[Id NSArray] (Id NSArray)
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id NSArray)
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

-- | @Selector@ for @trainingGraphWithSourceGradient:nodeHandler:@
trainingGraphWithSourceGradient_nodeHandlerSelector :: Selector '[Id MPSNNImageNode, Ptr ()] (Id NSArray)
trainingGraphWithSourceGradient_nodeHandlerSelector = mkSelector "trainingGraphWithSourceGradient:nodeHandler:"

-- | @Selector@ for @resultImage@
resultImageSelector :: Selector '[] (Id MPSNNImageNode)
resultImageSelector = mkSelector "resultImage"

-- | @Selector@ for @resultState@
resultStateSelector :: Selector '[] (Id MPSNNStateNode)
resultStateSelector = mkSelector "resultState"

-- | @Selector@ for @resultStates@
resultStatesSelector :: Selector '[] (Id NSArray)
resultStatesSelector = mkSelector "resultStates"

-- | @Selector@ for @paddingPolicy@
paddingPolicySelector :: Selector '[] RawId
paddingPolicySelector = mkSelector "paddingPolicy"

-- | @Selector@ for @setPaddingPolicy:@
setPaddingPolicySelector :: Selector '[RawId] ()
setPaddingPolicySelector = mkSelector "setPaddingPolicy:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

