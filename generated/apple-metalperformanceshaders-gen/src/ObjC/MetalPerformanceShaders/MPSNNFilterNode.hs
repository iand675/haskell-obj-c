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
  , initSelector
  , gradientFilterWithSourceSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourcesSelector
  , gradientFiltersWithSourceSelector
  , trainingGraphWithSourceGradient_nodeHandlerSelector
  , resultImageSelector
  , resultStateSelector
  , resultStatesSelector
  , paddingPolicySelector
  , setPaddingPolicySelector
  , labelSelector
  , setLabelSelector


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

-- | @- init@
init_ :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNFilterNode)
init_ mpsnnFilterNode  =
    sendMsg mpsnnFilterNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Return the gradient (backwards) version of this filter.
--
-- The backwards training version of the filter will be returned.              The non-gradient image and state arguments for the filter are automatically              obtained from the target.
--
-- @gradientImage@ — The gradient images corresponding with the resultImage                        of the target
--
-- ObjC selector: @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNFilterNode mpsnnFilterNode, IsMPSNNImageNode gradientImage) => mpsnnFilterNode -> gradientImage -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSource mpsnnFilterNode  gradientImage =
  withObjCPtr gradientImage $ \raw_gradientImage ->
      sendMsg mpsnnFilterNode (mkSelector "gradientFilterWithSource:") (retPtr retVoid) [argPtr (castPtr raw_gradientImage :: Ptr ())] >>= retainedObject . castPtr

-- | Return the gradient (backwards) version of this filter.
--
-- The backwards training version of the filter will be returned.              The non-gradient image and state arguments for the filter are automatically              obtained from the target.
--
-- @gradientImages@ — The gradient images corresponding with the resultImage                        of the target
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSArray gradientImages) => mpsnnFilterNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnFilterNode  gradientImages =
  withObjCPtr gradientImages $ \raw_gradientImages ->
      sendMsg mpsnnFilterNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | Return multiple gradient versions of the filter
--
-- MPSNNFilters that consume multiple inputs generally result in                  multiple conjugate filters for the gradient computation at                  the end of training. For example, a single concatenation operation                  that concatenates multple images will result in an array of slice                  operators that carve out subsections of the input gradient image.
--
-- ObjC selector: @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSArray gradientImages) => mpsnnFilterNode -> gradientImages -> IO (Id NSArray)
gradientFiltersWithSources mpsnnFilterNode  gradientImages =
  withObjCPtr gradientImages $ \raw_gradientImages ->
      sendMsg mpsnnFilterNode (mkSelector "gradientFiltersWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | Return multiple gradient versions of the filter
--
-- MPSNNFilters that consume multiple inputs generally result in                  multiple conjugate filters for the gradient computation at                  the end of training. For example, a single concatenation operation                  that concatenates multple images will result in an array of slice                  operators that carve out subsections of the input gradient image.
--
-- ObjC selector: @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNFilterNode mpsnnFilterNode, IsMPSNNImageNode gradientImage) => mpsnnFilterNode -> gradientImage -> IO (Id NSArray)
gradientFiltersWithSource mpsnnFilterNode  gradientImage =
  withObjCPtr gradientImage $ \raw_gradientImage ->
      sendMsg mpsnnFilterNode (mkSelector "gradientFiltersWithSource:") (retPtr retVoid) [argPtr (castPtr raw_gradientImage :: Ptr ())] >>= retainedObject . castPtr

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
trainingGraphWithSourceGradient_nodeHandler mpsnnFilterNode  gradientImage nodeHandler =
  withObjCPtr gradientImage $ \raw_gradientImage ->
      sendMsg mpsnnFilterNode (mkSelector "trainingGraphWithSourceGradient:nodeHandler:") (retPtr retVoid) [argPtr (castPtr raw_gradientImage :: Ptr ()), argPtr (castPtr nodeHandler :: Ptr ())] >>= retainedObject . castPtr

-- | Get the node representing the image result of the filter
--
-- Except where otherwise noted, the precision used for the               result image (see format property) is copied from the precision               from the first input image node.
--
-- ObjC selector: @- resultImage@
resultImage :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNImageNode)
resultImage mpsnnFilterNode  =
    sendMsg mpsnnFilterNode (mkSelector "resultImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | convenience method for resultStates[0]
--
-- If resultStates is nil, returns nil
--
-- ObjC selector: @- resultState@
resultState :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id MPSNNStateNode)
resultState mpsnnFilterNode  =
    sendMsg mpsnnFilterNode (mkSelector "resultState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the node representing the state result of the filter
--
-- If more than one, see description of subclass for ordering.
--
-- ObjC selector: @- resultStates@
resultStates :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id NSArray)
resultStates mpsnnFilterNode  =
    sendMsg mpsnnFilterNode (mkSelector "resultStates") (retPtr retVoid) [] >>= retainedObject . castPtr

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
paddingPolicy mpsnnFilterNode  =
    fmap (RawId . castPtr) $ sendMsg mpsnnFilterNode (mkSelector "paddingPolicy") (retPtr retVoid) []

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
setPaddingPolicy mpsnnFilterNode  value =
    sendMsg mpsnnFilterNode (mkSelector "setPaddingPolicy:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMPSNNFilterNode mpsnnFilterNode => mpsnnFilterNode -> IO (Id NSString)
label mpsnnFilterNode  =
    sendMsg mpsnnFilterNode (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSNNFilterNode mpsnnFilterNode, IsNSString value) => mpsnnFilterNode -> value -> IO ()
setLabel mpsnnFilterNode  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpsnnFilterNode (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

-- | @Selector@ for @trainingGraphWithSourceGradient:nodeHandler:@
trainingGraphWithSourceGradient_nodeHandlerSelector :: Selector
trainingGraphWithSourceGradient_nodeHandlerSelector = mkSelector "trainingGraphWithSourceGradient:nodeHandler:"

-- | @Selector@ for @resultImage@
resultImageSelector :: Selector
resultImageSelector = mkSelector "resultImage"

-- | @Selector@ for @resultState@
resultStateSelector :: Selector
resultStateSelector = mkSelector "resultState"

-- | @Selector@ for @resultStates@
resultStatesSelector :: Selector
resultStatesSelector = mkSelector "resultStates"

-- | @Selector@ for @paddingPolicy@
paddingPolicySelector :: Selector
paddingPolicySelector = mkSelector "paddingPolicy"

-- | @Selector@ for @setPaddingPolicy:@
setPaddingPolicySelector :: Selector
setPaddingPolicySelector = mkSelector "setPaddingPolicy:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

