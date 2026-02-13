{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A MPSNNFilterNode representing a MPSCNNConvolutionTranspose kernel
--
-- Generated bindings for @MPSCNNConvolutionTransposeNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTransposeNode
  ( MPSCNNConvolutionTransposeNode
  , IsMPSCNNConvolutionTransposeNode(..)
  , nodeWithSource_convolutionGradientState_weights
  , initWithSource_convolutionGradientState_weights
  , convolutionGradientState
  , convolutionGradientStateSelector
  , initWithSource_convolutionGradientState_weightsSelector
  , nodeWithSource_convolutionGradientState_weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init an autoreleased not representing a MPSCNNConvolutionTransposeNode kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @convolutionGradientState@ — When the convolution transpose is used to 'undo' an earlier convolution                                      in the graph, it is generally desired that the output image be the same                                      size as the input image to the earlier convolution. You may optionally                                       specify this size identity by passing in the MPSNNConvolutionGradientStateNode                                      created by the convolution node here.
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- Returns: A new MPSNNFilter node for a MPSCNNConvolutionTransposeNode kernel.
--
-- ObjC selector: @+ nodeWithSource:convolutionGradientState:weights:@
nodeWithSource_convolutionGradientState_weights :: (IsMPSNNImageNode sourceNode, IsMPSCNNConvolutionGradientStateNode convolutionGradientState) => sourceNode -> convolutionGradientState -> RawId -> IO (Id MPSCNNConvolutionTransposeNode)
nodeWithSource_convolutionGradientState_weights sourceNode convolutionGradientState weights =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionTransposeNode"
    sendClassMessage cls' nodeWithSource_convolutionGradientState_weightsSelector (toMPSNNImageNode sourceNode) (toMPSCNNConvolutionGradientStateNode convolutionGradientState) weights

-- | Init a node representing a MPSCNNConvolutionTransposeNode kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @convolutionGradientState@ — When the convolution transpose is used to 'undo' an earlier convolution                                      in the graph, it is generally desired that the output image be the same                                      size as the input image to the earlier convolution. You may optionally                                      specify this size identity by passing in the MPSCNNConvolutionGradientState node                                      here.
--
-- @weights@ — A pointer to a valid object conforming to the MPSCNNConvolutionDataSource                                      protocol. This object is provided by you to encapsulate storage for                                      convolution weights and biases.
--
-- Returns: A new MPSNNFilter node for a MPSCNNConvolutionTransposeNode kernel.
--
-- ObjC selector: @- initWithSource:convolutionGradientState:weights:@
initWithSource_convolutionGradientState_weights :: (IsMPSCNNConvolutionTransposeNode mpscnnConvolutionTransposeNode, IsMPSNNImageNode sourceNode, IsMPSCNNConvolutionGradientStateNode convolutionGradientState) => mpscnnConvolutionTransposeNode -> sourceNode -> convolutionGradientState -> RawId -> IO (Id MPSCNNConvolutionTransposeNode)
initWithSource_convolutionGradientState_weights mpscnnConvolutionTransposeNode sourceNode convolutionGradientState weights =
  sendOwnedMessage mpscnnConvolutionTransposeNode initWithSource_convolutionGradientState_weightsSelector (toMPSNNImageNode sourceNode) (toMPSCNNConvolutionGradientStateNode convolutionGradientState) weights

-- | unavailable
--
-- ObjC selector: @- convolutionGradientState@
convolutionGradientState :: IsMPSCNNConvolutionTransposeNode mpscnnConvolutionTransposeNode => mpscnnConvolutionTransposeNode -> IO RawId
convolutionGradientState mpscnnConvolutionTransposeNode =
  sendMessage mpscnnConvolutionTransposeNode convolutionGradientStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:convolutionGradientState:weights:@
nodeWithSource_convolutionGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNConvolutionGradientStateNode, RawId] (Id MPSCNNConvolutionTransposeNode)
nodeWithSource_convolutionGradientState_weightsSelector = mkSelector "nodeWithSource:convolutionGradientState:weights:"

-- | @Selector@ for @initWithSource:convolutionGradientState:weights:@
initWithSource_convolutionGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSCNNConvolutionGradientStateNode, RawId] (Id MPSCNNConvolutionTransposeNode)
initWithSource_convolutionGradientState_weightsSelector = mkSelector "initWithSource:convolutionGradientState:weights:"

-- | @Selector@ for @convolutionGradientState@
convolutionGradientStateSelector :: Selector '[] RawId
convolutionGradientStateSelector = mkSelector "convolutionGradientState"

