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
  , nodeWithSource_convolutionGradientState_weightsSelector
  , initWithSource_convolutionGradientState_weightsSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
        sendClassMsg cls' (mkSelector "nodeWithSource:convolutionGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= retainedObject . castPtr

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
initWithSource_convolutionGradientState_weights mpscnnConvolutionTransposeNode  sourceNode convolutionGradientState weights =
withObjCPtr sourceNode $ \raw_sourceNode ->
  withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
      sendMsg mpscnnConvolutionTransposeNode (mkSelector "initWithSource:convolutionGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:convolutionGradientState:weights:@
nodeWithSource_convolutionGradientState_weightsSelector :: Selector
nodeWithSource_convolutionGradientState_weightsSelector = mkSelector "nodeWithSource:convolutionGradientState:weights:"

-- | @Selector@ for @initWithSource:convolutionGradientState:weights:@
initWithSource_convolutionGradientState_weightsSelector :: Selector
initWithSource_convolutionGradientState_weightsSelector = mkSelector "initWithSource:convolutionGradientState:weights:"

