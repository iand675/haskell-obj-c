{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNConvolutionTransposeGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTransposeGradientNode
  ( MPSCNNConvolutionTransposeGradientNode
  , IsMPSCNNConvolutionTransposeGradientNode(..)
  , nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector
  , nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A node to represent the gradient calculation for convolution transpose training.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter. Often                          that is a neuron gradient filter node.
--
-- @sourceImage@ — The input image from the forward convolution transpose node
--
-- @gradientState@ — The gradient state from the forward convolution transpose
--
-- @weights@ — The data source from the forward convolution transpose. It may not contain                          an integrated neuron. Similary, any normalization should be                          broken out into a separate node. Pass nil to use the weights                          from the forward convolution transpose pass.
--
-- Returns: A MPSCNNConvolutionTransposeGradientNode
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSCNNConvolutionTransposeGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> RawId -> IO (Id MPSCNNConvolutionTransposeGradientNode)
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights sourceGradient sourceImage gradientState weights =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionTransposeGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSCNNConvolutionTransposeGradientStateNode gradientState) weights

-- | A node to represent the gradient calculation for convolution transpose training.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter. Often                          that is a neuron gradient filter node.
--
-- @sourceImage@ — The input image from the forward convolution transpose node
--
-- @gradientState@ — The gradient state from the forward convolution transpose
--
-- @weights@ — The data source from the forward convolution transpose. It may not contain                          an integrated neuron. Similary, any normalization should be                          broken out into a separate node. Pass nil to use the weights                          from the forward convolution transpose pass.
--
-- Returns: A MPSCNNConvolutionTransposeGradientNode
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights :: (IsMPSCNNConvolutionTransposeGradientNode mpscnnConvolutionTransposeGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSCNNConvolutionTransposeGradientStateNode gradientState) => mpscnnConvolutionTransposeGradientNode -> sourceGradient -> sourceImage -> gradientState -> RawId -> IO (Id MPSCNNConvolutionTransposeGradientNode)
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights mpscnnConvolutionTransposeGradientNode sourceGradient sourceImage gradientState weights =
  sendOwnedMessage mpscnnConvolutionTransposeGradientNode initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSCNNConvolutionTransposeGradientStateNode gradientState) weights

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNConvolutionTransposeGradientStateNode, RawId] (Id MPSCNNConvolutionTransposeGradientNode)
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector = mkSelector "nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNConvolutionTransposeGradientStateNode, RawId] (Id MPSCNNConvolutionTransposeGradientNode)
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector = mkSelector "initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:"

