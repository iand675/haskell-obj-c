{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNConvolutionGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionGradientNode
  ( MPSCNNConvolutionGradientNode
  , IsMPSCNNConvolutionGradientNode(..)
  , nodeWithSourceGradient_sourceImage_convolutionGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector
  , nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A node to represent the gradient calculation for convolution training.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter. Often                          that is a neuron gradient filter node.
--
-- @sourceImage@ — The input image from the forward convolution node
--
-- @gradientState@ — The gradient state from the forward convolution
--
-- @weights@ — The data source from the forward convolution. It may not contain                          an integrated neuron. Similary, any normalization should be                          broken out into a separate node. Pass nil to use the weights                          from the forward convolution pass.
--
-- Returns: A MPSCNNConvolutionGradientNode
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionGradientState_weights :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSCNNConvolutionGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> RawId -> IO (Id MPSCNNConvolutionGradientNode)
nodeWithSourceGradient_sourceImage_convolutionGradientState_weights sourceGradient sourceImage gradientState weights =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSCNNConvolutionGradientStateNode gradientState) weights

-- | A node to represent the gradient calculation for convolution training.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter. Often                          that is a neuron gradient filter node.
--
-- @sourceImage@ — The input image from the forward convolution node
--
-- @gradientState@ — The gradient state from the forward convolution
--
-- @weights@ — The data source from the forward convolution. It may not contain                          an integrated neuron. Similary, any normalization should be                          broken out into a separate node. Pass nil to use the weights                          from the forward convolution pass.
--
-- Returns: A MPSCNNConvolutionGradientNode
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:convolutionGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionGradientState_weights :: (IsMPSCNNConvolutionGradientNode mpscnnConvolutionGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSCNNConvolutionGradientStateNode gradientState) => mpscnnConvolutionGradientNode -> sourceGradient -> sourceImage -> gradientState -> RawId -> IO (Id MPSCNNConvolutionGradientNode)
initWithSourceGradient_sourceImage_convolutionGradientState_weights mpscnnConvolutionGradientNode sourceGradient sourceImage gradientState weights =
  sendOwnedMessage mpscnnConvolutionGradientNode initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSCNNConvolutionGradientStateNode gradientState) weights

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNConvolutionGradientStateNode, RawId] (Id MPSCNNConvolutionGradientNode)
nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector = mkSelector "nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:convolutionGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNConvolutionGradientStateNode, RawId] (Id MPSCNNConvolutionGradientNode)
initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector = mkSelector "initWithSourceGradient:sourceImage:convolutionGradientState:weights:"

