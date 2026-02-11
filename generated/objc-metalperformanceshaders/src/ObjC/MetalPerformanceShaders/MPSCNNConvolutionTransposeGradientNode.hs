{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNConvolutionTransposeGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTransposeGradientNode
  ( MPSCNNConvolutionTransposeGradientNode
  , IsMPSCNNConvolutionTransposeGradientNode(..)
  , nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights
  , nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector
  , initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector


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
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= retainedObject . castPtr

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
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weights mpscnnConvolutionTransposeGradientNode  sourceGradient sourceImage gradientState weights =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnConvolutionTransposeGradientNode (mkSelector "initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector :: Selector
nodeWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector = mkSelector "nodeWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector :: Selector
initWithSourceGradient_sourceImage_convolutionTransposeGradientState_weightsSelector = mkSelector "initWithSourceGradient:sourceImage:convolutionTransposeGradientState:weights:"

