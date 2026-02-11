{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNConvolutionGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionGradientNode
  ( MPSCNNConvolutionGradientNode
  , IsMPSCNNConvolutionGradientNode(..)
  , nodeWithSourceGradient_sourceImage_convolutionGradientState_weights
  , initWithSourceGradient_sourceImage_convolutionGradientState_weights
  , nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector
  , initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector


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
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= retainedObject . castPtr

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
initWithSourceGradient_sourceImage_convolutionGradientState_weights mpscnnConvolutionGradientNode  sourceGradient sourceImage gradientState weights =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnConvolutionGradientNode (mkSelector "initWithSourceGradient:sourceImage:convolutionGradientState:weights:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:@
nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector :: Selector
nodeWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector = mkSelector "nodeWithSourceGradient:sourceImage:convolutionGradientState:weights:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:convolutionGradientState:weights:@
initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector :: Selector
initWithSourceGradient_sourceImage_convolutionGradientState_weightsSelector = mkSelector "initWithSourceGradient:sourceImage:convolutionGradientState:weights:"

