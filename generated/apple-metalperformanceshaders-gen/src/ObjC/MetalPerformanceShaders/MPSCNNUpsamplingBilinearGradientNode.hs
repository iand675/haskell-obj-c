{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNUpsamplingBilinear kernel
--
-- Generated bindings for @MPSCNNUpsamplingBilinearGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingBilinearGradientNode
  ( MPSCNNUpsamplingBilinearGradientNode
  , IsMPSCNNUpsamplingBilinearGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY
  , initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY
  , scaleFactorX
  , scaleFactorY
  , initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector
  , nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A node to represent the gradient calculation for nearest upsampling training.
--
-- [forwardFilter gradientFilterWithSources:] is a more convient way to do this.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward filter node
--
-- @gradientState@ — The gradient state from the forward filter
--
-- @scaleFactorX@ — The X scale factor from the forward pass
--
-- @scaleFactorY@ — The Y scale factor from the forward pass
--
-- Returns: A MPSCNNUpsamplingBilinearGradientNode
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CDouble -> CDouble -> IO (Id MPSCNNUpsamplingBilinearGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY sourceGradient sourceImage gradientState scaleFactorX scaleFactorY =
  do
    cls' <- getRequiredClass "MPSCNNUpsamplingBilinearGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) scaleFactorX scaleFactorY

-- | A node to represent the gradient calculation for nearest upsampling training.
--
-- [forwardFilter gradientFilterWithSources:] is a more convient way to do this.
--
-- @sourceGradient@ — The input gradient from the 'downstream' gradient filter.
--
-- @sourceImage@ — The input image from the forward filter node
--
-- @gradientState@ — The gradient state from the forward filter
--
-- @scaleFactorX@ — The X scale factor from the forward pass
--
-- @scaleFactorY@ — The Y scale factor from the forward pass
--
-- Returns: A MPSCNNUpsamplingBilinearGradientNode
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY :: (IsMPSCNNUpsamplingBilinearGradientNode mpscnnUpsamplingBilinearGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnUpsamplingBilinearGradientNode -> sourceGradient -> sourceImage -> gradientState -> CDouble -> CDouble -> IO (Id MPSCNNUpsamplingBilinearGradientNode)
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY mpscnnUpsamplingBilinearGradientNode sourceGradient sourceImage gradientState scaleFactorX scaleFactorY =
  sendOwnedMessage mpscnnUpsamplingBilinearGradientNode initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) scaleFactorX scaleFactorY

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingBilinearGradientNode mpscnnUpsamplingBilinearGradientNode => mpscnnUpsamplingBilinearGradientNode -> IO CDouble
scaleFactorX mpscnnUpsamplingBilinearGradientNode =
  sendMessage mpscnnUpsamplingBilinearGradientNode scaleFactorXSelector

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingBilinearGradientNode mpscnnUpsamplingBilinearGradientNode => mpscnnUpsamplingBilinearGradientNode -> IO CDouble
scaleFactorY mpscnnUpsamplingBilinearGradientNode =
  sendMessage mpscnnUpsamplingBilinearGradientNode scaleFactorYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CDouble, CDouble] (Id MPSCNNUpsamplingBilinearGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CDouble, CDouble] (Id MPSCNNUpsamplingBilinearGradientNode)
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector '[] CDouble
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector '[] CDouble
scaleFactorYSelector = mkSelector "scaleFactorY"

