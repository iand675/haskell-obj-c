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
  , nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector
  , initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


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
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCDouble (fromIntegral scaleFactorX), argCDouble (fromIntegral scaleFactorY)] >>= retainedObject . castPtr

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
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorY mpscnnUpsamplingBilinearGradientNode  sourceGradient sourceImage gradientState scaleFactorX scaleFactorY =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnUpsamplingBilinearGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCDouble (fromIntegral scaleFactorX), argCDouble (fromIntegral scaleFactorY)] >>= ownedObject . castPtr

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingBilinearGradientNode mpscnnUpsamplingBilinearGradientNode => mpscnnUpsamplingBilinearGradientNode -> IO CDouble
scaleFactorX mpscnnUpsamplingBilinearGradientNode  =
  sendMsg mpscnnUpsamplingBilinearGradientNode (mkSelector "scaleFactorX") retCDouble []

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingBilinearGradientNode mpscnnUpsamplingBilinearGradientNode => mpscnnUpsamplingBilinearGradientNode -> IO CDouble
scaleFactorY mpscnnUpsamplingBilinearGradientNode  =
  sendMsg mpscnnUpsamplingBilinearGradientNode (mkSelector "scaleFactorY") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:@
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_scaleFactorX_scaleFactorYSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:scaleFactorX:scaleFactorY:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector
scaleFactorYSelector = mkSelector "scaleFactorY"

