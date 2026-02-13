{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNDilatedPoolingMaxGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNDilatedPoolingMaxGradientNode
  ( MPSCNNDilatedPoolingMaxGradientNode
  , IsMPSCNNDilatedPoolingMaxGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY
  , dilationRateX
  , dilationRateY
  , dilationRateXSelector
  , dilationRateYSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | make a pooling gradient node
--
-- It would be much easier to use [inferencePoolingNode gradientNodeForSourceGradient:] instead.
--
-- @sourceGradient@ — The gradient from the downstream gradient filter.
--
-- @sourceImage@ — The input image to the inference pooling filter
--
-- @gradientState@ — The gradient state produced by the inference poolin filter
--
-- @kernelWidth@ — The kernel width of the inference filter
--
-- @kernelHeight@ — The kernel height of the inference filter
--
-- @strideInPixelsX@ — The X stride from the inference filter
--
-- @strideInPixelsY@ — The Y stride from the inference filter
--
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY =
  do
    cls' <- getRequiredClass "MPSCNNDilatedPoolingMaxGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY

-- | make a pooling gradient node
--
-- It would be much easier to use [inferencePoolingNode gradientNodeForSourceGradient:] instead.
--
-- @sourceGradient@ — The gradient from the downstream gradient filter.
--
-- @sourceImage@ — The input image to the inference pooling filter
--
-- @gradientState@ — The gradient state produced by the inference poolin filter
--
-- @kernelWidth@ — The kernel width of the inference filter
--
-- @kernelHeight@ — The kernel height of the inference filter
--
-- @strideInPixelsX@ — The X stride from the inference filter
--
-- @strideInPixelsY@ — The Y stride from the inference filter
--
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY :: (IsMPSCNNDilatedPoolingMaxGradientNode mpscnnDilatedPoolingMaxGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnDilatedPoolingMaxGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY mpscnnDilatedPoolingMaxGradientNode sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY =
  sendOwnedMessage mpscnnDilatedPoolingMaxGradientNode initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY

-- | @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMaxGradientNode mpscnnDilatedPoolingMaxGradientNode => mpscnnDilatedPoolingMaxGradientNode -> IO CULong
dilationRateX mpscnnDilatedPoolingMaxGradientNode =
  sendMessage mpscnnDilatedPoolingMaxGradientNode dilationRateXSelector

-- | @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMaxGradientNode mpscnnDilatedPoolingMaxGradientNode => mpscnnDilatedPoolingMaxGradientNode -> IO CULong
dilationRateY mpscnnDilatedPoolingMaxGradientNode =
  sendMessage mpscnnDilatedPoolingMaxGradientNode dilationRateYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector '[] CULong
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector '[] CULong
dilationRateYSelector = mkSelector "dilationRateY"

