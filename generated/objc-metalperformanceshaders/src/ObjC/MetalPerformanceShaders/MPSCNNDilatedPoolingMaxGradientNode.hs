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
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector
  , dilationRateXSelector
  , dilationRateYSelector


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
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY), argCULong (fromIntegral dilationRateX), argCULong (fromIntegral dilationRateY)] >>= retainedObject . castPtr

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
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY mpscnnDilatedPoolingMaxGradientNode  sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnDilatedPoolingMaxGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY), argCULong (fromIntegral dilationRateX), argCULong (fromIntegral dilationRateY)] >>= ownedObject . castPtr

-- | @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMaxGradientNode mpscnnDilatedPoolingMaxGradientNode => mpscnnDilatedPoolingMaxGradientNode -> IO CULong
dilationRateX mpscnnDilatedPoolingMaxGradientNode  =
  sendMsg mpscnnDilatedPoolingMaxGradientNode (mkSelector "dilationRateX") retCULong []

-- | @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMaxGradientNode mpscnnDilatedPoolingMaxGradientNode => mpscnnDilatedPoolingMaxGradientNode -> IO CULong
dilationRateY mpscnnDilatedPoolingMaxGradientNode  =
  sendMsg mpscnnDilatedPoolingMaxGradientNode (mkSelector "dilationRateY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector
dilationRateYSelector = mkSelector "dilationRateY"

