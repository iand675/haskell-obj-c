{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNPoolingGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingGradientNode
  ( MPSCNNPoolingGradientNode
  , IsMPSCNNPoolingGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy
  , kernelWidth
  , kernelHeight
  , strideInPixelsX
  , strideInPixelsY
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector


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
-- ObjC selector: @+ nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> RawId -> IO (Id MPSCNNPoolingGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy =
  do
    cls' <- getRequiredClass "MPSCNNPoolingGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy

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
-- ObjC selector: @- initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy :: (IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnPoolingGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> CULong -> CULong -> RawId -> IO (Id MPSCNNPoolingGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicy mpscnnPoolingGradientNode sourceGradient sourceImage gradientState kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy =
  sendOwnedMessage mpscnnPoolingGradientNode initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight strideInPixelsX strideInPixelsY paddingPolicy

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
kernelWidth mpscnnPoolingGradientNode =
  sendMessage mpscnnPoolingGradientNode kernelWidthSelector

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
kernelHeight mpscnnPoolingGradientNode =
  sendMessage mpscnnPoolingGradientNode kernelHeightSelector

-- | @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
strideInPixelsX mpscnnPoolingGradientNode =
  sendMessage mpscnnPoolingGradientNode strideInPixelsXSelector

-- | @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNPoolingGradientNode mpscnnPoolingGradientNode => mpscnnPoolingGradientNode -> IO CULong
strideInPixelsY mpscnnPoolingGradientNode =
  sendMessage mpscnnPoolingGradientNode strideInPixelsYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong, CULong, CULong, RawId] (Id MPSCNNPoolingGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong, CULong, CULong, RawId] (Id MPSCNNPoolingGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_paddingPolicySelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:paddingPolicy:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector '[] CULong
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector '[] CULong
strideInPixelsYSelector = mkSelector "strideInPixelsY"

