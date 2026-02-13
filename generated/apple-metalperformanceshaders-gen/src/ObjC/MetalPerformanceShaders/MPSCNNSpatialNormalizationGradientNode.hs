{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNSpatialNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNSpatialNormalizationGradientNode
  ( MPSCNNSpatialNormalizationGradientNode
  , IsMPSCNNSpatialNormalizationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSize
  , initWithSourceGradient_sourceImage_gradientState_kernelSize
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , setAlphaSelector
  , setBetaSelector
  , setDeltaSelector
  , setKernelHeightSelector
  , setKernelWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNSpatialNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSize sourceGradient sourceImage gradientState kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNSpatialNormalizationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelSize

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnSpatialNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNSpatialNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSize mpscnnSpatialNormalizationGradientNode sourceGradient sourceImage gradientState kernelSize =
  sendOwnedMessage mpscnnSpatialNormalizationGradientNode initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelSize

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CULong
kernelWidth mpscnnSpatialNormalizationGradientNode =
  sendMessage mpscnnSpatialNormalizationGradientNode kernelWidthSelector

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CULong -> IO ()
setKernelWidth mpscnnSpatialNormalizationGradientNode value =
  sendMessage mpscnnSpatialNormalizationGradientNode setKernelWidthSelector value

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CULong
kernelHeight mpscnnSpatialNormalizationGradientNode =
  sendMessage mpscnnSpatialNormalizationGradientNode kernelHeightSelector

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CULong -> IO ()
setKernelHeight mpscnnSpatialNormalizationGradientNode value =
  sendMessage mpscnnSpatialNormalizationGradientNode setKernelHeightSelector value

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
alpha mpscnnSpatialNormalizationGradientNode =
  sendMessage mpscnnSpatialNormalizationGradientNode alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setAlpha mpscnnSpatialNormalizationGradientNode value =
  sendMessage mpscnnSpatialNormalizationGradientNode setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
beta mpscnnSpatialNormalizationGradientNode =
  sendMessage mpscnnSpatialNormalizationGradientNode betaSelector

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setBeta mpscnnSpatialNormalizationGradientNode value =
  sendMessage mpscnnSpatialNormalizationGradientNode setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
delta mpscnnSpatialNormalizationGradientNode =
  sendMessage mpscnnSpatialNormalizationGradientNode deltaSelector

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setDelta mpscnnSpatialNormalizationGradientNode value =
  sendMessage mpscnnSpatialNormalizationGradientNode setDeltaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong] (Id MPSCNNSpatialNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong] (Id MPSCNNSpatialNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector '[CULong] ()
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector '[CULong] ()
setKernelHeightSelector = mkSelector "setKernelHeight:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CFloat] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] CFloat
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector '[CFloat] ()
setBetaSelector = mkSelector "setBeta:"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector '[CFloat] ()
setDeltaSelector = mkSelector "setDelta:"

