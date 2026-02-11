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
  , nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector
  , kernelWidthSelector
  , setKernelWidthSelector
  , kernelHeightSelector
  , setKernelHeightSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector


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

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNSpatialNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelSize sourceGradient sourceImage gradientState kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNSpatialNormalizationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSize :: (IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnSpatialNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> IO (Id MPSCNNSpatialNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelSize mpscnnSpatialNormalizationGradientNode  sourceGradient sourceImage gradientState kernelSize =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CULong
kernelWidth mpscnnSpatialNormalizationGradientNode  =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "kernelWidth") retCULong []

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CULong -> IO ()
setKernelWidth mpscnnSpatialNormalizationGradientNode  value =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CULong
kernelHeight mpscnnSpatialNormalizationGradientNode  =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "kernelHeight") retCULong []

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CULong -> IO ()
setKernelHeight mpscnnSpatialNormalizationGradientNode  value =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
alpha mpscnnSpatialNormalizationGradientNode  =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setAlpha mpscnnSpatialNormalizationGradientNode  value =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
beta mpscnnSpatialNormalizationGradientNode  =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setBeta mpscnnSpatialNormalizationGradientNode  value =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> IO CFloat
delta mpscnnSpatialNormalizationGradientNode  =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNSpatialNormalizationGradientNode mpscnnSpatialNormalizationGradientNode => mpscnnSpatialNormalizationGradientNode -> CFloat -> IO ()
setDelta mpscnnSpatialNormalizationGradientNode  value =
  sendMsg mpscnnSpatialNormalizationGradientNode (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelSize:@
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelSize:@
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_kernelSizeSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelSize:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector
setKernelHeightSelector = mkSelector "setKernelHeight:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector
setBetaSelector = mkSelector "setBeta:"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector
setDeltaSelector = mkSelector "setDelta:"

