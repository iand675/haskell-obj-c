{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNLocalContrastNormalizationGradientNode@.
module ObjC.MetalPerformanceShaders.MPSCNNLocalContrastNormalizationGradientNode
  ( MPSCNNLocalContrastNormalizationGradientNode
  , IsMPSCNNLocalContrastNormalizationGradientNode(..)
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , p0
  , setP0
  , pm
  , setPm
  , ps
  , setPs
  , kernelWidth
  , kernelHeight
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector
  , p0Selector
  , pmSelector
  , psSelector
  , setAlphaSelector
  , setBetaSelector
  , setDeltaSelector
  , setP0Selector
  , setPmSelector
  , setPsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight sourceGradient sourceImage gradientState kernelWidth kernelHeight =
  do
    cls' <- getRequiredClass "MPSCNNLocalContrastNormalizationGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight :: (IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnLocalContrastNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight mpscnnLocalContrastNormalizationGradientNode sourceGradient sourceImage gradientState kernelWidth kernelHeight =
  sendOwnedMessage mpscnnLocalContrastNormalizationGradientNode initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNGradientStateNode gradientState) kernelWidth kernelHeight

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
alpha mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setAlpha mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
beta mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode betaSelector

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setBeta mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
delta mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode deltaSelector

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setDelta mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setDeltaSelector value

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- p0@
p0 :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
p0 mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode p0Selector

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setP0Selector value

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- pm@
pm :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
pm mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode pmSelector

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setPmSelector value

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- ps@
ps :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
ps mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode psSelector

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalizationGradientNode value =
  sendMessage mpscnnLocalContrastNormalizationGradientNode setPsSelector value

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CULong
kernelWidth mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode kernelWidthSelector

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CULong
kernelHeight mpscnnLocalContrastNormalizationGradientNode =
  sendMessage mpscnnLocalContrastNormalizationGradientNode kernelHeightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong] (Id MPSCNNLocalContrastNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, CULong, CULong] (Id MPSCNNLocalContrastNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:"

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

-- | @Selector@ for @p0@
p0Selector :: Selector '[] CFloat
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector '[CFloat] ()
setP0Selector = mkSelector "setP0:"

-- | @Selector@ for @pm@
pmSelector :: Selector '[] CFloat
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector '[CFloat] ()
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector '[] CFloat
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector '[CFloat] ()
setPsSelector = mkSelector "setPs:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

