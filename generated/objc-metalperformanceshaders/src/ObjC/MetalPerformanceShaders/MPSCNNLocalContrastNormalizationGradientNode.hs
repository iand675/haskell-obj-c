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
  , nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector
  , initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector
  , p0Selector
  , setP0Selector
  , pmSelector
  , setPmSelector
  , psSelector
  , setPsSelector
  , kernelWidthSelector
  , kernelHeightSelector


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

-- | @+ nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalizationGradientNode)
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight sourceGradient sourceImage gradientState kernelWidth kernelHeight =
  do
    cls' <- getRequiredClass "MPSCNNLocalContrastNormalizationGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr gradientState $ \raw_gradientState ->
          sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight :: (IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNGradientStateNode gradientState) => mpscnnLocalContrastNormalizationGradientNode -> sourceGradient -> sourceImage -> gradientState -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalizationGradientNode)
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeight mpscnnLocalContrastNormalizationGradientNode  sourceGradient sourceImage gradientState kernelWidth kernelHeight =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr gradientState $ \raw_gradientState ->
        sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= ownedObject . castPtr

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
alpha mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setAlpha mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
beta mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setBeta mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
delta mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setDelta mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- p0@
p0 :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
p0 mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "p0") retCFloat []

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setP0:") retVoid [argCFloat (fromIntegral value)]

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- pm@
pm :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
pm mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "pm") retCFloat []

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setPm:") retVoid [argCFloat (fromIntegral value)]

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- ps@
ps :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CFloat
ps mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "ps") retCFloat []

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalizationGradientNode  value =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "setPs:") retVoid [argCFloat (fromIntegral value)]

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CULong
kernelWidth mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "kernelWidth") retCULong []

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNLocalContrastNormalizationGradientNode mpscnnLocalContrastNormalizationGradientNode => mpscnnLocalContrastNormalizationGradientNode -> IO CULong
kernelHeight mpscnnLocalContrastNormalizationGradientNode  =
  sendMsg mpscnnLocalContrastNormalizationGradientNode (mkSelector "kernelHeight") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector :: Selector
nodeWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector = mkSelector "nodeWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:@
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector :: Selector
initWithSourceGradient_sourceImage_gradientState_kernelWidth_kernelHeightSelector = mkSelector "initWithSourceGradient:sourceImage:gradientState:kernelWidth:kernelHeight:"

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

-- | @Selector@ for @p0@
p0Selector :: Selector
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector
setP0Selector = mkSelector "setP0:"

-- | @Selector@ for @pm@
pmSelector :: Selector
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector
setPsSelector = mkSelector "setPs:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

