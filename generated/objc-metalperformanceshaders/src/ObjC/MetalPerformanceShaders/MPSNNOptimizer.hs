{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNOptimizer
--
-- The MPSNNOptimizer base class, use one of the child classes, not to be directly used. Optimizers are generally used to update trainable neural network parameters.              Users are usually expected to call these MPSKernels from the update methods on their Convolution or BatchNormalization data sources.
--
-- Before the gradient is used to update the original value, some preprocessing occurs on each gradient where it is scaled or clipped              If regularization is chosen the appropriate regularization loss gradient is added to the value gradient.
--
-- Generated bindings for @MPSNNOptimizer@.
module ObjC.MetalPerformanceShaders.MPSNNOptimizer
  ( MPSNNOptimizer
  , IsMPSNNOptimizer(..)
  , initWithDevice
  , setLearningRate
  , learningRate
  , gradientRescale
  , applyGradientClipping
  , setApplyGradientClipping
  , gradientClipMax
  , gradientClipMin
  , regularizationScale
  , regularizationType
  , initWithDeviceSelector
  , setLearningRateSelector
  , learningRateSelector
  , gradientRescaleSelector
  , applyGradientClippingSelector
  , setApplyGradientClippingSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , regularizationScaleSelector
  , regularizationTypeSelector

  -- * Enum types
  , MPSNNRegularizationType(MPSNNRegularizationType)
  , pattern MPSNNRegularizationTypeNone
  , pattern MPSNNRegularizationTypeL1
  , pattern MPSNNRegularizationTypeL2

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> RawId -> IO (Id MPSNNOptimizer)
initWithDevice mpsnnOptimizer  device =
  sendMsg mpsnnOptimizer (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- setLearningRate:@
setLearningRate :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> CFloat -> IO ()
setLearningRate mpsnnOptimizer  newLearningRate =
  sendMsg mpsnnOptimizer (mkSelector "setLearningRate:") retVoid [argCFloat (fromIntegral newLearningRate)]

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 1e-3
--
-- ObjC selector: @- learningRate@
learningRate :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
learningRate mpsnnOptimizer  =
  sendMsg mpsnnOptimizer (mkSelector "learningRate") retCFloat []

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientRescale mpsnnOptimizer  =
  sendMsg mpsnnOptimizer (mkSelector "gradientRescale") retCFloat []

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- applyGradientClipping@
applyGradientClipping :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO Bool
applyGradientClipping mpsnnOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnOptimizer (mkSelector "applyGradientClipping") retCULong []

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- setApplyGradientClipping:@
setApplyGradientClipping :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> Bool -> IO ()
setApplyGradientClipping mpsnnOptimizer  value =
  sendMsg mpsnnOptimizer (mkSelector "setApplyGradientClipping:") retVoid [argCULong (if value then 1 else 0)]

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientClipMax mpsnnOptimizer  =
  sendMsg mpsnnOptimizer (mkSelector "gradientClipMax") retCFloat []

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientClipMin mpsnnOptimizer  =
  sendMsg mpsnnOptimizer (mkSelector "gradientClipMin") retCFloat []

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
regularizationScale mpsnnOptimizer  =
  sendMsg mpsnnOptimizer (mkSelector "regularizationScale") retCFloat []

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO MPSNNRegularizationType
regularizationType mpsnnOptimizer  =
  fmap (coerce :: CULong -> MPSNNRegularizationType) $ sendMsg mpsnnOptimizer (mkSelector "regularizationType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @applyGradientClipping@
applyGradientClippingSelector :: Selector
applyGradientClippingSelector = mkSelector "applyGradientClipping"

-- | @Selector@ for @setApplyGradientClipping:@
setApplyGradientClippingSelector :: Selector
setApplyGradientClippingSelector = mkSelector "setApplyGradientClipping:"

-- | @Selector@ for @gradientClipMax@
gradientClipMaxSelector :: Selector
gradientClipMaxSelector = mkSelector "gradientClipMax"

-- | @Selector@ for @gradientClipMin@
gradientClipMinSelector :: Selector
gradientClipMinSelector = mkSelector "gradientClipMin"

-- | @Selector@ for @regularizationScale@
regularizationScaleSelector :: Selector
regularizationScaleSelector = mkSelector "regularizationScale"

-- | @Selector@ for @regularizationType@
regularizationTypeSelector :: Selector
regularizationTypeSelector = mkSelector "regularizationType"

