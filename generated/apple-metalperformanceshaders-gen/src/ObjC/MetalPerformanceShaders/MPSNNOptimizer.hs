{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , applyGradientClippingSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , gradientRescaleSelector
  , initWithDeviceSelector
  , learningRateSelector
  , regularizationScaleSelector
  , regularizationTypeSelector
  , setApplyGradientClippingSelector
  , setLearningRateSelector

  -- * Enum types
  , MPSNNRegularizationType(MPSNNRegularizationType)
  , pattern MPSNNRegularizationTypeNone
  , pattern MPSNNRegularizationTypeL1
  , pattern MPSNNRegularizationTypeL2

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> RawId -> IO (Id MPSNNOptimizer)
initWithDevice mpsnnOptimizer device =
  sendOwnedMessage mpsnnOptimizer initWithDeviceSelector device

-- | @- setLearningRate:@
setLearningRate :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> CFloat -> IO ()
setLearningRate mpsnnOptimizer newLearningRate =
  sendMessage mpsnnOptimizer setLearningRateSelector newLearningRate

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 1e-3
--
-- ObjC selector: @- learningRate@
learningRate :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
learningRate mpsnnOptimizer =
  sendMessage mpsnnOptimizer learningRateSelector

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientRescale mpsnnOptimizer =
  sendMessage mpsnnOptimizer gradientRescaleSelector

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- applyGradientClipping@
applyGradientClipping :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO Bool
applyGradientClipping mpsnnOptimizer =
  sendMessage mpsnnOptimizer applyGradientClippingSelector

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- setApplyGradientClipping:@
setApplyGradientClipping :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> Bool -> IO ()
setApplyGradientClipping mpsnnOptimizer value =
  sendMessage mpsnnOptimizer setApplyGradientClippingSelector value

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientClipMax mpsnnOptimizer =
  sendMessage mpsnnOptimizer gradientClipMaxSelector

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
gradientClipMin mpsnnOptimizer =
  sendMessage mpsnnOptimizer gradientClipMinSelector

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO CFloat
regularizationScale mpsnnOptimizer =
  sendMessage mpsnnOptimizer regularizationScaleSelector

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMPSNNOptimizer mpsnnOptimizer => mpsnnOptimizer -> IO MPSNNRegularizationType
regularizationType mpsnnOptimizer =
  sendMessage mpsnnOptimizer regularizationTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNOptimizer)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector '[CFloat] ()
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector '[] CFloat
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector '[] CFloat
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @applyGradientClipping@
applyGradientClippingSelector :: Selector '[] Bool
applyGradientClippingSelector = mkSelector "applyGradientClipping"

-- | @Selector@ for @setApplyGradientClipping:@
setApplyGradientClippingSelector :: Selector '[Bool] ()
setApplyGradientClippingSelector = mkSelector "setApplyGradientClipping:"

-- | @Selector@ for @gradientClipMax@
gradientClipMaxSelector :: Selector '[] CFloat
gradientClipMaxSelector = mkSelector "gradientClipMax"

-- | @Selector@ for @gradientClipMin@
gradientClipMinSelector :: Selector '[] CFloat
gradientClipMinSelector = mkSelector "gradientClipMin"

-- | @Selector@ for @regularizationScale@
regularizationScaleSelector :: Selector '[] CFloat
regularizationScaleSelector = mkSelector "regularizationScale"

-- | @Selector@ for @regularizationType@
regularizationTypeSelector :: Selector '[] MPSNNRegularizationType
regularizationTypeSelector = mkSelector "regularizationType"

