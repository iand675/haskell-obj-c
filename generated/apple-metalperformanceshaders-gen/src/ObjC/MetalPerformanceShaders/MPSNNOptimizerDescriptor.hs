{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNOptimizerDescriptor
--
-- The MPSNNOptimizerDescriptor base class. Optimizers are generally used to update trainable neural network parameters.              Users are usually expected to call these MPSKernels from the update methods on their Convolution or BatchNormalization data sources.
--
-- Before the gradient is used to update the original value, some preprocessing occurs on each gradient where it is scaled or clipped              If regularization is chosen the appropriate regularization loss gradient is added to the value gradient.
--
-- Generated bindings for @MPSNNOptimizerDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNNOptimizerDescriptor
  ( MPSNNOptimizerDescriptor
  , IsMPSNNOptimizerDescriptor(..)
  , initWithLearningRate_gradientRescale_regularizationType_regularizationScale
  , initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale
  , optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale
  , optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale
  , learningRate
  , setLearningRate
  , gradientRescale
  , setGradientRescale
  , applyGradientClipping
  , setApplyGradientClipping
  , gradientClipMax
  , setGradientClipMax
  , gradientClipMin
  , setGradientClipMin
  , regularizationScale
  , setRegularizationScale
  , regularizationType
  , setRegularizationType
  , applyGradientClippingSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , gradientRescaleSelector
  , initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , learningRateSelector
  , optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , regularizationScaleSelector
  , regularizationTypeSelector
  , setApplyGradientClippingSelector
  , setGradientClipMaxSelector
  , setGradientClipMinSelector
  , setGradientRescaleSelector
  , setLearningRateSelector
  , setRegularizationScaleSelector
  , setRegularizationTypeSelector

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

-- | @- initWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_regularizationType_regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_regularizationType_regularizationScale mpsnnOptimizerDescriptor learningRate gradientRescale regularizationType regularizationScale =
  sendOwnedMessage mpsnnOptimizerDescriptor initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector learningRate gradientRescale regularizationType regularizationScale

-- | @- initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> CFloat -> Bool -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale mpsnnOptimizerDescriptor learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale =
  sendOwnedMessage mpsnnOptimizerDescriptor initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale

-- | @+ optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale :: CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale learningRate gradientRescale regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MPSNNOptimizerDescriptor"
    sendClassMessage cls' optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector learningRate gradientRescale regularizationType regularizationScale

-- | @+ optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale :: CFloat -> CFloat -> Bool -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MPSNNOptimizerDescriptor"
    sendClassMessage cls' optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 0.001f
--
-- ObjC selector: @- learningRate@
learningRate :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
learningRate mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor learningRateSelector

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 0.001f
--
-- ObjC selector: @- setLearningRate:@
setLearningRate :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setLearningRate mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setLearningRateSelector value

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientRescale mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor gradientRescaleSelector

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- setGradientRescale:@
setGradientRescale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientRescale mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setGradientRescaleSelector value

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- applyGradientClipping@
applyGradientClipping :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO Bool
applyGradientClipping mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor applyGradientClippingSelector

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- setApplyGradientClipping:@
setApplyGradientClipping :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> Bool -> IO ()
setApplyGradientClipping mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setApplyGradientClippingSelector value

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientClipMax mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor gradientClipMaxSelector

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- setGradientClipMax:@
setGradientClipMax :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientClipMax mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setGradientClipMaxSelector value

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientClipMin mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor gradientClipMinSelector

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- setGradientClipMin:@
setGradientClipMin :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientClipMin mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setGradientClipMinSelector value

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
regularizationScale mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor regularizationScaleSelector

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- setRegularizationScale:@
setRegularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setRegularizationScale mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setRegularizationScaleSelector value

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO MPSNNRegularizationType
regularizationType mpsnnOptimizerDescriptor =
  sendMessage mpsnnOptimizerDescriptor regularizationTypeSelector

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- setRegularizationType:@
setRegularizationType :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> MPSNNRegularizationType -> IO ()
setRegularizationType mpsnnOptimizerDescriptor value =
  sendMessage mpsnnOptimizerDescriptor setRegularizationTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, MPSNNRegularizationType, CFloat] (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "initWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, Bool, CFloat, CFloat, MPSNNRegularizationType, CFloat] (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, MPSNNRegularizationType, CFloat] (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, Bool, CFloat, CFloat, MPSNNRegularizationType, CFloat] (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector '[] CFloat
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector '[CFloat] ()
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector '[] CFloat
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @setGradientRescale:@
setGradientRescaleSelector :: Selector '[CFloat] ()
setGradientRescaleSelector = mkSelector "setGradientRescale:"

-- | @Selector@ for @applyGradientClipping@
applyGradientClippingSelector :: Selector '[] Bool
applyGradientClippingSelector = mkSelector "applyGradientClipping"

-- | @Selector@ for @setApplyGradientClipping:@
setApplyGradientClippingSelector :: Selector '[Bool] ()
setApplyGradientClippingSelector = mkSelector "setApplyGradientClipping:"

-- | @Selector@ for @gradientClipMax@
gradientClipMaxSelector :: Selector '[] CFloat
gradientClipMaxSelector = mkSelector "gradientClipMax"

-- | @Selector@ for @setGradientClipMax:@
setGradientClipMaxSelector :: Selector '[CFloat] ()
setGradientClipMaxSelector = mkSelector "setGradientClipMax:"

-- | @Selector@ for @gradientClipMin@
gradientClipMinSelector :: Selector '[] CFloat
gradientClipMinSelector = mkSelector "gradientClipMin"

-- | @Selector@ for @setGradientClipMin:@
setGradientClipMinSelector :: Selector '[CFloat] ()
setGradientClipMinSelector = mkSelector "setGradientClipMin:"

-- | @Selector@ for @regularizationScale@
regularizationScaleSelector :: Selector '[] CFloat
regularizationScaleSelector = mkSelector "regularizationScale"

-- | @Selector@ for @setRegularizationScale:@
setRegularizationScaleSelector :: Selector '[CFloat] ()
setRegularizationScaleSelector = mkSelector "setRegularizationScale:"

-- | @Selector@ for @regularizationType@
regularizationTypeSelector :: Selector '[] MPSNNRegularizationType
regularizationTypeSelector = mkSelector "regularizationType"

-- | @Selector@ for @setRegularizationType:@
setRegularizationTypeSelector :: Selector '[MPSNNRegularizationType] ()
setRegularizationTypeSelector = mkSelector "setRegularizationType:"

