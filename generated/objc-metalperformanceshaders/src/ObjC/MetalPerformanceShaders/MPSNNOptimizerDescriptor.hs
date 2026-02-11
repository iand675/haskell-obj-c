{-# LANGUAGE PatternSynonyms #-}
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
  , initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , learningRateSelector
  , setLearningRateSelector
  , gradientRescaleSelector
  , setGradientRescaleSelector
  , applyGradientClippingSelector
  , setApplyGradientClippingSelector
  , gradientClipMaxSelector
  , setGradientClipMaxSelector
  , gradientClipMinSelector
  , setGradientClipMinSelector
  , regularizationScaleSelector
  , setRegularizationScaleSelector
  , regularizationTypeSelector
  , setRegularizationTypeSelector

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

-- | @- initWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_regularizationType_regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_regularizationType_regularizationScale mpsnnOptimizerDescriptor  learningRate gradientRescale regularizationType regularizationScale =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "initWithLearningRate:gradientRescale:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= ownedObject . castPtr

-- | @- initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> CFloat -> Bool -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale mpsnnOptimizerDescriptor  learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (if applyGradientClipping then 1 else 0), argCFloat (fromIntegral gradientClipMax), argCFloat (fromIntegral gradientClipMin), argCULong (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= ownedObject . castPtr

-- | @+ optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale :: CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale learningRate gradientRescale regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MPSNNOptimizerDescriptor"
    sendClassMsg cls' (mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= retainedObject . castPtr

-- | @+ optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale :: CFloat -> CFloat -> Bool -> CFloat -> CFloat -> MPSNNRegularizationType -> CFloat -> IO (Id MPSNNOptimizerDescriptor)
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale learningRate gradientRescale applyGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MPSNNOptimizerDescriptor"
    sendClassMsg cls' (mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (if applyGradientClipping then 1 else 0), argCFloat (fromIntegral gradientClipMax), argCFloat (fromIntegral gradientClipMin), argCULong (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= retainedObject . castPtr

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 0.001f
--
-- ObjC selector: @- learningRate@
learningRate :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
learningRate mpsnnOptimizerDescriptor  =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "learningRate") retCFloat []

-- | learningRate
--
-- The learningRate at which we update values
--
-- The default value is 0.001f
--
-- ObjC selector: @- setLearningRate:@
setLearningRate :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setLearningRate mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setLearningRate:") retVoid [argCFloat (fromIntegral value)]

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientRescale mpsnnOptimizerDescriptor  =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "gradientRescale") retCFloat []

-- | gradientRescale
--
-- The gradientRescale at which we apply to incoming gradient values
--
-- The default value is 1.0
--
-- ObjC selector: @- setGradientRescale:@
setGradientRescale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientRescale mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setGradientRescale:") retVoid [argCFloat (fromIntegral value)]

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- applyGradientClipping@
applyGradientClipping :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO Bool
applyGradientClipping mpsnnOptimizerDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnOptimizerDescriptor (mkSelector "applyGradientClipping") retCULong []

-- | applyGradientClipping
--
-- A bool which decides if gradient will be clipped
--
-- The default value is NO
--
-- ObjC selector: @- setApplyGradientClipping:@
setApplyGradientClipping :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> Bool -> IO ()
setApplyGradientClipping mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setApplyGradientClipping:") retVoid [argCULong (if value then 1 else 0)]

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientClipMax mpsnnOptimizerDescriptor  =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "gradientClipMax") retCFloat []

-- | gradientClipMax
--
-- The maximum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- setGradientClipMax:@
setGradientClipMax :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientClipMax mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setGradientClipMax:") retVoid [argCFloat (fromIntegral value)]

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
gradientClipMin mpsnnOptimizerDescriptor  =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "gradientClipMin") retCFloat []

-- | gradientClipMin
--
-- The minimum value at which incoming gradient will be clipped before rescaling, applyGradientClipping must be true
--
-- ObjC selector: @- setGradientClipMin:@
setGradientClipMin :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setGradientClipMin mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setGradientClipMin:") retVoid [argCFloat (fromIntegral value)]

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO CFloat
regularizationScale mpsnnOptimizerDescriptor  =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "regularizationScale") retCFloat []

-- | regularizationScale
--
-- The regularizationScale at which we apply L1 or L2 regularization, it gets ignored if regularization is None
--
-- The default value is 0.0
--
-- ObjC selector: @- setRegularizationScale:@
setRegularizationScale :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> CFloat -> IO ()
setRegularizationScale mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setRegularizationScale:") retVoid [argCFloat (fromIntegral value)]

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> IO MPSNNRegularizationType
regularizationType mpsnnOptimizerDescriptor  =
  fmap (coerce :: CULong -> MPSNNRegularizationType) $ sendMsg mpsnnOptimizerDescriptor (mkSelector "regularizationType") retCULong []

-- | regularizationType
--
-- The regularizationType which we apply.
--
-- The default value is MPSRegularizationTypeNone
--
-- ObjC selector: @- setRegularizationType:@
setRegularizationType :: IsMPSNNOptimizerDescriptor mpsnnOptimizerDescriptor => mpsnnOptimizerDescriptor -> MPSNNRegularizationType -> IO ()
setRegularizationType mpsnnOptimizerDescriptor  value =
  sendMsg mpsnnOptimizerDescriptor (mkSelector "setRegularizationType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector
initWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "initWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector
initWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "initWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector
optimizerDescriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector
optimizerDescriptorWithLearningRate_gradientRescale_applyGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "optimizerDescriptorWithLearningRate:gradientRescale:applyGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @setGradientRescale:@
setGradientRescaleSelector :: Selector
setGradientRescaleSelector = mkSelector "setGradientRescale:"

-- | @Selector@ for @applyGradientClipping@
applyGradientClippingSelector :: Selector
applyGradientClippingSelector = mkSelector "applyGradientClipping"

-- | @Selector@ for @setApplyGradientClipping:@
setApplyGradientClippingSelector :: Selector
setApplyGradientClippingSelector = mkSelector "setApplyGradientClipping:"

-- | @Selector@ for @gradientClipMax@
gradientClipMaxSelector :: Selector
gradientClipMaxSelector = mkSelector "gradientClipMax"

-- | @Selector@ for @setGradientClipMax:@
setGradientClipMaxSelector :: Selector
setGradientClipMaxSelector = mkSelector "setGradientClipMax:"

-- | @Selector@ for @gradientClipMin@
gradientClipMinSelector :: Selector
gradientClipMinSelector = mkSelector "gradientClipMin"

-- | @Selector@ for @setGradientClipMin:@
setGradientClipMinSelector :: Selector
setGradientClipMinSelector = mkSelector "setGradientClipMin:"

-- | @Selector@ for @regularizationScale@
regularizationScaleSelector :: Selector
regularizationScaleSelector = mkSelector "regularizationScale"

-- | @Selector@ for @setRegularizationScale:@
setRegularizationScaleSelector :: Selector
setRegularizationScaleSelector = mkSelector "setRegularizationScale:"

-- | @Selector@ for @regularizationType@
regularizationTypeSelector :: Selector
regularizationTypeSelector = mkSelector "regularizationType"

-- | @Selector@ for @setRegularizationType:@
setRegularizationTypeSelector :: Selector
setRegularizationTypeSelector = mkSelector "setRegularizationType:"

