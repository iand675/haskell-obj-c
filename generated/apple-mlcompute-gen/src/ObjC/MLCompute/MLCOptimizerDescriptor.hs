{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCOptimizerDescriptor
--
-- The MLCOptimizerDescriptor specifies an optimizer descriptor.
--
-- Generated bindings for @MLCOptimizerDescriptor@.
module ObjC.MLCompute.MLCOptimizerDescriptor
  ( MLCOptimizerDescriptor
  , IsMLCOptimizerDescriptor(..)
  , descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScale
  , learningRate
  , gradientRescale
  , appliesGradientClipping
  , gradientClipMax
  , gradientClipMin
  , regularizationScale
  , regularizationType
  , gradientClippingType
  , maximumClippingNorm
  , customGlobalNorm
  , appliesGradientClippingSelector
  , customGlobalNormSelector
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector
  , descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , gradientClippingTypeSelector
  , gradientRescaleSelector
  , learningRateSelector
  , maximumClippingNormSelector
  , regularizationScaleSelector
  , regularizationTypeSelector

  -- * Enum types
  , MLCGradientClippingType(MLCGradientClippingType)
  , pattern MLCGradientClippingTypeByValue
  , pattern MLCGradientClippingTypeByNorm
  , pattern MLCGradientClippingTypeByGlobalNorm
  , MLCRegularizationType(MLCRegularizationType)
  , pattern MLCRegularizationTypeNone
  , pattern MLCRegularizationTypeL1
  , pattern MLCRegularizationTypeL2

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a MLCOptimizerDescriptor object
--
-- @learningRate@ — The learning rate
--
-- @gradientRescale@ — The gradient rescale value
--
-- @regularizationType@ — The regularization type
--
-- @regularizationScale@ — The regularization scale
--
-- Returns: A new MLCOptimizerDescriptor object.
--
-- ObjC selector: @+ descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale :: CFloat -> CFloat -> MLCRegularizationType -> CFloat -> IO (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScale learningRate gradientRescale regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MLCOptimizerDescriptor"
    sendClassMessage cls' descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector learningRate gradientRescale regularizationType regularizationScale

-- | Create a MLCOptimizerDescriptor object
--
-- @learningRate@ — The learning rate
--
-- @gradientRescale@ — The gradient rescale value
--
-- @appliesGradientClipping@ — Whether to apply gradient clipping
--
-- @gradientClipMax@ — The maximum gradient value to be used with gradient clipping
--
-- @gradientClipMin@ — The minimum gradient value to be used with gradient clipping
--
-- @regularizationType@ — The regularization type
--
-- @regularizationScale@ — The regularization scale
--
-- Returns: A new MLCOptimizerDescriptor object.
--
-- ObjC selector: @+ descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale :: CFloat -> CFloat -> Bool -> CFloat -> CFloat -> MLCRegularizationType -> CFloat -> IO (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScale learningRate gradientRescale appliesGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MLCOptimizerDescriptor"
    sendClassMessage cls' descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector learningRate gradientRescale appliesGradientClipping gradientClipMax gradientClipMin regularizationType regularizationScale

-- | Create an MLCOptimizerDescriptor object
--
-- @learningRate@ — The learning rate
--
-- @gradientRescale@ — The gradient rescale value
--
-- @appliesGradientClipping@ — Whether to apply gradient clipping
--
-- @gradientClippingType@ — The type of clipping applied to gradients
--
-- @gradientClipMax@ — The maximum gradient value to be used with gradient clipping
--
-- @gradientClipMin@ — The minimum gradient value to be used with gradient clipping
--
-- @maximumClippingNorm@ — The maximum norm to be used with gradient clipping
--
-- @customGlobalNorm@ — If non-zero, the norm to be used instead of calculating the global norm
--
-- @regularizationType@ — The regularization type
--
-- @regularizationScale@ — The regularization scale
--
-- Returns: A new MLCOptimizerDescriptor object.
--
-- ObjC selector: @+ descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScale :: CFloat -> CFloat -> Bool -> MLCGradientClippingType -> CFloat -> CFloat -> CFloat -> CFloat -> MLCRegularizationType -> CFloat -> IO (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScale learningRate gradientRescale appliesGradientClipping gradientClippingType gradientClipMax gradientClipMin maximumClippingNorm customGlobalNorm regularizationType regularizationScale =
  do
    cls' <- getRequiredClass "MLCOptimizerDescriptor"
    sendClassMessage cls' descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector learningRate gradientRescale appliesGradientClipping gradientClippingType gradientClipMax gradientClipMin maximumClippingNorm customGlobalNorm regularizationType regularizationScale

-- | learningRate
--
-- The learning rate
--
-- ObjC selector: @- learningRate@
learningRate :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
learningRate mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor learningRateSelector

-- | gradientRescale
--
-- The rescale value applied to gradients during optimizer update
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientRescale mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor gradientRescaleSelector

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- The default is false
--
-- ObjC selector: @- appliesGradientClipping@
appliesGradientClipping :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO Bool
appliesGradientClipping mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor appliesGradientClippingSelector

-- | gradientClipMax
--
-- The maximum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientClipMax mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor gradientClipMaxSelector

-- | gradientClipMin
--
-- The minimum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientClipMin mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor gradientClipMinSelector

-- | regularizationScale
--
-- The regularization scale.
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
regularizationScale mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor regularizationScaleSelector

-- | regularizationType
--
-- The regularization type.
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO MLCRegularizationType
regularizationType mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor regularizationTypeSelector

-- | gradientClippingType
--
-- The type of clipping applied to gradient
--
-- ObjC selector: @- gradientClippingType@
gradientClippingType :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO MLCGradientClippingType
gradientClippingType mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor gradientClippingTypeSelector

-- | maximumClippingNorm
--
-- The maximum clipping value
--
-- ObjC selector: @- maximumClippingNorm@
maximumClippingNorm :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
maximumClippingNorm mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor maximumClippingNormSelector

-- | customGlobalNorm
--
-- Used only with MLCGradientClippingTypeByGlobalNorm. If non zero, this norm will be used in place of global norm.
--
-- ObjC selector: @- customGlobalNorm@
customGlobalNorm :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
customGlobalNorm mlcOptimizerDescriptor =
  sendMessage mlcOptimizerDescriptor customGlobalNormSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, MLCRegularizationType, CFloat] (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, Bool, CFloat, CFloat, MLCRegularizationType, CFloat] (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector :: Selector '[CFloat, CFloat, Bool, MLCGradientClippingType, CFloat, CFloat, CFloat, CFloat, MLCRegularizationType, CFloat] (Id MLCOptimizerDescriptor)
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector '[] CFloat
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector '[] CFloat
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @appliesGradientClipping@
appliesGradientClippingSelector :: Selector '[] Bool
appliesGradientClippingSelector = mkSelector "appliesGradientClipping"

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
regularizationTypeSelector :: Selector '[] MLCRegularizationType
regularizationTypeSelector = mkSelector "regularizationType"

-- | @Selector@ for @gradientClippingType@
gradientClippingTypeSelector :: Selector '[] MLCGradientClippingType
gradientClippingTypeSelector = mkSelector "gradientClippingType"

-- | @Selector@ for @maximumClippingNorm@
maximumClippingNormSelector :: Selector '[] CFloat
maximumClippingNormSelector = mkSelector "maximumClippingNorm"

-- | @Selector@ for @customGlobalNorm@
customGlobalNormSelector :: Selector '[] CFloat
customGlobalNormSelector = mkSelector "customGlobalNorm"

