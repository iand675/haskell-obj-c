{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector
  , descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector
  , learningRateSelector
  , gradientRescaleSelector
  , appliesGradientClippingSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , regularizationScaleSelector
  , regularizationTypeSelector
  , gradientClippingTypeSelector
  , maximumClippingNormSelector
  , customGlobalNormSelector

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
    sendClassMsg cls' (mkSelector "descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCInt (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (if appliesGradientClipping then 1 else 0), argCFloat (fromIntegral gradientClipMax), argCFloat (fromIntegral gradientClipMin), argCInt (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:") (retPtr retVoid) [argCFloat (fromIntegral learningRate), argCFloat (fromIntegral gradientRescale), argCULong (if appliesGradientClipping then 1 else 0), argCInt (coerce gradientClippingType), argCFloat (fromIntegral gradientClipMax), argCFloat (fromIntegral gradientClipMin), argCFloat (fromIntegral maximumClippingNorm), argCFloat (fromIntegral customGlobalNorm), argCInt (coerce regularizationType), argCFloat (fromIntegral regularizationScale)] >>= retainedObject . castPtr

-- | learningRate
--
-- The learning rate
--
-- ObjC selector: @- learningRate@
learningRate :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
learningRate mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "learningRate") retCFloat []

-- | gradientRescale
--
-- The rescale value applied to gradients during optimizer update
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientRescale mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "gradientRescale") retCFloat []

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- The default is false
--
-- ObjC selector: @- appliesGradientClipping@
appliesGradientClipping :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO Bool
appliesGradientClipping mlcOptimizerDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcOptimizerDescriptor (mkSelector "appliesGradientClipping") retCULong []

-- | gradientClipMax
--
-- The maximum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientClipMax mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "gradientClipMax") retCFloat []

-- | gradientClipMin
--
-- The minimum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
gradientClipMin mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "gradientClipMin") retCFloat []

-- | regularizationScale
--
-- The regularization scale.
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
regularizationScale mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "regularizationScale") retCFloat []

-- | regularizationType
--
-- The regularization type.
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO MLCRegularizationType
regularizationType mlcOptimizerDescriptor  =
  fmap (coerce :: CInt -> MLCRegularizationType) $ sendMsg mlcOptimizerDescriptor (mkSelector "regularizationType") retCInt []

-- | gradientClippingType
--
-- The type of clipping applied to gradient
--
-- ObjC selector: @- gradientClippingType@
gradientClippingType :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO MLCGradientClippingType
gradientClippingType mlcOptimizerDescriptor  =
  fmap (coerce :: CInt -> MLCGradientClippingType) $ sendMsg mlcOptimizerDescriptor (mkSelector "gradientClippingType") retCInt []

-- | maximumClippingNorm
--
-- The maximum clipping value
--
-- ObjC selector: @- maximumClippingNorm@
maximumClippingNorm :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
maximumClippingNorm mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "maximumClippingNorm") retCFloat []

-- | customGlobalNorm
--
-- Used only with MLCGradientClippingTypeByGlobalNorm. If non zero, this norm will be used in place of global norm.
--
-- ObjC selector: @- customGlobalNorm@
customGlobalNorm :: IsMLCOptimizerDescriptor mlcOptimizerDescriptor => mlcOptimizerDescriptor -> IO CFloat
customGlobalNorm mlcOptimizerDescriptor  =
  sendMsg mlcOptimizerDescriptor (mkSelector "customGlobalNorm") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector :: Selector
descriptorWithLearningRate_gradientRescale_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:regularizationType:regularizationScale:"

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector :: Selector
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClipMax_gradientClipMin_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClipMax:gradientClipMin:regularizationType:regularizationScale:"

-- | @Selector@ for @descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:@
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector :: Selector
descriptorWithLearningRate_gradientRescale_appliesGradientClipping_gradientClippingType_gradientClipMax_gradientClipMin_maximumClippingNorm_customGlobalNorm_regularizationType_regularizationScaleSelector = mkSelector "descriptorWithLearningRate:gradientRescale:appliesGradientClipping:gradientClippingType:gradientClipMax:gradientClipMin:maximumClippingNorm:customGlobalNorm:regularizationType:regularizationScale:"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @appliesGradientClipping@
appliesGradientClippingSelector :: Selector
appliesGradientClippingSelector = mkSelector "appliesGradientClipping"

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

-- | @Selector@ for @gradientClippingType@
gradientClippingTypeSelector :: Selector
gradientClippingTypeSelector = mkSelector "gradientClippingType"

-- | @Selector@ for @maximumClippingNorm@
maximumClippingNormSelector :: Selector
maximumClippingNormSelector = mkSelector "maximumClippingNorm"

-- | @Selector@ for @customGlobalNorm@
customGlobalNormSelector :: Selector
customGlobalNormSelector = mkSelector "customGlobalNorm"

