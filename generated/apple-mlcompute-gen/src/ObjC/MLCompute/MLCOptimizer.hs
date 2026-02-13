{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCOptimizer
--
-- The MLCOptimizer specifies a base optimizer.
--
-- Generated bindings for @MLCOptimizer@.
module ObjC.MLCompute.MLCOptimizer
  ( MLCOptimizer
  , IsMLCOptimizer(..)
  , new
  , init_
  , learningRate
  , setLearningRate
  , gradientRescale
  , appliesGradientClipping
  , setAppliesGradientClipping
  , gradientClipMax
  , gradientClipMin
  , regularizationScale
  , regularizationType
  , gradientClippingType
  , maximumClippingNorm
  , customGlobalNorm
  , appliesGradientClippingSelector
  , customGlobalNormSelector
  , gradientClipMaxSelector
  , gradientClipMinSelector
  , gradientClippingTypeSelector
  , gradientRescaleSelector
  , initSelector
  , learningRateSelector
  , maximumClippingNormSelector
  , newSelector
  , regularizationScaleSelector
  , regularizationTypeSelector
  , setAppliesGradientClippingSelector
  , setLearningRateSelector

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

-- | @+ new@
new :: IO (Id MLCOptimizer)
new  =
  do
    cls' <- getRequiredClass "MLCOptimizer"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO (Id MLCOptimizer)
init_ mlcOptimizer =
  sendOwnedMessage mlcOptimizer initSelector

-- | learningRate
--
-- The learning rate.  This property is 'readwrite' so that callers can implement a 'decay' during training
--
-- ObjC selector: @- learningRate@
learningRate :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
learningRate mlcOptimizer =
  sendMessage mlcOptimizer learningRateSelector

-- | learningRate
--
-- The learning rate.  This property is 'readwrite' so that callers can implement a 'decay' during training
--
-- ObjC selector: @- setLearningRate:@
setLearningRate :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> CFloat -> IO ()
setLearningRate mlcOptimizer value =
  sendMessage mlcOptimizer setLearningRateSelector value

-- | gradientRescale
--
-- The rescale value applied to gradients during optimizer update
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientRescale mlcOptimizer =
  sendMessage mlcOptimizer gradientRescaleSelector

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- ObjC selector: @- appliesGradientClipping@
appliesGradientClipping :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO Bool
appliesGradientClipping mlcOptimizer =
  sendMessage mlcOptimizer appliesGradientClippingSelector

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- ObjC selector: @- setAppliesGradientClipping:@
setAppliesGradientClipping :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> Bool -> IO ()
setAppliesGradientClipping mlcOptimizer value =
  sendMessage mlcOptimizer setAppliesGradientClippingSelector value

-- | gradientClipMax
--
-- The maximum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientClipMax mlcOptimizer =
  sendMessage mlcOptimizer gradientClipMaxSelector

-- | gradientClipMin
--
-- The minimum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientClipMin mlcOptimizer =
  sendMessage mlcOptimizer gradientClipMinSelector

-- | regularizationScale
--
-- The regularization scale.
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
regularizationScale mlcOptimizer =
  sendMessage mlcOptimizer regularizationScaleSelector

-- | regularizationType
--
-- The regularization type.
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO MLCRegularizationType
regularizationType mlcOptimizer =
  sendMessage mlcOptimizer regularizationTypeSelector

-- | gradientClippingType
--
-- The type of clipping applied to gradient
--
-- ObjC selector: @- gradientClippingType@
gradientClippingType :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO MLCGradientClippingType
gradientClippingType mlcOptimizer =
  sendMessage mlcOptimizer gradientClippingTypeSelector

-- | maximumClippingNorm
--
-- The maximum clipping value
--
-- ObjC selector: @- maximumClippingNorm@
maximumClippingNorm :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
maximumClippingNorm mlcOptimizer =
  sendMessage mlcOptimizer maximumClippingNormSelector

-- | customGlobalNorm
--
-- Used only with MLCGradientClippingTypeByGlobalNorm. If non zero, this norm will be used in place of global norm.
--
-- ObjC selector: @- customGlobalNorm@
customGlobalNorm :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
customGlobalNorm mlcOptimizer =
  sendMessage mlcOptimizer customGlobalNormSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCOptimizer)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCOptimizer)
initSelector = mkSelector "init"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector '[] CFloat
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector '[CFloat] ()
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector '[] CFloat
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @appliesGradientClipping@
appliesGradientClippingSelector :: Selector '[] Bool
appliesGradientClippingSelector = mkSelector "appliesGradientClipping"

-- | @Selector@ for @setAppliesGradientClipping:@
setAppliesGradientClippingSelector :: Selector '[Bool] ()
setAppliesGradientClippingSelector = mkSelector "setAppliesGradientClipping:"

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

