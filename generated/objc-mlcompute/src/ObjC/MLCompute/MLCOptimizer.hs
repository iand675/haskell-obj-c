{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , learningRateSelector
  , setLearningRateSelector
  , gradientRescaleSelector
  , appliesGradientClippingSelector
  , setAppliesGradientClippingSelector
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

-- | @+ new@
new :: IO (Id MLCOptimizer)
new  =
  do
    cls' <- getRequiredClass "MLCOptimizer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO (Id MLCOptimizer)
init_ mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | learningRate
--
-- The learning rate.  This property is 'readwrite' so that callers can implement a 'decay' during training
--
-- ObjC selector: @- learningRate@
learningRate :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
learningRate mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "learningRate") retCFloat []

-- | learningRate
--
-- The learning rate.  This property is 'readwrite' so that callers can implement a 'decay' during training
--
-- ObjC selector: @- setLearningRate:@
setLearningRate :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> CFloat -> IO ()
setLearningRate mlcOptimizer  value =
  sendMsg mlcOptimizer (mkSelector "setLearningRate:") retVoid [argCFloat (fromIntegral value)]

-- | gradientRescale
--
-- The rescale value applied to gradients during optimizer update
--
-- ObjC selector: @- gradientRescale@
gradientRescale :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientRescale mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "gradientRescale") retCFloat []

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- ObjC selector: @- appliesGradientClipping@
appliesGradientClipping :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO Bool
appliesGradientClipping mlcOptimizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcOptimizer (mkSelector "appliesGradientClipping") retCULong []

-- | appliesGradientClipping
--
-- Whether gradient clipping should be applied or not.
--
-- ObjC selector: @- setAppliesGradientClipping:@
setAppliesGradientClipping :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> Bool -> IO ()
setAppliesGradientClipping mlcOptimizer  value =
  sendMsg mlcOptimizer (mkSelector "setAppliesGradientClipping:") retVoid [argCULong (if value then 1 else 0)]

-- | gradientClipMax
--
-- The maximum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMax@
gradientClipMax :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientClipMax mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "gradientClipMax") retCFloat []

-- | gradientClipMin
--
-- The minimum gradient value if gradient clipping is enabled before gradient is rescaled.
--
-- ObjC selector: @- gradientClipMin@
gradientClipMin :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
gradientClipMin mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "gradientClipMin") retCFloat []

-- | regularizationScale
--
-- The regularization scale.
--
-- ObjC selector: @- regularizationScale@
regularizationScale :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
regularizationScale mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "regularizationScale") retCFloat []

-- | regularizationType
--
-- The regularization type.
--
-- ObjC selector: @- regularizationType@
regularizationType :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO MLCRegularizationType
regularizationType mlcOptimizer  =
  fmap (coerce :: CInt -> MLCRegularizationType) $ sendMsg mlcOptimizer (mkSelector "regularizationType") retCInt []

-- | gradientClippingType
--
-- The type of clipping applied to gradient
--
-- ObjC selector: @- gradientClippingType@
gradientClippingType :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO MLCGradientClippingType
gradientClippingType mlcOptimizer  =
  fmap (coerce :: CInt -> MLCGradientClippingType) $ sendMsg mlcOptimizer (mkSelector "gradientClippingType") retCInt []

-- | maximumClippingNorm
--
-- The maximum clipping value
--
-- ObjC selector: @- maximumClippingNorm@
maximumClippingNorm :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
maximumClippingNorm mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "maximumClippingNorm") retCFloat []

-- | customGlobalNorm
--
-- Used only with MLCGradientClippingTypeByGlobalNorm. If non zero, this norm will be used in place of global norm.
--
-- ObjC selector: @- customGlobalNorm@
customGlobalNorm :: IsMLCOptimizer mlcOptimizer => mlcOptimizer -> IO CFloat
customGlobalNorm mlcOptimizer  =
  sendMsg mlcOptimizer (mkSelector "customGlobalNorm") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @learningRate@
learningRateSelector :: Selector
learningRateSelector = mkSelector "learningRate"

-- | @Selector@ for @setLearningRate:@
setLearningRateSelector :: Selector
setLearningRateSelector = mkSelector "setLearningRate:"

-- | @Selector@ for @gradientRescale@
gradientRescaleSelector :: Selector
gradientRescaleSelector = mkSelector "gradientRescale"

-- | @Selector@ for @appliesGradientClipping@
appliesGradientClippingSelector :: Selector
appliesGradientClippingSelector = mkSelector "appliesGradientClipping"

-- | @Selector@ for @setAppliesGradientClipping:@
setAppliesGradientClippingSelector :: Selector
setAppliesGradientClippingSelector = mkSelector "setAppliesGradientClipping:"

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

