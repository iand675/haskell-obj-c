{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCBatchNormalizationLayer
--
-- A batch normalizaion layer
--
-- Generated bindings for @MLCBatchNormalizationLayer@.
module ObjC.MLCompute.MLCBatchNormalizationLayer
  ( MLCBatchNormalizationLayer
  , IsMLCBatchNormalizationLayer(..)
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentum
  , featureChannelCount
  , mean
  , variance
  , beta
  , gamma
  , betaParameter
  , gammaParameter
  , varianceEpsilon
  , momentum
  , betaParameterSelector
  , betaSelector
  , featureChannelCountSelector
  , gammaParameterSelector
  , gammaSelector
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector
  , meanSelector
  , momentumSelector
  , varianceEpsilonSelector
  , varianceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a batch normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @mean@ — The mean tensor
--
-- @variance@ — The variance tensor
--
-- @beta@ — The beta tensor
--
-- @gamma@ — The gamma tensor
--
-- @varianceEpsilon@ — The  epslion value
--
-- Returns: A new batch normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon :: (IsMLCTensor mean, IsMLCTensor variance, IsMLCTensor beta, IsMLCTensor gamma) => CULong -> mean -> variance -> beta -> gamma -> CFloat -> IO (Id MLCBatchNormalizationLayer)
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon featureChannelCount mean variance beta gamma varianceEpsilon =
  do
    cls' <- getRequiredClass "MLCBatchNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector featureChannelCount (toMLCTensor mean) (toMLCTensor variance) (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon

-- | Create a batch normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @mean@ — The mean tensor
--
-- @variance@ — The variance tensor
--
-- @beta@ — The beta tensor
--
-- @gamma@ — The gamma tensor
--
-- @varianceEpsilon@ — The  epslion value
--
-- @momentum@ — The  momentum value for the running mean and variance computation
--
-- Returns: A new batch normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentum :: (IsMLCTensor mean, IsMLCTensor variance, IsMLCTensor beta, IsMLCTensor gamma) => CULong -> mean -> variance -> beta -> gamma -> CFloat -> CFloat -> IO (Id MLCBatchNormalizationLayer)
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentum featureChannelCount mean variance beta gamma varianceEpsilon momentum =
  do
    cls' <- getRequiredClass "MLCBatchNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector featureChannelCount (toMLCTensor mean) (toMLCTensor variance) (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon momentum

-- | featureChannelCount
--
-- The number of feature channels
--
-- ObjC selector: @- featureChannelCount@
featureChannelCount :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CULong
featureChannelCount mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer featureChannelCountSelector

-- | mean
--
-- The mean tensor
--
-- ObjC selector: @- mean@
mean :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
mean mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer meanSelector

-- | variance
--
-- The variance tensor
--
-- ObjC selector: @- variance@
variance :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
variance mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer varianceSelector

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
beta mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer betaSelector

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
gamma mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer gammaSelector

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer betaParameterSelector

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer gammaParameterSelector

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CFloat
varianceEpsilon mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer varianceEpsilonSelector

-- | momentum
--
-- The value used for the running mean and variance computation
--
-- The default is 0.99f.
--
-- ObjC selector: @- momentum@
momentum :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CFloat
momentum mlcBatchNormalizationLayer =
  sendMessage mlcBatchNormalizationLayer momentumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, Id MLCTensor, Id MLCTensor, CFloat] (Id MLCBatchNormalizationLayer)
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, Id MLCTensor, Id MLCTensor, CFloat, CFloat] (Id MLCBatchNormalizationLayer)
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector = mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:"

-- | @Selector@ for @featureChannelCount@
featureChannelCountSelector :: Selector '[] CULong
featureChannelCountSelector = mkSelector "featureChannelCount"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] (Id MLCTensor)
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector '[] (Id MLCTensor)
varianceSelector = mkSelector "variance"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] (Id MLCTensor)
betaSelector = mkSelector "beta"

-- | @Selector@ for @gamma@
gammaSelector :: Selector '[] (Id MLCTensor)
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @betaParameter@
betaParameterSelector :: Selector '[] (Id MLCTensorParameter)
betaParameterSelector = mkSelector "betaParameter"

-- | @Selector@ for @gammaParameter@
gammaParameterSelector :: Selector '[] (Id MLCTensorParameter)
gammaParameterSelector = mkSelector "gammaParameter"

-- | @Selector@ for @varianceEpsilon@
varianceEpsilonSelector :: Selector '[] CFloat
varianceEpsilonSelector = mkSelector "varianceEpsilon"

-- | @Selector@ for @momentum@
momentumSelector :: Selector '[] CFloat
momentumSelector = mkSelector "momentum"

