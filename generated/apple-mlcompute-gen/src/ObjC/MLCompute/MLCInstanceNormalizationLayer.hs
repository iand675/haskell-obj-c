{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCInstanceNormalizationLayer
--
-- An instance normalization layer.  For more information refer to https://pytorch.org/docs/stable/nn.html#instancenorm2d
--
-- Generated bindings for @MLCInstanceNormalizationLayer@.
module ObjC.MLCompute.MLCInstanceNormalizationLayer
  ( MLCInstanceNormalizationLayer
  , IsMLCInstanceNormalizationLayer(..)
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilon
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentum
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
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector
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

-- | Create an instance normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @beta@ — The beta tensor
--
-- @gamma@ — The gamma tensor
--
-- @varianceEpsilon@ — The  epslion value
--
-- Returns: A new instance normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon :: (IsMLCTensor beta, IsMLCTensor gamma) => CULong -> beta -> gamma -> CFloat -> IO (Id MLCInstanceNormalizationLayer)
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon featureChannelCount beta gamma varianceEpsilon =
  do
    cls' <- getRequiredClass "MLCInstanceNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector featureChannelCount (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon

-- | Create an instance normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @beta@ — The beta tensor
--
-- @gamma@ — The gamma tensor
--
-- @varianceEpsilon@ — The  epslion value
--
-- @momentum@ — The  momentum value for the running mean and variance computation
--
-- Returns: A new instance normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentum :: (IsMLCTensor beta, IsMLCTensor gamma) => CULong -> beta -> gamma -> CFloat -> CFloat -> IO (Id MLCInstanceNormalizationLayer)
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentum featureChannelCount beta gamma varianceEpsilon momentum =
  do
    cls' <- getRequiredClass "MLCInstanceNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector featureChannelCount (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon momentum

-- | Create an instance normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @mean@ — The running mean tensor
--
-- @variance@ — The running variance tensor
--
-- @beta@ — The beta tensor
--
-- @gamma@ — The gamma tensor
--
-- @varianceEpsilon@ — The  epslion value
--
-- @momentum@ — The  momentum value for the running mean and variance computation
--
-- Returns: A new instance normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentum :: (IsMLCTensor mean, IsMLCTensor variance, IsMLCTensor beta, IsMLCTensor gamma) => CULong -> mean -> variance -> beta -> gamma -> CFloat -> CFloat -> IO (Id MLCInstanceNormalizationLayer)
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentum featureChannelCount mean variance beta gamma varianceEpsilon momentum =
  do
    cls' <- getRequiredClass "MLCInstanceNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector featureChannelCount (toMLCTensor mean) (toMLCTensor variance) (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon momentum

-- | featureChannelCount
--
-- The number of feature channels
--
-- ObjC selector: @- featureChannelCount@
featureChannelCount :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CULong
featureChannelCount mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer featureChannelCountSelector

-- | mean
--
-- The running mean tensor
--
-- ObjC selector: @- mean@
mean :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
mean mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer meanSelector

-- | variance
--
-- The running variance tensor
--
-- ObjC selector: @- variance@
variance :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
variance mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer varianceSelector

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
beta mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer betaSelector

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
gamma mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer gammaSelector

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer betaParameterSelector

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer gammaParameterSelector

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CFloat
varianceEpsilon mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer varianceEpsilonSelector

-- | momentum
--
-- The value used for the running mean and variance computation
--
-- The default is 0.99f.
--
-- ObjC selector: @- momentum@
momentum :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CFloat
momentum mlcInstanceNormalizationLayer =
  sendMessage mlcInstanceNormalizationLayer momentumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, CFloat] (Id MLCInstanceNormalizationLayer)
layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, CFloat, CFloat] (Id MLCInstanceNormalizationLayer)
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector = mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:"

-- | @Selector@ for @layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector :: Selector '[CULong, Id MLCTensor, Id MLCTensor, Id MLCTensor, Id MLCTensor, CFloat, CFloat] (Id MLCInstanceNormalizationLayer)
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

