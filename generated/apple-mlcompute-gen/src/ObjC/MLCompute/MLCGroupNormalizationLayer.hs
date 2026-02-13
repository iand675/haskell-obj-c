{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCGroupNormalizationLayer
--
-- A group normalizaion layer.  For more information, refer to https://pytorch.org/docs/stable/nn.html#groupnorm
--
-- Generated bindings for @MLCGroupNormalizationLayer@.
module ObjC.MLCompute.MLCGroupNormalizationLayer
  ( MLCGroupNormalizationLayer
  , IsMLCGroupNormalizationLayer(..)
  , layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilon
  , featureChannelCount
  , groupCount
  , beta
  , gamma
  , betaParameter
  , gammaParameter
  , varianceEpsilon
  , betaParameterSelector
  , betaSelector
  , featureChannelCountSelector
  , gammaParameterSelector
  , gammaSelector
  , groupCountSelector
  , layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector
  , varianceEpsilonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a group normalization layer
--
-- @featureChannelCount@ — The number of feature channels
--
-- @beta@ — Training parameter
--
-- @gamma@ — Training parameter
--
-- @groupCount@ — The number of groups to divide the feature channels into
--
-- @varianceEpsilon@ — A small numerical value added to variance for stability
--
-- Returns: A new group normalization layer.
--
-- ObjC selector: @+ layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilon :: (IsMLCTensor beta, IsMLCTensor gamma) => CULong -> CULong -> beta -> gamma -> CFloat -> IO (Id MLCGroupNormalizationLayer)
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilon featureChannelCount groupCount beta gamma varianceEpsilon =
  do
    cls' <- getRequiredClass "MLCGroupNormalizationLayer"
    sendClassMessage cls' layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector featureChannelCount groupCount (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon

-- | featureChannelCount
--
-- The number of feature channels
--
-- ObjC selector: @- featureChannelCount@
featureChannelCount :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CULong
featureChannelCount mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer featureChannelCountSelector

-- | groupCount
--
-- The number of groups to separate the channels into
--
-- ObjC selector: @- groupCount@
groupCount :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CULong
groupCount mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer groupCountSelector

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensor)
beta mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer betaSelector

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensor)
gamma mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer gammaSelector

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer betaParameterSelector

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer gammaParameterSelector

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CFloat
varianceEpsilon mlcGroupNormalizationLayer =
  sendMessage mlcGroupNormalizationLayer varianceEpsilonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector :: Selector '[CULong, CULong, Id MLCTensor, Id MLCTensor, CFloat] (Id MLCGroupNormalizationLayer)
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @featureChannelCount@
featureChannelCountSelector :: Selector '[] CULong
featureChannelCountSelector = mkSelector "featureChannelCount"

-- | @Selector@ for @groupCount@
groupCountSelector :: Selector '[] CULong
groupCountSelector = mkSelector "groupCount"

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

