{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLayerNormalizationLayer
--
-- The layer normalizaion layer.  For more information, refer to https://pytorch.org/docs/stable/nn.html#layernorm.
--
-- Generated bindings for @MLCLayerNormalizationLayer@.
module ObjC.MLCompute.MLCLayerNormalizationLayer
  ( MLCLayerNormalizationLayer
  , IsMLCLayerNormalizationLayer(..)
  , layerWithNormalizedShape_beta_gamma_varianceEpsilon
  , normalizedShape
  , beta
  , gamma
  , betaParameter
  , gammaParameter
  , varianceEpsilon
  , betaParameterSelector
  , betaSelector
  , gammaParameterSelector
  , gammaSelector
  , layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector
  , normalizedShapeSelector
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

-- | Create a layer normalization layer
--
-- @normalizedShape@ — The shape of the axes over which normalization occurs, currently (C,H,W) only
--
-- @beta@ — Training parameter
--
-- @gamma@ — Training parameter
--
-- @varianceEpsilon@ — A small numerical value added to variance for stability
--
-- Returns: A new layer normalization layer.
--
-- ObjC selector: @+ layerWithNormalizedShape:beta:gamma:varianceEpsilon:@
layerWithNormalizedShape_beta_gamma_varianceEpsilon :: (IsNSArray normalizedShape, IsMLCTensor beta, IsMLCTensor gamma) => normalizedShape -> beta -> gamma -> CFloat -> IO (Id MLCLayerNormalizationLayer)
layerWithNormalizedShape_beta_gamma_varianceEpsilon normalizedShape beta gamma varianceEpsilon =
  do
    cls' <- getRequiredClass "MLCLayerNormalizationLayer"
    sendClassMessage cls' layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector (toNSArray normalizedShape) (toMLCTensor beta) (toMLCTensor gamma) varianceEpsilon

-- | normalizedShape
--
-- The shape of the axes over which normalization occurs, (W), (H,W) or (C,H,W)
--
-- ObjC selector: @- normalizedShape@
normalizedShape :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id NSArray)
normalizedShape mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer normalizedShapeSelector

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensor)
beta mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer betaSelector

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensor)
gamma mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer gammaSelector

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer betaParameterSelector

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer gammaParameterSelector

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO CFloat
varianceEpsilon mlcLayerNormalizationLayer =
  sendMessage mlcLayerNormalizationLayer varianceEpsilonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithNormalizedShape:beta:gamma:varianceEpsilon:@
layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector :: Selector '[Id NSArray, Id MLCTensor, Id MLCTensor, CFloat] (Id MLCLayerNormalizationLayer)
layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithNormalizedShape:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @normalizedShape@
normalizedShapeSelector :: Selector '[] (Id NSArray)
normalizedShapeSelector = mkSelector "normalizedShape"

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

