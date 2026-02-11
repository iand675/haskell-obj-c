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
  , beta
  , gamma
  , betaParameter
  , gammaParameter
  , varianceEpsilon
  , layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector
  , betaSelector
  , gammaSelector
  , betaParameterSelector
  , gammaParameterSelector
  , varianceEpsilonSelector


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
    withObjCPtr normalizedShape $ \raw_normalizedShape ->
      withObjCPtr beta $ \raw_beta ->
        withObjCPtr gamma $ \raw_gamma ->
          sendClassMsg cls' (mkSelector "layerWithNormalizedShape:beta:gamma:varianceEpsilon:") (retPtr retVoid) [argPtr (castPtr raw_normalizedShape :: Ptr ()), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon)] >>= retainedObject . castPtr

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensor)
beta mlcLayerNormalizationLayer  =
  sendMsg mlcLayerNormalizationLayer (mkSelector "beta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensor)
gamma mlcLayerNormalizationLayer  =
  sendMsg mlcLayerNormalizationLayer (mkSelector "gamma") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcLayerNormalizationLayer  =
  sendMsg mlcLayerNormalizationLayer (mkSelector "betaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcLayerNormalizationLayer  =
  sendMsg mlcLayerNormalizationLayer (mkSelector "gammaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCLayerNormalizationLayer mlcLayerNormalizationLayer => mlcLayerNormalizationLayer -> IO CFloat
varianceEpsilon mlcLayerNormalizationLayer  =
  sendMsg mlcLayerNormalizationLayer (mkSelector "varianceEpsilon") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithNormalizedShape:beta:gamma:varianceEpsilon:@
layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector :: Selector
layerWithNormalizedShape_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithNormalizedShape:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @gamma@
gammaSelector :: Selector
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @betaParameter@
betaParameterSelector :: Selector
betaParameterSelector = mkSelector "betaParameter"

-- | @Selector@ for @gammaParameter@
gammaParameterSelector :: Selector
gammaParameterSelector = mkSelector "gammaParameter"

-- | @Selector@ for @varianceEpsilon@
varianceEpsilonSelector :: Selector
varianceEpsilonSelector = mkSelector "varianceEpsilon"

