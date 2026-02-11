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
  , layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector
  , featureChannelCountSelector
  , groupCountSelector
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
    withObjCPtr beta $ \raw_beta ->
      withObjCPtr gamma $ \raw_gamma ->
        sendClassMsg cls' (mkSelector "layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:") (retPtr retVoid) [argCULong (fromIntegral featureChannelCount), argCULong (fromIntegral groupCount), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon)] >>= retainedObject . castPtr

-- | featureChannelCount
--
-- The number of feature channels
--
-- ObjC selector: @- featureChannelCount@
featureChannelCount :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CULong
featureChannelCount mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "featureChannelCount") retCULong []

-- | groupCount
--
-- The number of groups to separate the channels into
--
-- ObjC selector: @- groupCount@
groupCount :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CULong
groupCount mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "groupCount") retCULong []

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensor)
beta mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "beta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensor)
gamma mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "gamma") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "betaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "gammaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCGroupNormalizationLayer mlcGroupNormalizationLayer => mlcGroupNormalizationLayer -> IO CFloat
varianceEpsilon mlcGroupNormalizationLayer  =
  sendMsg mlcGroupNormalizationLayer (mkSelector "varianceEpsilon") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector :: Selector
layerWithFeatureChannelCount_groupCount_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:groupCount:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @featureChannelCount@
featureChannelCountSelector :: Selector
featureChannelCountSelector = mkSelector "featureChannelCount"

-- | @Selector@ for @groupCount@
groupCountSelector :: Selector
groupCountSelector = mkSelector "groupCount"

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

