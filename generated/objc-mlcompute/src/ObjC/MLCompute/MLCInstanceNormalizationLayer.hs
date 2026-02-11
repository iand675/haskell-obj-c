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
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector
  , layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector
  , featureChannelCountSelector
  , meanSelector
  , varianceSelector
  , betaSelector
  , gammaSelector
  , betaParameterSelector
  , gammaParameterSelector
  , varianceEpsilonSelector
  , momentumSelector


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
    withObjCPtr beta $ \raw_beta ->
      withObjCPtr gamma $ \raw_gamma ->
        sendClassMsg cls' (mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:") (retPtr retVoid) [argCULong (fromIntegral featureChannelCount), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon)] >>= retainedObject . castPtr

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
    withObjCPtr beta $ \raw_beta ->
      withObjCPtr gamma $ \raw_gamma ->
        sendClassMsg cls' (mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:") (retPtr retVoid) [argCULong (fromIntegral featureChannelCount), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon), argCFloat (fromIntegral momentum)] >>= retainedObject . castPtr

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
    withObjCPtr mean $ \raw_mean ->
      withObjCPtr variance $ \raw_variance ->
        withObjCPtr beta $ \raw_beta ->
          withObjCPtr gamma $ \raw_gamma ->
            sendClassMsg cls' (mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:") (retPtr retVoid) [argCULong (fromIntegral featureChannelCount), argPtr (castPtr raw_mean :: Ptr ()), argPtr (castPtr raw_variance :: Ptr ()), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon), argCFloat (fromIntegral momentum)] >>= retainedObject . castPtr

-- | featureChannelCount
--
-- The number of feature channels
--
-- ObjC selector: @- featureChannelCount@
featureChannelCount :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CULong
featureChannelCount mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "featureChannelCount") retCULong []

-- | mean
--
-- The running mean tensor
--
-- ObjC selector: @- mean@
mean :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
mean mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "mean") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | variance
--
-- The running variance tensor
--
-- ObjC selector: @- variance@
variance :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
variance mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "variance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
beta mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "beta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensor)
gamma mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "gamma") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "betaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "gammaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CFloat
varianceEpsilon mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "varianceEpsilon") retCFloat []

-- | momentum
--
-- The value used for the running mean and variance computation
--
-- The default is 0.99f.
--
-- ObjC selector: @- momentum@
momentum :: IsMLCInstanceNormalizationLayer mlcInstanceNormalizationLayer => mlcInstanceNormalizationLayer -> IO CFloat
momentum mlcInstanceNormalizationLayer  =
  sendMsg mlcInstanceNormalizationLayer (mkSelector "momentum") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector :: Selector
layerWithFeatureChannelCount_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:"

-- | @Selector@ for @layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector :: Selector
layerWithFeatureChannelCount_beta_gamma_varianceEpsilon_momentumSelector = mkSelector "layerWithFeatureChannelCount:beta:gamma:varianceEpsilon:momentum:"

-- | @Selector@ for @layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector :: Selector
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilon_momentumSelector = mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:momentum:"

-- | @Selector@ for @featureChannelCount@
featureChannelCountSelector :: Selector
featureChannelCountSelector = mkSelector "featureChannelCount"

-- | @Selector@ for @mean@
meanSelector :: Selector
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector
varianceSelector = mkSelector "variance"

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

-- | @Selector@ for @momentum@
momentumSelector :: Selector
momentumSelector = mkSelector "momentum"

