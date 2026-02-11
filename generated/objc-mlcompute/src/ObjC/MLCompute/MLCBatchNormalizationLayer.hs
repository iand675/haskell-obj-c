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
  , layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector
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
    withObjCPtr mean $ \raw_mean ->
      withObjCPtr variance $ \raw_variance ->
        withObjCPtr beta $ \raw_beta ->
          withObjCPtr gamma $ \raw_gamma ->
            sendClassMsg cls' (mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:") (retPtr retVoid) [argCULong (fromIntegral featureChannelCount), argPtr (castPtr raw_mean :: Ptr ()), argPtr (castPtr raw_variance :: Ptr ()), argPtr (castPtr raw_beta :: Ptr ()), argPtr (castPtr raw_gamma :: Ptr ()), argCFloat (fromIntegral varianceEpsilon)] >>= retainedObject . castPtr

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
featureChannelCount :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CULong
featureChannelCount mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "featureChannelCount") retCULong []

-- | mean
--
-- The mean tensor
--
-- ObjC selector: @- mean@
mean :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
mean mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "mean") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | variance
--
-- The variance tensor
--
-- ObjC selector: @- variance@
variance :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
variance mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "variance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | beta
--
-- The beta tensor
--
-- ObjC selector: @- beta@
beta :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
beta mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "beta") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gamma
--
-- The gamma tensor
--
-- ObjC selector: @- gamma@
gamma :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensor)
gamma mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "gamma") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | betaParameter
--
-- The beta tensor parameter used for optimizer update
--
-- ObjC selector: @- betaParameter@
betaParameter :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensorParameter)
betaParameter mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "betaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gammaParameter
--
-- The gamma tensor parameter used for optimizer update
--
-- ObjC selector: @- gammaParameter@
gammaParameter :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO (Id MLCTensorParameter)
gammaParameter mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "gammaParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | varianceEpsilon
--
-- A value used for numerical stability
--
-- ObjC selector: @- varianceEpsilon@
varianceEpsilon :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CFloat
varianceEpsilon mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "varianceEpsilon") retCFloat []

-- | momentum
--
-- The value used for the running mean and variance computation
--
-- The default is 0.99f.
--
-- ObjC selector: @- momentum@
momentum :: IsMLCBatchNormalizationLayer mlcBatchNormalizationLayer => mlcBatchNormalizationLayer -> IO CFloat
momentum mlcBatchNormalizationLayer  =
  sendMsg mlcBatchNormalizationLayer (mkSelector "momentum") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:@
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector :: Selector
layerWithFeatureChannelCount_mean_variance_beta_gamma_varianceEpsilonSelector = mkSelector "layerWithFeatureChannelCount:mean:variance:beta:gamma:varianceEpsilon:"

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

