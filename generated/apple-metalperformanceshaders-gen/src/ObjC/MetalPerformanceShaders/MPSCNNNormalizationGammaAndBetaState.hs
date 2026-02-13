{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNormalizationGammaAndBetaState
--
-- A state which contains gamma and beta terms used to apply a scale               and bias in either an MPSCNNInstanceNormalization or MPSCNNBatchNormalization               operation.
--
-- Generated bindings for @MPSCNNNormalizationGammaAndBetaState@.
module ObjC.MetalPerformanceShaders.MPSCNNNormalizationGammaAndBetaState
  ( MPSCNNNormalizationGammaAndBetaState
  , IsMPSCNNNormalizationGammaAndBetaState(..)
  , initWithGamma_beta
  , temporaryStateWithCommandBuffer_numberOfFeatureChannels
  , gamma
  , beta
  , betaSelector
  , gammaSelector
  , initWithGamma_betaSelector
  , temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSCNNNormalizationGammaAndBetaState object using values              contained in MTLBuffers.
--
-- @gamma@ — The MTLBuffer containing gamma terms.
--
-- @beta@ — The MTLBuffer containing beta terms.
--
-- ObjC selector: @- initWithGamma:beta:@
initWithGamma_beta :: IsMPSCNNNormalizationGammaAndBetaState mpscnnNormalizationGammaAndBetaState => mpscnnNormalizationGammaAndBetaState -> RawId -> RawId -> IO (Id MPSCNNNormalizationGammaAndBetaState)
initWithGamma_beta mpscnnNormalizationGammaAndBetaState gamma beta =
  sendOwnedMessage mpscnnNormalizationGammaAndBetaState initWithGamma_betaSelector gamma beta

-- | Create a temporary MPSCNNNormalizationGammaAndBetaState suitable              for a normalization operation on images containing no more than              the specified number of feature channels.
--
-- @commandBuffer@ — The command buffer on which the temporary state will                                      be used.
--
-- @numberOfFeatureChannels@ — The number of feature channels used to size the                                      state.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannels :: RawId -> CULong -> IO (Id MPSCNNNormalizationGammaAndBetaState)
temporaryStateWithCommandBuffer_numberOfFeatureChannels commandBuffer numberOfFeatureChannels =
  do
    cls' <- getRequiredClass "MPSCNNNormalizationGammaAndBetaState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector commandBuffer numberOfFeatureChannels

-- | gamma
--
-- A MTLBuffer containing the gamma terms.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNNormalizationGammaAndBetaState mpscnnNormalizationGammaAndBetaState => mpscnnNormalizationGammaAndBetaState -> IO RawId
gamma mpscnnNormalizationGammaAndBetaState =
  sendMessage mpscnnNormalizationGammaAndBetaState gammaSelector

-- | beta
--
-- A MTLBuffer containing the beta terms.
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNNormalizationGammaAndBetaState mpscnnNormalizationGammaAndBetaState => mpscnnNormalizationGammaAndBetaState -> IO RawId
beta mpscnnNormalizationGammaAndBetaState =
  sendMessage mpscnnNormalizationGammaAndBetaState betaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithGamma:beta:@
initWithGamma_betaSelector :: Selector '[RawId, RawId] (Id MPSCNNNormalizationGammaAndBetaState)
initWithGamma_betaSelector = mkSelector "initWithGamma:beta:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector :: Selector '[RawId, CULong] (Id MPSCNNNormalizationGammaAndBetaState)
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector = mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:"

-- | @Selector@ for @gamma@
gammaSelector :: Selector '[] RawId
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] RawId
betaSelector = mkSelector "beta"

