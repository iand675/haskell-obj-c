{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNormalizationMeanAndVarianceState
--
-- A state which contains mean and variance terms used to apply a               normalization in a MPSCNNBatchNormalization operation.
--
-- Generated bindings for @MPSCNNNormalizationMeanAndVarianceState@.
module ObjC.MetalPerformanceShaders.MPSCNNNormalizationMeanAndVarianceState
  ( MPSCNNNormalizationMeanAndVarianceState
  , IsMPSCNNNormalizationMeanAndVarianceState(..)
  , initWithMean_variance
  , temporaryStateWithCommandBuffer_numberOfFeatureChannels
  , mean
  , variance
  , initWithMean_varianceSelector
  , meanSelector
  , temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector
  , varianceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSCNNNormalizationMeanAndVarianceState object using values              contained in MTLBuffers.
--
-- @mean@ — The MTLBuffer containing mean terms.
--
-- @variance@ — The MTLBuffer containing variance terms.
--
-- ObjC selector: @- initWithMean:variance:@
initWithMean_variance :: IsMPSCNNNormalizationMeanAndVarianceState mpscnnNormalizationMeanAndVarianceState => mpscnnNormalizationMeanAndVarianceState -> RawId -> RawId -> IO (Id MPSCNNNormalizationMeanAndVarianceState)
initWithMean_variance mpscnnNormalizationMeanAndVarianceState mean variance =
  sendOwnedMessage mpscnnNormalizationMeanAndVarianceState initWithMean_varianceSelector mean variance

-- | Create a temporary MPSCNNNormalizationMeanAndVarianceState suitable              for a normalization operation on images containing no more than              the specified number of feature channels.
--
-- @commandBuffer@ — The command buffer on which the temporary state will                                      be used.
--
-- @numberOfFeatureChannels@ — The number of feature channels used to size the                                      state.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannels :: RawId -> CULong -> IO (Id MPSCNNNormalizationMeanAndVarianceState)
temporaryStateWithCommandBuffer_numberOfFeatureChannels commandBuffer numberOfFeatureChannels =
  do
    cls' <- getRequiredClass "MPSCNNNormalizationMeanAndVarianceState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector commandBuffer numberOfFeatureChannels

-- | mean
--
-- A MTLBuffer containing the mean terms.
--
-- ObjC selector: @- mean@
mean :: IsMPSCNNNormalizationMeanAndVarianceState mpscnnNormalizationMeanAndVarianceState => mpscnnNormalizationMeanAndVarianceState -> IO RawId
mean mpscnnNormalizationMeanAndVarianceState =
  sendMessage mpscnnNormalizationMeanAndVarianceState meanSelector

-- | variance
--
-- A MTLBuffer containing the variance terms.
--
-- ObjC selector: @- variance@
variance :: IsMPSCNNNormalizationMeanAndVarianceState mpscnnNormalizationMeanAndVarianceState => mpscnnNormalizationMeanAndVarianceState -> IO RawId
variance mpscnnNormalizationMeanAndVarianceState =
  sendMessage mpscnnNormalizationMeanAndVarianceState varianceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMean:variance:@
initWithMean_varianceSelector :: Selector '[RawId, RawId] (Id MPSCNNNormalizationMeanAndVarianceState)
initWithMean_varianceSelector = mkSelector "initWithMean:variance:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector :: Selector '[RawId, CULong] (Id MPSCNNNormalizationMeanAndVarianceState)
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector = mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] RawId
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector '[] RawId
varianceSelector = mkSelector "variance"

