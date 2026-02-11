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
  , temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector
  , meanSelector
  , varianceSelector


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
initWithMean_variance mpscnnNormalizationMeanAndVarianceState  mean variance =
    sendMsg mpscnnNormalizationMeanAndVarianceState (mkSelector "initWithMean:variance:") (retPtr retVoid) [argPtr (castPtr (unRawId mean) :: Ptr ()), argPtr (castPtr (unRawId variance) :: Ptr ())] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong numberOfFeatureChannels] >>= retainedObject . castPtr

-- | mean
--
-- A MTLBuffer containing the mean terms.
--
-- ObjC selector: @- mean@
mean :: IsMPSCNNNormalizationMeanAndVarianceState mpscnnNormalizationMeanAndVarianceState => mpscnnNormalizationMeanAndVarianceState -> IO RawId
mean mpscnnNormalizationMeanAndVarianceState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnNormalizationMeanAndVarianceState (mkSelector "mean") (retPtr retVoid) []

-- | variance
--
-- A MTLBuffer containing the variance terms.
--
-- ObjC selector: @- variance@
variance :: IsMPSCNNNormalizationMeanAndVarianceState mpscnnNormalizationMeanAndVarianceState => mpscnnNormalizationMeanAndVarianceState -> IO RawId
variance mpscnnNormalizationMeanAndVarianceState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnNormalizationMeanAndVarianceState (mkSelector "variance") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMean:variance:@
initWithMean_varianceSelector :: Selector
initWithMean_varianceSelector = mkSelector "initWithMean:variance:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector :: Selector
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector = mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:"

-- | @Selector@ for @mean@
meanSelector :: Selector
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector
varianceSelector = mkSelector "variance"

