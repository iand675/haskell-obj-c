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
  , initWithGamma_betaSelector
  , temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector


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

-- | Initialize a MPSCNNNormalizationGammaAndBetaState object using values              contained in MTLBuffers.
--
-- @gamma@ — The MTLBuffer containing gamma terms.
--
-- @beta@ — The MTLBuffer containing beta terms.
--
-- ObjC selector: @- initWithGamma:beta:@
initWithGamma_beta :: IsMPSCNNNormalizationGammaAndBetaState mpscnnNormalizationGammaAndBetaState => mpscnnNormalizationGammaAndBetaState -> RawId -> RawId -> IO (Id MPSCNNNormalizationGammaAndBetaState)
initWithGamma_beta mpscnnNormalizationGammaAndBetaState  gamma beta =
  sendMsg mpscnnNormalizationGammaAndBetaState (mkSelector "initWithGamma:beta:") (retPtr retVoid) [argPtr (castPtr (unRawId gamma) :: Ptr ()), argPtr (castPtr (unRawId beta) :: Ptr ())] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (fromIntegral numberOfFeatureChannels)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithGamma:beta:@
initWithGamma_betaSelector :: Selector
initWithGamma_betaSelector = mkSelector "initWithGamma:beta:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:numberOfFeatureChannels:@
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector :: Selector
temporaryStateWithCommandBuffer_numberOfFeatureChannelsSelector = mkSelector "temporaryStateWithCommandBuffer:numberOfFeatureChannels:"

