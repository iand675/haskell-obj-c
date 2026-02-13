{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationState
--
-- MPSCNNBatchNormalizationState encapsulates the data necessary              to execute batch normalization.
--
-- MPSCNNBatchNormalizationState cannot initialize the size of its own              underlying resources.  Use [MPSCNNBatchNormalizationStatistics resultStateForSourceImages:]              or [MPSCNNBatchNormalizationStatistics temporaryResultStateForCommandBuffer:sourceImages:].
--
-- Generated bindings for @MPSCNNBatchNormalizationState@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationState
  ( MPSCNNBatchNormalizationState
  , IsMPSCNNBatchNormalizationState(..)
  , initWithResource
  , temporaryStateWithCommandBuffer_bufferSize
  , temporaryStateWithCommandBuffer_textureDescriptor
  , reset
  , gamma
  , beta
  , mean
  , variance
  , gradientForGamma
  , gradientForBeta
  , batchNormalization
  , batchNormalizationSelector
  , betaSelector
  , gammaSelector
  , gradientForBetaSelector
  , gradientForGammaSelector
  , initWithResourceSelector
  , meanSelector
  , resetSelector
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , temporaryStateWithCommandBuffer_textureDescriptorSelector
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
import ObjC.Metal.Internal.Classes

-- | Unavailable.  Use MPSCNNBatchNormalizationStatistics methods to initialize the state object.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> RawId -> IO (Id MPSCNNBatchNormalizationState)
initWithResource mpscnnBatchNormalizationState resource =
  sendOwnedMessage mpscnnBatchNormalizationState initWithResourceSelector resource

-- | Unavailable.  Use MPSCNNBatchNormalizationStatistics methods to create the temporary state object.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_bufferSizeSelector cmdBuf bufferSize

-- | @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_textureDescriptorSelector cmdBuf (toMTLTextureDescriptor descriptor)

-- | Reset any accumulated state data to its initial values.
--
-- ObjC selector: @- reset@
reset :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO ()
reset mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState resetSelector

-- | Return an MTLBuffer object with the state's current gamma values.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gamma mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState gammaSelector

-- | Return an MTLBuffer object with the state's current beta values..
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
beta mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState betaSelector

-- | Return an MTLBuffer object with the most recently computed batch mean values.
--
-- ObjC selector: @- mean@
mean :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
mean mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState meanSelector

-- | Return an MTLBuffer object with the most recently computed batch variance values.
--
-- ObjC selector: @- variance@
variance :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
variance mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState varianceSelector

-- | Return an MTLBuffer object containing the values of the gradient of the loss function              with respect to the scale factors.  If a MPSCNNBatchNormalizationGradient kernel              has not successfully generated these values nil will be returned.
--
-- ObjC selector: @- gradientForGamma@
gradientForGamma :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gradientForGamma mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState gradientForGammaSelector

-- | Return an MTLBuffer object containing the values of the gradient of the loss function              with respect to the bias terms.  If a MPSCNNBatchNormalizationGradient kernel              has not successfully generated these values nil will be returned.
--
-- ObjC selector: @- gradientForBeta@
gradientForBeta :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gradientForBeta mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState gradientForBetaSelector

-- | @- batchNormalization@
batchNormalization :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO (Id MPSCNNBatchNormalization)
batchNormalization mpscnnBatchNormalizationState =
  sendMessage mpscnnBatchNormalizationState batchNormalizationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector '[RawId] (Id MPSCNNBatchNormalizationState)
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector '[RawId, Id MTLTextureDescriptor] (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @gamma@
gammaSelector :: Selector '[] RawId
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] RawId
betaSelector = mkSelector "beta"

-- | @Selector@ for @mean@
meanSelector :: Selector '[] RawId
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector '[] RawId
varianceSelector = mkSelector "variance"

-- | @Selector@ for @gradientForGamma@
gradientForGammaSelector :: Selector '[] RawId
gradientForGammaSelector = mkSelector "gradientForGamma"

-- | @Selector@ for @gradientForBeta@
gradientForBetaSelector :: Selector '[] RawId
gradientForBetaSelector = mkSelector "gradientForBeta"

-- | @Selector@ for @batchNormalization@
batchNormalizationSelector :: Selector '[] (Id MPSCNNBatchNormalization)
batchNormalizationSelector = mkSelector "batchNormalization"

