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
  , initWithResourceSelector
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , temporaryStateWithCommandBuffer_textureDescriptorSelector
  , resetSelector
  , gammaSelector
  , betaSelector
  , meanSelector
  , varianceSelector
  , gradientForGammaSelector
  , gradientForBetaSelector
  , batchNormalizationSelector


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
import ObjC.Metal.Internal.Classes

-- | Unavailable.  Use MPSCNNBatchNormalizationStatistics methods to initialize the state object.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> RawId -> IO (Id MPSCNNBatchNormalizationState)
initWithResource mpscnnBatchNormalizationState  resource =
  sendMsg mpscnnBatchNormalizationState (mkSelector "initWithResource:") (retPtr retVoid) [argPtr (castPtr (unRawId resource) :: Ptr ())] >>= ownedObject . castPtr

-- | Unavailable.  Use MPSCNNBatchNormalizationStatistics methods to create the temporary state object.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argCULong (fromIntegral bufferSize)] >>= retainedObject . castPtr

-- | @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNBatchNormalizationState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNBatchNormalizationState"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Reset any accumulated state data to its initial values.
--
-- ObjC selector: @- reset@
reset :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO ()
reset mpscnnBatchNormalizationState  =
  sendMsg mpscnnBatchNormalizationState (mkSelector "reset") retVoid []

-- | Return an MTLBuffer object with the state's current gamma values.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gamma mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "gamma") (retPtr retVoid) []

-- | Return an MTLBuffer object with the state's current beta values..
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
beta mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "beta") (retPtr retVoid) []

-- | Return an MTLBuffer object with the most recently computed batch mean values.
--
-- ObjC selector: @- mean@
mean :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
mean mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "mean") (retPtr retVoid) []

-- | Return an MTLBuffer object with the most recently computed batch variance values.
--
-- ObjC selector: @- variance@
variance :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
variance mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "variance") (retPtr retVoid) []

-- | Return an MTLBuffer object containing the values of the gradient of the loss function              with respect to the scale factors.  If a MPSCNNBatchNormalizationGradient kernel              has not successfully generated these values nil will be returned.
--
-- ObjC selector: @- gradientForGamma@
gradientForGamma :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gradientForGamma mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "gradientForGamma") (retPtr retVoid) []

-- | Return an MTLBuffer object containing the values of the gradient of the loss function              with respect to the bias terms.  If a MPSCNNBatchNormalizationGradient kernel              has not successfully generated these values nil will be returned.
--
-- ObjC selector: @- gradientForBeta@
gradientForBeta :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO RawId
gradientForBeta mpscnnBatchNormalizationState  =
  fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationState (mkSelector "gradientForBeta") (retPtr retVoid) []

-- | @- batchNormalization@
batchNormalization :: IsMPSCNNBatchNormalizationState mpscnnBatchNormalizationState => mpscnnBatchNormalizationState -> IO (Id MPSCNNBatchNormalization)
batchNormalization mpscnnBatchNormalizationState  =
  sendMsg mpscnnBatchNormalizationState (mkSelector "batchNormalization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @gamma@
gammaSelector :: Selector
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @mean@
meanSelector :: Selector
meanSelector = mkSelector "mean"

-- | @Selector@ for @variance@
varianceSelector :: Selector
varianceSelector = mkSelector "variance"

-- | @Selector@ for @gradientForGamma@
gradientForGammaSelector :: Selector
gradientForGammaSelector = mkSelector "gradientForGamma"

-- | @Selector@ for @gradientForBeta@
gradientForBetaSelector :: Selector
gradientForBetaSelector = mkSelector "gradientForBeta"

-- | @Selector@ for @batchNormalization@
batchNormalizationSelector :: Selector
batchNormalizationSelector = mkSelector "batchNormalization"

