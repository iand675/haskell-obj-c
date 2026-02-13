{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNGroupNormalizationGradientState
--
-- This depends on Metal.framework
--
-- A state to hold information necessary to execute a gradient              pass for MPSCNNGroupNormalization.  Gradient states should              be created by using the forward kernel's methods.  This will              ensure that the state captures all information necessary to              execute the corresponding gradient pass.
--
-- Generated bindings for @MPSCNNGroupNormalizationGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNGroupNormalizationGradientState
  ( MPSCNNGroupNormalizationGradientState
  , IsMPSCNNGroupNormalizationGradientState(..)
  , temporaryStateWithCommandBuffer_textureDescriptor
  , temporaryStateWithCommandBuffer
  , temporaryStateWithCommandBuffer_bufferSize
  , initWithDevice_textureDescriptor
  , initWithResource
  , initWithDevice_bufferSize
  , groupNormalization
  , gamma
  , beta
  , gradientForGamma
  , gradientForBeta
  , betaSelector
  , gammaSelector
  , gradientForBetaSelector
  , gradientForGammaSelector
  , groupNormalizationSelector
  , initWithDevice_bufferSizeSelector
  , initWithDevice_textureDescriptorSelector
  , initWithResourceSelector
  , temporaryStateWithCommandBufferSelector
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , temporaryStateWithCommandBuffer_textureDescriptorSelector


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

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_textureDescriptorSelector cmdBuf (toMTLTextureDescriptor descriptor)

-- | @+ temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBuffer :: RawId -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer cmdBuf =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBufferSelector cmdBuf

-- | @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_bufferSizeSelector cmdBuf bufferSize

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @- initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptor :: (IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState, IsMTLTextureDescriptor descriptor) => mpscnnGroupNormalizationGradientState -> RawId -> descriptor -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_textureDescriptor mpscnnGroupNormalizationGradientState device descriptor =
  sendOwnedMessage mpscnnGroupNormalizationGradientState initWithDevice_textureDescriptorSelector device (toMTLTextureDescriptor descriptor)

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> RawId -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithResource mpscnnGroupNormalizationGradientState resource =
  sendOwnedMessage mpscnnGroupNormalizationGradientState initWithResourceSelector resource

-- | @- initWithDevice:bufferSize:@
initWithDevice_bufferSize :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> RawId -> CULong -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_bufferSize mpscnnGroupNormalizationGradientState device bufferSize =
  sendOwnedMessage mpscnnGroupNormalizationGradientState initWithDevice_bufferSizeSelector device bufferSize

-- | The MPSCNNGroupNormalization object that created this state object.
--
-- ObjC selector: @- groupNormalization@
groupNormalization :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO (Id MPSCNNGroupNormalization)
groupNormalization mpscnnGroupNormalizationGradientState =
  sendMessage mpscnnGroupNormalizationGradientState groupNormalizationSelector

-- | Return an MTLBuffer object with the state's current gamma values.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gamma mpscnnGroupNormalizationGradientState =
  sendMessage mpscnnGroupNormalizationGradientState gammaSelector

-- | Return an MTLBuffer object with the state's current beta values..
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
beta mpscnnGroupNormalizationGradientState =
  sendMessage mpscnnGroupNormalizationGradientState betaSelector

-- | The MTLBuffer containing the gradient values for gamma.
--
-- ObjC selector: @- gradientForGamma@
gradientForGamma :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gradientForGamma mpscnnGroupNormalizationGradientState =
  sendMessage mpscnnGroupNormalizationGradientState gradientForGammaSelector

-- | The MTLBuffer containing the gradient values for beta.
--
-- ObjC selector: @- gradientForBeta@
gradientForBeta :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gradientForBeta mpscnnGroupNormalizationGradientState =
  sendMessage mpscnnGroupNormalizationGradientState gradientForBetaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector '[RawId, Id MTLTextureDescriptor] (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBufferSelector :: Selector '[RawId] (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBufferSelector = mkSelector "temporaryStateWithCommandBuffer:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptorSelector :: Selector '[RawId, Id MTLTextureDescriptor] (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_textureDescriptorSelector = mkSelector "initWithDevice:textureDescriptor:"

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector '[RawId] (Id MPSCNNGroupNormalizationGradientState)
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @initWithDevice:bufferSize:@
initWithDevice_bufferSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_bufferSizeSelector = mkSelector "initWithDevice:bufferSize:"

-- | @Selector@ for @groupNormalization@
groupNormalizationSelector :: Selector '[] (Id MPSCNNGroupNormalization)
groupNormalizationSelector = mkSelector "groupNormalization"

-- | @Selector@ for @gamma@
gammaSelector :: Selector '[] RawId
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] RawId
betaSelector = mkSelector "beta"

-- | @Selector@ for @gradientForGamma@
gradientForGammaSelector :: Selector '[] RawId
gradientForGammaSelector = mkSelector "gradientForGamma"

-- | @Selector@ for @gradientForBeta@
gradientForBetaSelector :: Selector '[] RawId
gradientForBetaSelector = mkSelector "gradientForBeta"

