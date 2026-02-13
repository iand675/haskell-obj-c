{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNInstanceNormalizationGradientState
--
-- This depends on Metal.framework
--
-- A state to hold information necessary to execute a gradient              pass for MPSCNNInstanceNormalization.  Gradient states should              be created by using the forward kernel's methods.  This will              ensure that the state captures all information necessary to              execute the corresponding gradient pass.
--
-- Generated bindings for @MPSCNNInstanceNormalizationGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNInstanceNormalizationGradientState
  ( MPSCNNInstanceNormalizationGradientState
  , IsMPSCNNInstanceNormalizationGradientState(..)
  , temporaryStateWithCommandBuffer_textureDescriptor
  , temporaryStateWithCommandBuffer
  , temporaryStateWithCommandBuffer_bufferSize
  , initWithDevice_textureDescriptor
  , initWithResource
  , initWithDevice_bufferSize
  , instanceNormalization
  , gamma
  , beta
  , gradientForGamma
  , gradientForBeta
  , betaSelector
  , gammaSelector
  , gradientForBetaSelector
  , gradientForGammaSelector
  , initWithDevice_bufferSizeSelector
  , initWithDevice_textureDescriptorSelector
  , initWithResourceSelector
  , instanceNormalizationSelector
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

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_textureDescriptorSelector cmdBuf (toMTLTextureDescriptor descriptor)

-- | @+ temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBuffer :: RawId -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer cmdBuf =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBufferSelector cmdBuf

-- | @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    sendClassMessage cls' temporaryStateWithCommandBuffer_bufferSizeSelector cmdBuf bufferSize

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @- initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptor :: (IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState, IsMTLTextureDescriptor descriptor) => mpscnnInstanceNormalizationGradientState -> RawId -> descriptor -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_textureDescriptor mpscnnInstanceNormalizationGradientState device descriptor =
  sendOwnedMessage mpscnnInstanceNormalizationGradientState initWithDevice_textureDescriptorSelector device (toMTLTextureDescriptor descriptor)

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> RawId -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithResource mpscnnInstanceNormalizationGradientState resource =
  sendOwnedMessage mpscnnInstanceNormalizationGradientState initWithResourceSelector resource

-- | @- initWithDevice:bufferSize:@
initWithDevice_bufferSize :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> RawId -> CULong -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_bufferSize mpscnnInstanceNormalizationGradientState device bufferSize =
  sendOwnedMessage mpscnnInstanceNormalizationGradientState initWithDevice_bufferSizeSelector device bufferSize

-- | The MPSCNNInstanceNormalization object that created this state object.
--
-- ObjC selector: @- instanceNormalization@
instanceNormalization :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO (Id MPSCNNInstanceNormalization)
instanceNormalization mpscnnInstanceNormalizationGradientState =
  sendMessage mpscnnInstanceNormalizationGradientState instanceNormalizationSelector

-- | Return an MTLBuffer object with the state's current gamma values.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO RawId
gamma mpscnnInstanceNormalizationGradientState =
  sendMessage mpscnnInstanceNormalizationGradientState gammaSelector

-- | Return an MTLBuffer object with the state's current beta values..
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO RawId
beta mpscnnInstanceNormalizationGradientState =
  sendMessage mpscnnInstanceNormalizationGradientState betaSelector

-- | The MTLBuffer containing the gradient values for gamma.
--
-- ObjC selector: @- gradientForGamma@
gradientForGamma :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO RawId
gradientForGamma mpscnnInstanceNormalizationGradientState =
  sendMessage mpscnnInstanceNormalizationGradientState gradientForGammaSelector

-- | The MTLBuffer containing the gradient values for beta.
--
-- ObjC selector: @- gradientForBeta@
gradientForBeta :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO RawId
gradientForBeta mpscnnInstanceNormalizationGradientState =
  sendMessage mpscnnInstanceNormalizationGradientState gradientForBetaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector '[RawId, Id MTLTextureDescriptor] (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBufferSelector :: Selector '[RawId] (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBufferSelector = mkSelector "temporaryStateWithCommandBuffer:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptorSelector :: Selector '[RawId, Id MTLTextureDescriptor] (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_textureDescriptorSelector = mkSelector "initWithDevice:textureDescriptor:"

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector '[RawId] (Id MPSCNNInstanceNormalizationGradientState)
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @initWithDevice:bufferSize:@
initWithDevice_bufferSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_bufferSizeSelector = mkSelector "initWithDevice:bufferSize:"

-- | @Selector@ for @instanceNormalization@
instanceNormalizationSelector :: Selector '[] (Id MPSCNNInstanceNormalization)
instanceNormalizationSelector = mkSelector "instanceNormalization"

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

