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
  , temporaryStateWithCommandBuffer_textureDescriptorSelector
  , temporaryStateWithCommandBufferSelector
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , initWithDevice_textureDescriptorSelector
  , initWithResourceSelector
  , initWithDevice_bufferSizeSelector
  , groupNormalizationSelector
  , gammaSelector
  , betaSelector
  , gradientForGammaSelector
  , gradientForBetaSelector


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

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @+ temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBuffer :: RawId -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer cmdBuf =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNGroupNormalizationGradientState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argCULong bufferSize] >>= retainedObject . castPtr

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @- initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptor :: (IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState, IsMTLTextureDescriptor descriptor) => mpscnnGroupNormalizationGradientState -> RawId -> descriptor -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_textureDescriptor mpscnnGroupNormalizationGradientState  device descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpscnnGroupNormalizationGradientState (mkSelector "initWithDevice:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Unavailable.  Use MPSCNNGroupNormalization state creation methods.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> RawId -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithResource mpscnnGroupNormalizationGradientState  resource =
    sendMsg mpscnnGroupNormalizationGradientState (mkSelector "initWithResource:") (retPtr retVoid) [argPtr (castPtr (unRawId resource) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:bufferSize:@
initWithDevice_bufferSize :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> RawId -> CULong -> IO (Id MPSCNNGroupNormalizationGradientState)
initWithDevice_bufferSize mpscnnGroupNormalizationGradientState  device bufferSize =
    sendMsg mpscnnGroupNormalizationGradientState (mkSelector "initWithDevice:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong bufferSize] >>= ownedObject . castPtr

-- | The MPSCNNGroupNormalization object that created this state object.
--
-- ObjC selector: @- groupNormalization@
groupNormalization :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO (Id MPSCNNGroupNormalization)
groupNormalization mpscnnGroupNormalizationGradientState  =
    sendMsg mpscnnGroupNormalizationGradientState (mkSelector "groupNormalization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Return an MTLBuffer object with the state's current gamma values.
--
-- ObjC selector: @- gamma@
gamma :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gamma mpscnnGroupNormalizationGradientState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnGroupNormalizationGradientState (mkSelector "gamma") (retPtr retVoid) []

-- | Return an MTLBuffer object with the state's current beta values..
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
beta mpscnnGroupNormalizationGradientState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnGroupNormalizationGradientState (mkSelector "beta") (retPtr retVoid) []

-- | The MTLBuffer containing the gradient values for gamma.
--
-- ObjC selector: @- gradientForGamma@
gradientForGamma :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gradientForGamma mpscnnGroupNormalizationGradientState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnGroupNormalizationGradientState (mkSelector "gradientForGamma") (retPtr retVoid) []

-- | The MTLBuffer containing the gradient values for beta.
--
-- ObjC selector: @- gradientForBeta@
gradientForBeta :: IsMPSCNNGroupNormalizationGradientState mpscnnGroupNormalizationGradientState => mpscnnGroupNormalizationGradientState -> IO RawId
gradientForBeta mpscnnGroupNormalizationGradientState  =
    fmap (RawId . castPtr) $ sendMsg mpscnnGroupNormalizationGradientState (mkSelector "gradientForBeta") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptorSelector :: Selector
temporaryStateWithCommandBuffer_textureDescriptorSelector = mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBufferSelector :: Selector
temporaryStateWithCommandBufferSelector = mkSelector "temporaryStateWithCommandBuffer:"

-- | @Selector@ for @temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSizeSelector :: Selector
temporaryStateWithCommandBuffer_bufferSizeSelector = mkSelector "temporaryStateWithCommandBuffer:bufferSize:"

-- | @Selector@ for @initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptorSelector :: Selector
initWithDevice_textureDescriptorSelector = mkSelector "initWithDevice:textureDescriptor:"

-- | @Selector@ for @initWithResource:@
initWithResourceSelector :: Selector
initWithResourceSelector = mkSelector "initWithResource:"

-- | @Selector@ for @initWithDevice:bufferSize:@
initWithDevice_bufferSizeSelector :: Selector
initWithDevice_bufferSizeSelector = mkSelector "initWithDevice:bufferSize:"

-- | @Selector@ for @groupNormalization@
groupNormalizationSelector :: Selector
groupNormalizationSelector = mkSelector "groupNormalization"

-- | @Selector@ for @gamma@
gammaSelector :: Selector
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @gradientForGamma@
gradientForGammaSelector :: Selector
gradientForGammaSelector = mkSelector "gradientForGamma"

-- | @Selector@ for @gradientForBeta@
gradientForBetaSelector :: Selector
gradientForBetaSelector = mkSelector "gradientForBeta"

