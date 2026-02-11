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
  , temporaryStateWithCommandBuffer_textureDescriptorSelector
  , temporaryStateWithCommandBufferSelector
  , temporaryStateWithCommandBuffer_bufferSizeSelector
  , initWithDevice_textureDescriptorSelector
  , initWithResourceSelector
  , initWithDevice_bufferSizeSelector
  , instanceNormalizationSelector


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

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @+ temporaryStateWithCommandBuffer:textureDescriptor:@
temporaryStateWithCommandBuffer_textureDescriptor :: IsMTLTextureDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_textureDescriptor cmdBuf descriptor =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @+ temporaryStateWithCommandBuffer:@
temporaryStateWithCommandBuffer :: RawId -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer cmdBuf =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ temporaryStateWithCommandBuffer:bufferSize:@
temporaryStateWithCommandBuffer_bufferSize :: RawId -> CULong -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryStateWithCommandBuffer_bufferSize cmdBuf bufferSize =
  do
    cls' <- getRequiredClass "MPSCNNInstanceNormalizationGradientState"
    sendClassMsg cls' (mkSelector "temporaryStateWithCommandBuffer:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argCULong (fromIntegral bufferSize)] >>= retainedObject . castPtr

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @- initWithDevice:textureDescriptor:@
initWithDevice_textureDescriptor :: (IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState, IsMTLTextureDescriptor descriptor) => mpscnnInstanceNormalizationGradientState -> RawId -> descriptor -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_textureDescriptor mpscnnInstanceNormalizationGradientState  device descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpscnnInstanceNormalizationGradientState (mkSelector "initWithDevice:textureDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Unavailable.  Use MPSCNNInstanceNormalization state creation methods.
--
-- ObjC selector: @- initWithResource:@
initWithResource :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> RawId -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithResource mpscnnInstanceNormalizationGradientState  resource =
  sendMsg mpscnnInstanceNormalizationGradientState (mkSelector "initWithResource:") (retPtr retVoid) [argPtr (castPtr (unRawId resource) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:bufferSize:@
initWithDevice_bufferSize :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> RawId -> CULong -> IO (Id MPSCNNInstanceNormalizationGradientState)
initWithDevice_bufferSize mpscnnInstanceNormalizationGradientState  device bufferSize =
  sendMsg mpscnnInstanceNormalizationGradientState (mkSelector "initWithDevice:bufferSize:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral bufferSize)] >>= ownedObject . castPtr

-- | The MPSCNNInstanceNormalization object that created this state object.
--
-- ObjC selector: @- instanceNormalization@
instanceNormalization :: IsMPSCNNInstanceNormalizationGradientState mpscnnInstanceNormalizationGradientState => mpscnnInstanceNormalizationGradientState -> IO (Id MPSCNNInstanceNormalization)
instanceNormalization mpscnnInstanceNormalizationGradientState  =
  sendMsg mpscnnInstanceNormalizationGradientState (mkSelector "instanceNormalization") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @instanceNormalization@
instanceNormalizationSelector :: Selector
instanceNormalizationSelector = mkSelector "instanceNormalization"

