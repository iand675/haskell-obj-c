{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNNReshape@.
module ObjC.MetalPerformanceShaders.MPSNNReshape
  ( MPSNNReshape
  , IsMPSNNReshape(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannels
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels
  , encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannels
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a MPSNNReshape kernel
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNReshape object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReshape mpsnnReshape => mpsnnReshape -> RawId -> IO (Id MPSNNReshape)
initWithDevice mpsnnReshape device =
  sendOwnedMessage mpsnnReshape initWithDeviceSelector device

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReshape mpsnnReshape, IsNSCoder aDecoder) => mpsnnReshape -> aDecoder -> RawId -> IO (Id MPSNNReshape)
initWithCoder_device mpsnnReshape aDecoder device =
  sendOwnedMessage mpsnnReshape initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a reshape to a command buffer for a given shape.
--
-- @commandBuffer@ — The command buffer on which to encode the reshape operation.
--
-- @sourceImage@ — The input image to be reshaped.
--
-- @reshapedWidth@ — The width of the resulting reshaped image.
--
-- @reshapedHeight@ — The height of the resulting reshaped image.
--
-- @reshapedFeatureChannels@ — The number of feature channels in the resulting reshaped image.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannels :: (IsMPSNNReshape mpsnnReshape, IsMPSImage sourceImage) => mpsnnReshape -> RawId -> sourceImage -> CULong -> CULong -> CULong -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape commandBuffer sourceImage reshapedWidth reshapedHeight reshapedFeatureChannels =
  sendMessage mpsnnReshape encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector commandBuffer (toMPSImage sourceImage) reshapedWidth reshapedHeight reshapedFeatureChannels

-- | Encode a reshape to a command buffer for a given shape.
--
-- @commandBuffer@ — The command buffer on which to encode the reshape operation.
--
-- @outState@ — A state to be created and autoreleased which will hold information about this execution                                  to be provided to a subsequent gradient pass.
--
-- @isTemporary@ — YES if the state is to be created as a temporary state, NO otherwise.
--
-- @sourceImage@ — The input image to be reshaped.
--
-- @reshapedWidth@ — The width of the resulting reshaped image.
--
-- @reshapedHeight@ — The height of the resulting reshaped image.
--
-- @reshapedFeatureChannels@ — The number of feature channels in the resulting reshaped image.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels :: (IsMPSNNReshape mpsnnReshape, IsMPSImage sourceImage, IsMPSState outState) => mpsnnReshape -> RawId -> sourceImage -> outState -> Bool -> CULong -> CULong -> CULong -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape commandBuffer sourceImage outState isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels =
  sendMessage mpsnnReshape encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector commandBuffer (toMPSImage sourceImage) (toMPSState outState) isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels

-- | Encode a reshape to a command buffer for a given shape.
--
-- @commandBuffer@ — The command buffer on which to encode the reshape operation.
--
-- @sourceImages@ — The image batch containing images to be reshaped.
--
-- @reshapedWidth@ — The width of the resulting reshaped images.
--
-- @reshapedHeight@ — The height of the resulting reshaped images.
--
-- @reshapedFeatureChannels@ — The number of feature channels in each of the resulting reshaped images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannels :: IsMPSNNReshape mpsnnReshape => mpsnnReshape -> RawId -> RawId -> CULong -> CULong -> CULong -> IO RawId
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape commandBuffer sourceImages reshapedWidth reshapedHeight reshapedFeatureChannels =
  sendMessage mpsnnReshape encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector commandBuffer sourceImages reshapedWidth reshapedHeight reshapedFeatureChannels

-- | Encode a reshape to a command buffer for a given shape.
--
-- @commandBuffer@ — The command buffer on which to encode the reshape operation.
--
-- @outStates@ — A batch of states to be created and autoreleased which will hold information about this execution                                  to be provided to a subsequent gradient pass.
--
-- @isTemporary@ — YES if the states are to be created as temporary states, NO otherwise.
--
-- @sourceImages@ — The batch of input images to be reshaped.
--
-- @reshapedWidth@ — The width of the resulting reshaped images.
--
-- @reshapedHeight@ — The height of the resulting reshaped images.
--
-- @reshapedFeatureChannels@ — The number of feature channels in each of the resulting reshaped images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels :: IsMPSNNReshape mpsnnReshape => mpsnnReshape -> RawId -> RawId -> RawId -> Bool -> CULong -> CULong -> CULong -> IO RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape commandBuffer sourceImages outStates isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels =
  sendMessage mpsnnReshape encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector commandBuffer sourceImages outStates isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNReshape)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNReshape)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector '[RawId, Id MPSImage, CULong, CULong, CULong] (Id MPSImage)
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector '[RawId, Id MPSImage, Id MPSState, Bool, CULong, CULong, CULong] (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector '[RawId, RawId, CULong, CULong, CULong] RawId
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector '[RawId, RawId, RawId, Bool, CULong, CULong, CULong] RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

