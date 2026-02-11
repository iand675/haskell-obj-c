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
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector


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

-- | Initialize a MPSNNReshape kernel
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNReshape object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNReshape mpsnnReshape => mpsnnReshape -> RawId -> IO (Id MPSNNReshape)
initWithDevice mpsnnReshape  device =
    sendMsg mpsnnReshape (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNReshape mpsnnReshape, IsNSCoder aDecoder) => mpsnnReshape -> aDecoder -> RawId -> IO (Id MPSNNReshape)
initWithCoder_device mpsnnReshape  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpsnnReshape (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape  commandBuffer sourceImage reshapedWidth reshapedHeight reshapedFeatureChannels =
  withObjCPtr sourceImage $ \raw_sourceImage ->
      sendMsg mpsnnReshape (mkSelector "encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argCULong reshapedWidth, argCULong reshapedHeight, argCULong reshapedFeatureChannels] >>= retainedObject . castPtr

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
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape  commandBuffer sourceImage outState isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr outState $ \raw_outState ->
        sendMsg mpsnnReshape (mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_outState :: Ptr ()), argCULong (if isTemporary then 1 else 0), argCULong reshapedWidth, argCULong reshapedHeight, argCULong reshapedFeatureChannels] >>= retainedObject . castPtr

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
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape  commandBuffer sourceImages reshapedWidth reshapedHeight reshapedFeatureChannels =
    fmap (RawId . castPtr) $ sendMsg mpsnnReshape (mkSelector "encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argCULong reshapedWidth, argCULong reshapedHeight, argCULong reshapedFeatureChannels]

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
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannels mpsnnReshape  commandBuffer sourceImages outStates isTemporary reshapedWidth reshapedHeight reshapedFeatureChannels =
    fmap (RawId . castPtr) $ sendMsg mpsnnReshape (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId outStates) :: Ptr ()), argCULong (if isTemporary then 1 else 0), argCULong reshapedWidth, argCULong reshapedHeight, argCULong reshapedFeatureChannels]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector
encodeToCommandBuffer_sourceImage_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary_reshapedWidth_reshapedHeight_reshapedFeatureChannelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:reshapedWidth:reshapedHeight:reshapedFeatureChannels:"

