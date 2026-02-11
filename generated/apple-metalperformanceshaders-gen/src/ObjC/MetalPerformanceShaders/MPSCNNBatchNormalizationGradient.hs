{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationGradient
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationGradient computes the gradients of a              loss function resulting from a network containing a corresponding              MPSCNNBatchNormalization kernel.
--
-- Two sets of values are computed: the gradient of the loss function              with respect to the batch normalization source images, and the              gradient of the loss function with respect to the scale and bias              terms used to compute the batch normalization.
--
-- Generated bindings for @MPSCNNBatchNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationGradient
  ( MPSCNNBatchNormalizationGradient
  , IsMPSCNNBatchNormalizationGradient(..)
  , initWithDevice_fusedNeuronDescriptor
  , initWithCoder_device
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradient
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradients
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages
  , encodeToCommandBuffer_primaryImage_secondaryImage
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages
  , initWithDevice_fusedNeuronDescriptorSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector
  , encodeToCommandBuffer_primaryImage_secondaryImageSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector


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

-- | Initializes a batch normalization gradient kernel using a device and neuron descriptor.
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- @fusedNeuronDescriptor@ — A MPSNNNeuronDescriptor object which specifies a neuron activation function whose                                              gradient should be applied prior to computing the resulting gradient.                                              This neuron descriptor should match that used in the corresponding forward batch                                              normalization kernel as well as the preceeding batch normalization statistics gradient                                              kernel.
--
-- Returns: A valid MPSCNNBatchNormalizationGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:fusedNeuronDescriptor:@
initWithDevice_fusedNeuronDescriptor :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSNNNeuronDescriptor fusedNeuronDescriptor) => mpscnnBatchNormalizationGradient -> RawId -> fusedNeuronDescriptor -> IO (Id MPSCNNBatchNormalizationGradient)
initWithDevice_fusedNeuronDescriptor mpscnnBatchNormalizationGradient  device fusedNeuronDescriptor =
  withObjCPtr fusedNeuronDescriptor $ \raw_fusedNeuronDescriptor ->
      sendMsg mpscnnBatchNormalizationGradient (mkSelector "initWithDevice:fusedNeuronDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_fusedNeuronDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use a subclass of NSCoder that              implements the <MPSDeviceProvider> protocol  to              tell MPS the MTLDevice to use.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNBatchNormalizationGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsNSCoder aDecoder) => mpscnnBatchNormalizationGradient -> aDecoder -> RawId -> IO (Id MPSCNNBatchNormalizationGradient)
initWithCoder_device mpscnnBatchNormalizationGradient  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnBatchNormalizationGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode this operation to a command buffer for a single image.
--
-- @commandBuffer@ — The command buffer.
--
-- @sourceGradient@ — An MPSImage containing the gradient of the loss function with                                              respect to the results of batch normalization on the source image.
--
-- @sourceImage@ — An MPSImage containing the source image for batch normalization.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which                                              has been previously updated using a MPSCNNBatchNormalizationStatisticsGradient                                              kernel and the source images. If the state is temporary its read count will be decremented.
--
-- @destinationGradient@ — An MPSImage which contains the gradient of the loss function with respect to the source image.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradient :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSCNNBatchNormalizationState batchNormalizationState, IsMPSImage destinationGradient) => mpscnnBatchNormalizationGradient -> RawId -> sourceGradient -> sourceImage -> batchNormalizationState -> destinationGradient -> IO ()
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradient mpscnnBatchNormalizationGradient  commandBuffer sourceGradient sourceImage batchNormalizationState destinationGradient =
  withObjCPtr sourceGradient $ \raw_sourceGradient ->
    withObjCPtr sourceImage $ \raw_sourceImage ->
      withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
        withObjCPtr destinationGradient $ \raw_destinationGradient ->
            sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_destinationGradient :: Ptr ())]

-- | Encode this operation to a command buffer.
--
-- @commandBuffer@ — The command buffer.
--
-- @sourceGradients@ — An MPSImageBatch containing the gradient of the                                              loss function with respect to the results of batch normalization                                              on the source images.
--
-- @sourceImages@ — An MPSImageBatch containing the source images for                                              batch normalization.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which                                              has been previously updated using a MPSCNNBatchNormalizationStatisticsGradient                                              kernel and the source images. If the state is temporary its read count will be decremented.
--
-- @destinationGradients@ — An MPSImageBatch whose images will contain the gradient                                              of the loss function with respect to the source images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradients :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> batchNormalizationState -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradients mpscnnBatchNormalizationGradient  commandBuffer sourceGradients sourceImages batchNormalizationState destinationGradients =
  withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
      sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceGradients) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr (unRawId destinationGradients) :: Ptr ())]

-- | Encode this operation to a command buffer.  Create an MPSImage to contain                  the result and return it.                  See encodeToCommandBuffer:sourceImage:sourceGradient:sourceImage:batchNormalizationState:destinationGradient                  for further details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationGradient -> RawId -> sourceGradient -> sourceImage -> batchNormalizationState -> IO (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState mpscnnBatchNormalizationGradient  commandBuffer sourceGradient sourceImage batchNormalizationState =
  withObjCPtr sourceGradient $ \raw_sourceGradient ->
    withObjCPtr sourceImage $ \raw_sourceImage ->
      withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
          sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ())] >>= retainedObject . castPtr

-- | Encode this operation to a command buffer.  Create an MPSImageBatch to contain                  the result and return it.                  See encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients                  for further details.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> batchNormalizationState -> IO RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState mpscnnBatchNormalizationGradient  commandBuffer sourceGradients sourceImages batchNormalizationState =
  withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
      fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceGradients) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ())]

-- | @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSImage destinationImage) => mpscnnBatchNormalizationGradient -> RawId -> primaryImage -> secondaryImage -> destinationImage -> IO ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage mpscnnBatchNormalizationGradient  commandBuffer primaryImage secondaryImage destinationImage =
  withObjCPtr primaryImage $ \raw_primaryImage ->
    withObjCPtr secondaryImage $ \raw_secondaryImage ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_primaryImage :: Ptr ()), argPtr (castPtr raw_secondaryImage :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages :: IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages mpscnnBatchNormalizationGradient  commandBuffer primaryImages secondaryImages destinationImages =
    sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId primaryImages) :: Ptr ()), argPtr (castPtr (unRawId secondaryImages) :: Ptr ()), argPtr (castPtr (unRawId destinationImages) :: Ptr ())]

-- | @- encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImage :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage primaryImage, IsMPSImage secondaryImage) => mpscnnBatchNormalizationGradient -> RawId -> primaryImage -> secondaryImage -> IO (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImage mpscnnBatchNormalizationGradient  commandBuffer primaryImage secondaryImage =
  withObjCPtr primaryImage $ \raw_primaryImage ->
    withObjCPtr secondaryImage $ \raw_secondaryImage ->
        sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_primaryImage :: Ptr ()), argPtr (castPtr raw_secondaryImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages :: IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImages mpscnnBatchNormalizationGradient  commandBuffer primaryImage secondaryImage =
    fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationGradient (mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId primaryImage) :: Ptr ()), argPtr (castPtr (unRawId secondaryImage) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:fusedNeuronDescriptor:@
initWithDevice_fusedNeuronDescriptorSelector :: Selector
initWithDevice_fusedNeuronDescriptorSelector = mkSelector "initWithDevice:fusedNeuronDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector :: Selector
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector :: Selector
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector :: Selector
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector :: Selector
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector :: Selector
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImageSelector :: Selector
encodeToCommandBuffer_primaryImage_secondaryImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector :: Selector
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:"

