{-# LANGUAGE DataKinds #-}
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
  , encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector
  , encodeToCommandBuffer_primaryImage_secondaryImageSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector
  , initWithCoder_deviceSelector
  , initWithDevice_fusedNeuronDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice_fusedNeuronDescriptor mpscnnBatchNormalizationGradient device fusedNeuronDescriptor =
  sendOwnedMessage mpscnnBatchNormalizationGradient initWithDevice_fusedNeuronDescriptorSelector device (toMPSNNNeuronDescriptor fusedNeuronDescriptor)

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
initWithCoder_device mpscnnBatchNormalizationGradient aDecoder device =
  sendOwnedMessage mpscnnBatchNormalizationGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradient mpscnnBatchNormalizationGradient commandBuffer sourceGradient sourceImage batchNormalizationState destinationGradient =
  sendMessage mpscnnBatchNormalizationGradient encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector commandBuffer (toMPSImage sourceGradient) (toMPSImage sourceImage) (toMPSCNNBatchNormalizationState batchNormalizationState) (toMPSImage destinationGradient)

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
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradients mpscnnBatchNormalizationGradient commandBuffer sourceGradients sourceImages batchNormalizationState destinationGradients =
  sendMessage mpscnnBatchNormalizationGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector commandBuffer sourceGradients sourceImages (toMPSCNNBatchNormalizationState batchNormalizationState) destinationGradients

-- | Encode this operation to a command buffer.  Create an MPSImage to contain                  the result and return it.                  See encodeToCommandBuffer:sourceImage:sourceGradient:sourceImage:batchNormalizationState:destinationGradient                  for further details.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationGradient -> RawId -> sourceGradient -> sourceImage -> batchNormalizationState -> IO (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState mpscnnBatchNormalizationGradient commandBuffer sourceGradient sourceImage batchNormalizationState =
  sendMessage mpscnnBatchNormalizationGradient encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector commandBuffer (toMPSImage sourceGradient) (toMPSImage sourceImage) (toMPSCNNBatchNormalizationState batchNormalizationState)

-- | Encode this operation to a command buffer.  Create an MPSImageBatch to contain                  the result and return it.                  See encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients                  for further details.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> batchNormalizationState -> IO RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState mpscnnBatchNormalizationGradient commandBuffer sourceGradients sourceImages batchNormalizationState =
  sendMessage mpscnnBatchNormalizationGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector commandBuffer sourceGradients sourceImages (toMPSCNNBatchNormalizationState batchNormalizationState)

-- | @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSImage destinationImage) => mpscnnBatchNormalizationGradient -> RawId -> primaryImage -> secondaryImage -> destinationImage -> IO ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage mpscnnBatchNormalizationGradient commandBuffer primaryImage secondaryImage destinationImage =
  sendMessage mpscnnBatchNormalizationGradient encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage) (toMPSImage destinationImage)

-- | @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages :: IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages mpscnnBatchNormalizationGradient commandBuffer primaryImages secondaryImages destinationImages =
  sendMessage mpscnnBatchNormalizationGradient encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector commandBuffer primaryImages secondaryImages destinationImages

-- | @- encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImage :: (IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient, IsMPSImage primaryImage, IsMPSImage secondaryImage) => mpscnnBatchNormalizationGradient -> RawId -> primaryImage -> secondaryImage -> IO (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImage mpscnnBatchNormalizationGradient commandBuffer primaryImage secondaryImage =
  sendMessage mpscnnBatchNormalizationGradient encodeToCommandBuffer_primaryImage_secondaryImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage)

-- | @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages :: IsMPSCNNBatchNormalizationGradient mpscnnBatchNormalizationGradient => mpscnnBatchNormalizationGradient -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImages mpscnnBatchNormalizationGradient commandBuffer primaryImage secondaryImage =
  sendMessage mpscnnBatchNormalizationGradient encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector commandBuffer primaryImage secondaryImage

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:fusedNeuronDescriptor:@
initWithDevice_fusedNeuronDescriptorSelector :: Selector '[RawId, Id MPSNNNeuronDescriptor] (Id MPSCNNBatchNormalizationGradient)
initWithDevice_fusedNeuronDescriptorSelector = mkSelector "initWithDevice:fusedNeuronDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNBatchNormalizationGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSCNNBatchNormalizationState, Id MPSImage] ()
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationState_destinationGradientSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:destinationGradient:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector :: Selector '[RawId, RawId, RawId, Id MPSCNNBatchNormalizationState, RawId] ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState_destinationGradientsSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:destinationGradients:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:@
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSCNNBatchNormalizationState] (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_batchNormalizationStateSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:batchNormalizationState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector :: Selector '[RawId, RawId, RawId, Id MPSCNNBatchNormalizationState] RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSImage] ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage] (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector :: Selector '[RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:"

