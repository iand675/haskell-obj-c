{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationStatisticsGradient
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationStatisticsGradient updates a MPSCNNBatchNormalizationState              with the gradient of the loss function with respect to the batch statistics and              batch normalization weights used to perform a batch normalization.
--
-- Generated bindings for @MPSCNNBatchNormalizationStatisticsGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationStatisticsGradient
  ( MPSCNNBatchNormalizationStatisticsGradient
  , IsMPSCNNBatchNormalizationStatisticsGradient(..)
  , initWithDevice_fusedNeuronDescriptor
  , initWithCoder_device
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector
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

-- | Initializes a batch normalization statistics gradient kernel using a device and neuron descriptor.
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- @fusedNeuronDescriptor@ — A MPSNNNeuronDescriptor object which specifies a neuron activation function whose                                              gradient should be applied prior to computing the statistics of the input gradient.                                              This neuron descriptor should match that used in the corresponding forward batch                                              normalization kernel.
--
-- Returns: A valid MPSCNNBatchNormalizationStatisticsGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:fusedNeuronDescriptor:@
initWithDevice_fusedNeuronDescriptor :: (IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient, IsMPSNNNeuronDescriptor fusedNeuronDescriptor) => mpscnnBatchNormalizationStatisticsGradient -> RawId -> fusedNeuronDescriptor -> IO (Id MPSCNNBatchNormalizationStatisticsGradient)
initWithDevice_fusedNeuronDescriptor mpscnnBatchNormalizationStatisticsGradient device fusedNeuronDescriptor =
  sendOwnedMessage mpscnnBatchNormalizationStatisticsGradient initWithDevice_fusedNeuronDescriptorSelector device (toMPSNNNeuronDescriptor fusedNeuronDescriptor)

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use a subclass of NSCoder that              implements the <MPSDeviceProvider> protocol  to              tell MPS the MTLDevice to use.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNBatchNormalizationStatisticsGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient, IsNSCoder aDecoder) => mpscnnBatchNormalizationStatisticsGradient -> aDecoder -> RawId -> IO (Id MPSCNNBatchNormalizationStatisticsGradient)
initWithCoder_device mpscnnBatchNormalizationStatisticsGradient aDecoder device =
  sendOwnedMessage mpscnnBatchNormalizationStatisticsGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode this operation to a command buffer.
--
-- @commandBuffer@ — The command buffer.
--
-- @sourceGradients@ — An MPSImageBatch containing the gradient of the                                              loss function with respect to the results of batch normalization                                              on the source images.
--
-- @sourceImages@ — An MPSImageBatch containing the source images for                                              batch normalization.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which                                              has been previously updated using a MPSCNNBatchNormalizationStatistics                                              kernel and the source images.  Upon completion of the                                              command buffer, will contain the (possibly partially updated)                                              gradients for the loss function with respect to the scale and                                              bias parameters used to compute the batch normalization.  The                                              state will be considered to be completely updated when all                                              MPSImages in the training batch have been processed.  If the state                                              is temporary its read count will be decremented.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState :: (IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationStatisticsGradient -> RawId -> RawId -> RawId -> batchNormalizationState -> IO ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationState mpscnnBatchNormalizationStatisticsGradient commandBuffer sourceGradients sourceImages batchNormalizationState =
  sendMessage mpscnnBatchNormalizationStatisticsGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector commandBuffer sourceGradients sourceImages (toMPSCNNBatchNormalizationState batchNormalizationState)

-- | @- encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState :: (IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSState gradientState) => mpscnnBatchNormalizationStatisticsGradient -> RawId -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState mpscnnBatchNormalizationStatisticsGradient commandBuffer sourceGradient sourceImage gradientState =
  sendMessage mpscnnBatchNormalizationStatisticsGradient encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector commandBuffer (toMPSImage sourceGradient) (toMPSImage sourceImage) (toMPSState gradientState)

-- | @- encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient :: (IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSState gradientState, IsMPSImage destinationGradient) => mpscnnBatchNormalizationStatisticsGradient -> RawId -> sourceGradient -> sourceImage -> gradientState -> destinationGradient -> IO ()
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient mpscnnBatchNormalizationStatisticsGradient commandBuffer sourceGradient sourceImage gradientState destinationGradient =
  sendMessage mpscnnBatchNormalizationStatisticsGradient encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector commandBuffer (toMPSImage sourceGradient) (toMPSImage sourceImage) (toMPSState gradientState) (toMPSImage destinationGradient)

-- | @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates :: IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient => mpscnnBatchNormalizationStatisticsGradient -> RawId -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates mpscnnBatchNormalizationStatisticsGradient commandBuffer sourceGradients sourceImages gradientStates =
  sendMessage mpscnnBatchNormalizationStatisticsGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector commandBuffer sourceGradients sourceImages gradientStates

-- | @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients :: IsMPSCNNBatchNormalizationStatisticsGradient mpscnnBatchNormalizationStatisticsGradient => mpscnnBatchNormalizationStatisticsGradient -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients mpscnnBatchNormalizationStatisticsGradient commandBuffer sourceGradients sourceImages gradientStates destinationGradients =
  sendMessage mpscnnBatchNormalizationStatisticsGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector commandBuffer sourceGradients sourceImages gradientStates destinationGradients

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:fusedNeuronDescriptor:@
initWithDevice_fusedNeuronDescriptorSelector :: Selector '[RawId, Id MPSNNNeuronDescriptor] (Id MPSCNNBatchNormalizationStatisticsGradient)
initWithDevice_fusedNeuronDescriptorSelector = mkSelector "initWithDevice:fusedNeuronDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNBatchNormalizationStatisticsGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector :: Selector '[RawId, RawId, RawId, Id MPSCNNBatchNormalizationState] ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_batchNormalizationStateSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:batchNormalizationState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSState] (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSState, Id MPSImage] ()
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector :: Selector '[RawId, RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector :: Selector '[RawId, RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:"

