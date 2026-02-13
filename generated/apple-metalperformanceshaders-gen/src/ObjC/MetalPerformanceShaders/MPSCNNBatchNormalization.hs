{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalization
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalization normalizes input images using per-channel              means and variances.
--
-- for (c = 0; c < numberOfFeatureChannels; ++c)              {                  input_image = in(:,:,c,:);                  output_image = (input_image - mean[c]) * gamma[c] / sqrt(variance[c] + epsilon) + beta[c];                  out(:,:,c,:) = output_image;              }
--
-- Generated bindings for @MPSCNNBatchNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalization
  ( MPSCNNBatchNormalization
  , IsMPSCNNBatchNormalization(..)
  , initWithDevice_dataSource
  , initWithDevice_dataSource_fusedNeuronDescriptor
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImage
  , encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImages
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImage
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary
  , resultStateForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , reloadDataSource
  , reloadGammaAndBetaFromDataSource
  , reloadMeanAndVarianceFromDataSource
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState
  , reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceState
  , numberOfFeatureChannels
  , epsilon
  , setEpsilon
  , dataSource
  , dataSourceSelector
  , encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector
  , encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_dataSourceSelector
  , initWithDevice_dataSource_fusedNeuronDescriptorSelector
  , numberOfFeatureChannelsSelector
  , reloadDataSourceSelector
  , reloadGammaAndBetaFromDataSourceSelector
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector
  , reloadMeanAndVarianceFromDataSourceSelector
  , reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , setEpsilonSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a batch normalization kernel using a data source.
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- @dataSource@ — A pointer to a object that conforms to the MPSCNNBatchNormalizationDataSource                                              protocol.  The data source provides filter weights and bias terms and, optionally,                                              image statistics which may be used to perform the normalization.
--
-- Returns: A valid MPSCNNBatchNormalization object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:dataSource:@
initWithDevice_dataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> RawId -> IO (Id MPSCNNBatchNormalization)
initWithDevice_dataSource mpscnnBatchNormalization device dataSource =
  sendOwnedMessage mpscnnBatchNormalization initWithDevice_dataSourceSelector device dataSource

-- | Initializes a batch normalization kernel using a data source and a neuron descriptor.
--
-- @device@ — The MTLDevice on which this filter will be used
--
-- @dataSource@ — A pointer to a object that conforms to the MPSCNNBatchNormalizationDataSource                                              protocol.  The data source provides filter weights and bias terms and, optionally,                                              image statistics which may be used to perform the normalization.
--
-- @fusedNeuronDescriptor@ — A MPSNNNeuronDescriptor object which specifies a neuron activation function to                                              be applied to the result of the batch normalization.
--
-- Returns: A valid MPSCNNBatchNormalization object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:dataSource:fusedNeuronDescriptor:@
initWithDevice_dataSource_fusedNeuronDescriptor :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSNNNeuronDescriptor fusedNeuronDescriptor) => mpscnnBatchNormalization -> RawId -> RawId -> fusedNeuronDescriptor -> IO (Id MPSCNNBatchNormalization)
initWithDevice_dataSource_fusedNeuronDescriptor mpscnnBatchNormalization device dataSource fusedNeuronDescriptor =
  sendOwnedMessage mpscnnBatchNormalization initWithDevice_dataSource_fusedNeuronDescriptorSelector device dataSource (toMPSNNNeuronDescriptor fusedNeuronDescriptor)

-- | Use initWithDevice:dataSource instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> IO (Id MPSCNNBatchNormalization)
initWithDevice mpscnnBatchNormalization device =
  sendOwnedMessage mpscnnBatchNormalization initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use a subclass of NSCoder that              implements the <MPSDeviceProvider> protocol  to              tell MPS the MTLDevice to use.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNBatchNormalization object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsNSCoder aDecoder) => mpscnnBatchNormalization -> aDecoder -> RawId -> IO (Id MPSCNNBatchNormalization)
initWithCoder_device mpscnnBatchNormalization aDecoder device =
  sendOwnedMessage mpscnnBatchNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode this kernel to a command buffer for a single image using              a batch normalization state.
--
-- @commandBuffer@ — A valid command buffer to receive the kernel.
--
-- @sourceImage@ — The source MPSImage.
--
-- @batchNormalizationState@ — A MPSCNNBatchNormalizationState containing weights and/or                                          statistics to use for the batch normalization. If the state                                          is temporary its read count will be decremented.
--
-- @destinationImage@ — An MPSImage to contain the resulting normalized and scaled                                          image.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:@
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsMPSCNNBatchNormalizationState batchNormalizationState, IsMPSImage destinationImage) => mpscnnBatchNormalization -> RawId -> sourceImage -> batchNormalizationState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImage mpscnnBatchNormalization commandBuffer sourceImage batchNormalizationState destinationImage =
  sendMessage mpscnnBatchNormalization encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNBatchNormalizationState batchNormalizationState) (toMPSImage destinationImage)

-- | Encode this kernel to a command buffer for a batch of images using              a batch normalization state.
--
-- @commandBuffer@ — A valid command buffer to receive the kernel.
--
-- @sourceImages@ — The batch of source images.
--
-- @batchNormalizationState@ — A MPSCNNBatchNormalizationState containing weights and/or                                          statistics to use for the batch normalization. If the state                                          is temporary its read count will be decremented.
--
-- @destinationImages@ — The batch of images to contain the normalized and scaled                                          result images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImages :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalization -> RawId -> RawId -> batchNormalizationState -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImages mpscnnBatchNormalization commandBuffer sourceImages batchNormalizationState destinationImages =
  sendMessage mpscnnBatchNormalization encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImagesSelector commandBuffer sourceImages (toMPSCNNBatchNormalizationState batchNormalizationState) destinationImages

-- | @- encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsMPSState destinationState, IsMPSImage destinationImage) => mpscnnBatchNormalization -> RawId -> sourceImage -> destinationState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationState_destinationImage mpscnnBatchNormalization commandBuffer sourceImage destinationState destinationImage =
  sendMessage mpscnnBatchNormalization encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSState destinationState) (toMPSImage destinationImage)

-- | @- encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsMPSState outState) => mpscnnBatchNormalization -> RawId -> sourceImage -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary mpscnnBatchNormalization commandBuffer sourceImage outState isTemporary =
  sendMessage mpscnnBatchNormalization encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector commandBuffer (toMPSImage sourceImage) (toMPSState outState) isTemporary

-- | @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages mpscnnBatchNormalization commandBuffer sourceImages destinationStates destinationImages =
  sendMessage mpscnnBatchNormalization encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector commandBuffer sourceImages destinationStates destinationImages

-- | @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary mpscnnBatchNormalization commandBuffer sourceImages outStates isTemporary =
  sendMessage mpscnnBatchNormalization encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector commandBuffer sourceImages outStates isTemporary

-- | Return an MPSCNNBatchNormalizationState object which may be used with a MPSCNNBatchNormalization filter.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBatchNormalization -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNBatchNormalizationState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnBatchNormalization sourceImage sourceStates destinationImage =
  sendMessage mpscnnBatchNormalization resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | Return a temporary MPSCNNBatchNormalizationState object which may be used with                  a MPSCNNBatchNormalization filter.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBatchNormalization -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNBatchNormalizationState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnBatchNormalization commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnBatchNormalization temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | Reinitialize the filter using a data source.
--
-- @dataSource@ — The data source which will provide the weights and, optionally, the                          image batch statistics with which to normalize.
--
-- ObjC selector: @- reloadDataSource:@
reloadDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> IO ()
reloadDataSource mpscnnBatchNormalization dataSource =
  sendMessage mpscnnBatchNormalization reloadDataSourceSelector dataSource

-- | Reinitialize the filter's gamma and beta values using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO ()
reloadGammaAndBetaFromDataSource mpscnnBatchNormalization =
  sendMessage mpscnnBatchNormalization reloadGammaAndBetaFromDataSourceSelector

-- | Reinitialize the filter's mean and variance values using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadMeanAndVarianceFromDataSource@
reloadMeanAndVarianceFromDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO ()
reloadMeanAndVarianceFromDataSource mpscnnBatchNormalization =
  sendMessage mpscnnBatchNormalization reloadMeanAndVarianceFromDataSourceSelector

-- | Reload data using new gamma and beta terms contained within an              MPSCNNNormalizationGammaAndBetaState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @gammaAndBetaState@ — The state containing the updated weights which are to                                          be reloaded.
--
-- ObjC selector: @- reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSCNNNormalizationGammaAndBetaState gammaAndBetaState) => mpscnnBatchNormalization -> RawId -> gammaAndBetaState -> IO ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState mpscnnBatchNormalization commandBuffer gammaAndBetaState =
  sendMessage mpscnnBatchNormalization reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector commandBuffer (toMPSCNNNormalizationGammaAndBetaState gammaAndBetaState)

-- | Reload data using new mean and variance terms contained within an              MPSCNNNormalizationMeanAndVarianceState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @meanAndVarianceState@ — The state containing the updated statistics which are to                                          be reloaded.
--
-- ObjC selector: @- reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:@
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceState :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSCNNNormalizationMeanAndVarianceState meanAndVarianceState) => mpscnnBatchNormalization -> RawId -> meanAndVarianceState -> IO ()
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceState mpscnnBatchNormalization commandBuffer meanAndVarianceState =
  sendMessage mpscnnBatchNormalization reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector commandBuffer (toMPSCNNNormalizationMeanAndVarianceState meanAndVarianceState)

-- | numberOfFeatureChannels
--
-- The number of feature channels in an image to be normalized.
--
-- ObjC selector: @- numberOfFeatureChannels@
numberOfFeatureChannels :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO CULong
numberOfFeatureChannels mpscnnBatchNormalization =
  sendMessage mpscnnBatchNormalization numberOfFeatureChannelsSelector

-- | epsilon
--
-- The epsilon value used in the batch normalization formula to              bias the variance when normalizing.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO CFloat
epsilon mpscnnBatchNormalization =
  sendMessage mpscnnBatchNormalization epsilonSelector

-- | epsilon
--
-- The epsilon value used in the batch normalization formula to              bias the variance when normalizing.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> CFloat -> IO ()
setEpsilon mpscnnBatchNormalization value =
  sendMessage mpscnnBatchNormalization setEpsilonSelector value

-- | The data source the batch normalization was initialized with
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO RawId
dataSource mpscnnBatchNormalization =
  sendMessage mpscnnBatchNormalization dataSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataSource:@
initWithDevice_dataSourceSelector :: Selector '[RawId, RawId] (Id MPSCNNBatchNormalization)
initWithDevice_dataSourceSelector = mkSelector "initWithDevice:dataSource:"

-- | @Selector@ for @initWithDevice:dataSource:fusedNeuronDescriptor:@
initWithDevice_dataSource_fusedNeuronDescriptorSelector :: Selector '[RawId, RawId, Id MPSNNNeuronDescriptor] (Id MPSCNNBatchNormalization)
initWithDevice_dataSource_fusedNeuronDescriptorSelector = mkSelector "initWithDevice:dataSource:fusedNeuronDescriptor:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNBatchNormalization)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNBatchNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:@
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSCNNBatchNormalizationState, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImagesSelector :: Selector '[RawId, RawId, Id MPSCNNBatchNormalizationState, RawId] ()
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSState, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector :: Selector '[RawId, Id MPSImage, Id MPSState, Bool] (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector :: Selector '[RawId, RawId, RawId, Bool] RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNBatchNormalizationState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNBatchNormalizationState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @reloadDataSource:@
reloadDataSourceSelector :: Selector '[RawId] ()
reloadDataSourceSelector = mkSelector "reloadDataSource:"

-- | @Selector@ for @reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSourceSelector :: Selector '[] ()
reloadGammaAndBetaFromDataSourceSelector = mkSelector "reloadGammaAndBetaFromDataSource"

-- | @Selector@ for @reloadMeanAndVarianceFromDataSource@
reloadMeanAndVarianceFromDataSourceSelector :: Selector '[] ()
reloadMeanAndVarianceFromDataSourceSelector = mkSelector "reloadMeanAndVarianceFromDataSource"

-- | @Selector@ for @reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector :: Selector '[RawId, Id MPSCNNNormalizationGammaAndBetaState] ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector = mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:"

-- | @Selector@ for @reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:@
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector :: Selector '[RawId, Id MPSCNNNormalizationMeanAndVarianceState] ()
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector = mkSelector "reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:"

-- | @Selector@ for @numberOfFeatureChannels@
numberOfFeatureChannelsSelector :: Selector '[] CULong
numberOfFeatureChannelsSelector = mkSelector "numberOfFeatureChannels"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector '[CFloat] ()
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

