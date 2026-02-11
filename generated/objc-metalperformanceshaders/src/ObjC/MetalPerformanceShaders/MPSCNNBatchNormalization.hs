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
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImage
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary
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
  , initWithDevice_dataSourceSelector
  , initWithDevice_dataSource_fusedNeuronDescriptorSelector
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , reloadDataSourceSelector
  , reloadGammaAndBetaFromDataSourceSelector
  , reloadMeanAndVarianceFromDataSourceSelector
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector
  , reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector
  , numberOfFeatureChannelsSelector
  , epsilonSelector
  , setEpsilonSelector


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
initWithDevice_dataSource mpscnnBatchNormalization  device dataSource =
  sendMsg mpscnnBatchNormalization (mkSelector "initWithDevice:dataSource:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_dataSource_fusedNeuronDescriptor mpscnnBatchNormalization  device dataSource fusedNeuronDescriptor =
withObjCPtr fusedNeuronDescriptor $ \raw_fusedNeuronDescriptor ->
    sendMsg mpscnnBatchNormalization (mkSelector "initWithDevice:dataSource:fusedNeuronDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ()), argPtr (castPtr raw_fusedNeuronDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Use initWithDevice:dataSource instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> IO (Id MPSCNNBatchNormalization)
initWithDevice mpscnnBatchNormalization  device =
  sendMsg mpscnnBatchNormalization (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnBatchNormalization  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnBatchNormalization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImage mpscnnBatchNormalization  commandBuffer sourceImage batchNormalizationState destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnBatchNormalization (mkSelector "encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsMPSState destinationState, IsMPSImage destinationImage) => mpscnnBatchNormalization -> RawId -> sourceImage -> destinationState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationState_destinationImage mpscnnBatchNormalization  commandBuffer sourceImage destinationState destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr destinationState $ \raw_destinationState ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnBatchNormalization (mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_destinationState :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsMPSState outState) => mpscnnBatchNormalization -> RawId -> sourceImage -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary mpscnnBatchNormalization  commandBuffer sourceImage outState isTemporary =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr outState $ \raw_outState ->
      sendMsg mpscnnBatchNormalization (mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_outState :: Ptr ()), argCULong (if isTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Return an MPSCNNBatchNormalizationState object which may be used with a MPSCNNBatchNormalization filter.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBatchNormalization -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNBatchNormalizationState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnBatchNormalization  sourceImage sourceStates destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnBatchNormalization (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | Return a temporary MPSCNNBatchNormalizationState object which may be used with                  a MPSCNNBatchNormalization filter.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBatchNormalization -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNBatchNormalizationState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnBatchNormalization  commandBuffer sourceImage sourceStates destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnBatchNormalization (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | Reinitialize the filter using a data source.
--
-- @dataSource@ — The data source which will provide the weights and, optionally, the                          image batch statistics with which to normalize.
--
-- ObjC selector: @- reloadDataSource:@
reloadDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> RawId -> IO ()
reloadDataSource mpscnnBatchNormalization  dataSource =
  sendMsg mpscnnBatchNormalization (mkSelector "reloadDataSource:") retVoid [argPtr (castPtr (unRawId dataSource) :: Ptr ())]

-- | Reinitialize the filter's gamma and beta values using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO ()
reloadGammaAndBetaFromDataSource mpscnnBatchNormalization  =
  sendMsg mpscnnBatchNormalization (mkSelector "reloadGammaAndBetaFromDataSource") retVoid []

-- | Reinitialize the filter's mean and variance values using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadMeanAndVarianceFromDataSource@
reloadMeanAndVarianceFromDataSource :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO ()
reloadMeanAndVarianceFromDataSource mpscnnBatchNormalization  =
  sendMsg mpscnnBatchNormalization (mkSelector "reloadMeanAndVarianceFromDataSource") retVoid []

-- | Reload data using new gamma and beta terms contained within an              MPSCNNNormalizationGammaAndBetaState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @gammaAndBetaState@ — The state containing the updated weights which are to                                          be reloaded.
--
-- ObjC selector: @- reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSCNNNormalizationGammaAndBetaState gammaAndBetaState) => mpscnnBatchNormalization -> RawId -> gammaAndBetaState -> IO ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState mpscnnBatchNormalization  commandBuffer gammaAndBetaState =
withObjCPtr gammaAndBetaState $ \raw_gammaAndBetaState ->
    sendMsg mpscnnBatchNormalization (mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_gammaAndBetaState :: Ptr ())]

-- | Reload data using new mean and variance terms contained within an              MPSCNNNormalizationMeanAndVarianceState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @meanAndVarianceState@ — The state containing the updated statistics which are to                                          be reloaded.
--
-- ObjC selector: @- reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:@
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceState :: (IsMPSCNNBatchNormalization mpscnnBatchNormalization, IsMPSCNNNormalizationMeanAndVarianceState meanAndVarianceState) => mpscnnBatchNormalization -> RawId -> meanAndVarianceState -> IO ()
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceState mpscnnBatchNormalization  commandBuffer meanAndVarianceState =
withObjCPtr meanAndVarianceState $ \raw_meanAndVarianceState ->
    sendMsg mpscnnBatchNormalization (mkSelector "reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_meanAndVarianceState :: Ptr ())]

-- | numberOfFeatureChannels
--
-- The number of feature channels in an image to be normalized.
--
-- ObjC selector: @- numberOfFeatureChannels@
numberOfFeatureChannels :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO CULong
numberOfFeatureChannels mpscnnBatchNormalization  =
  sendMsg mpscnnBatchNormalization (mkSelector "numberOfFeatureChannels") retCULong []

-- | epsilon
--
-- The epsilon value used in the batch normalization formula to              bias the variance when normalizing.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> IO CFloat
epsilon mpscnnBatchNormalization  =
  sendMsg mpscnnBatchNormalization (mkSelector "epsilon") retCFloat []

-- | epsilon
--
-- The epsilon value used in the batch normalization formula to              bias the variance when normalizing.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNBatchNormalization mpscnnBatchNormalization => mpscnnBatchNormalization -> CFloat -> IO ()
setEpsilon mpscnnBatchNormalization  value =
  sendMsg mpscnnBatchNormalization (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataSource:@
initWithDevice_dataSourceSelector :: Selector
initWithDevice_dataSourceSelector = mkSelector "initWithDevice:dataSource:"

-- | @Selector@ for @initWithDevice:dataSource:fusedNeuronDescriptor:@
initWithDevice_dataSource_fusedNeuronDescriptorSelector :: Selector
initWithDevice_dataSource_fusedNeuronDescriptorSelector = mkSelector "initWithDevice:dataSource:fusedNeuronDescriptor:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:@
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_batchNormalizationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:batchNormalizationState:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @reloadDataSource:@
reloadDataSourceSelector :: Selector
reloadDataSourceSelector = mkSelector "reloadDataSource:"

-- | @Selector@ for @reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSourceSelector :: Selector
reloadGammaAndBetaFromDataSourceSelector = mkSelector "reloadGammaAndBetaFromDataSource"

-- | @Selector@ for @reloadMeanAndVarianceFromDataSource@
reloadMeanAndVarianceFromDataSourceSelector :: Selector
reloadMeanAndVarianceFromDataSourceSelector = mkSelector "reloadMeanAndVarianceFromDataSource"

-- | @Selector@ for @reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector :: Selector
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector = mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:"

-- | @Selector@ for @reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:@
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector :: Selector
reloadMeanAndVarianceWithCommandBuffer_meanAndVarianceStateSelector = mkSelector "reloadMeanAndVarianceWithCommandBuffer:meanAndVarianceState:"

-- | @Selector@ for @numberOfFeatureChannels@
numberOfFeatureChannelsSelector :: Selector
numberOfFeatureChannelsSelector = mkSelector "numberOfFeatureChannels"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector
setEpsilonSelector = mkSelector "setEpsilon:"

