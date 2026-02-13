{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNInstanceNormalization
--
-- This depends on Metal.framework
--
-- This kernel normalizes each image, on a per-channel basis, to              have zero mean and unit variance:
--
-- for each image:                  for each channel:                      y = (x - mean) * gamma / sqrt(variance + epsilon) + beta;
--
-- Generated bindings for @MPSCNNInstanceNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNInstanceNormalization
  ( MPSCNNInstanceNormalization
  , IsMPSCNNInstanceNormalization(..)
  , initWithDevice_dataSource
  , initWithDevice
  , initWithCoder_device
  , reloadDataSource
  , reloadGammaAndBetaFromDataSource
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState
  , resultStateForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , epsilon
  , setEpsilon
  , dataSource
  , dataSourceSelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_dataSourceSelector
  , reloadDataSourceSelector
  , reloadGammaAndBetaFromDataSourceSelector
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector
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

-- | Initialize a MPSCNNInstanceNormalization kernel on a device.
--
-- @dataSource@ — An object conforming to the MPSCNNInstanceNormalizationDataSource                          protocol which
--
-- ObjC selector: @- initWithDevice:dataSource:@
initWithDevice_dataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> RawId -> IO (Id MPSCNNInstanceNormalization)
initWithDevice_dataSource mpscnnInstanceNormalization device dataSource =
  sendOwnedMessage mpscnnInstanceNormalization initWithDevice_dataSourceSelector device dataSource

-- | Use initWithDevice:dataSource instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> IO (Id MPSCNNInstanceNormalization)
initWithDevice mpscnnInstanceNormalization device =
  sendOwnedMessage mpscnnInstanceNormalization initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNInstanceNormalization object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsNSCoder aDecoder) => mpscnnInstanceNormalization -> aDecoder -> RawId -> IO (Id MPSCNNInstanceNormalization)
initWithCoder_device mpscnnInstanceNormalization aDecoder device =
  sendOwnedMessage mpscnnInstanceNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Reload data using a data source.
--
-- @dataSource@ — The data source which will provide the gamma and beta terms                          to scale and bias the normalized result respectively.
--
-- ObjC selector: @- reloadDataSource:@
reloadDataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> IO ()
reloadDataSource mpscnnInstanceNormalization dataSource =
  sendMessage mpscnnInstanceNormalization reloadDataSourceSelector dataSource

-- | Reinitialize the filter using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO ()
reloadGammaAndBetaFromDataSource mpscnnInstanceNormalization =
  sendMessage mpscnnInstanceNormalization reloadGammaAndBetaFromDataSourceSelector

-- | Reload data using new gamma and beta terms contained within an              MPSCNNInstanceNormalizationGradientState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @gammaAndBetaState@ — The state containing the updated weights which are to                                          be reloaded.
--
-- ObjC selector: @- reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSCNNNormalizationGammaAndBetaState gammaAndBetaState) => mpscnnInstanceNormalization -> RawId -> gammaAndBetaState -> IO ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState mpscnnInstanceNormalization commandBuffer gammaAndBetaState =
  sendMessage mpscnnInstanceNormalization reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector commandBuffer (toMPSCNNNormalizationGammaAndBetaState gammaAndBetaState)

-- | Return a MPSCNNInstanceNormalizationGradientState object for the provided              source image, source states, and destination image.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnInstanceNormalization -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNInstanceNormalizationGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnInstanceNormalization sourceImage sourceStates destinationImage =
  sendMessage mpscnnInstanceNormalization resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | Return a temporary MPSCNNInstanceNormalizationGradientState object which may be used with                  a MPSCNNInstanceNormalization filter.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnInstanceNormalization -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnInstanceNormalization commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnInstanceNormalization temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO CFloat
epsilon mpscnnInstanceNormalization =
  sendMessage mpscnnInstanceNormalization epsilonSelector

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> CFloat -> IO ()
setEpsilon mpscnnInstanceNormalization value =
  sendMessage mpscnnInstanceNormalization setEpsilonSelector value

-- | The data source that the object was initialized with
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO RawId
dataSource mpscnnInstanceNormalization =
  sendMessage mpscnnInstanceNormalization dataSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataSource:@
initWithDevice_dataSourceSelector :: Selector '[RawId, RawId] (Id MPSCNNInstanceNormalization)
initWithDevice_dataSourceSelector = mkSelector "initWithDevice:dataSource:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNInstanceNormalization)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNInstanceNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @reloadDataSource:@
reloadDataSourceSelector :: Selector '[RawId] ()
reloadDataSourceSelector = mkSelector "reloadDataSource:"

-- | @Selector@ for @reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSourceSelector :: Selector '[] ()
reloadGammaAndBetaFromDataSourceSelector = mkSelector "reloadGammaAndBetaFromDataSource"

-- | @Selector@ for @reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector :: Selector '[RawId, Id MPSCNNNormalizationGammaAndBetaState] ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector = mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNInstanceNormalizationGradientState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNInstanceNormalizationGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector '[CFloat] ()
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

