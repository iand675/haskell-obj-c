{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNGroupNormalization
--
-- This depends on Metal.framework
--
-- This kernel normalizes each image, on a per-group basis, to              have zero mean and unit variance:
--
-- for each image:                  for each channel:                      y = (x - mean) * gamma / sqrt(variance + epsilon) + beta;
--
-- The mean and variance are computed per group of channels, as given by the dataSource.
--
-- Generated bindings for @MPSCNNGroupNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNGroupNormalization
  ( MPSCNNGroupNormalization
  , IsMPSCNNGroupNormalization(..)
  , initWithDevice_dataSource
  , initWithDevice
  , initWithCoder_device
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

-- | Initialize a MPSCNNGroupNormalization kernel on a device.
--
-- @dataSource@ — An object conforming to the MPSCNNGroupNormalizationDataSource                          protocol which
--
-- ObjC selector: @- initWithDevice:dataSource:@
initWithDevice_dataSource :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> RawId -> RawId -> IO (Id MPSCNNGroupNormalization)
initWithDevice_dataSource mpscnnGroupNormalization device dataSource =
  sendOwnedMessage mpscnnGroupNormalization initWithDevice_dataSourceSelector device dataSource

-- | Use initWithDevice:dataSource instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> RawId -> IO (Id MPSCNNGroupNormalization)
initWithDevice mpscnnGroupNormalization device =
  sendOwnedMessage mpscnnGroupNormalization initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNGroupNormalization object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNGroupNormalization mpscnnGroupNormalization, IsNSCoder aDecoder) => mpscnnGroupNormalization -> aDecoder -> RawId -> IO (Id MPSCNNGroupNormalization)
initWithCoder_device mpscnnGroupNormalization aDecoder device =
  sendOwnedMessage mpscnnGroupNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Reinitialize the filter using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSource :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> IO ()
reloadGammaAndBetaFromDataSource mpscnnGroupNormalization =
  sendMessage mpscnnGroupNormalization reloadGammaAndBetaFromDataSourceSelector

-- | Reload data using new gamma and beta terms contained within an              MPSCNNGroupNormalizationGradientState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @gammaAndBetaState@ — The state containing the updated weights which are to                                          be reloaded.
--
-- ObjC selector: @- reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState :: (IsMPSCNNGroupNormalization mpscnnGroupNormalization, IsMPSCNNNormalizationGammaAndBetaState gammaAndBetaState) => mpscnnGroupNormalization -> RawId -> gammaAndBetaState -> IO ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState mpscnnGroupNormalization commandBuffer gammaAndBetaState =
  sendMessage mpscnnGroupNormalization reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector commandBuffer (toMPSCNNNormalizationGammaAndBetaState gammaAndBetaState)

-- | Return a MPSCNNGroupNormalizationGradientState object for the provided              source image, source states, and destination image.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNGroupNormalization mpscnnGroupNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnGroupNormalization -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNGroupNormalizationGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnGroupNormalization sourceImage sourceStates destinationImage =
  sendMessage mpscnnGroupNormalization resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | Return a temporary MPSCNNGroupNormalizationGradientState object which may be used with                  a MPSCNNGroupNormalization filter.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNGroupNormalization mpscnnGroupNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnGroupNormalization -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNGroupNormalizationGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnGroupNormalization commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnGroupNormalization temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> IO CFloat
epsilon mpscnnGroupNormalization =
  sendMessage mpscnnGroupNormalization epsilonSelector

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> CFloat -> IO ()
setEpsilon mpscnnGroupNormalization value =
  sendMessage mpscnnGroupNormalization setEpsilonSelector value

-- | The data source that the object was initialized with
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNGroupNormalization mpscnnGroupNormalization => mpscnnGroupNormalization -> IO RawId
dataSource mpscnnGroupNormalization =
  sendMessage mpscnnGroupNormalization dataSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataSource:@
initWithDevice_dataSourceSelector :: Selector '[RawId, RawId] (Id MPSCNNGroupNormalization)
initWithDevice_dataSourceSelector = mkSelector "initWithDevice:dataSource:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNGroupNormalization)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNGroupNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSourceSelector :: Selector '[] ()
reloadGammaAndBetaFromDataSourceSelector = mkSelector "reloadGammaAndBetaFromDataSource"

-- | @Selector@ for @reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector :: Selector '[RawId, Id MPSCNNNormalizationGammaAndBetaState] ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector = mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNGroupNormalizationGradientState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNGroupNormalizationGradientState)
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

