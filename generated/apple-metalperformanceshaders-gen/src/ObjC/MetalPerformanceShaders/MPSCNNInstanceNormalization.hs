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
  , initWithDevice_dataSourceSelector
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , reloadDataSourceSelector
  , reloadGammaAndBetaFromDataSourceSelector
  , reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , epsilonSelector
  , setEpsilonSelector
  , dataSourceSelector


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

-- | Initialize a MPSCNNInstanceNormalization kernel on a device.
--
-- @dataSource@ — An object conforming to the MPSCNNInstanceNormalizationDataSource                          protocol which
--
-- ObjC selector: @- initWithDevice:dataSource:@
initWithDevice_dataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> RawId -> IO (Id MPSCNNInstanceNormalization)
initWithDevice_dataSource mpscnnInstanceNormalization  device dataSource =
    sendMsg mpscnnInstanceNormalization (mkSelector "initWithDevice:dataSource:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId dataSource) :: Ptr ())] >>= ownedObject . castPtr

-- | Use initWithDevice:dataSource instead
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> IO (Id MPSCNNInstanceNormalization)
initWithDevice mpscnnInstanceNormalization  device =
    sendMsg mpscnnInstanceNormalization (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnInstanceNormalization  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnInstanceNormalization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Reload data using a data source.
--
-- @dataSource@ — The data source which will provide the gamma and beta terms                          to scale and bias the normalized result respectively.
--
-- ObjC selector: @- reloadDataSource:@
reloadDataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> RawId -> IO ()
reloadDataSource mpscnnInstanceNormalization  dataSource =
    sendMsg mpscnnInstanceNormalization (mkSelector "reloadDataSource:") retVoid [argPtr (castPtr (unRawId dataSource) :: Ptr ())]

-- | Reinitialize the filter using the data source provided at kernel initialization.
--
-- ObjC selector: @- reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO ()
reloadGammaAndBetaFromDataSource mpscnnInstanceNormalization  =
    sendMsg mpscnnInstanceNormalization (mkSelector "reloadGammaAndBetaFromDataSource") retVoid []

-- | Reload data using new gamma and beta terms contained within an              MPSCNNInstanceNormalizationGradientState object.
--
-- @commandBuffer@ — The command buffer on which to encode the reload.
--
-- @gammaAndBetaState@ — The state containing the updated weights which are to                                          be reloaded.
--
-- ObjC selector: @- reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSCNNNormalizationGammaAndBetaState gammaAndBetaState) => mpscnnInstanceNormalization -> RawId -> gammaAndBetaState -> IO ()
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaState mpscnnInstanceNormalization  commandBuffer gammaAndBetaState =
  withObjCPtr gammaAndBetaState $ \raw_gammaAndBetaState ->
      sendMsg mpscnnInstanceNormalization (mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_gammaAndBetaState :: Ptr ())]

-- | Return a MPSCNNInstanceNormalizationGradientState object for the provided              source image, source states, and destination image.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnInstanceNormalization -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNInstanceNormalizationGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnInstanceNormalization  sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnInstanceNormalization (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | Return a temporary MPSCNNInstanceNormalizationGradientState object which may be used with                  a MPSCNNInstanceNormalization filter.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNInstanceNormalization mpscnnInstanceNormalization, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnInstanceNormalization -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNInstanceNormalizationGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnInstanceNormalization  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnInstanceNormalization (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO CFloat
epsilon mpscnnInstanceNormalization  =
    sendMsg mpscnnInstanceNormalization (mkSelector "epsilon") retCFloat []

-- | epsilon
--
-- The epsilon value used to bias the variance when normalizing.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> CFloat -> IO ()
setEpsilon mpscnnInstanceNormalization  value =
    sendMsg mpscnnInstanceNormalization (mkSelector "setEpsilon:") retVoid [argCFloat value]

-- | The data source that the object was initialized with
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNInstanceNormalization mpscnnInstanceNormalization => mpscnnInstanceNormalization -> IO RawId
dataSource mpscnnInstanceNormalization  =
    fmap (RawId . castPtr) $ sendMsg mpscnnInstanceNormalization (mkSelector "dataSource") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataSource:@
initWithDevice_dataSourceSelector :: Selector
initWithDevice_dataSourceSelector = mkSelector "initWithDevice:dataSource:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @reloadDataSource:@
reloadDataSourceSelector :: Selector
reloadDataSourceSelector = mkSelector "reloadDataSource:"

-- | @Selector@ for @reloadGammaAndBetaFromDataSource@
reloadGammaAndBetaFromDataSourceSelector :: Selector
reloadGammaAndBetaFromDataSourceSelector = mkSelector "reloadGammaAndBetaFromDataSource"

-- | @Selector@ for @reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:@
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector :: Selector
reloadGammaAndBetaWithCommandBuffer_gammaAndBetaStateSelector = mkSelector "reloadGammaAndBetaWithCommandBuffer:gammaAndBetaState:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

