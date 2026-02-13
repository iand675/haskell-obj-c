{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDropout
--
-- This depends on Metal.framework
--
-- Dropout is a regularization technique used to prevent neural networks from              overfitting during training. With probability keepProbability, this filter              outputs the input element scaled by 1 / keepProbability. Otherwise, it              outputs 0. Each input element is kept or dropped independently. The scaling              is performed to keep the energy of the output unchanged.
--
-- Generated bindings for @MPSCNNDropout@.
module ObjC.MetalPerformanceShaders.MPSCNNDropout
  ( MPSCNNDropout
  , IsMPSCNNDropout(..)
  , initWithDevice
  , initWithCoder_device
  , resultStateForSourceImage_sourceStates_destinationImage
  , resultStateBatchForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage
  , keepProbability
  , seed
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , keepProbabilitySelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , seedSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
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

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> RawId -> IO (Id MPSCNNDropout)
initWithDevice mpscnnDropout device =
  sendOwnedMessage mpscnnDropout initWithDeviceSelector device

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDropout mpscnnDropout, IsNSCoder aDecoder) => mpscnnDropout -> aDecoder -> RawId -> IO (Id MPSCNNDropout)
initWithCoder_device mpscnnDropout aDecoder device =
  sendOwnedMessage mpscnnDropout initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnDropout -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNDropoutGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnDropout sourceImage sourceStates destinationImage =
  sendMessage mpscnnDropout resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsNSArray sourceStates) => mpscnnDropout -> RawId -> sourceStates -> RawId -> IO (Id MPSCNNDropoutGradientState)
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnDropout sourceImage sourceStates destinationImage =
  sendMessage mpscnnDropout resultStateBatchForSourceImage_sourceStates_destinationImageSelector sourceImage (toNSArray sourceStates) destinationImage

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnDropout -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNDropoutGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnDropout commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnDropout temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsNSArray sourceStates) => mpscnnDropout -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnDropout commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnDropout temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer sourceImage (toNSArray sourceStates) destinationImage

-- | keepProbability
--
-- The probability that each element in the input is kept.              The valid range is (0.0f, 1.0f).
--
-- ObjC selector: @- keepProbability@
keepProbability :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> IO CFloat
keepProbability mpscnnDropout =
  sendMessage mpscnnDropout keepProbabilitySelector

-- | seed
--
-- The seed used to generate random numbers.
--
-- ObjC selector: @- seed@
seed :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> IO CULong
seed mpscnnDropout =
  sendMessage mpscnnDropout seedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNDropout)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNDropout)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNDropoutGradientState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id NSArray, RawId] (Id MPSCNNDropoutGradientState)
resultStateBatchForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNDropoutGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, Id NSArray, RawId] RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector '[] CFloat
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

