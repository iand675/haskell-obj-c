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
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , keepProbability
  , seed
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , keepProbabilitySelector
  , seedSelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> RawId -> IO (Id MPSCNNDropout)
initWithDevice mpscnnDropout  device =
  sendMsg mpscnnDropout (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDropout mpscnnDropout, IsNSCoder aDecoder) => mpscnnDropout -> aDecoder -> RawId -> IO (Id MPSCNNDropout)
initWithCoder_device mpscnnDropout  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnDropout (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnDropout -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNDropoutGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnDropout  sourceImage sourceStates destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnDropout (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNDropout mpscnnDropout, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnDropout -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNDropoutGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnDropout  commandBuffer sourceImage sourceStates destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr sourceStates $ \raw_sourceStates ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnDropout (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | keepProbability
--
-- The probability that each element in the input is kept.              The valid range is (0.0f, 1.0f).
--
-- ObjC selector: @- keepProbability@
keepProbability :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> IO CFloat
keepProbability mpscnnDropout  =
  sendMsg mpscnnDropout (mkSelector "keepProbability") retCFloat []

-- | seed
--
-- The seed used to generate random numbers.
--
-- ObjC selector: @- seed@
seed :: IsMPSCNNDropout mpscnnDropout => mpscnnDropout -> IO CULong
seed mpscnnDropout  =
  sendMsg mpscnnDropout (mkSelector "seed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

