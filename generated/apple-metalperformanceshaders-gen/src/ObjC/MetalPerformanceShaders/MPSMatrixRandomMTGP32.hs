{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixRandomMTGP32
--
-- Generates random numbers using a Mersenne Twister algorithm              suitable for GPU execution.  It uses a period of 2**11214.              For further details see:          Mutsuo Saito. A Variant of Mersenne Twister Suitable for Graphic Processors. arXiv:1005.4973
--
-- Generated bindings for @MPSMatrixRandomMTGP32@.
module ObjC.MetalPerformanceShaders.MPSMatrixRandomMTGP32
  ( MPSMatrixRandomMTGP32
  , IsMPSMatrixRandomMTGP32(..)
  , initWithDevice
  , initWithDevice_destinationDataType_seed_distributionDescriptor
  , synchronizeStateOnCommandBuffer
  , initWithDevice_destinationDataType_seed
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_destinationDataType_seedSelector
  , initWithDevice_destinationDataType_seed_distributionDescriptorSelector
  , synchronizeStateOnCommandBufferSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initialize a MPSMatrixRandomMTGP32 filter to generate 32-bit unsigned              integer values with an initial seed of 0.
--
-- @device@ — The device the filter will run on
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32 => mpsMatrixRandomMTGP32 -> RawId -> IO (Id MPSMatrixRandomMTGP32)
initWithDevice mpsMatrixRandomMTGP32 device =
  sendOwnedMessage mpsMatrixRandomMTGP32 initWithDeviceSelector device

-- | initialize a MPSMatrixRandomMTGP32 filter
--
-- @device@ — The device the filter will run on
--
-- @destinationDataType@ — The data type of the result.
--
-- @seed@ — The seed to initialize the random number generators with.
--
-- @distributionDescriptor@ — A descriptor containing information about the distribution.
--
-- ObjC selector: @- initWithDevice:destinationDataType:seed:distributionDescriptor:@
initWithDevice_destinationDataType_seed_distributionDescriptor :: (IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32, IsMPSMatrixRandomDistributionDescriptor distributionDescriptor) => mpsMatrixRandomMTGP32 -> RawId -> MPSDataType -> CULong -> distributionDescriptor -> IO (Id MPSMatrixRandomMTGP32)
initWithDevice_destinationDataType_seed_distributionDescriptor mpsMatrixRandomMTGP32 device destinationDataType seed distributionDescriptor =
  sendOwnedMessage mpsMatrixRandomMTGP32 initWithDevice_destinationDataType_seed_distributionDescriptorSelector device destinationDataType seed (toMPSMatrixRandomDistributionDescriptor distributionDescriptor)

-- | Synchronize internal MTGP32 state between GPU and CPU.
--
-- @commandBuffer@ — The command buffer on which to encode the synchronization.
--
-- ObjC selector: @- synchronizeStateOnCommandBuffer:@
synchronizeStateOnCommandBuffer :: IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32 => mpsMatrixRandomMTGP32 -> RawId -> IO ()
synchronizeStateOnCommandBuffer mpsMatrixRandomMTGP32 commandBuffer =
  sendMessage mpsMatrixRandomMTGP32 synchronizeStateOnCommandBufferSelector commandBuffer

-- | initialize a MPSMatrixRandomMTGP32 filter using a default distribution.
--
-- @device@ — The device the filter will run on
--
-- @destinationDataType@ — The data type of the result.
--
-- @seed@ — The seed to initialize the random number generators with.
--
-- ObjC selector: @- initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seed :: IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32 => mpsMatrixRandomMTGP32 -> RawId -> MPSDataType -> CULong -> IO (Id MPSMatrixRandomMTGP32)
initWithDevice_destinationDataType_seed mpsMatrixRandomMTGP32 device destinationDataType seed =
  sendOwnedMessage mpsMatrixRandomMTGP32 initWithDevice_destinationDataType_seedSelector device destinationDataType seed

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32, IsNSCoder aDecoder) => mpsMatrixRandomMTGP32 -> aDecoder -> RawId -> IO (Id MPSMatrixRandomMTGP32)
initWithCoder_device mpsMatrixRandomMTGP32 aDecoder device =
  sendOwnedMessage mpsMatrixRandomMTGP32 initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixRandomMTGP32)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:distributionDescriptor:@
initWithDevice_destinationDataType_seed_distributionDescriptorSelector :: Selector '[RawId, MPSDataType, CULong, Id MPSMatrixRandomDistributionDescriptor] (Id MPSMatrixRandomMTGP32)
initWithDevice_destinationDataType_seed_distributionDescriptorSelector = mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:"

-- | @Selector@ for @synchronizeStateOnCommandBuffer:@
synchronizeStateOnCommandBufferSelector :: Selector '[RawId] ()
synchronizeStateOnCommandBufferSelector = mkSelector "synchronizeStateOnCommandBuffer:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seedSelector :: Selector '[RawId, MPSDataType, CULong] (Id MPSMatrixRandomMTGP32)
initWithDevice_destinationDataType_seedSelector = mkSelector "initWithDevice:destinationDataType:seed:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixRandomMTGP32)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

