{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixRandomPhilox
--
-- Generates random numbers using a counter based algorithm.              For further details see:          John K. Salmon, Mark A. Moraes, Ron O. Dror, and David E. Shaw. Parallel Random Numbers: As Easy as 1, 2, 3.
--
-- Generated bindings for @MPSMatrixRandomPhilox@.
module ObjC.MetalPerformanceShaders.MPSMatrixRandomPhilox
  ( MPSMatrixRandomPhilox
  , IsMPSMatrixRandomPhilox(..)
  , initWithDevice
  , initWithDevice_destinationDataType_seed_distributionDescriptor
  , initWithDevice_destinationDataType_seed
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_destinationDataType_seedSelector
  , initWithDevice_destinationDataType_seed_distributionDescriptorSelector

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

-- | initialize a MPSMatrixRandomPhilox filter to generate 32-bit unsigned              integer values with an initial seed of 0.
--
-- @device@ — The device the filter will run on
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox => mpsMatrixRandomPhilox -> RawId -> IO (Id MPSMatrixRandomPhilox)
initWithDevice mpsMatrixRandomPhilox device =
  sendOwnedMessage mpsMatrixRandomPhilox initWithDeviceSelector device

-- | initialize a MPSMatrixRandomPhilox filter
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
initWithDevice_destinationDataType_seed_distributionDescriptor :: (IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox, IsMPSMatrixRandomDistributionDescriptor distributionDescriptor) => mpsMatrixRandomPhilox -> RawId -> MPSDataType -> CULong -> distributionDescriptor -> IO (Id MPSMatrixRandomPhilox)
initWithDevice_destinationDataType_seed_distributionDescriptor mpsMatrixRandomPhilox device destinationDataType seed distributionDescriptor =
  sendOwnedMessage mpsMatrixRandomPhilox initWithDevice_destinationDataType_seed_distributionDescriptorSelector device destinationDataType seed (toMPSMatrixRandomDistributionDescriptor distributionDescriptor)

-- | initialize a MPSMatrixRandomPhilox filter using a default distribution.
--
-- @device@ — The device the filter will run on
--
-- @destinationDataType@ — The data type of the result.
--
-- @seed@ — The seed to initialize the random number generators with.
--
-- ObjC selector: @- initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seed :: IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox => mpsMatrixRandomPhilox -> RawId -> MPSDataType -> CULong -> IO (Id MPSMatrixRandomPhilox)
initWithDevice_destinationDataType_seed mpsMatrixRandomPhilox device destinationDataType seed =
  sendOwnedMessage mpsMatrixRandomPhilox initWithDevice_destinationDataType_seedSelector device destinationDataType seed

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox, IsNSCoder aDecoder) => mpsMatrixRandomPhilox -> aDecoder -> RawId -> IO (Id MPSMatrixRandomPhilox)
initWithCoder_device mpsMatrixRandomPhilox aDecoder device =
  sendOwnedMessage mpsMatrixRandomPhilox initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixRandomPhilox)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:distributionDescriptor:@
initWithDevice_destinationDataType_seed_distributionDescriptorSelector :: Selector '[RawId, MPSDataType, CULong, Id MPSMatrixRandomDistributionDescriptor] (Id MPSMatrixRandomPhilox)
initWithDevice_destinationDataType_seed_distributionDescriptorSelector = mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seedSelector :: Selector '[RawId, MPSDataType, CULong] (Id MPSMatrixRandomPhilox)
initWithDevice_destinationDataType_seedSelector = mkSelector "initWithDevice:destinationDataType:seed:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixRandomPhilox)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

