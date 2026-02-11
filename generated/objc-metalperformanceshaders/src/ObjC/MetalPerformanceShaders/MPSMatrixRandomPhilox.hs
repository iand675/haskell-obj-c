{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , initWithDevice_destinationDataType_seed_distributionDescriptorSelector
  , initWithDevice_destinationDataType_seedSelector
  , initWithCoder_deviceSelector

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initialize a MPSMatrixRandomPhilox filter to generate 32-bit unsigned              integer values with an initial seed of 0.
--
-- @device@ — The device the filter will run on
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox => mpsMatrixRandomPhilox -> RawId -> IO (Id MPSMatrixRandomPhilox)
initWithDevice mpsMatrixRandomPhilox  device =
  sendMsg mpsMatrixRandomPhilox (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_destinationDataType_seed_distributionDescriptor mpsMatrixRandomPhilox  device destinationDataType seed distributionDescriptor =
withObjCPtr distributionDescriptor $ \raw_distributionDescriptor ->
    sendMsg mpsMatrixRandomPhilox (mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCUInt (coerce destinationDataType), argCULong (fromIntegral seed), argPtr (castPtr raw_distributionDescriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_destinationDataType_seed mpsMatrixRandomPhilox  device destinationDataType seed =
  sendMsg mpsMatrixRandomPhilox (mkSelector "initWithDevice:destinationDataType:seed:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCUInt (coerce destinationDataType), argCULong (fromIntegral seed)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixRandomPhilox mpsMatrixRandomPhilox, IsNSCoder aDecoder) => mpsMatrixRandomPhilox -> aDecoder -> RawId -> IO (Id MPSMatrixRandomPhilox)
initWithCoder_device mpsMatrixRandomPhilox  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixRandomPhilox (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:distributionDescriptor:@
initWithDevice_destinationDataType_seed_distributionDescriptorSelector :: Selector
initWithDevice_destinationDataType_seed_distributionDescriptorSelector = mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seedSelector :: Selector
initWithDevice_destinationDataType_seedSelector = mkSelector "initWithDevice:destinationDataType:seed:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

