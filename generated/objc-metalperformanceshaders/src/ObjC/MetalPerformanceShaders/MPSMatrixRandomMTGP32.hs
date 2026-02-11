{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , initWithDevice_destinationDataType_seed_distributionDescriptorSelector
  , synchronizeStateOnCommandBufferSelector
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

-- | initialize a MPSMatrixRandomMTGP32 filter to generate 32-bit unsigned              integer values with an initial seed of 0.
--
-- @device@ — The device the filter will run on
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32 => mpsMatrixRandomMTGP32 -> RawId -> IO (Id MPSMatrixRandomMTGP32)
initWithDevice mpsMatrixRandomMTGP32  device =
  sendMsg mpsMatrixRandomMTGP32 (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_destinationDataType_seed_distributionDescriptor mpsMatrixRandomMTGP32  device destinationDataType seed distributionDescriptor =
withObjCPtr distributionDescriptor $ \raw_distributionDescriptor ->
    sendMsg mpsMatrixRandomMTGP32 (mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCUInt (coerce destinationDataType), argCULong (fromIntegral seed), argPtr (castPtr raw_distributionDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Synchronize internal MTGP32 state between GPU and CPU.
--
-- @commandBuffer@ — The command buffer on which to encode the synchronization.
--
-- ObjC selector: @- synchronizeStateOnCommandBuffer:@
synchronizeStateOnCommandBuffer :: IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32 => mpsMatrixRandomMTGP32 -> RawId -> IO ()
synchronizeStateOnCommandBuffer mpsMatrixRandomMTGP32  commandBuffer =
  sendMsg mpsMatrixRandomMTGP32 (mkSelector "synchronizeStateOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

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
initWithDevice_destinationDataType_seed mpsMatrixRandomMTGP32  device destinationDataType seed =
  sendMsg mpsMatrixRandomMTGP32 (mkSelector "initWithDevice:destinationDataType:seed:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCUInt (coerce destinationDataType), argCULong (fromIntegral seed)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixRandomMTGP32 mpsMatrixRandomMTGP32, IsNSCoder aDecoder) => mpsMatrixRandomMTGP32 -> aDecoder -> RawId -> IO (Id MPSMatrixRandomMTGP32)
initWithCoder_device mpsMatrixRandomMTGP32  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixRandomMTGP32 (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:distributionDescriptor:@
initWithDevice_destinationDataType_seed_distributionDescriptorSelector :: Selector
initWithDevice_destinationDataType_seed_distributionDescriptorSelector = mkSelector "initWithDevice:destinationDataType:seed:distributionDescriptor:"

-- | @Selector@ for @synchronizeStateOnCommandBuffer:@
synchronizeStateOnCommandBufferSelector :: Selector
synchronizeStateOnCommandBufferSelector = mkSelector "synchronizeStateOnCommandBuffer:"

-- | @Selector@ for @initWithDevice:destinationDataType:seed:@
initWithDevice_destinationDataType_seedSelector :: Selector
initWithDevice_destinationDataType_seedSelector = mkSelector "initWithDevice:destinationDataType:seed:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

