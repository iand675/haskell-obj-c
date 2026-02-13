{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixRandom
--
-- Kernels that implement random number generation.
--
-- Generated bindings for @MPSMatrixRandom@.
module ObjC.MetalPerformanceShaders.MPSMatrixRandom
  ( MPSMatrixRandom
  , IsMPSMatrixRandom(..)
  , initWithDevice
  , encodeToCommandBuffer_destinationVector
  , encodeToCommandBuffer_destinationMatrix
  , destinationDataType
  , distributionType
  , batchStart
  , setBatchStart
  , batchSize
  , setBatchSize
  , batchSizeSelector
  , batchStartSelector
  , destinationDataTypeSelector
  , distributionTypeSelector
  , encodeToCommandBuffer_destinationMatrixSelector
  , encodeToCommandBuffer_destinationVectorSelector
  , initWithDeviceSelector
  , setBatchSizeSelector
  , setBatchStartSelector

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
  , MPSMatrixRandomDistribution(MPSMatrixRandomDistribution)
  , pattern MPSMatrixRandomDistributionDefault
  , pattern MPSMatrixRandomDistributionUniform
  , pattern MPSMatrixRandomDistributionNormal

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> RawId -> IO (Id MPSMatrixRandom)
initWithDevice mpsMatrixRandom device =
  sendOwnedMessage mpsMatrixRandom initWithDeviceSelector device

-- | Encode a MPSMatrixRandom kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @destinationVector@ — A valid MPSVector to contain the result.
--
-- ObjC selector: @- encodeToCommandBuffer:destinationVector:@
encodeToCommandBuffer_destinationVector :: (IsMPSMatrixRandom mpsMatrixRandom, IsMPSVector destinationVector) => mpsMatrixRandom -> RawId -> destinationVector -> IO ()
encodeToCommandBuffer_destinationVector mpsMatrixRandom commandBuffer destinationVector =
  sendMessage mpsMatrixRandom encodeToCommandBuffer_destinationVectorSelector commandBuffer (toMPSVector destinationVector)

-- | Encode a MPSMatrixRandom kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @destinationMatrix@ — A valid MPSMatrix to contain the result.
--
-- ObjC selector: @- encodeToCommandBuffer:destinationMatrix:@
encodeToCommandBuffer_destinationMatrix :: (IsMPSMatrixRandom mpsMatrixRandom, IsMPSMatrix destinationMatrix) => mpsMatrixRandom -> RawId -> destinationMatrix -> IO ()
encodeToCommandBuffer_destinationMatrix mpsMatrixRandom commandBuffer destinationMatrix =
  sendMessage mpsMatrixRandom encodeToCommandBuffer_destinationMatrixSelector commandBuffer (toMPSMatrix destinationMatrix)

-- | destinationDataType
--
-- The type of the data which makes up the values of the result.              Supported values are:                  MPSDataTypeUInt32                  MPSDataTypeFloat32
--
-- Default is MPSDataTypeUInt32
--
-- ObjC selector: @- destinationDataType@
destinationDataType :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO MPSDataType
destinationDataType mpsMatrixRandom =
  sendMessage mpsMatrixRandom destinationDataTypeSelector

-- | distributionType
--
-- The distribution from which to generate random values.
--
-- Default is MPSMatrixRandomDistributionDefault
--
-- ObjC selector: @- distributionType@
distributionType :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO MPSMatrixRandomDistribution
distributionType mpsMatrixRandom =
  sendMessage mpsMatrixRandom distributionTypeSelector

-- | batchStart
--
-- The starting index in the destination batch.
--
-- ObjC selector: @- batchStart@
batchStart :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO CULong
batchStart mpsMatrixRandom =
  sendMessage mpsMatrixRandom batchStartSelector

-- | batchStart
--
-- The starting index in the destination batch.
--
-- ObjC selector: @- setBatchStart:@
setBatchStart :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> CULong -> IO ()
setBatchStart mpsMatrixRandom value =
  sendMessage mpsMatrixRandom setBatchStartSelector value

-- | batchSize
--
-- The size of the batch to process.
--
-- ObjC selector: @- batchSize@
batchSize :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO CULong
batchSize mpsMatrixRandom =
  sendMessage mpsMatrixRandom batchSizeSelector

-- | batchSize
--
-- The size of the batch to process.
--
-- ObjC selector: @- setBatchSize:@
setBatchSize :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> CULong -> IO ()
setBatchSize mpsMatrixRandom value =
  sendMessage mpsMatrixRandom setBatchSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixRandom)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:destinationVector:@
encodeToCommandBuffer_destinationVectorSelector :: Selector '[RawId, Id MPSVector] ()
encodeToCommandBuffer_destinationVectorSelector = mkSelector "encodeToCommandBuffer:destinationVector:"

-- | @Selector@ for @encodeToCommandBuffer:destinationMatrix:@
encodeToCommandBuffer_destinationMatrixSelector :: Selector '[RawId, Id MPSMatrix] ()
encodeToCommandBuffer_destinationMatrixSelector = mkSelector "encodeToCommandBuffer:destinationMatrix:"

-- | @Selector@ for @destinationDataType@
destinationDataTypeSelector :: Selector '[] MPSDataType
destinationDataTypeSelector = mkSelector "destinationDataType"

-- | @Selector@ for @distributionType@
distributionTypeSelector :: Selector '[] MPSMatrixRandomDistribution
distributionTypeSelector = mkSelector "distributionType"

-- | @Selector@ for @batchStart@
batchStartSelector :: Selector '[] CULong
batchStartSelector = mkSelector "batchStart"

-- | @Selector@ for @setBatchStart:@
setBatchStartSelector :: Selector '[CULong] ()
setBatchStartSelector = mkSelector "setBatchStart:"

-- | @Selector@ for @batchSize@
batchSizeSelector :: Selector '[] CULong
batchSizeSelector = mkSelector "batchSize"

-- | @Selector@ for @setBatchSize:@
setBatchSizeSelector :: Selector '[CULong] ()
setBatchSizeSelector = mkSelector "setBatchSize:"

