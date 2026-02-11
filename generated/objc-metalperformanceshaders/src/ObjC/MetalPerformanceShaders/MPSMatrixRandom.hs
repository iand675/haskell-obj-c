{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , encodeToCommandBuffer_destinationVectorSelector
  , encodeToCommandBuffer_destinationMatrixSelector
  , destinationDataTypeSelector
  , distributionTypeSelector
  , batchStartSelector
  , setBatchStartSelector
  , batchSizeSelector
  , setBatchSizeSelector

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> RawId -> IO (Id MPSMatrixRandom)
initWithDevice mpsMatrixRandom  device =
  sendMsg mpsMatrixRandom (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixRandom kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @destinationVector@ — A valid MPSVector to contain the result.
--
-- ObjC selector: @- encodeToCommandBuffer:destinationVector:@
encodeToCommandBuffer_destinationVector :: (IsMPSMatrixRandom mpsMatrixRandom, IsMPSVector destinationVector) => mpsMatrixRandom -> RawId -> destinationVector -> IO ()
encodeToCommandBuffer_destinationVector mpsMatrixRandom  commandBuffer destinationVector =
withObjCPtr destinationVector $ \raw_destinationVector ->
    sendMsg mpsMatrixRandom (mkSelector "encodeToCommandBuffer:destinationVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_destinationVector :: Ptr ())]

-- | Encode a MPSMatrixRandom kernel into a command Buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @destinationMatrix@ — A valid MPSMatrix to contain the result.
--
-- ObjC selector: @- encodeToCommandBuffer:destinationMatrix:@
encodeToCommandBuffer_destinationMatrix :: (IsMPSMatrixRandom mpsMatrixRandom, IsMPSMatrix destinationMatrix) => mpsMatrixRandom -> RawId -> destinationMatrix -> IO ()
encodeToCommandBuffer_destinationMatrix mpsMatrixRandom  commandBuffer destinationMatrix =
withObjCPtr destinationMatrix $ \raw_destinationMatrix ->
    sendMsg mpsMatrixRandom (mkSelector "encodeToCommandBuffer:destinationMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_destinationMatrix :: Ptr ())]

-- | destinationDataType
--
-- The type of the data which makes up the values of the result.              Supported values are:                  MPSDataTypeUInt32                  MPSDataTypeFloat32
--
-- Default is MPSDataTypeUInt32
--
-- ObjC selector: @- destinationDataType@
destinationDataType :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO MPSDataType
destinationDataType mpsMatrixRandom  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsMatrixRandom (mkSelector "destinationDataType") retCUInt []

-- | distributionType
--
-- The distribution from which to generate random values.
--
-- Default is MPSMatrixRandomDistributionDefault
--
-- ObjC selector: @- distributionType@
distributionType :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO MPSMatrixRandomDistribution
distributionType mpsMatrixRandom  =
  fmap (coerce :: CULong -> MPSMatrixRandomDistribution) $ sendMsg mpsMatrixRandom (mkSelector "distributionType") retCULong []

-- | batchStart
--
-- The starting index in the destination batch.
--
-- ObjC selector: @- batchStart@
batchStart :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO CULong
batchStart mpsMatrixRandom  =
  sendMsg mpsMatrixRandom (mkSelector "batchStart") retCULong []

-- | batchStart
--
-- The starting index in the destination batch.
--
-- ObjC selector: @- setBatchStart:@
setBatchStart :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> CULong -> IO ()
setBatchStart mpsMatrixRandom  value =
  sendMsg mpsMatrixRandom (mkSelector "setBatchStart:") retVoid [argCULong (fromIntegral value)]

-- | batchSize
--
-- The size of the batch to process.
--
-- ObjC selector: @- batchSize@
batchSize :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> IO CULong
batchSize mpsMatrixRandom  =
  sendMsg mpsMatrixRandom (mkSelector "batchSize") retCULong []

-- | batchSize
--
-- The size of the batch to process.
--
-- ObjC selector: @- setBatchSize:@
setBatchSize :: IsMPSMatrixRandom mpsMatrixRandom => mpsMatrixRandom -> CULong -> IO ()
setBatchSize mpsMatrixRandom  value =
  sendMsg mpsMatrixRandom (mkSelector "setBatchSize:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:destinationVector:@
encodeToCommandBuffer_destinationVectorSelector :: Selector
encodeToCommandBuffer_destinationVectorSelector = mkSelector "encodeToCommandBuffer:destinationVector:"

-- | @Selector@ for @encodeToCommandBuffer:destinationMatrix:@
encodeToCommandBuffer_destinationMatrixSelector :: Selector
encodeToCommandBuffer_destinationMatrixSelector = mkSelector "encodeToCommandBuffer:destinationMatrix:"

-- | @Selector@ for @destinationDataType@
destinationDataTypeSelector :: Selector
destinationDataTypeSelector = mkSelector "destinationDataType"

-- | @Selector@ for @distributionType@
distributionTypeSelector :: Selector
distributionTypeSelector = mkSelector "distributionType"

-- | @Selector@ for @batchStart@
batchStartSelector :: Selector
batchStartSelector = mkSelector "batchStart"

-- | @Selector@ for @setBatchStart:@
setBatchStartSelector :: Selector
setBatchStartSelector = mkSelector "setBatchStart:"

-- | @Selector@ for @batchSize@
batchSizeSelector :: Selector
batchSizeSelector = mkSelector "batchSize"

-- | @Selector@ for @setBatchSize:@
setBatchSizeSelector :: Selector
setBatchSizeSelector = mkSelector "setBatchSize:"

