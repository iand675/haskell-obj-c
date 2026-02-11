{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that describes the properties of a create sparse operation.
--
-- Generated bindings for @MPSGraphCreateSparseOpDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphCreateSparseOpDescriptor
  ( MPSGraphCreateSparseOpDescriptor
  , IsMPSGraphCreateSparseOpDescriptor(..)
  , descriptorWithStorageType_dataType
  , sparseStorageType
  , setSparseStorageType
  , dataType
  , setDataType
  , descriptorWithStorageType_dataTypeSelector
  , sparseStorageTypeSelector
  , setSparseStorageTypeSelector
  , dataTypeSelector
  , setDataTypeSelector

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
  , MPSGraphSparseStorageType(MPSGraphSparseStorageType)
  , pattern MPSGraphSparseStorageCOO
  , pattern MPSGraphSparseStorageCSC
  , pattern MPSGraphSparseStorageCSR

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a descriptor for a sparse tensor.
--
-- - Parameters:   - sparseStorageType: A sparseStorageType.   - dataType: A dataType of the sparse tensor. - Returns: The descriptor.
--
-- ObjC selector: @+ descriptorWithStorageType:dataType:@
descriptorWithStorageType_dataType :: MPSGraphSparseStorageType -> MPSDataType -> IO (Id MPSGraphCreateSparseOpDescriptor)
descriptorWithStorageType_dataType sparseStorageType dataType =
  do
    cls' <- getRequiredClass "MPSGraphCreateSparseOpDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithStorageType:dataType:") (retPtr retVoid) [argCULong (coerce sparseStorageType), argCUInt (coerce dataType)] >>= retainedObject . castPtr

-- | Defines the storage format of the sparse tensor.
--
-- ObjC selector: @- sparseStorageType@
sparseStorageType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> IO MPSGraphSparseStorageType
sparseStorageType mpsGraphCreateSparseOpDescriptor  =
  fmap (coerce :: CULong -> MPSGraphSparseStorageType) $ sendMsg mpsGraphCreateSparseOpDescriptor (mkSelector "sparseStorageType") retCULong []

-- | Defines the storage format of the sparse tensor.
--
-- ObjC selector: @- setSparseStorageType:@
setSparseStorageType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> MPSGraphSparseStorageType -> IO ()
setSparseStorageType mpsGraphCreateSparseOpDescriptor  value =
  sendMsg mpsGraphCreateSparseOpDescriptor (mkSelector "setSparseStorageType:") retVoid [argCULong (coerce value)]

-- | Defines the datatype of the sparse tensor.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> IO MPSDataType
dataType mpsGraphCreateSparseOpDescriptor  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsGraphCreateSparseOpDescriptor (mkSelector "dataType") retCUInt []

-- | Defines the datatype of the sparse tensor.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> MPSDataType -> IO ()
setDataType mpsGraphCreateSparseOpDescriptor  value =
  sendMsg mpsGraphCreateSparseOpDescriptor (mkSelector "setDataType:") retVoid [argCUInt (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStorageType:dataType:@
descriptorWithStorageType_dataTypeSelector :: Selector
descriptorWithStorageType_dataTypeSelector = mkSelector "descriptorWithStorageType:dataType:"

-- | @Selector@ for @sparseStorageType@
sparseStorageTypeSelector :: Selector
sparseStorageTypeSelector = mkSelector "sparseStorageType"

-- | @Selector@ for @setSparseStorageType:@
setSparseStorageTypeSelector :: Selector
setSparseStorageTypeSelector = mkSelector "setSparseStorageType:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

