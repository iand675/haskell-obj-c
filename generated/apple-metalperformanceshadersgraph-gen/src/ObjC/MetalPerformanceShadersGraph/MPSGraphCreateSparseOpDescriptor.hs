{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataTypeSelector
  , descriptorWithStorageType_dataTypeSelector
  , setDataTypeSelector
  , setSparseStorageTypeSelector
  , sparseStorageTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' descriptorWithStorageType_dataTypeSelector sparseStorageType dataType

-- | Defines the storage format of the sparse tensor.
--
-- ObjC selector: @- sparseStorageType@
sparseStorageType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> IO MPSGraphSparseStorageType
sparseStorageType mpsGraphCreateSparseOpDescriptor =
  sendMessage mpsGraphCreateSparseOpDescriptor sparseStorageTypeSelector

-- | Defines the storage format of the sparse tensor.
--
-- ObjC selector: @- setSparseStorageType:@
setSparseStorageType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> MPSGraphSparseStorageType -> IO ()
setSparseStorageType mpsGraphCreateSparseOpDescriptor value =
  sendMessage mpsGraphCreateSparseOpDescriptor setSparseStorageTypeSelector value

-- | Defines the datatype of the sparse tensor.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> IO MPSDataType
dataType mpsGraphCreateSparseOpDescriptor =
  sendMessage mpsGraphCreateSparseOpDescriptor dataTypeSelector

-- | Defines the datatype of the sparse tensor.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSGraphCreateSparseOpDescriptor mpsGraphCreateSparseOpDescriptor => mpsGraphCreateSparseOpDescriptor -> MPSDataType -> IO ()
setDataType mpsGraphCreateSparseOpDescriptor value =
  sendMessage mpsGraphCreateSparseOpDescriptor setDataTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorWithStorageType:dataType:@
descriptorWithStorageType_dataTypeSelector :: Selector '[MPSGraphSparseStorageType, MPSDataType] (Id MPSGraphCreateSparseOpDescriptor)
descriptorWithStorageType_dataTypeSelector = mkSelector "descriptorWithStorageType:dataType:"

-- | @Selector@ for @sparseStorageType@
sparseStorageTypeSelector :: Selector '[] MPSGraphSparseStorageType
sparseStorageTypeSelector = mkSelector "sparseStorageType"

-- | @Selector@ for @setSparseStorageType:@
setSparseStorageTypeSelector :: Selector '[MPSGraphSparseStorageType] ()
setSparseStorageTypeSelector = mkSelector "setSparseStorageType:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MPSDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

