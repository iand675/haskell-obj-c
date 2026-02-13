{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The symbolic representation of a compute data type.
--
-- @NSCopy@ will take a refrence, this is so @NSDictionary@ can work with the tensor. All tensors are created, owned and destroyed by the MPSGraph
--
-- Generated bindings for @MPSGraphTensor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphTensor
  ( MPSGraphTensor
  , IsMPSGraphTensor(..)
  , init_
  , shape
  , dataType
  , operation
  , dataTypeSelector
  , initSelector
  , operationSelector
  , shapeSelector

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unavailable, please utilize graph methods to create and initialize tensors.
--
-- ObjC selector: @- init@
init_ :: IsMPSGraphTensor mpsGraphTensor => mpsGraphTensor -> IO (Id MPSGraphTensor)
init_ mpsGraphTensor =
  sendOwnedMessage mpsGraphTensor initSelector

-- | The shape of the tensor.
--
-- nil shape represents an unranked tensor. -1 value for a dimension represents that it will be resolved via shape inference at runtime and it can be anything.
--
-- ObjC selector: @- shape@
shape :: IsMPSGraphTensor mpsGraphTensor => mpsGraphTensor -> IO RawId
shape mpsGraphTensor =
  sendMessage mpsGraphTensor shapeSelector

-- | The data type of the tensor.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphTensor mpsGraphTensor => mpsGraphTensor -> IO MPSDataType
dataType mpsGraphTensor =
  sendMessage mpsGraphTensor dataTypeSelector

-- | The operation responsible for creating this tensor.
--
-- ObjC selector: @- operation@
operation :: IsMPSGraphTensor mpsGraphTensor => mpsGraphTensor -> IO (Id MPSGraphOperation)
operation mpsGraphTensor =
  sendMessage mpsGraphTensor operationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSGraphTensor)
initSelector = mkSelector "init"

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] RawId
shapeSelector = mkSelector "shape"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @operation@
operationSelector :: Selector '[] (Id MPSGraphOperation)
operationSelector = mkSelector "operation"

