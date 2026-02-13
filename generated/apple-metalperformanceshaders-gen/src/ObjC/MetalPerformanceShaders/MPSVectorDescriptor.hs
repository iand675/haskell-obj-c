{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSVectorDescriptor
--
-- This depends on Metal.framework
--
-- A MPSVectorDescriptor describes the length and data type of a              an array of 1-dimensional vectors.  All vectors are stored as              contiguous arrays of data.
--
-- Generated bindings for @MPSVectorDescriptor@.
module ObjC.MetalPerformanceShaders.MPSVectorDescriptor
  ( MPSVectorDescriptor
  , IsMPSVectorDescriptor(..)
  , vectorDescriptorWithLength_dataType
  , vectorDescriptorWithLength_vectors_vectorBytes_dataType
  , vectorBytesForLength_dataType
  , length_
  , setLength
  , vectors
  , dataType
  , setDataType
  , vectorBytes
  , dataTypeSelector
  , lengthSelector
  , setDataTypeSelector
  , setLengthSelector
  , vectorBytesForLength_dataTypeSelector
  , vectorBytesSelector
  , vectorDescriptorWithLength_dataTypeSelector
  , vectorDescriptorWithLength_vectors_vectorBytes_dataTypeSelector
  , vectorsSelector

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

-- | Create a MPSVectorDescriptor with the specified length and data type.
--
-- @length@ — The number of elements in a single vector.
--
-- @dataType@ — The type of the data to be stored in the vector.
--
-- Use this function for creating a descriptor of a MPSVector object              containing a single vector.
--
-- ObjC selector: @+ vectorDescriptorWithLength:dataType:@
vectorDescriptorWithLength_dataType :: CULong -> MPSDataType -> IO (Id MPSVectorDescriptor)
vectorDescriptorWithLength_dataType length_ dataType =
  do
    cls' <- getRequiredClass "MPSVectorDescriptor"
    sendClassMessage cls' vectorDescriptorWithLength_dataTypeSelector length_ dataType

-- | Create a MPSVectorDescriptor with the specified length and data type.
--
-- @length@ — The number of elements in a single vector.
--
-- @vectors@ — The number of vectors in the MPSVector object.
--
-- @vectorBytes@ — The number of bytes between starting elements of consecutive                                  vectors.
--
-- @dataType@ — The type of the data to be stored in the vector.
--
-- For performance considerations the optimal stride between vectors may not necessarily be equal              to the vector length.  The MPSVectorDescriptor class provides a method which              may be used to determine this value, see the vectorBytesForLength API.
--
-- ObjC selector: @+ vectorDescriptorWithLength:vectors:vectorBytes:dataType:@
vectorDescriptorWithLength_vectors_vectorBytes_dataType :: CULong -> CULong -> CULong -> MPSDataType -> IO (Id MPSVectorDescriptor)
vectorDescriptorWithLength_vectors_vectorBytes_dataType length_ vectors vectorBytes dataType =
  do
    cls' <- getRequiredClass "MPSVectorDescriptor"
    sendClassMessage cls' vectorDescriptorWithLength_vectors_vectorBytes_dataTypeSelector length_ vectors vectorBytes dataType

-- | Return the recommended stride, in bytes, to be used for an array              of vectors of a given length.
--
-- @length@ — The number of elements in a single vector.
--
-- @dataType@ — The type of vector data values.
--
-- To achieve best performance the optimal stride between vectors within an array of              vectors is not necessarily equivalent to the number of elements per vector.  This method              returns the stride, in bytes, which gives best performance for a given vector length.              Using this stride to construct your array is recommended, but not required (provided that              the stride used is still large enough to allocate a full vector of data).
--
-- ObjC selector: @+ vectorBytesForLength:dataType:@
vectorBytesForLength_dataType :: CULong -> MPSDataType -> IO CULong
vectorBytesForLength_dataType length_ dataType =
  do
    cls' <- getRequiredClass "MPSVectorDescriptor"
    sendClassMessage cls' vectorBytesForLength_dataTypeSelector length_ dataType

-- | length
--
-- The number of elements in the vector.
--
-- ObjC selector: @- length@
length_ :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> IO CULong
length_ mpsVectorDescriptor =
  sendMessage mpsVectorDescriptor lengthSelector

-- | length
--
-- The number of elements in the vector.
--
-- ObjC selector: @- setLength:@
setLength :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> CULong -> IO ()
setLength mpsVectorDescriptor value =
  sendMessage mpsVectorDescriptor setLengthSelector value

-- | vectors
--
-- The number of vectors.
--
-- ObjC selector: @- vectors@
vectors :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> IO CULong
vectors mpsVectorDescriptor =
  sendMessage mpsVectorDescriptor vectorsSelector

-- | dataType
--
-- The type of the data which makes up the values of the vector.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> IO MPSDataType
dataType mpsVectorDescriptor =
  sendMessage mpsVectorDescriptor dataTypeSelector

-- | dataType
--
-- The type of the data which makes up the values of the vector.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> MPSDataType -> IO ()
setDataType mpsVectorDescriptor value =
  sendMessage mpsVectorDescriptor setDataTypeSelector value

-- | vectorBytes
--
-- The stride, in bytes, between corresponding elements of              consecutive vectors.  Must be a multiple of the element size
--
-- ObjC selector: @- vectorBytes@
vectorBytes :: IsMPSVectorDescriptor mpsVectorDescriptor => mpsVectorDescriptor -> IO CULong
vectorBytes mpsVectorDescriptor =
  sendMessage mpsVectorDescriptor vectorBytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vectorDescriptorWithLength:dataType:@
vectorDescriptorWithLength_dataTypeSelector :: Selector '[CULong, MPSDataType] (Id MPSVectorDescriptor)
vectorDescriptorWithLength_dataTypeSelector = mkSelector "vectorDescriptorWithLength:dataType:"

-- | @Selector@ for @vectorDescriptorWithLength:vectors:vectorBytes:dataType:@
vectorDescriptorWithLength_vectors_vectorBytes_dataTypeSelector :: Selector '[CULong, CULong, CULong, MPSDataType] (Id MPSVectorDescriptor)
vectorDescriptorWithLength_vectors_vectorBytes_dataTypeSelector = mkSelector "vectorDescriptorWithLength:vectors:vectorBytes:dataType:"

-- | @Selector@ for @vectorBytesForLength:dataType:@
vectorBytesForLength_dataTypeSelector :: Selector '[CULong, MPSDataType] CULong
vectorBytesForLength_dataTypeSelector = mkSelector "vectorBytesForLength:dataType:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CULong] ()
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @vectors@
vectorsSelector :: Selector '[] CULong
vectorsSelector = mkSelector "vectors"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MPSDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MPSDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @vectorBytes@
vectorBytesSelector :: Selector '[] CULong
vectorBytesSelector = mkSelector "vectorBytes"

