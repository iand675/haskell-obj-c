{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLArgument
--
-- Generated bindings for @MTLArgument@.
module ObjC.Metal.MTLArgument
  ( MTLArgument
  , IsMTLArgument(..)
  , name
  , type_
  , access
  , index
  , active
  , bufferAlignment
  , bufferDataSize
  , bufferDataType
  , bufferStructType
  , bufferPointerType
  , threadgroupMemoryAlignment
  , threadgroupMemoryDataSize
  , textureType
  , textureDataType
  , isDepthTexture
  , arrayLength
  , accessSelector
  , activeSelector
  , arrayLengthSelector
  , bufferAlignmentSelector
  , bufferDataSizeSelector
  , bufferDataTypeSelector
  , bufferPointerTypeSelector
  , bufferStructTypeSelector
  , indexSelector
  , isDepthTextureSelector
  , nameSelector
  , textureDataTypeSelector
  , textureTypeSelector
  , threadgroupMemoryAlignmentSelector
  , threadgroupMemoryDataSizeSelector
  , typeSelector

  -- * Enum types
  , MTLArgumentType(MTLArgumentType)
  , pattern MTLArgumentTypeBuffer
  , pattern MTLArgumentTypeThreadgroupMemory
  , pattern MTLArgumentTypeTexture
  , pattern MTLArgumentTypeSampler
  , pattern MTLArgumentTypeImageblockData
  , pattern MTLArgumentTypeImageblock
  , pattern MTLArgumentTypeVisibleFunctionTable
  , pattern MTLArgumentTypePrimitiveAccelerationStructure
  , pattern MTLArgumentTypeInstanceAccelerationStructure
  , pattern MTLArgumentTypeIntersectionFunctionTable
  , MTLBindingAccess(MTLBindingAccess)
  , pattern MTLBindingAccessReadOnly
  , pattern MTLBindingAccessReadWrite
  , pattern MTLBindingAccessWriteOnly
  , pattern MTLArgumentAccessReadOnly
  , pattern MTLArgumentAccessReadWrite
  , pattern MTLArgumentAccessWriteOnly
  , MTLDataType(MTLDataType)
  , pattern MTLDataTypeNone
  , pattern MTLDataTypeStruct
  , pattern MTLDataTypeArray
  , pattern MTLDataTypeFloat
  , pattern MTLDataTypeFloat2
  , pattern MTLDataTypeFloat3
  , pattern MTLDataTypeFloat4
  , pattern MTLDataTypeFloat2x2
  , pattern MTLDataTypeFloat2x3
  , pattern MTLDataTypeFloat2x4
  , pattern MTLDataTypeFloat3x2
  , pattern MTLDataTypeFloat3x3
  , pattern MTLDataTypeFloat3x4
  , pattern MTLDataTypeFloat4x2
  , pattern MTLDataTypeFloat4x3
  , pattern MTLDataTypeFloat4x4
  , pattern MTLDataTypeHalf
  , pattern MTLDataTypeHalf2
  , pattern MTLDataTypeHalf3
  , pattern MTLDataTypeHalf4
  , pattern MTLDataTypeHalf2x2
  , pattern MTLDataTypeHalf2x3
  , pattern MTLDataTypeHalf2x4
  , pattern MTLDataTypeHalf3x2
  , pattern MTLDataTypeHalf3x3
  , pattern MTLDataTypeHalf3x4
  , pattern MTLDataTypeHalf4x2
  , pattern MTLDataTypeHalf4x3
  , pattern MTLDataTypeHalf4x4
  , pattern MTLDataTypeInt
  , pattern MTLDataTypeInt2
  , pattern MTLDataTypeInt3
  , pattern MTLDataTypeInt4
  , pattern MTLDataTypeUInt
  , pattern MTLDataTypeUInt2
  , pattern MTLDataTypeUInt3
  , pattern MTLDataTypeUInt4
  , pattern MTLDataTypeShort
  , pattern MTLDataTypeShort2
  , pattern MTLDataTypeShort3
  , pattern MTLDataTypeShort4
  , pattern MTLDataTypeUShort
  , pattern MTLDataTypeUShort2
  , pattern MTLDataTypeUShort3
  , pattern MTLDataTypeUShort4
  , pattern MTLDataTypeChar
  , pattern MTLDataTypeChar2
  , pattern MTLDataTypeChar3
  , pattern MTLDataTypeChar4
  , pattern MTLDataTypeUChar
  , pattern MTLDataTypeUChar2
  , pattern MTLDataTypeUChar3
  , pattern MTLDataTypeUChar4
  , pattern MTLDataTypeBool
  , pattern MTLDataTypeBool2
  , pattern MTLDataTypeBool3
  , pattern MTLDataTypeBool4
  , pattern MTLDataTypeTexture
  , pattern MTLDataTypeSampler
  , pattern MTLDataTypePointer
  , pattern MTLDataTypeR8Unorm
  , pattern MTLDataTypeR8Snorm
  , pattern MTLDataTypeR16Unorm
  , pattern MTLDataTypeR16Snorm
  , pattern MTLDataTypeRG8Unorm
  , pattern MTLDataTypeRG8Snorm
  , pattern MTLDataTypeRG16Unorm
  , pattern MTLDataTypeRG16Snorm
  , pattern MTLDataTypeRGBA8Unorm
  , pattern MTLDataTypeRGBA8Unorm_sRGB
  , pattern MTLDataTypeRGBA8Snorm
  , pattern MTLDataTypeRGBA16Unorm
  , pattern MTLDataTypeRGBA16Snorm
  , pattern MTLDataTypeRGB10A2Unorm
  , pattern MTLDataTypeRG11B10Float
  , pattern MTLDataTypeRGB9E5Float
  , pattern MTLDataTypeRenderPipeline
  , pattern MTLDataTypeComputePipeline
  , pattern MTLDataTypeIndirectCommandBuffer
  , pattern MTLDataTypeLong
  , pattern MTLDataTypeLong2
  , pattern MTLDataTypeLong3
  , pattern MTLDataTypeLong4
  , pattern MTLDataTypeULong
  , pattern MTLDataTypeULong2
  , pattern MTLDataTypeULong3
  , pattern MTLDataTypeULong4
  , pattern MTLDataTypeVisibleFunctionTable
  , pattern MTLDataTypeIntersectionFunctionTable
  , pattern MTLDataTypePrimitiveAccelerationStructure
  , pattern MTLDataTypeInstanceAccelerationStructure
  , pattern MTLDataTypeBFloat
  , pattern MTLDataTypeBFloat2
  , pattern MTLDataTypeBFloat3
  , pattern MTLDataTypeBFloat4
  , pattern MTLDataTypeDepthStencilState
  , pattern MTLDataTypeTensor
  , MTLTextureType(MTLTextureType)
  , pattern MTLTextureType1D
  , pattern MTLTextureType1DArray
  , pattern MTLTextureType2D
  , pattern MTLTextureType2DArray
  , pattern MTLTextureType2DMultisample
  , pattern MTLTextureTypeCube
  , pattern MTLTextureTypeCubeArray
  , pattern MTLTextureType3D
  , pattern MTLTextureType2DMultisampleArray
  , pattern MTLTextureTypeTextureBuffer

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id NSString)
name mtlArgument =
  sendMessage mtlArgument nameSelector

-- | @- type@
type_ :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLArgumentType
type_ mtlArgument =
  sendMessage mtlArgument typeSelector

-- | @- access@
access :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLBindingAccess
access mtlArgument =
  sendMessage mtlArgument accessSelector

-- | @- index@
index :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
index mtlArgument =
  sendMessage mtlArgument indexSelector

-- | @- active@
active :: IsMTLArgument mtlArgument => mtlArgument -> IO Bool
active mtlArgument =
  sendMessage mtlArgument activeSelector

-- | @- bufferAlignment@
bufferAlignment :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
bufferAlignment mtlArgument =
  sendMessage mtlArgument bufferAlignmentSelector

-- | @- bufferDataSize@
bufferDataSize :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
bufferDataSize mtlArgument =
  sendMessage mtlArgument bufferDataSizeSelector

-- | @- bufferDataType@
bufferDataType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLDataType
bufferDataType mtlArgument =
  sendMessage mtlArgument bufferDataTypeSelector

-- | @- bufferStructType@
bufferStructType :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id MTLStructType)
bufferStructType mtlArgument =
  sendMessage mtlArgument bufferStructTypeSelector

-- | @- bufferPointerType@
bufferPointerType :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id MTLPointerType)
bufferPointerType mtlArgument =
  sendMessage mtlArgument bufferPointerTypeSelector

-- | @- threadgroupMemoryAlignment@
threadgroupMemoryAlignment :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
threadgroupMemoryAlignment mtlArgument =
  sendMessage mtlArgument threadgroupMemoryAlignmentSelector

-- | @- threadgroupMemoryDataSize@
threadgroupMemoryDataSize :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
threadgroupMemoryDataSize mtlArgument =
  sendMessage mtlArgument threadgroupMemoryDataSizeSelector

-- | @- textureType@
textureType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLTextureType
textureType mtlArgument =
  sendMessage mtlArgument textureTypeSelector

-- | @- textureDataType@
textureDataType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLDataType
textureDataType mtlArgument =
  sendMessage mtlArgument textureDataTypeSelector

-- | @- isDepthTexture@
isDepthTexture :: IsMTLArgument mtlArgument => mtlArgument -> IO Bool
isDepthTexture mtlArgument =
  sendMessage mtlArgument isDepthTextureSelector

-- | @- arrayLength@
arrayLength :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
arrayLength mtlArgument =
  sendMessage mtlArgument arrayLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTLArgumentType
typeSelector = mkSelector "type"

-- | @Selector@ for @access@
accessSelector :: Selector '[] MTLBindingAccess
accessSelector = mkSelector "access"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @bufferAlignment@
bufferAlignmentSelector :: Selector '[] CULong
bufferAlignmentSelector = mkSelector "bufferAlignment"

-- | @Selector@ for @bufferDataSize@
bufferDataSizeSelector :: Selector '[] CULong
bufferDataSizeSelector = mkSelector "bufferDataSize"

-- | @Selector@ for @bufferDataType@
bufferDataTypeSelector :: Selector '[] MTLDataType
bufferDataTypeSelector = mkSelector "bufferDataType"

-- | @Selector@ for @bufferStructType@
bufferStructTypeSelector :: Selector '[] (Id MTLStructType)
bufferStructTypeSelector = mkSelector "bufferStructType"

-- | @Selector@ for @bufferPointerType@
bufferPointerTypeSelector :: Selector '[] (Id MTLPointerType)
bufferPointerTypeSelector = mkSelector "bufferPointerType"

-- | @Selector@ for @threadgroupMemoryAlignment@
threadgroupMemoryAlignmentSelector :: Selector '[] CULong
threadgroupMemoryAlignmentSelector = mkSelector "threadgroupMemoryAlignment"

-- | @Selector@ for @threadgroupMemoryDataSize@
threadgroupMemoryDataSizeSelector :: Selector '[] CULong
threadgroupMemoryDataSizeSelector = mkSelector "threadgroupMemoryDataSize"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector '[] MTLTextureType
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @textureDataType@
textureDataTypeSelector :: Selector '[] MTLDataType
textureDataTypeSelector = mkSelector "textureDataType"

-- | @Selector@ for @isDepthTexture@
isDepthTextureSelector :: Selector '[] Bool
isDepthTextureSelector = mkSelector "isDepthTexture"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector '[] CULong
arrayLengthSelector = mkSelector "arrayLength"

