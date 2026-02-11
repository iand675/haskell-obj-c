{-# LANGUAGE PatternSynonyms #-}
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
  , nameSelector
  , typeSelector
  , accessSelector
  , indexSelector
  , activeSelector
  , bufferAlignmentSelector
  , bufferDataSizeSelector
  , bufferDataTypeSelector
  , bufferStructTypeSelector
  , bufferPointerTypeSelector
  , threadgroupMemoryAlignmentSelector
  , threadgroupMemoryDataSizeSelector
  , textureTypeSelector
  , textureDataTypeSelector
  , isDepthTextureSelector
  , arrayLengthSelector

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id NSString)
name mtlArgument  =
  sendMsg mtlArgument (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLArgumentType
type_ mtlArgument  =
  fmap (coerce :: CULong -> MTLArgumentType) $ sendMsg mtlArgument (mkSelector "type") retCULong []

-- | @- access@
access :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLBindingAccess
access mtlArgument  =
  fmap (coerce :: CULong -> MTLBindingAccess) $ sendMsg mtlArgument (mkSelector "access") retCULong []

-- | @- index@
index :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
index mtlArgument  =
  sendMsg mtlArgument (mkSelector "index") retCULong []

-- | @- active@
active :: IsMTLArgument mtlArgument => mtlArgument -> IO Bool
active mtlArgument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlArgument (mkSelector "active") retCULong []

-- | @- bufferAlignment@
bufferAlignment :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
bufferAlignment mtlArgument  =
  sendMsg mtlArgument (mkSelector "bufferAlignment") retCULong []

-- | @- bufferDataSize@
bufferDataSize :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
bufferDataSize mtlArgument  =
  sendMsg mtlArgument (mkSelector "bufferDataSize") retCULong []

-- | @- bufferDataType@
bufferDataType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLDataType
bufferDataType mtlArgument  =
  fmap (coerce :: CULong -> MTLDataType) $ sendMsg mtlArgument (mkSelector "bufferDataType") retCULong []

-- | @- bufferStructType@
bufferStructType :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id MTLStructType)
bufferStructType mtlArgument  =
  sendMsg mtlArgument (mkSelector "bufferStructType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bufferPointerType@
bufferPointerType :: IsMTLArgument mtlArgument => mtlArgument -> IO (Id MTLPointerType)
bufferPointerType mtlArgument  =
  sendMsg mtlArgument (mkSelector "bufferPointerType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- threadgroupMemoryAlignment@
threadgroupMemoryAlignment :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
threadgroupMemoryAlignment mtlArgument  =
  sendMsg mtlArgument (mkSelector "threadgroupMemoryAlignment") retCULong []

-- | @- threadgroupMemoryDataSize@
threadgroupMemoryDataSize :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
threadgroupMemoryDataSize mtlArgument  =
  sendMsg mtlArgument (mkSelector "threadgroupMemoryDataSize") retCULong []

-- | @- textureType@
textureType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLTextureType
textureType mtlArgument  =
  fmap (coerce :: CULong -> MTLTextureType) $ sendMsg mtlArgument (mkSelector "textureType") retCULong []

-- | @- textureDataType@
textureDataType :: IsMTLArgument mtlArgument => mtlArgument -> IO MTLDataType
textureDataType mtlArgument  =
  fmap (coerce :: CULong -> MTLDataType) $ sendMsg mtlArgument (mkSelector "textureDataType") retCULong []

-- | @- isDepthTexture@
isDepthTexture :: IsMTLArgument mtlArgument => mtlArgument -> IO Bool
isDepthTexture mtlArgument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlArgument (mkSelector "isDepthTexture") retCULong []

-- | @- arrayLength@
arrayLength :: IsMTLArgument mtlArgument => mtlArgument -> IO CULong
arrayLength mtlArgument  =
  sendMsg mtlArgument (mkSelector "arrayLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @access@
accessSelector :: Selector
accessSelector = mkSelector "access"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @bufferAlignment@
bufferAlignmentSelector :: Selector
bufferAlignmentSelector = mkSelector "bufferAlignment"

-- | @Selector@ for @bufferDataSize@
bufferDataSizeSelector :: Selector
bufferDataSizeSelector = mkSelector "bufferDataSize"

-- | @Selector@ for @bufferDataType@
bufferDataTypeSelector :: Selector
bufferDataTypeSelector = mkSelector "bufferDataType"

-- | @Selector@ for @bufferStructType@
bufferStructTypeSelector :: Selector
bufferStructTypeSelector = mkSelector "bufferStructType"

-- | @Selector@ for @bufferPointerType@
bufferPointerTypeSelector :: Selector
bufferPointerTypeSelector = mkSelector "bufferPointerType"

-- | @Selector@ for @threadgroupMemoryAlignment@
threadgroupMemoryAlignmentSelector :: Selector
threadgroupMemoryAlignmentSelector = mkSelector "threadgroupMemoryAlignment"

-- | @Selector@ for @threadgroupMemoryDataSize@
threadgroupMemoryDataSizeSelector :: Selector
threadgroupMemoryDataSizeSelector = mkSelector "threadgroupMemoryDataSize"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @textureDataType@
textureDataTypeSelector :: Selector
textureDataTypeSelector = mkSelector "textureDataType"

-- | @Selector@ for @isDepthTexture@
isDepthTextureSelector :: Selector
isDepthTextureSelector = mkSelector "isDepthTexture"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector
arrayLengthSelector = mkSelector "arrayLength"

