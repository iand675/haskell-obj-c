{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLArrayType@.
module ObjC.Metal.MTLArrayType
  ( MTLArrayType
  , IsMTLArrayType(..)
  , elementStructType
  , elementArrayType
  , elementTextureReferenceType
  , elementPointerType
  , elementTensorReferenceType
  , elementType
  , arrayLength
  , stride
  , argumentIndexStride
  , argumentIndexStrideSelector
  , arrayLengthSelector
  , elementArrayTypeSelector
  , elementPointerTypeSelector
  , elementStructTypeSelector
  , elementTensorReferenceTypeSelector
  , elementTextureReferenceTypeSelector
  , elementTypeSelector
  , strideSelector

  -- * Enum types
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

-- | @- elementStructType@
elementStructType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO (Id MTLStructType)
elementStructType mtlArrayType =
  sendMessage mtlArrayType elementStructTypeSelector

-- | @- elementArrayType@
elementArrayType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO (Id MTLArrayType)
elementArrayType mtlArrayType =
  sendMessage mtlArrayType elementArrayTypeSelector

-- | @- elementTextureReferenceType@
elementTextureReferenceType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO (Id MTLTextureReferenceType)
elementTextureReferenceType mtlArrayType =
  sendMessage mtlArrayType elementTextureReferenceTypeSelector

-- | @- elementPointerType@
elementPointerType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO (Id MTLPointerType)
elementPointerType mtlArrayType =
  sendMessage mtlArrayType elementPointerTypeSelector

-- | Provides a description of the underlying tensor type when this array holds tensors as its elements.
--
-- - Returns: A description of the tensor type that this array holds, or @nil@ if this struct member doesn't hold a tensor.
--
-- ObjC selector: @- elementTensorReferenceType@
elementTensorReferenceType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO (Id MTLTensorReferenceType)
elementTensorReferenceType mtlArrayType =
  sendMessage mtlArrayType elementTensorReferenceTypeSelector

-- | @- elementType@
elementType :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO MTLDataType
elementType mtlArrayType =
  sendMessage mtlArrayType elementTypeSelector

-- | @- arrayLength@
arrayLength :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO CULong
arrayLength mtlArrayType =
  sendMessage mtlArrayType arrayLengthSelector

-- | @- stride@
stride :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO CULong
stride mtlArrayType =
  sendMessage mtlArrayType strideSelector

-- | @- argumentIndexStride@
argumentIndexStride :: IsMTLArrayType mtlArrayType => mtlArrayType -> IO CULong
argumentIndexStride mtlArrayType =
  sendMessage mtlArrayType argumentIndexStrideSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @elementStructType@
elementStructTypeSelector :: Selector '[] (Id MTLStructType)
elementStructTypeSelector = mkSelector "elementStructType"

-- | @Selector@ for @elementArrayType@
elementArrayTypeSelector :: Selector '[] (Id MTLArrayType)
elementArrayTypeSelector = mkSelector "elementArrayType"

-- | @Selector@ for @elementTextureReferenceType@
elementTextureReferenceTypeSelector :: Selector '[] (Id MTLTextureReferenceType)
elementTextureReferenceTypeSelector = mkSelector "elementTextureReferenceType"

-- | @Selector@ for @elementPointerType@
elementPointerTypeSelector :: Selector '[] (Id MTLPointerType)
elementPointerTypeSelector = mkSelector "elementPointerType"

-- | @Selector@ for @elementTensorReferenceType@
elementTensorReferenceTypeSelector :: Selector '[] (Id MTLTensorReferenceType)
elementTensorReferenceTypeSelector = mkSelector "elementTensorReferenceType"

-- | @Selector@ for @elementType@
elementTypeSelector :: Selector '[] MTLDataType
elementTypeSelector = mkSelector "elementType"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector '[] CULong
arrayLengthSelector = mkSelector "arrayLength"

-- | @Selector@ for @stride@
strideSelector :: Selector '[] CULong
strideSelector = mkSelector "stride"

-- | @Selector@ for @argumentIndexStride@
argumentIndexStrideSelector :: Selector '[] CULong
argumentIndexStrideSelector = mkSelector "argumentIndexStride"

