{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLStructMember@.
module ObjC.Metal.MTLStructMember
  ( MTLStructMember
  , IsMTLStructMember(..)
  , structType
  , arrayType
  , textureReferenceType
  , pointerType
  , tensorReferenceType
  , name
  , offset
  , dataType
  , argumentIndex
  , argumentIndexSelector
  , arrayTypeSelector
  , dataTypeSelector
  , nameSelector
  , offsetSelector
  , pointerTypeSelector
  , structTypeSelector
  , tensorReferenceTypeSelector
  , textureReferenceTypeSelector

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

-- | @- structType@
structType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id MTLStructType)
structType mtlStructMember =
  sendMessage mtlStructMember structTypeSelector

-- | @- arrayType@
arrayType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id MTLArrayType)
arrayType mtlStructMember =
  sendMessage mtlStructMember arrayTypeSelector

-- | @- textureReferenceType@
textureReferenceType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id MTLTextureReferenceType)
textureReferenceType mtlStructMember =
  sendMessage mtlStructMember textureReferenceTypeSelector

-- | @- pointerType@
pointerType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id MTLPointerType)
pointerType mtlStructMember =
  sendMessage mtlStructMember pointerTypeSelector

-- | Provides a description of the underlying tensor type when this struct member holds a tensor.
--
-- - Returns: A description of the tensor type that this struct member holds, or @nil@ if this struct member doesn't hold a tensor.
--
-- ObjC selector: @- tensorReferenceType@
tensorReferenceType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id MTLTensorReferenceType)
tensorReferenceType mtlStructMember =
  sendMessage mtlStructMember tensorReferenceTypeSelector

-- | @- name@
name :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO (Id NSString)
name mtlStructMember =
  sendMessage mtlStructMember nameSelector

-- | @- offset@
offset :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO CULong
offset mtlStructMember =
  sendMessage mtlStructMember offsetSelector

-- | @- dataType@
dataType :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO MTLDataType
dataType mtlStructMember =
  sendMessage mtlStructMember dataTypeSelector

-- | @- argumentIndex@
argumentIndex :: IsMTLStructMember mtlStructMember => mtlStructMember -> IO CULong
argumentIndex mtlStructMember =
  sendMessage mtlStructMember argumentIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @structType@
structTypeSelector :: Selector '[] (Id MTLStructType)
structTypeSelector = mkSelector "structType"

-- | @Selector@ for @arrayType@
arrayTypeSelector :: Selector '[] (Id MTLArrayType)
arrayTypeSelector = mkSelector "arrayType"

-- | @Selector@ for @textureReferenceType@
textureReferenceTypeSelector :: Selector '[] (Id MTLTextureReferenceType)
textureReferenceTypeSelector = mkSelector "textureReferenceType"

-- | @Selector@ for @pointerType@
pointerTypeSelector :: Selector '[] (Id MTLPointerType)
pointerTypeSelector = mkSelector "pointerType"

-- | @Selector@ for @tensorReferenceType@
tensorReferenceTypeSelector :: Selector '[] (Id MTLTensorReferenceType)
tensorReferenceTypeSelector = mkSelector "tensorReferenceType"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MTLDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @argumentIndex@
argumentIndexSelector :: Selector '[] CULong
argumentIndexSelector = mkSelector "argumentIndex"

