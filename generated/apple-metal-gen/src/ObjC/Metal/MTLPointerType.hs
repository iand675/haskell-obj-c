{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLPointerType@.
module ObjC.Metal.MTLPointerType
  ( MTLPointerType
  , IsMTLPointerType(..)
  , elementStructType
  , elementArrayType
  , elementType
  , access
  , alignment
  , dataSize
  , elementIsArgumentBuffer
  , accessSelector
  , alignmentSelector
  , dataSizeSelector
  , elementArrayTypeSelector
  , elementIsArgumentBufferSelector
  , elementStructTypeSelector
  , elementTypeSelector

  -- * Enum types
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
elementStructType :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO (Id MTLStructType)
elementStructType mtlPointerType =
  sendMessage mtlPointerType elementStructTypeSelector

-- | @- elementArrayType@
elementArrayType :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO (Id MTLArrayType)
elementArrayType mtlPointerType =
  sendMessage mtlPointerType elementArrayTypeSelector

-- | @- elementType@
elementType :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO MTLDataType
elementType mtlPointerType =
  sendMessage mtlPointerType elementTypeSelector

-- | @- access@
access :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO MTLBindingAccess
access mtlPointerType =
  sendMessage mtlPointerType accessSelector

-- | @- alignment@
alignment :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO CULong
alignment mtlPointerType =
  sendMessage mtlPointerType alignmentSelector

-- | @- dataSize@
dataSize :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO CULong
dataSize mtlPointerType =
  sendMessage mtlPointerType dataSizeSelector

-- | @- elementIsArgumentBuffer@
elementIsArgumentBuffer :: IsMTLPointerType mtlPointerType => mtlPointerType -> IO Bool
elementIsArgumentBuffer mtlPointerType =
  sendMessage mtlPointerType elementIsArgumentBufferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @elementStructType@
elementStructTypeSelector :: Selector '[] (Id MTLStructType)
elementStructTypeSelector = mkSelector "elementStructType"

-- | @Selector@ for @elementArrayType@
elementArrayTypeSelector :: Selector '[] (Id MTLArrayType)
elementArrayTypeSelector = mkSelector "elementArrayType"

-- | @Selector@ for @elementType@
elementTypeSelector :: Selector '[] MTLDataType
elementTypeSelector = mkSelector "elementType"

-- | @Selector@ for @access@
accessSelector :: Selector '[] MTLBindingAccess
accessSelector = mkSelector "access"

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] CULong
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @dataSize@
dataSizeSelector :: Selector '[] CULong
dataSizeSelector = mkSelector "dataSize"

-- | @Selector@ for @elementIsArgumentBuffer@
elementIsArgumentBufferSelector :: Selector '[] Bool
elementIsArgumentBufferSelector = mkSelector "elementIsArgumentBuffer"

