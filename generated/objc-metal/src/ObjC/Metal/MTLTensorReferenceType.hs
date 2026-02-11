{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a tensor in the shading language in a struct or array.
--
-- Generated bindings for @MTLTensorReferenceType@.
module ObjC.Metal.MTLTensorReferenceType
  ( MTLTensorReferenceType
  , IsMTLTensorReferenceType(..)
  , tensorDataType
  , indexType
  , dimensions
  , access
  , tensorDataTypeSelector
  , indexTypeSelector
  , dimensionsSelector
  , accessSelector

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
  , MTLTensorDataType(MTLTensorDataType)
  , pattern MTLTensorDataTypeNone
  , pattern MTLTensorDataTypeFloat32
  , pattern MTLTensorDataTypeFloat16
  , pattern MTLTensorDataTypeBFloat16
  , pattern MTLTensorDataTypeInt8
  , pattern MTLTensorDataTypeUInt8
  , pattern MTLTensorDataTypeInt16
  , pattern MTLTensorDataTypeUInt16
  , pattern MTLTensorDataTypeInt32
  , pattern MTLTensorDataTypeUInt32

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

-- | The underlying data format of the tensor.
--
-- ObjC selector: @- tensorDataType@
tensorDataType :: IsMTLTensorReferenceType mtlTensorReferenceType => mtlTensorReferenceType -> IO MTLTensorDataType
tensorDataType mtlTensorReferenceType  =
  fmap (coerce :: CLong -> MTLTensorDataType) $ sendMsg mtlTensorReferenceType (mkSelector "tensorDataType") retCLong []

-- | The data format you use for indexing into the tensor.
--
-- ObjC selector: @- indexType@
indexType :: IsMTLTensorReferenceType mtlTensorReferenceType => mtlTensorReferenceType -> IO MTLDataType
indexType mtlTensorReferenceType  =
  fmap (coerce :: CULong -> MTLDataType) $ sendMsg mtlTensorReferenceType (mkSelector "indexType") retCULong []

-- | The array of sizes, in elements, one for each dimension of this tensor.
--
-- Because shader-bound tensors have dynamic extents, the ``MTLTensorExtents/rank`` of @dimensions@ corresponds to the rank the shader function specifies, and ``MTLTensorExtents/extentsAtDimensionIndex:`` always returns a value of -1.
--
-- ObjC selector: @- dimensions@
dimensions :: IsMTLTensorReferenceType mtlTensorReferenceType => mtlTensorReferenceType -> IO (Id MTLTensorExtents)
dimensions mtlTensorReferenceType  =
  sendMsg mtlTensorReferenceType (mkSelector "dimensions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A value that represents the read/write permissions of the tensor.
--
-- ObjC selector: @- access@
access :: IsMTLTensorReferenceType mtlTensorReferenceType => mtlTensorReferenceType -> IO MTLBindingAccess
access mtlTensorReferenceType  =
  fmap (coerce :: CULong -> MTLBindingAccess) $ sendMsg mtlTensorReferenceType (mkSelector "access") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tensorDataType@
tensorDataTypeSelector :: Selector
tensorDataTypeSelector = mkSelector "tensorDataType"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @dimensions@
dimensionsSelector :: Selector
dimensionsSelector = mkSelector "dimensions"

-- | @Selector@ for @access@
accessSelector :: Selector
accessSelector = mkSelector "access"

