{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLArgumentDescriptor
--
-- Represents a member of an argument buffer
--
-- Generated bindings for @MTLArgumentDescriptor@.
module ObjC.Metal.MTLArgumentDescriptor
  ( MTLArgumentDescriptor
  , IsMTLArgumentDescriptor(..)
  , argumentDescriptor
  , dataType
  , setDataType
  , index
  , setIndex
  , arrayLength
  , setArrayLength
  , access
  , setAccess
  , textureType
  , setTextureType
  , constantBlockAlignment
  , setConstantBlockAlignment
  , accessSelector
  , argumentDescriptorSelector
  , arrayLengthSelector
  , constantBlockAlignmentSelector
  , dataTypeSelector
  , indexSelector
  , setAccessSelector
  , setArrayLengthSelector
  , setConstantBlockAlignmentSelector
  , setDataTypeSelector
  , setIndexSelector
  , setTextureTypeSelector
  , textureTypeSelector

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

-- | argumentDescriptor
--
-- Create an autoreleased default argument descriptor
--
-- ObjC selector: @+ argumentDescriptor@
argumentDescriptor :: IO (Id MTLArgumentDescriptor)
argumentDescriptor  =
  do
    cls' <- getRequiredClass "MTLArgumentDescriptor"
    sendClassMessage cls' argumentDescriptorSelector

-- | dataType
--
-- For constants, the data type. Otherwise, MTLDataTypeTexture, MTLDataTypeSampler, or MTLDataTypePointer.
--
-- ObjC selector: @- dataType@
dataType :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO MTLDataType
dataType mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor dataTypeSelector

-- | dataType
--
-- For constants, the data type. Otherwise, MTLDataTypeTexture, MTLDataTypeSampler, or MTLDataTypePointer.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> MTLDataType -> IO ()
setDataType mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setDataTypeSelector value

-- | index
--
-- The binding point index of the argument
--
-- ObjC selector: @- index@
index :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO CULong
index mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor indexSelector

-- | index
--
-- The binding point index of the argument
--
-- ObjC selector: @- setIndex:@
setIndex :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> CULong -> IO ()
setIndex mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setIndexSelector value

-- | arrayLength
--
-- The length of an array of constants, textures, or samplers, or 0 for non-array arguments
--
-- ObjC selector: @- arrayLength@
arrayLength :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO CULong
arrayLength mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor arrayLengthSelector

-- | arrayLength
--
-- The length of an array of constants, textures, or samplers, or 0 for non-array arguments
--
-- ObjC selector: @- setArrayLength:@
setArrayLength :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> CULong -> IO ()
setArrayLength mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setArrayLengthSelector value

-- | access
--
-- Access flags for the argument
--
-- ObjC selector: @- access@
access :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO MTLBindingAccess
access mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor accessSelector

-- | access
--
-- Access flags for the argument
--
-- ObjC selector: @- setAccess:@
setAccess :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> MTLBindingAccess -> IO ()
setAccess mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setAccessSelector value

-- | textureType
--
-- For texture arguments, the texture type
--
-- ObjC selector: @- textureType@
textureType :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO MTLTextureType
textureType mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor textureTypeSelector

-- | textureType
--
-- For texture arguments, the texture type
--
-- ObjC selector: @- setTextureType:@
setTextureType :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> MTLTextureType -> IO ()
setTextureType mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setTextureTypeSelector value

-- | constantBlockAlignment
--
-- if set forces the constant block to be aligned to the given alignment
--
-- Should only be set on the first constant of the block and is only valid if a corresponding     explicit "alignas" is applied to the constant in the metal shader language.
--
-- ObjC selector: @- constantBlockAlignment@
constantBlockAlignment :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> IO CULong
constantBlockAlignment mtlArgumentDescriptor =
  sendMessage mtlArgumentDescriptor constantBlockAlignmentSelector

-- | constantBlockAlignment
--
-- if set forces the constant block to be aligned to the given alignment
--
-- Should only be set on the first constant of the block and is only valid if a corresponding     explicit "alignas" is applied to the constant in the metal shader language.
--
-- ObjC selector: @- setConstantBlockAlignment:@
setConstantBlockAlignment :: IsMTLArgumentDescriptor mtlArgumentDescriptor => mtlArgumentDescriptor -> CULong -> IO ()
setConstantBlockAlignment mtlArgumentDescriptor value =
  sendMessage mtlArgumentDescriptor setConstantBlockAlignmentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @argumentDescriptor@
argumentDescriptorSelector :: Selector '[] (Id MTLArgumentDescriptor)
argumentDescriptorSelector = mkSelector "argumentDescriptor"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MTLDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[MTLDataType] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector '[CULong] ()
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @arrayLength@
arrayLengthSelector :: Selector '[] CULong
arrayLengthSelector = mkSelector "arrayLength"

-- | @Selector@ for @setArrayLength:@
setArrayLengthSelector :: Selector '[CULong] ()
setArrayLengthSelector = mkSelector "setArrayLength:"

-- | @Selector@ for @access@
accessSelector :: Selector '[] MTLBindingAccess
accessSelector = mkSelector "access"

-- | @Selector@ for @setAccess:@
setAccessSelector :: Selector '[MTLBindingAccess] ()
setAccessSelector = mkSelector "setAccess:"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector '[] MTLTextureType
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @setTextureType:@
setTextureTypeSelector :: Selector '[MTLTextureType] ()
setTextureTypeSelector = mkSelector "setTextureType:"

-- | @Selector@ for @constantBlockAlignment@
constantBlockAlignmentSelector :: Selector '[] CULong
constantBlockAlignmentSelector = mkSelector "constantBlockAlignment"

-- | @Selector@ for @setConstantBlockAlignment:@
setConstantBlockAlignmentSelector :: Selector '[CULong] ()
setConstantBlockAlignmentSelector = mkSelector "setConstantBlockAlignment:"

