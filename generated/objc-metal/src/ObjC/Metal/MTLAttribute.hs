{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLAttribute@.
module ObjC.Metal.MTLAttribute
  ( MTLAttribute
  , IsMTLAttribute(..)
  , name
  , attributeIndex
  , attributeType
  , active
  , patchData
  , patchControlPointData
  , nameSelector
  , attributeIndexSelector
  , attributeTypeSelector
  , activeSelector
  , patchDataSelector
  , patchControlPointDataSelector

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
name :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO (Id NSString)
name mtlAttribute  =
  sendMsg mtlAttribute (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributeIndex@
attributeIndex :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO CULong
attributeIndex mtlAttribute  =
  sendMsg mtlAttribute (mkSelector "attributeIndex") retCULong []

-- | @- attributeType@
attributeType :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO MTLDataType
attributeType mtlAttribute  =
  fmap (coerce :: CULong -> MTLDataType) $ sendMsg mtlAttribute (mkSelector "attributeType") retCULong []

-- | @- active@
active :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO Bool
active mtlAttribute  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlAttribute (mkSelector "active") retCULong []

-- | @- patchData@
patchData :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO Bool
patchData mtlAttribute  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlAttribute (mkSelector "patchData") retCULong []

-- | @- patchControlPointData@
patchControlPointData :: IsMTLAttribute mtlAttribute => mtlAttribute -> IO Bool
patchControlPointData mtlAttribute  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlAttribute (mkSelector "patchControlPointData") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @attributeIndex@
attributeIndexSelector :: Selector
attributeIndexSelector = mkSelector "attributeIndex"

-- | @Selector@ for @attributeType@
attributeTypeSelector :: Selector
attributeTypeSelector = mkSelector "attributeType"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @patchData@
patchDataSelector :: Selector
patchDataSelector = mkSelector "patchData"

-- | @Selector@ for @patchControlPointData@
patchControlPointDataSelector :: Selector
patchControlPointDataSelector = mkSelector "patchControlPointData"

