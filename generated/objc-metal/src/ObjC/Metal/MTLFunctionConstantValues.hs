{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLFunctionConstantValues@.
module ObjC.Metal.MTLFunctionConstantValues
  ( MTLFunctionConstantValues
  , IsMTLFunctionConstantValues(..)
  , setConstantValue_type_atIndex
  , setConstantValues_type_withRange
  , setConstantValue_type_withName
  , reset
  , setConstantValue_type_atIndexSelector
  , setConstantValues_type_withRangeSelector
  , setConstantValue_type_withNameSelector
  , resetSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setConstantValue:type:atIndex:@
setConstantValue_type_atIndex :: IsMTLFunctionConstantValues mtlFunctionConstantValues => mtlFunctionConstantValues -> Const (Ptr ()) -> MTLDataType -> CULong -> IO ()
setConstantValue_type_atIndex mtlFunctionConstantValues  value type_ index =
  sendMsg mtlFunctionConstantValues (mkSelector "setConstantValue:type:atIndex:") retVoid [argPtr (unConst value), argCULong (coerce type_), argCULong (fromIntegral index)]

-- | @- setConstantValues:type:withRange:@
setConstantValues_type_withRange :: IsMTLFunctionConstantValues mtlFunctionConstantValues => mtlFunctionConstantValues -> Const (Ptr ()) -> MTLDataType -> NSRange -> IO ()
setConstantValues_type_withRange mtlFunctionConstantValues  values type_ range =
  sendMsg mtlFunctionConstantValues (mkSelector "setConstantValues:type:withRange:") retVoid [argPtr (unConst values), argCULong (coerce type_), argNSRange range]

-- | @- setConstantValue:type:withName:@
setConstantValue_type_withName :: (IsMTLFunctionConstantValues mtlFunctionConstantValues, IsNSString name) => mtlFunctionConstantValues -> Const (Ptr ()) -> MTLDataType -> name -> IO ()
setConstantValue_type_withName mtlFunctionConstantValues  value type_ name =
withObjCPtr name $ \raw_name ->
    sendMsg mtlFunctionConstantValues (mkSelector "setConstantValue:type:withName:") retVoid [argPtr (unConst value), argCULong (coerce type_), argPtr (castPtr raw_name :: Ptr ())]

-- | @- reset@
reset :: IsMTLFunctionConstantValues mtlFunctionConstantValues => mtlFunctionConstantValues -> IO ()
reset mtlFunctionConstantValues  =
  sendMsg mtlFunctionConstantValues (mkSelector "reset") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setConstantValue:type:atIndex:@
setConstantValue_type_atIndexSelector :: Selector
setConstantValue_type_atIndexSelector = mkSelector "setConstantValue:type:atIndex:"

-- | @Selector@ for @setConstantValues:type:withRange:@
setConstantValues_type_withRangeSelector :: Selector
setConstantValues_type_withRangeSelector = mkSelector "setConstantValues:type:withRange:"

-- | @Selector@ for @setConstantValue:type:withName:@
setConstantValue_type_withNameSelector :: Selector
setConstantValue_type_withNameSelector = mkSelector "setConstantValue:type:withName:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

