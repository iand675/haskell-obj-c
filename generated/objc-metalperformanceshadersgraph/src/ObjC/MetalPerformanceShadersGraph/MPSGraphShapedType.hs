{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The shaped type class for types on tensors with a shape and data type.
--
-- Generated bindings for @MPSGraphShapedType@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphShapedType
  ( MPSGraphShapedType
  , IsMPSGraphShapedType(..)
  , isEqualTo
  , dataType
  , setDataType
  , isEqualToSelector
  , dataTypeSelector
  , setDataTypeSelector

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Checks if shapes and element data type are the same as the input shaped type.
--
-- - Parameters:   - object: shapedType to compare to - Returns: true if equal, false if unequal
--
-- ObjC selector: @- isEqualTo:@
isEqualTo :: (IsMPSGraphShapedType mpsGraphShapedType, IsMPSGraphShapedType object) => mpsGraphShapedType -> object -> IO Bool
isEqualTo mpsGraphShapedType  object =
withObjCPtr object $ \raw_object ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphShapedType (mkSelector "isEqualTo:") retCULong [argPtr (castPtr raw_object :: Ptr ())]

-- | The data type of the shaped type.
--
-- ObjC selector: @- dataType@
dataType :: IsMPSGraphShapedType mpsGraphShapedType => mpsGraphShapedType -> IO MPSDataType
dataType mpsGraphShapedType  =
  fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsGraphShapedType (mkSelector "dataType") retCUInt []

-- | The data type of the shaped type.
--
-- ObjC selector: @- setDataType:@
setDataType :: IsMPSGraphShapedType mpsGraphShapedType => mpsGraphShapedType -> MPSDataType -> IO ()
setDataType mpsGraphShapedType  value =
  sendMsg mpsGraphShapedType (mkSelector "setDataType:") retVoid [argCUInt (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualTo:@
isEqualToSelector :: Selector
isEqualToSelector = mkSelector "isEqualTo:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

