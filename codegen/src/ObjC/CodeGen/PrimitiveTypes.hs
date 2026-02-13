{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Canonical lookup table for C primitive types and their Haskell
-- FFI representations.
--
-- Every piece of knowledge about how a C primitive maps to Haskell
-- lives here: the Haskell type name, the libffi ret\/arg function
-- names, the FFI type constant, and the size\/alignment.
--
-- Other modules ('TypeMap', 'Generate.Method', 'Generate.Structs',
-- 'Generate.Enums') all delegate to 'lookupPrimitive' instead of
-- reimplementing the same guard chains.
module ObjC.CodeGen.PrimitiveTypes
  ( PrimitiveTypeInfo(..)
  , lookupPrimitive
  , lookupPrimitiveByDesugared
  , allDesugaredForms
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Everything we need to know about a C primitive type.
data PrimitiveTypeInfo = PrimitiveTypeInfo
  { ptHsType      :: Text
    -- ^ Haskell FFI type name (e.g., @\"CLong\"@, @\"CDouble\"@).
  , ptRetFunction :: Text
    -- ^ libffi RetType function name (e.g., @\"retCLong\"@).
  , ptRetFuncType :: Text
    -- ^ Haskell type returned by the ret function.
    -- Usually the same as 'ptHsType' but differs for types without
    -- a direct ret function (e.g., @CShort@ → @retInt16@ returns @Int16@).
  , ptArgFunction :: Text
    -- ^ libffi Arg function name (e.g., @\"argCLong\"@).
  , ptFFIType     :: Text
    -- ^ libffi ffi_type constant (e.g., @\"ffi_type_slong\"@).
  , ptSize        :: Int
    -- ^ Size in bytes (on 64-bit platforms).
  , ptAlignment   :: Int
    -- ^ Alignment in bytes.
  } deriving (Eq, Show)

-- | Look up a C primitive by its @qualType@ / @desugaredQualType@ pair.
--
-- Tries the desugared form first (canonical C type), then falls back
-- to known typedef names in the qualType.
lookupPrimitive :: Text -> Text -> Maybe PrimitiveTypeInfo
lookupPrimitive qualType desugared =
  case Map.lookup desugared desugaredMap of
    Just info -> Just info
    Nothing   -> Map.lookup qualType typedefMap

-- | Look up by desugared form only.
lookupPrimitiveByDesugared :: Text -> Maybe PrimitiveTypeInfo
lookupPrimitiveByDesugared = flip Map.lookup desugaredMap

-- | All known desugared form keys.
allDesugaredForms :: [Text]
allDesugaredForms = Map.keys desugaredMap

-- ---------------------------------------------------------------------------
-- Tables
-- ---------------------------------------------------------------------------

-- | Canonical map from desugared C type strings to their info.
desugaredMap :: Map Text PrimitiveTypeInfo
desugaredMap = Map.fromList
  -- Signed integers
  [ ("long",               PrimitiveTypeInfo "CLong"   "retCLong"   "CLong"   "argCLong"   "ffi_type_slong"   8 8)
  , ("long long",          PrimitiveTypeInfo "CLong"   "retCLong"   "CLong"   "argCLong"   "ffi_type_slong"   8 8)
  , ("int",                PrimitiveTypeInfo "CInt"    "retCInt"    "CInt"    "argCInt"    "ffi_type_sint"    4 4)
  , ("short",              PrimitiveTypeInfo "CShort"  "retCInt"    "CInt"    "argCInt"    "ffi_type_sint16"  2 2)
  , ("signed char",        PrimitiveTypeInfo "CSChar"  "retInt8"    "Int8"    "argCChar"   "ffi_type_sint8"   1 1)
  , ("char",               PrimitiveTypeInfo "CChar"   "retCChar"   "CChar"   "argCChar"   "ffi_type_sint8"   1 1)
  -- Unsigned integers
  , ("unsigned long",      PrimitiveTypeInfo "CULong"  "retCULong"  "CULong"  "argCULong"  "ffi_type_ulong"   8 8)
  , ("unsigned long long", PrimitiveTypeInfo "CULong"  "retCULong"  "CULong"  "argCULong"  "ffi_type_ulong"   8 8)
  , ("unsigned int",       PrimitiveTypeInfo "CUInt"   "retCUInt"   "CUInt"   "argCUInt"   "ffi_type_uint"    4 4)
  , ("unsigned short",     PrimitiveTypeInfo "CUShort" "retCUInt"   "CUInt"   "argCUInt"   "ffi_type_uint16"  2 2)
  , ("unsigned char",      PrimitiveTypeInfo "CUChar"  "retCUChar"  "CUChar"  "argCUChar"  "ffi_type_uint8"   1 1)
  -- Floating point
  , ("double",             PrimitiveTypeInfo "CDouble" "retCDouble" "CDouble" "argCDouble" "ffi_type_double"  8 8)
  , ("float",              PrimitiveTypeInfo "CFloat"  "retCFloat"  "CFloat"  "argCFloat"  "ffi_type_float"   4 4)
  -- Fixed-width (aliases into the above, but with explicit names)
  , ("uint8_t",            PrimitiveTypeInfo "CUChar"  "retCUChar"  "CUChar"  "argCUChar"  "ffi_type_uint8"   1 1)
  , ("int8_t",             PrimitiveTypeInfo "CSChar"  "retInt8"    "Int8"    "argCChar"   "ffi_type_sint8"   1 1)
  , ("uint16_t",           PrimitiveTypeInfo "CUShort" "retCUInt"   "CUInt"   "argCUInt"   "ffi_type_uint16"  2 2)
  , ("int16_t",            PrimitiveTypeInfo "CShort"  "retCInt"    "CInt"    "argCInt"    "ffi_type_sint16"  2 2)
  , ("uint32_t",           PrimitiveTypeInfo "CUInt"   "retCUInt"   "CUInt"   "argCUInt"   "ffi_type_uint"    4 4)
  , ("int32_t",            PrimitiveTypeInfo "CInt"    "retCInt"    "CInt"    "argCInt"    "ffi_type_sint"    4 4)
  , ("uint64_t",           PrimitiveTypeInfo "CULong"  "retCULong"  "CULong"  "argCULong"  "ffi_type_ulong"   8 8)
  , ("int64_t",            PrimitiveTypeInfo "CLong"   "retCLong"   "CLong"   "argCLong"   "ffi_type_slong"   8 8)
  ]

-- | Known ObjC typedef aliases that map to primitive types.
-- These are checked when the desugared form doesn't match.
typedefMap :: Map Text PrimitiveTypeInfo
typedefMap = Map.fromList
  [ ("NSInteger",      PrimitiveTypeInfo "CLong"   "retCLong"   "CLong"   "argCLong"   "ffi_type_slong"  8 8)
  , ("NSUInteger",     PrimitiveTypeInfo "CULong"  "retCULong"  "CULong"  "argCULong"  "ffi_type_ulong"  8 8)
  , ("CGFloat",        PrimitiveTypeInfo "CDouble" "retCDouble" "CDouble" "argCDouble" "ffi_type_double" 8 8)
  , ("NSTimeInterval", PrimitiveTypeInfo "CDouble" "retCDouble" "CDouble" "argCDouble" "ffi_type_double" 8 8)
  , ("unichar",        PrimitiveTypeInfo "CUShort" "retCUInt"   "CUInt"   "argCUInt"   "ffi_type_uint16" 2 2)
  , ("CFIndex",        PrimitiveTypeInfo "CLong"   "retCLong"   "CLong"   "argCLong"   "ffi_type_slong"  8 8)
  ]
