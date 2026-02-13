{-# LANGUAGE OverloadedStrings #-}

-- | Mapping from Objective-C types ('ObjCType') to Haskell type expressions.
--
-- This module translates the parsed ObjC type IR into Haskell type strings
-- suitable for code generation, handling:
--
-- * Nullability → @Maybe@
-- * @instancetype@ → type variable @a@
-- * Primitive typedefs → FFI types
-- * Object pointers → managed @Id@ wrappers
-- * @BOOL@ → @ObjCBool@
module ObjC.CodeGen.TypeMap
  ( HsType(..)
  , KnownTypes(..)
  , ktAll
  , mapType
  , mapReturnType
  , hsTypeToText
  , isTypeSupported
  , isPrimitiveSupported
  , extractClassName
  , isIdDesugared
  , isSELDesugared
  , isBlockDesugared
  , isObjPtrDesugared
  , isFuncPtrDesugared
  , isStructLikePrimitive
  , isKnownTypedef
  , typedefToHsType
  , lookupEnumByQualType
  , enumUnderlyingHsType
  ) where

import qualified Data.Char as Char
import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import ObjC.CodeGen.IR (ObjCType(..), Nullability(..), TypeQualifier(..),
                         EnumDef(..))

-- | A simple Haskell type expression for code generation.
data HsType
  = HsTyCon Text
    -- ^ A type constructor (e.g., @\"NSString\"@, @\"Word\"@, @\"Id\"@).
  | HsTyApp HsType HsType
    -- ^ Type application (e.g., @Maybe NSString@).
  | HsTyVar Text
    -- ^ A type variable (e.g., @\"a\"@ for instancetype).
  | HsTyIO HsType
    -- ^ @IO a@.
  | HsTyUnit
    -- ^ @()@.
  | HsTyFunPtr [HsType] HsType
    -- ^ @FunPtr (a -> b -> IO c)@.
  | HsTyPtr HsType
    -- ^ @Ptr a@.
  deriving (Eq, Show)

-- | Render a 'HsType' to Haskell source text.
hsTypeToText :: HsType -> Text
hsTypeToText (HsTyCon name)      = name
hsTypeToText (HsTyApp f x)       = hsTypeToText f <> " " <> parensIfCompound x
hsTypeToText (HsTyVar name)      = name
hsTypeToText (HsTyIO inner)      = "IO " <> parensIfCompound inner
hsTypeToText HsTyUnit            = "()"
hsTypeToText (HsTyFunPtr _ _)    = "FunPtr a"  -- simplified
hsTypeToText (HsTyPtr inner)     = "Ptr " <> parensIfCompound inner

-- | Wrap a rendered type in parens if it contains spaces (i.e., is a
-- multi-token type like @Ptr ()@ or @IO Int@).  Simple names and unit
-- are left alone.
parensIfCompound :: HsType -> Text
parensIfCompound t = case t of
  HsTyCon _  -> hsTypeToText t
  HsTyVar _  -> hsTypeToText t
  HsTyUnit   -> hsTypeToText t
  _          -> "(" <> hsTypeToText t <> ")"

-- | Known generated types, split into class names, struct typedef names,
-- and enum definitions.
-- Passed in from the hierarchy so we know which names to treat as typed.
data KnownTypes = KnownTypes
  { ktClasses            :: Set Text           -- ^ Generated ObjC class names (e.g., @NSString@, @NSView@)
  , ktStructs            :: Set Text           -- ^ Generated struct typedef names (e.g., @NSRect@, @CGPoint@)
  , ktEnums              :: Map Text EnumDef   -- ^ Enum type name → enum definition
  , ktAvailableFrameworks :: Set Text          -- ^ Frameworks that will have generated packages
  }

-- | The union of class, struct, and enum names.
ktAll :: KnownTypes -> Set Text
ktAll kt = Set.union (ktClasses kt) (Set.union (ktStructs kt) (Map.keysSet (ktEnums kt)))

-- | Map an ObjC type to a Haskell type for use as a parameter type.
mapType :: KnownTypes -> ObjCType -> HsType
mapType kt = go
  where
    -- Check if an enum's framework is available for code generation.
    isEnumAvailable :: EnumDef -> Bool
    isEnumAvailable ed = case enumFramework ed of
      Nothing -> True
      Just fw
        | Set.null (ktAvailableFrameworks kt) -> True
        | otherwise -> Set.member fw (ktAvailableFrameworks kt)

    go ObjCVoid = HsTyUnit

    go ObjCBool = HsTyCon "Bool"

    go ObjCSEL = HsTyCon "Sel"

    go ObjCInstancetype = HsTyApp (HsTyCon "Id") (HsTyVar "a")

    go (ObjCClassType _) = HsTyCon "Class"

    go (ObjCId Nothing nullab) = wrapNullab nullab (HsTyCon "RawId")

    go (ObjCId (Just name) nullab)
      -- Fixed non-class typedefs (BOOL, NSInteger, NSZone, etc.) that appear
      -- as ObjCId due to parser mis-classification.  ObjCId implies pointer
      -- context, so wrap value types in Ptr.
      | isKnownTypedef name || isKnownTypedef baseName =
          let tdName = if isKnownTypedef name then name else baseName
              inner  = typedefToHsType tdName
          in case inner of
               HsTyPtr _ -> inner          -- already a pointer type (e.g. NSZone → Ptr ())
               _         -> HsTyPtr inner  -- value type in pointer context → add Ptr
      -- Generated struct typedef appearing in ObjCId context → struct pointer
      | Set.member name (ktStructs kt) = HsTyPtr (HsTyCon name)
      | Set.member baseName (ktStructs kt) = HsTyPtr (HsTyCon baseName)
      -- Enum typedef name appearing in pointer/id context → pointer to enum
      -- (only if the enum's framework is available)
      | Just ed <- Map.lookup name (ktEnums kt), isEnumAvailable ed
        = HsTyPtr (HsTyCon (enumName ed))
      | Just ed <- Map.lookup baseName (ktEnums kt), isEnumAvailable ed
        = HsTyPtr (HsTyCon (enumName ed))
      -- Generated class name → managed Id wrapper
      | Set.member name (ktClasses kt) = wrapNullab nullab (HsTyApp (HsTyCon "Id") (HsTyCon name))
      | Set.member baseName (ktClasses kt) = wrapNullab nullab (HsTyApp (HsTyCon "Id") (HsTyCon baseName))
      -- Generic type parameter names — treat as RawId
      -- Single uppercase letters (K, V, T, etc.) are also type parameters.
      | baseName == "ObjectType" || baseName == "KeyType" || baseName == "ValueType"
        || (T.length baseName == 1 && T.all Char.isUpper baseName)
        = wrapNullab nullab (HsTyCon "RawId")
      -- "Protocol" is a runtime type — treat as opaque RawId
      | baseName == "Protocol" = wrapNullab nullab (HsTyCon "RawId")
      -- "void" appearing as a class name (from NS_RETURNS_INNER_POINTER)
      | baseName == "void" = HsTyPtr HsTyUnit
      -- "const char" etc.
      | baseName == "const char" || baseName == "char" = HsTyPtr (HsTyCon "CChar")
      -- Fallback: unknown type in pointer/id context → opaque RawId.
      -- This can happen when an enum's framework isn't available or a type
      -- wasn't captured by the AST parser.
      | otherwise = wrapNullab nullab (HsTyCon "RawId")
      where baseName = extractClassName name

    go (ObjCGeneric name _ nullab)
      | Set.member name (ktClasses kt) = wrapNullab nullab (HsTyApp (HsTyCon "Id") (HsTyCon name))
      | Set.member baseName (ktClasses kt) = wrapNullab nullab (HsTyApp (HsTyCon "Id") (HsTyCon baseName))
      | Set.member name (ktStructs kt) = wrapNullab nullab (HsTyCon name)
      | Set.member baseName (ktStructs kt) = wrapNullab nullab (HsTyCon baseName)
      -- Enum typedef in generic/pointer context → pointer to enum
      | Just ed <- Map.lookup name (ktEnums kt), isEnumAvailable ed
        = HsTyPtr (HsTyCon (enumName ed))
      | Just ed <- Map.lookup baseName (ktEnums kt), isEnumAvailable ed
        = HsTyPtr (HsTyCon (enumName ed))
      -- "id<Proto>" parses as ObjCGeneric "id" [...] — treat as RawId
      | name == "id" || baseName == "id" = wrapNullab nullab (HsTyCon "RawId")
      -- "Class<Proto>" → Class
      | name == "Class" || baseName == "Class" = HsTyCon "Class"
      -- Fallback: unknown generic type → opaque RawId
      | otherwise = HsTyCon "RawId"
      where baseName = extractClassName name

    -- Handle ObjCPointer wrapping a mis-parsed ObjCPrimitive for id
    go (ObjCPointer inner@(ObjCPrimitive _ d))
      | isIdDesugared d = HsTyPtr (HsTyCon "RawId")
      -- __kindof X * → object pointer, resolve to class type or RawId
      | "__kindof " `T.isPrefixOf` d =
          let clsName = extractClassName (T.drop 9 d)
          in if Set.member clsName (ktClasses kt) then HsTyApp (HsTyCon "Id") (HsTyCon clsName) else HsTyCon "RawId"
      | otherwise       = HsTyPtr (go inner)
    go (ObjCPointer inner) = HsTyPtr (go inner)

    go (ObjCBlock retTy paramTys) =
      HsTyFunPtr (fmap go paramTys) (go retTy)

    go (ObjCStruct name) = HsTyCon name

    go (ObjCPrimitive qualType desugared) = mapPrimitive kt qualType desugared

    go (ObjCQualified QConst inner) = HsTyApp (HsTyCon "Const") (go inner)
    go (ObjCQualified QVolatile inner) = HsTyApp (HsTyCon "Volatile") (go inner)

-- | Map an ObjC return type. Same as 'mapType' but wraps in IO.
mapReturnType :: KnownTypes -> ObjCType -> HsType
mapReturnType kt ty = HsTyIO (mapType kt ty)

-- | Wrap a type in @Maybe@ according to nullability.
wrapNullab :: Nullability -> HsType -> HsType
wrapNullab Nonnull     ty = ty
wrapNullab Nullable    ty = HsTyApp (HsTyCon "Maybe") ty
wrapNullab Unspecified ty = HsTyApp (HsTyCon "Maybe") ty
  -- Conservative: unspecified nullability treated as nullable

-- | Map a C primitive type (by its desugared form) to a Haskell type.
mapPrimitive :: KnownTypes -> Text -> Text -> HsType
mapPrimitive kt qualType desugared
  -- __kindof annotation: strip and treat as the underlying class name
  | "__kindof " `T.isPrefixOf` desugared =
      let clsName = extractClassName (T.drop 9 desugared)
      in if Set.member clsName (ktClasses kt)
           then HsTyApp (HsTyCon "Id") (HsTyCon clsName)
           else HsTyCon "RawId"

  -- Enum types: desugared as "enum Xxx" — use generated newtype if known
  | "enum " `T.isPrefixOf` desugared
  , Just ed <- lookupEnumByQualType kt qualType desugared
    = HsTyCon (enumName ed)
  -- Enum type not in our generated set — fall back to CInt
  | "enum " `T.isPrefixOf` desugared   = HsTyCon "CInt"

  -- Known enum typedef where desugared is the underlying int type
  -- (e.g., qualType="NSWindowStyleMask", desugared="unsigned long").
  -- Must come before the integer type checks below.
  | Just ed <- lookupEnumByQualType kt qualType desugared
    = HsTyCon (enumName ed)

  -- Integer types
  | desugared == "long"                = HsTyCon "CLong"
  | desugared == "unsigned long"       = HsTyCon "CULong"
  | desugared == "int"                 = HsTyCon "CInt"
  | desugared == "unsigned int"        = HsTyCon "CUInt"
  | desugared == "long long"           = HsTyCon "CLong"
  | desugared == "unsigned long long"  = HsTyCon "CULong"
  | desugared == "short"               = HsTyCon "CShort"
  | desugared == "unsigned short"      = HsTyCon "CUShort"
  | desugared == "char"                = HsTyCon "CChar"
  | desugared == "unsigned char"       = HsTyCon "CUChar"
  | desugared == "signed char"         = HsTyCon "CSChar"

  -- Floating point
  | desugared == "double"              = HsTyCon "CDouble"
  | desugared == "float"               = HsTyCon "CFloat"

  -- Known typedef names that map to specific Haskell types
  | qualType == "NSInteger"            = HsTyCon "CLong"
  | qualType == "NSUInteger"           = HsTyCon "CULong"
  | qualType == "CGFloat"              = HsTyCon "CDouble"
  | qualType == "NSTimeInterval"       = HsTyCon "CDouble"
  | qualType == "unichar"              = HsTyCon "CUShort"
  | qualType == "CFIndex"              = HsTyCon "CLong"

  -- Fixed-width integer types
  | desugared == "uint8_t" || qualType == "uint8_t"    = HsTyCon "CUChar"
  | desugared == "int8_t"  || qualType == "int8_t"     = HsTyCon "CSChar"
  | desugared == "uint16_t" || qualType == "uint16_t"  = HsTyCon "CUShort"
  | desugared == "int16_t" || qualType == "int16_t"    = HsTyCon "CShort"
  | desugared == "uint32_t" || qualType == "uint32_t"  = HsTyCon "CUInt"
  | desugared == "int32_t" || qualType == "int32_t"    = HsTyCon "CInt"
  | desugared == "uint64_t" || qualType == "uint64_t"  = HsTyCon "CULong"
  | desugared == "int64_t" || qualType == "int64_t"    = HsTyCon "CLong"

  -- Struct types mis-parsed as primitives (qualType == desugared, no space/brackets)
  -- e.g., "NSDecimal" / "NSDecimal", "NSAffineTransformStruct" etc.
  | qualType == desugared && isStructLikePrimitive desugared
                                       = HsTyCon desugared

  -- Function pointer types (IMP, comparators, etc.) → opaque Ptr ()
  | isFuncPtrDesugared desugared       = HsTyPtr HsTyUnit

  -- Mis-parsed 'id' (generic type parameters like ObjectType)
  | isIdDesugared desugared            = HsTyCon "RawId"

  -- Mis-parsed SEL (with nullability annotation)
  | isSELDesugared desugared           = HsTyCon "Sel"

  -- Mis-parsed Class
  | desugared == "Class"               = HsTyCon "Class"

  -- Block types: desugared as "void (^)(...)" or similar
  | isBlockDesugared desugared          = HsTyPtr (HsTyCon "()")

  -- Typedef to struct pointer: desugared ends with " *" and pointed-to
  -- name is a generated struct typedef (e.g. NSRangePointer → NSRange *)
  | isObjPtrDesugared desugared
  , let pointee = T.strip (T.dropEnd 1 desugared)
  , Set.member pointee (ktStructs kt)
                                       = HsTyPtr (HsTyCon pointee)

  -- Typedef to object pointer: desugared ends with " *"
  -- e.g., qualType="NSRunLoopMode _Nonnull", desugared="NSString *"
  | isObjPtrDesugared desugared =
      let clsName = T.strip (T.dropEnd 1 desugared)
          baseCls = extractClassName clsName
      in if Set.member baseCls (ktClasses kt)
         then HsTyApp (HsTyCon "Id") (HsTyCon baseCls)
         else if Set.member clsName (ktAll kt)
         then HsTyCon clsName
         else HsTyCon "RawId"

  -- Fallback: unsupported primitive → opaque Ptr () (void pointer).
  -- This avoids crashing when encountering novel C types from large
  -- multi-framework imports.
  | otherwise = HsTyPtr HsTyUnit

-- | Check whether all types in an ObjCType tree are supported by 'mapType',
-- 'mkRetExprs', and 'mkArgExpr'.  Returns 'True' when code generation can
-- handle the type; 'False' when the method should be skipped.
isTypeSupported :: KnownTypes -> ObjCType -> Bool
isTypeSupported kt = go
  where
    known = ktAll kt
    go ObjCVoid              = True
    go ObjCBool              = True
    go ObjCSEL               = True
    go ObjCInstancetype      = True
    go (ObjCClassType _)     = True
    go (ObjCId _ _)          = True
    -- ObjCId is always supported: known class names map to @Id ClassName@,
    -- and everything else (protocols, type params, unknown names) maps to
    -- @RawId@ — see the fallback in 'mapType'.
    go (ObjCGeneric n _ _)   = Set.member n known || Set.member bn known
                              || n == "id" || bn == "id"
                              || n == "Class" || bn == "Class"
      where bn = extractClassName n
    go (ObjCPointer inner)   = go inner
    go (ObjCBlock ret params)= go ret && all go params
    go (ObjCStruct n)        = Set.member n known
    go (ObjCQualified _ inner) = go inner
    go (ObjCPrimitive q d)
      -- Struct-like primitives must have generated struct definitions
      | q == d && isStructLikePrimitive d = Set.member d known
      | otherwise = isPrimitiveSupported q d

-- | Check whether a C primitive type (qualType, desugaredType) is one we
-- know how to map to Haskell.
isPrimitiveSupported :: Text -> Text -> Bool
isPrimitiveSupported q d
  -- Desugared forms we handle
  | d == "long"               = True
  | d == "unsigned long"      = True
  | d == "int"                = True
  | d == "unsigned int"       = True
  | d == "long long"          = True
  | d == "unsigned long long" = True
  | d == "short"              = True
  | d == "unsigned short"     = True
  | d == "char"               = True
  | d == "unsigned char"      = True
  | d == "signed char"        = True
  | d == "double"             = True
  | d == "float"              = True
  -- Known typedef qualTypes
  | q == "NSInteger"          = True
  | q == "NSUInteger"         = True
  | q == "CGFloat"            = True
  | q == "NSTimeInterval"     = True
  | q == "unichar"            = True
  | q == "CFIndex"            = True
  -- Fixed-width types
  | d == "uint8_t" || q == "uint8_t"   = True
  | d == "int8_t"  || q == "int8_t"    = True
  | d == "uint16_t" || q == "uint16_t" = True
  | d == "int16_t" || q == "int16_t"   = True
  | d == "uint32_t" || q == "uint32_t" = True
  | d == "int32_t" || q == "int32_t"   = True
  | d == "uint64_t" || q == "uint64_t" = True
  | d == "int64_t" || q == "int64_t"   = True
  -- __kindof annotation
  | "__kindof " `T.isPrefixOf` d       = True
  -- Struct types mis-parsed as primitives
  | q == d && isStructLikePrimitive d = True
  -- Function pointers
  | isFuncPtrDesugared d       = True
  -- Mis-parsed id (generic type params, e.g., "ObjectType _Nonnull" / "id")
  | isIdDesugared d            = True
  -- Mis-parsed SEL with nullability
  | isSELDesugared d           = True
  -- Mis-parsed Class
  | d == "Class"               = True
  -- Enum types
  | "enum " `T.isPrefixOf` d  = True
  -- Block types
  | isBlockDesugared d         = True
  -- Object pointer typedefs (desugared ends with " *")
  | isObjPtrDesugared d        = True
  | otherwise                  = False

-- ---------------------------------------------------------------------------
-- Helpers for recognising mis-parsed primitive types
-- ---------------------------------------------------------------------------

-- | Extract a clean ObjC class name from a possibly-mangled qualType.
-- E.g., @\"NSError * _Nullable\"@ → @\"NSError\"@,
--        @\"API_AVAILABLE NSURL\"@ → @\"NSURL\"@.
extractClassName :: Text -> Text
extractClassName name = cleanLoop (T.strip name)
  where
    -- Iteratively strip annotations, macros, const, pointers until stable.
    cleanLoop :: Text -> Text
    cleanLoop t =
      let t1 = stripMacros t
          t2 = stripAnnotations t1
          t3 = T.strip $ mb t2 (T.stripSuffix "const" t2)
          t4 = if " *" `T.isSuffixOf` t3
               then T.strip (T.dropEnd 2 t3)
               else t3
      in if t4 == t then t else cleanLoop t4

    mb def_ Nothing  = def_
    mb _    (Just x) = x

    -- Strip trailing nullability annotations and __kindof prefix.
    stripAnnotations :: Text -> Text
    stripAnnotations t =
      let t1 = T.strip $ foldl (\n a -> mb n (T.stripSuffix a n)) t
                 ["_Nullable", "_Nonnull", "_Null_unspecified"]
          t2 = T.strip $ mb t1 (T.stripPrefix "__kindof " t1)
          -- Strip protocol annotations: "NSObject<OS_xpc_object>" → "NSObject"
          t3 = case T.breakOn "<" t2 of
                 (before, after) | not (T.null after) && ">" `T.isSuffixOf` after
                                   -> T.strip before
                 _ -> t2
      in t3

    -- Strip known leading attribute macros from the name.
    stripMacros :: Text -> Text
    stripMacros t =
      let stripOne s = foldl (\acc m -> mb acc (fmap T.strip (T.stripPrefix (m <> " ") acc))) s macros
          -- Also handle macros with parenthesised args: "API_AVAILABLE(...) NSFoo"
          stripParens s = case T.breakOn ")" s of
            (_, rest) | not (T.null rest) && hasLeadingMacro s -> T.strip (T.drop 1 rest)
            _ -> s
          hasLeadingMacro s = any (\m -> T.isPrefixOf m s) macros
          result = stripParens (stripOne t)
      in if result == t then t else stripMacros result  -- iterate until stable

    macros :: [Text]
    macros =
      [ "API_AVAILABLE", "API_DEPRECATED_WITH_REPLACEMENT"
      , "API_DEPRECATED", "API_UNAVAILABLE"
      , "NS_REFINED_FOR_SWIFT", "NS_RETURNS_INNER_POINTER"
      , "NS_RETURNS_NOT_RETAINED", "NS_RETURNS_RETAINED"
      , "NS_FORMAT_FUNCTION", "NS_FORMAT_ARGUMENT"
      , "NS_SWIFT_UNAVAILABLE_FROM_ASYNC", "NS_SWIFT_UNAVAILABLE"
      , "NS_SWIFT_NAME"
      , "CF_RETURNS_RETAINED", "CF_RETURNS_NOT_RETAINED"
      ]

-- | Check whether a name is a known *fixed* typedef (not a class, not a
-- generated struct, not a generated enum).  Struct typedefs are handled
-- dynamically via 'ktStructs'; enum typedefs via 'ktEnums'.
-- This covers primitive-numeric typedefs and opaque pointer typedefs.
isKnownTypedef :: Text -> Bool
isKnownTypedef n = n `Set.member` typedefNames
  where
    typedefNames = Set.fromList
      [ "NSInteger", "NSUInteger", "NSStringEncoding", "NSZone"
      , "BOOL", "NSTimeInterval", "CGFloat", "CFIndex"
      -- Opaque struct typedefs (no generated Storable instance)
      , "NSFastEnumerationState"
      ]

-- | Map a known *fixed* typedef name to its Haskell type.
-- Only covers the names in 'isKnownTypedef'.  Generated struct typedefs
-- are handled inline (struct name → @HsTyCon name@).
typedefToHsType :: Text -> HsType
typedefToHsType "NSInteger"         = HsTyCon "CLong"
typedefToHsType "NSUInteger"        = HsTyCon "CULong"
typedefToHsType "NSStringEncoding"  = HsTyCon "CULong"
typedefToHsType "BOOL"              = HsTyCon "Bool"
typedefToHsType "NSTimeInterval"    = HsTyCon "CDouble"
typedefToHsType "CGFloat"           = HsTyCon "CDouble"
typedefToHsType "CFIndex"           = HsTyCon "CLong"
typedefToHsType "NSZone"            = HsTyPtr HsTyUnit  -- opaque struct
-- Opaque struct typedef
typedefToHsType "NSFastEnumerationState" = HsTyPtr HsTyUnit
typedefToHsType n                   = error $ "typedefToHsType: unhandled " <> T.unpack n

-- | Desugared form represents @id@ (bare or with nullability).
isIdDesugared :: Text -> Bool
isIdDesugared d = d == "id" || T.isPrefixOf "id " d

-- | Desugared form represents @SEL@.
isSELDesugared :: Text -> Bool
isSELDesugared d = d == "SEL *" || d == "SEL"

-- | Desugared form is a block type @void (^)(...)@.
isBlockDesugared :: Text -> Bool
isBlockDesugared d = "(^)" `T.isInfixOf` d

-- | Desugared form is a function pointer type @... (*)(...)@.
isFuncPtrDesugared :: Text -> Bool
isFuncPtrDesugared d = "(*)" `T.isInfixOf` d

-- | A primitive that looks like it should be a struct type:
-- qualType == desugared, no spaces, no brackets, starts with uppercase.
isStructLikePrimitive :: Text -> Bool
isStructLikePrimitive d = not (T.any (== ' ') d)
  && not (T.any (== '[') d) && not (T.any (== '(') d)
  && T.any isUpper d

-- | Desugared form is an object pointer typedef (ends with @\" *\"@).
isObjPtrDesugared :: Text -> Bool
isObjPtrDesugared d = " *" `T.isSuffixOf` T.strip d && not (isSELDesugared d)

-- ---------------------------------------------------------------------------
-- Enum type helpers
-- ---------------------------------------------------------------------------

-- | Look up an enum definition from a primitive's qualType/desugared pair.
--
-- Handles two patterns:
--
-- 1. @desugared = \"enum NSWindowStyleMask\"@ → strip prefix, look up
-- 2. @qualType = \"NSWindowStyleMask\"@ → direct lookup in 'ktEnums'
--
-- Only returns enums whose framework is in 'ktAvailableFrameworks'.
-- Enums from unavailable frameworks are silently dropped so the caller
-- falls through to the generic integer type mapping.
lookupEnumByQualType :: KnownTypes -> Text -> Text -> Maybe EnumDef
lookupEnumByQualType kt qualType desugared =
  -- Pattern 1: desugared starts with "enum "
  let fromDesugared = do
        eName <- T.stripPrefix "enum " desugared
        Map.lookup (T.strip eName) (ktEnums kt)
      -- Pattern 2: qualType is a known enum name
      fromQualType = Map.lookup qualType (ktEnums kt)
      result = fromDesugared <|> fromQualType
  in result >>= filterAvailable
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> r = r
    l       <|> _ = l

    filterAvailable :: EnumDef -> Maybe EnumDef
    filterAvailable ed = case enumFramework ed of
      Nothing -> Just ed  -- no framework → always available
      Just fw
        | Set.null (ktAvailableFrameworks kt) -> Just ed  -- empty set → no filtering
        | Set.member fw (ktAvailableFrameworks kt) -> Just ed
        | otherwise -> Nothing  -- framework not available

-- | Map an enum's underlying desugared type to the corresponding Haskell
-- FFI type name (e.g., @\"unsigned long\"@ → @\"CULong\"@).
enumUnderlyingHsType :: EnumDef -> Text
enumUnderlyingHsType ed = case enumUnderlyingDesugared ed of
  "unsigned long"       -> "CULong"
  "long"                -> "CLong"
  "unsigned int"        -> "CUInt"
  "int"                 -> "CInt"
  "unsigned long long"  -> "CULong"
  "long long"           -> "CLong"
  "unsigned short"      -> "CUShort"
  "short"               -> "CShort"
  "unsigned char"       -> "CUChar"
  "signed char"         -> "CSChar"
  "char"                -> "CChar"
  -- Fall back based on qualType
  _ -> case enumUnderlyingQual ed of
    "NSUInteger"  -> "CULong"
    "NSInteger"   -> "CLong"
    _             -> "CInt"  -- conservative default
