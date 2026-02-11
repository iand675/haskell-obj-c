{-# LANGUAGE OverloadedStrings #-}

-- | Parser for clang @qualType@ and @desugaredQualType@ strings.
--
-- These strings encode C/ObjC types in a compact textual form, e.g.:
--
-- @
-- \"NSArray\<NSString *\> * _Nullable\"
-- \"instancetype _Nonnull\"
-- \"void\"
-- \"NSInteger\"
-- @
--
-- This module parses them into the 'ObjCType' ADT from "ObjC.CodeGen.IR".
module ObjC.CodeGen.QualType
  ( parseQualType
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR (ObjCType(..), Nullability(..), TypeQualifier(..))

-- | Parse a clang @qualType@ string (and optionally a @desugaredQualType@)
-- into an 'ObjCType'.
--
-- The first argument is the @qualType@ string, the second is the optional
-- @desugaredQualType@ (used for resolving typedefs like @NSInteger@ → @long@).
parseQualType :: Text -> Maybe Text -> ObjCType
parseQualType qualType mDesugared = parse (T.strip qualType)
  where
    parse :: Text -> ObjCType
    parse t
      -- Preserve C type qualifiers as ObjCQualified wrappers.
      -- "const char *" becomes ObjCQualified QConst (ObjCPointer ...).
      -- We must also strip the qualifier from mDesugared so that the
      -- inner parse sees a matching desugared type (e.g. "char" not "const char").
      | Just rest <- T.stripPrefix "const " t
      = let innerDesugared = mDesugared >>= T.stripPrefix "const " >>= Just . T.strip
        in ObjCQualified QConst (parseQualType (T.strip rest) (innerDesugared <|> mDesugared))
      | Just rest <- T.stripPrefix "volatile " t
      = let innerDesugared = mDesugared >>= T.stripPrefix "volatile " >>= Just . T.strip
        in ObjCQualified QVolatile (parseQualType (T.strip rest) (innerDesugared <|> mDesugared))

      -- void
      | t == "void"
      = ObjCVoid

      -- BOOL / _Bool
      | t == "BOOL" || t == "_Bool"
      = ObjCBool

      -- instancetype (with optional nullability)
      | "instancetype" `T.isPrefixOf` t
      = ObjCInstancetype

      -- SEL
      | t == "SEL" || t == "SEL *"
      = ObjCSEL

      -- Class
      | t == "Class"
      = ObjCClassType Nothing

      -- id (with optional nullability and protocol)
      | t == "id" || t == "id _Nonnull" || t == "id _Nullable"
      = ObjCId Nothing (parseNullability t)

      -- Generic type: e.g., "NSArray<NSString *> * _Nullable"
      | Just (className, rest) <- parseGenericPrefix t
      = let (typeArgs, afterGeneric) = parseTypeArgs rest
            nullab = parseNullability afterGeneric
        in ObjCGeneric className typeArgs nullab

      -- Object pointer: e.g., "NSString * _Nonnull" or "NSString *"
      | Just (className, nullab) <- parseObjCPointer t
      = ObjCId (Just className) nullab

      -- Pointer to pointer or other pointer types: e.g., "NSError **",
      -- "char *", "char * _Nullable"
      | Just inner <- stripStarSuffix t
      = let innerDesugared = do
              d <- mDesugared
              stripStarSuffix d
        in ObjCPointer (parseQualType inner innerDesugared)

      -- Struct types: desugaredQualType starts with "struct ".
      -- Distinguish struct-by-value ("struct Foo") from pointer-to-struct
      -- ("struct Foo *"), the latter being opaque pointer typedefs like
      -- CFRunLoopRef, SecTrustRef, etc.
      | Just d <- mDesugared, "struct " `T.isPrefixOf` d
      = if "*" `T.isInfixOf` d
          then ObjCPointer ObjCVoid  -- pointer to (opaque) struct → Ptr ()
          else ObjCStruct (stripNullabilitySuffix t)
      -- qualType itself might be "struct Foo" (when typedef isn't used)
      | "struct " `T.isPrefixOf` t
      = if "*" `T.isInfixOf` t
          then ObjCPointer ObjCVoid
          else ObjCStruct (T.strip (T.drop 7 t))

      -- Known primitive typedefs
      | t `elem` ["NSInteger", "NSUInteger", "CGFloat", "NSTimeInterval",
                   "unichar", "CFIndex", "CFTypeRef"]
      = ObjCPrimitive t (maybe t id mDesugared)

      -- Fall through to desugared type if available
      | Just desugared <- mDesugared
      , desugared /= qualType
      = parsePrimitive t desugared

      -- Unknown → treat as primitive
      | otherwise
      = parsePrimitive t (maybe t id mDesugared)

    parsePrimitive :: Text -> Text -> ObjCType
    parsePrimitive qt dqt = ObjCPrimitive qt dqt

    -- Parse nullability from the end of a type string
    parseNullability :: Text -> Nullability
    parseNullability t
      | "_Nonnull" `T.isSuffixOf` t  = Nonnull
      | "_Nullable" `T.isSuffixOf` t = Nullable
      | otherwise                     = Unspecified

    -- Try to parse "ClassName * [_Nonnull|_Nullable]"
    parseObjCPointer :: Text -> Maybe (Text, Nullability)
    parseObjCPointer t = do
      -- Strip trailing nullability
      let (stripped, nullab) = stripNullability t
      -- Strip trailing " *"
      rest <- T.stripSuffix "*" (T.stripEnd stripped)
      let className = T.strip rest
      -- Must look like an identifier (starts with uppercase or NS/CF/CL prefix)
      if T.null className || not (isClassNameChar (T.head className))
        then Nothing
        else Just (className, nullab)

    stripNullability :: Text -> (Text, Nullability)
    stripNullability t
      | Just rest <- T.stripSuffix "_Nonnull" t          = (T.stripEnd rest, Nonnull)
      | Just rest <- T.stripSuffix "_Nullable" t         = (T.stripEnd rest, Nullable)
      | Just rest <- T.stripSuffix "_Null_unspecified" t = (T.stripEnd rest, Unspecified)
      | otherwise = (t, Unspecified)

    isClassNameChar :: Char -> Bool
    isClassNameChar c = c >= 'A' && c <= 'Z'

    -- Strip trailing nullability annotations from a type string.
    stripNullabilitySuffix :: Text -> Text
    stripNullabilitySuffix s
      | Just rest <- T.stripSuffix " _Nonnull" s           = rest
      | Just rest <- T.stripSuffix " _Nullable" s          = rest
      | Just rest <- T.stripSuffix " _Null_unspecified" s   = rest
      | otherwise                                           = s

    -- Strip trailing "* [nullability]" suffix and return the inner type.
    -- Handles: "char *", "char * _Nullable", "NSError **", etc.
    stripStarSuffix :: Text -> Maybe Text
    stripStarSuffix s =
      let stripped = stripNullabilitySuffix s
      in case T.stripSuffix "*" (T.stripEnd stripped) of
           Just inner -> Just (T.strip inner)
           Nothing    -> Nothing

    -- Parse "ClassName<" prefix for generic types
    parseGenericPrefix :: Text -> Maybe (Text, Text)
    parseGenericPrefix t =
      let (before, after) = T.breakOn "<" t
          className = T.strip before
      in if T.null after || T.null className
         then Nothing
         else Just (className, after)

    -- Parse "<TypeArg1, TypeArg2, ...> * [nullability]" from generic suffix
    parseTypeArgs :: Text -> ([ObjCType], Text)
    parseTypeArgs t
      | Just rest <- T.stripPrefix "<" t
      = let (argsStr, afterClose) = breakMatchingAngle rest
            args = fmap (parse . T.strip) (splitTopLevel argsStr)
            remaining = T.strip afterClose
        in (args, remaining)
      | otherwise
      = ([], t)

    -- Break at the matching '>' for a '<' that was already consumed
    breakMatchingAngle :: Text -> (Text, Text)
    breakMatchingAngle = go 0 T.empty
      where
        go :: Int -> Text -> Text -> (Text, Text)
        go depth acc t
          | T.null t = (acc, T.empty)
          | T.head t == '<' = go (depth + 1) (acc <> "<") (T.tail t)
          | T.head t == '>' && depth == 0 = (acc, T.tail t)
          | T.head t == '>' = go (depth - 1) (acc <> ">") (T.tail t)
          | otherwise = go depth (acc <> T.singleton (T.head t)) (T.tail t)

    -- Split on commas at the top level (not inside angle brackets)
    splitTopLevel :: Text -> [Text]
    splitTopLevel = go 0 T.empty
      where
        go :: Int -> Text -> Text -> [Text]
        go depth acc t
          | T.null t = [acc]
          | T.head t == '<' = go (depth + 1) (acc <> "<") (T.tail t)
          | T.head t == '>' = go (depth - 1) (acc <> ">") (T.tail t)
          | T.head t == ',' && depth == 0 =
              acc : go 0 T.empty (T.tail t)
          | otherwise = go depth (acc <> T.singleton (T.head t)) (T.tail t)
