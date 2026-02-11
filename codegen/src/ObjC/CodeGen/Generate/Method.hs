{-# LANGUAGE OverloadedStrings #-}

-- | Method body generation and FFI expression builders.
--
-- This module provides:
--
-- * Unified method body generation ('generateMethodBody') that
--   replaces the four near-identical generators in the old code.
-- * 'mkRetExprs' and 'mkArgExpr' for building libffi return\/argument
--   expressions.
-- * Top-level 'generateMethod' and 'generateSelectors' for per-class
--   module output.
module ObjC.CodeGen.Generate.Method
  ( -- * Per-method generation
    generateMethod
  , generateMethods
  , generateSelectors
    -- * Unified method body
  , ReceiverKind(..)
  , MethodBodyConfig(..)
  , generateMethodBody
    -- * FFI expression builders
  , mkRetExprs
  , mkArgExpr
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.PrimitiveTypes (lookupPrimitive, PrimitiveTypeInfo(..))
import ObjC.CodeGen.Generate.Naming
import ObjC.CodeGen.Generate.Shared
import ObjC.CodeGen.Generate.Enums (enumRetFunction, enumRetFunctionType, enumArgFunction)

-- ---------------------------------------------------------------------------
-- Method list generation
-- ---------------------------------------------------------------------------

-- | Generate all method bindings for a class.
generateMethods :: KnownTypes -> ClassHierarchy -> ObjCClass -> Set Text -> (ObjCMethod -> Bool) -> [Text]
generateMethods allKnown _hierarchy cls importable originFilter =
  let methods = filter (\m -> not (methodIsImplicit m) && originFilter m
                              && isMethodSupported allKnown m)
                       (allClassMethods importable cls)
      dedupedByName = dedupByHsName cls methods
      instNames = instanceMethodNameSet cls dedupedByName
  in concatMap (generateMethod allKnown cls instNames) dedupedByName

-- | Generate top-level @Selector@ bindings.
generateSelectors :: KnownTypes -> ObjCClass -> Set Text -> (ObjCMethod -> Bool) -> [Text]
generateSelectors allKnown cls importable originFilter =
  let methods = filter (\m -> not (methodIsImplicit m) && originFilter m
                              && isMethodSupported allKnown m)
                       (allClassMethods importable cls)
      dedupedByName = dedupByHsName cls methods
      uniqueSels = dedup Set.empty (fmap methodSelector dedupedByName)
      dedup _ [] = []
      dedup seen (s:ss)
        | Set.member s seen = dedup seen ss
        | otherwise         = s : dedup (Set.insert s seen) ss
  in concatMap genSel uniqueSels
  where
    genSel sel =
      let hsName = selectorHaskellName sel
          selStr = T.pack (show (T.unpack sel))
      in [ "-- | @Selector@ for @" <> sel <> "@"
         , hsName <> " :: Selector"
         , hsName <> " = mkSelector " <> selStr
         , ""
         ]

-- ---------------------------------------------------------------------------
-- Per-method generation
-- ---------------------------------------------------------------------------

-- | Generate a single method binding (type sig + body).
generateMethod :: KnownTypes -> ObjCClass -> Set Text -> ObjCMethod -> [Text]
generateMethod known cls instNames method =
  docLines
  ++ [ hsName <> " :: " <> typeSig
     , hsName <> " " <> argPatterns <> " ="
     ] ++ body ++ [""]
  where
    name = className cls
    hsName = methodHaskellName cls instNames method
    sel = methodSelector method
    selfVar = lowerFirst name

    selectorComment
      | methodIsClass method = "+ " <> sel
      | otherwise            = "- " <> sel

    docLines = case methodDoc method of
      Just doc ->
        formatHaddock doc
        ++ ["--", "-- ObjC selector: @" <> selectorComment <> "@"]
      Nothing ->
        ["-- | @" <> selectorComment <> "@"]

    retHsType = HsTyIO (mapType known (stripNullab (methodReturnType method)))
    concreteInstancetypeRet = HsTyIO (HsTyApp (HsTyCon "Id") (HsTyCon name))

    finalRetHsType
      | isInstancetype (methodReturnType method) = concreteInstancetypeRet
      | otherwise = retHsType

    isInstancetype ObjCInstancetype = True
    isInstancetype _ = False

    paramInfoTyped :: [(Text, ObjCType, HsType, Maybe Text)]
    paramInfoTyped = fmap classifyParamType (methodParams method)

    classifyParamType (pName, pType) =
      let sanitized = sanitizeParamName pName
          hsType = mapType known (stripNullab pType)
          polyClass = case hsType of
            HsTyApp (HsTyCon "Id") (HsTyCon cn) -> Just cn
            _ -> Nothing
      in (sanitized, pType, hsType, polyClass)

    selfConstraint
      | methodIsClass method = []
      | otherwise = ["Is" <> name <> " " <> selfVar]

    paramConstraints = mapMaybe (\(pn, _, _, mCls) ->
      case mCls of
        Just cn -> Just ("Is" <> cn <> " " <> pn)
        Nothing -> Nothing
      ) paramInfoTyped

    allConstraints = selfConstraint ++ paramConstraints

    constraintText
      | null allConstraints = ""
      | [c] <- allConstraints = c <> " => "
      | otherwise = "(" <> T.intercalate ", " allConstraints <> ") => "

    selfTypePart
      | methodIsClass method = []
      | otherwise = [selfVar]

    paramTypeParts = fmap (\(pn, _, hsType, mCls) ->
      case mCls of
        Just _  -> pn
        Nothing -> hsTypeToText hsType
      ) paramInfoTyped

    typeSig =
      let allParts = selfTypePart ++ paramTypeParts ++ [hsTypeToText finalRetHsType]
      in constraintText <> T.intercalate " -> " allParts

    argPatterns =
      let selfPat
            | methodIsClass method = ""
            | otherwise = selfVar <> " "
          paramPats = fmap (\(n, _, _, _) -> n) paramInfoTyped
      in T.unwords (filter (not . T.null) (selfPat : paramPats))

    body
      | methodIsClass method =
          generateMethodBody known cls
            MethodBodyConfig
              { mbcReceiverKind   = ClassReceiver (className cls)
              , mbcBaseIndent     = 2
              }
            method
      | otherwise =
          generateMethodBody known cls
            MethodBodyConfig
              { mbcReceiverKind   = InstanceReceiver (lowerFirst (className cls))
              , mbcBaseIndent     = 0
              }
            method

-- ---------------------------------------------------------------------------
-- Unified method body generation
-- ---------------------------------------------------------------------------

-- | How the receiver is obtained.
data ReceiverKind
  = InstanceReceiver Text    -- ^ Instance method; text is the self variable
  | ClassReceiver Text       -- ^ Class method; text is the ObjC class name
  | DirectReceiver Text      -- ^ Direct receiver expression (e.g., \"obj\")
  deriving (Eq, Show)

-- | Configuration for 'generateMethodBody'.
data MethodBodyConfig = MethodBodyConfig
  { mbcReceiverKind :: ReceiverKind
  , mbcBaseIndent   :: Int
  }

-- | Generate the body lines for a method call.
--
-- This unifies the previously duplicated logic from
-- @generateInstanceMethodBody@, @generateClassMethodBody@,
-- @protoMethodImpl@, and @generateOptionalMethod@.
generateMethodBody :: KnownTypes -> ObjCClass -> MethodBodyConfig -> ObjCMethod -> [Text]
generateMethodBody known _cls cfg method =
  let selector = methodSelector method
      selStr = T.pack (show (T.unpack selector))
      (retPrefix, retArg, retSuffix) = mkRetExprs known selector (methodReturnType method)

      -- Classify each parameter
      paramInfos = fmap classifyParam (methodParams method)
      classifyParam (pName, pType) =
        let sanitized = sanitizeParamName pName
            managed = isManagedObjCParam known pType
            rawVar = "raw_" <> sanitized
        in (sanitized, pType, managed, rawVar)

      managedParams = [(s, r) | (s, _, True, r) <- paramInfos]
      managedCount = length managedParams
      nestIndent = T.replicate managedCount "  "

      -- withObjCPtr nesting
      withObjCPtrLines =
        [T.replicate (i * 2) " " <> baseIndent <> "withObjCPtr " <> sName <> " $ \\" <> rawVar <> " ->"
        | (i, (sName, rawVar)) <- zip [0..] managedParams]

      -- Arg expressions
      mkArg (sName, pType, False, _) = mkArgExpr known pType sName
      mkArg (_, _, True, rawVar) = "argPtr (castPtr " <> rawVar <> " :: Ptr ())"
      argsExpr = "[" <> T.intercalate ", " (fmap mkArg paramInfos) <> "]"

      -- Send function (stret for struct returns)
      sendFn = case methodReturnType method of
        ObjCStruct _ -> case mbcReceiverKind cfg of
          ClassReceiver _ -> "sendClassMsgStret"
          _               -> "sendMsgStret"
        _ -> case mbcReceiverKind cfg of
          ClassReceiver _ -> "sendClassMsg"
          _               -> "sendMsg"

      prefixPart = if T.null retPrefix then "" else retPrefix <> " "
      suffixPart = if T.null retSuffix then "" else " " <> retSuffix

      baseIndent = T.replicate (mbcBaseIndent cfg) " "

  in case mbcReceiverKind cfg of
    ClassReceiver clsName ->
      [ baseIndent <> "do" ]
      ++ [baseIndent <> "  cls' <- getRequiredClass " <> T.pack (show (T.unpack clsName))]
      ++ fmap ("  " <>) withObjCPtrLines
      ++ [baseIndent <> "  " <> nestIndent <> prefixPart
           <> sendFn <> " cls' (mkSelector " <> selStr <> ") "
           <> retArg <> " " <> argsExpr <> suffixPart]

    InstanceReceiver selfVar ->
      withObjCPtrLines
      ++ [baseIndent <> "  " <> nestIndent <> prefixPart
           <> sendFn <> " " <> selfVar <> " (mkSelector " <> selStr <> ") "
           <> retArg <> " " <> argsExpr <> suffixPart]

    DirectReceiver recvExpr ->
      withObjCPtrLines
      ++ [baseIndent <> "  " <> nestIndent <> prefixPart
           <> sendFn <> " " <> recvExpr <> " (mkSelector " <> selStr <> ") "
           <> retArg <> " " <> argsExpr <> suffixPart]

-- ---------------------------------------------------------------------------
-- Return expression builder
-- ---------------------------------------------------------------------------

-- | Generate the return wrapper.
--
-- Returns @(retPrefix, retArg, retSuffix)@ where:
--
-- * @retPrefix@ is prepended to the sendMsg call
-- * @retArg@ is the libffi RetType
-- * @retSuffix@ is appended after the sendMsg call
mkRetExprs :: KnownTypes -> Text -> ObjCType -> (Text, Text, Text)
mkRetExprs kt selector retTy = case retTy of
  ObjCQualified qual inner ->
    let (pre, ra, suf) = mkRetExprs kt selector inner
        wrapper = case qual of QConst -> "Const"; QVolatile -> "Volatile"
    in if T.null suf
       then (if T.null pre then "fmap " <> wrapper <> " $" else "fmap " <> wrapper <> " $ " <> pre, ra, "")
       else ("", ra, suf <> " >>= \\x -> pure (" <> wrapper <> " x)")
  ObjCVoid        -> ("", "retVoid", "")
  ObjCInstancetype -> ("", "(retPtr retVoid)", managed)
  ObjCId (Just n) _
    | let bn = extractClassName n
    , isKnownTypedef n || isKnownTypedef bn
      || Set.member n structs || Set.member bn structs ->
        ("fmap castPtr $", "(retPtr retVoid)", "")
  ObjCId (Just n) _
    | let bn = extractClassName n
    , Set.member n classes || Set.member bn classes ->
        ("", "(retPtr retVoid)", managed)
  ObjCId _ _        -> ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  ObjCGeneric n _ _
    | n == "Class" || extractClassName n == "Class"
                      -> ("fmap (Class . castPtr) $", "(retPtr retVoid)", "")
    | Set.member n classes || Set.member (extractClassName n) classes
                      -> ("", "(retPtr retVoid)", managed)
    | otherwise       -> ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  ObjCSEL          -> ("fmap (Selector . castPtr) $", "(retPtr retVoid)", "")
  ObjCClassType _  -> ("fmap (Class . castPtr) $", "(retPtr retVoid)", "")
  ObjCStruct name  -> ("", "ret" <> name, "")
  ObjCBool         -> ("fmap ((/= 0) :: CULong -> Bool) $", "retCULong", "")
  ObjCBlock _ _    -> ("fmap castPtr $", "(retPtr retVoid)", "")
  ObjCPointer (ObjCPrimitive _ d)
    | "__kindof " `T.isPrefixOf` d
    , let clsName = extractClassName (T.drop 9 d)
    , Set.member clsName classes
                     -> ("", "(retPtr retVoid)", managed)
    | "__kindof " `T.isPrefixOf` d
                     -> ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  ObjCPointer _    -> ("fmap castPtr $", "(retPtr retVoid)", "")
  ObjCPrimitive q d -> mkPrimitiveRetExprs kt q d selector
  where
    structs = ktStructs kt
    classes = ktClasses kt
    managed = ">>= " <> ownershipWrapper selector <> " . castPtr"

-- | Handle primitive return types, using 'PrimitiveTypes' table where possible.
mkPrimitiveRetExprs :: KnownTypes -> Text -> Text -> Text -> (Text, Text, Text)
mkPrimitiveRetExprs kt q d selector
  -- Known enum types
  | Just ed <- lookupEnumByQualType kt q d
    = let retFn = enumRetFunction ed
          retTy = enumRetFunctionType ed
      in ("fmap (coerce :: " <> retTy <> " -> " <> enumName ed <> ") $", retFn, "")
  -- __kindof annotation
  | "__kindof " `T.isPrefixOf` d
  , let clsName = extractClassName (T.drop 9 d)
  , Set.member clsName (ktClasses kt)
    = ("", "(retPtr retVoid)", ">>= " <> ownershipWrapper selector <> " . castPtr")
  | "__kindof " `T.isPrefixOf` d
    = ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  -- Struct types mis-parsed as primitives
  | q == d && isStructLikePrimitive d
    = ("", "ret" <> d, "")
  -- Function pointers
  | isFuncPtrDesugared d  = ("fmap castPtr $", "(retPtr retVoid)", "")
  -- Mis-parsed id
  | isIdDesugared d       = ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  -- Struct pointer
  | isObjPtrDesugared d
  , let pointee = T.strip (T.dropEnd 1 d)
  , Set.member pointee (ktStructs kt)
    = ("fmap castPtr $", "(retPtr retVoid)", "")
  -- Object pointer → managed or raw
  | isObjPtrDesugared d
  , let pointee = extractClassName (T.strip (T.dropEnd 1 d))
  , Set.member pointee (ktClasses kt)
    = ("", "(retPtr retVoid)", ">>= " <> ownershipWrapper selector <> " . castPtr")
  | isObjPtrDesugared d
    = ("fmap (RawId . castPtr) $", "(retPtr retVoid)", "")
  -- Mis-parsed SEL
  | isSELDesugared d      = ("fmap (Selector . castPtr) $", "(retPtr retVoid)", "")
  -- Mis-parsed Class
  | d == "Class"          = ("fmap (Class . castPtr) $", "(retPtr retVoid)", "")
  -- Unknown enum
  | "enum " `T.isPrefixOf` d = ("", "retCInt", "")
  -- Block types
  | isBlockDesugared d    = ("fmap castPtr $", "(retPtr retVoid)", "")
  -- Delegate to PrimitiveTypes table
  | Just info <- lookupPrimitive q d
    = let retFn = ptRetFunction info
          retTy = ptRetFuncType info
          hsType = ptHsType info
      in if retTy == hsType
         then ("", retFn, "")
         else ("fmap fromIntegral $", retFn, "")
  -- Conservative fallback
  | otherwise = ("", "retCInt", "")

-- ---------------------------------------------------------------------------
-- Argument expression builder
-- ---------------------------------------------------------------------------

-- | Generate an argument expression for a parameter.
mkArgExpr :: KnownTypes -> ObjCType -> Text -> Text
mkArgExpr kt ty paramName =
  let structs = ktStructs kt
      classes = ktClasses kt
  in case ty of
  ObjCQualified qual inner ->
    let unwrap = case qual of QConst -> "unConst"; QVolatile -> "unVolatile"
    in mkArgExpr kt inner ("(" <> unwrap <> " " <> paramName <> ")")
  ObjCId (Just n) _
    | let bn = extractClassName n
    , isKnownTypedef n || isKnownTypedef bn
      || Set.member n structs || Set.member bn structs
      || Map.member n (ktEnums kt) || Map.member bn (ktEnums kt) ->
        "argPtr " <> paramName
  ObjCId (Just n2) _
    | Set.member n2 classes || Set.member (extractClassName n2) classes
      -> "argPtr (castPtr " <> paramName <> " :: Ptr ())"
    | otherwise
      -> "argPtr (castPtr (unRawId " <> paramName <> ") :: Ptr ())"
  ObjCId Nothing _    -> "argPtr (castPtr (unRawId " <> paramName <> ") :: Ptr ())"
  ObjCGeneric n _ _
    | n == "Class" || extractClassName n == "Class"
                      -> "argPtr (unClass " <> paramName <> ")"
    | Set.member n classes || Set.member (extractClassName n) classes
                      -> "argPtr (castPtr " <> paramName <> " :: Ptr ())"
    | otherwise       -> "argPtr (castPtr (unRawId " <> paramName <> ") :: Ptr ())"
  ObjCInstancetype  -> "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  ObjCSEL           -> "argPtr (unSelector " <> paramName <> ")"
  ObjCClassType _   -> "argPtr (unClass " <> paramName <> ")"
  ObjCBool          -> "argCULong (if " <> paramName <> " then 1 else 0)"
  ObjCStruct name   -> "arg" <> name <> " " <> paramName
  ObjCPointer (ObjCPrimitive _ d)
    | "__kindof " `T.isPrefixOf` d
                      -> "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  ObjCPointer _     -> "argPtr " <> paramName
  ObjCPrimitive q d -> mkPrimitiveArgExpr kt q d paramName
  ObjCBlock _ _ -> "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  ObjCVoid      -> "argPtr " <> paramName  -- shouldn't happen but don't crash

-- | Handle primitive argument types.
mkPrimitiveArgExpr :: KnownTypes -> Text -> Text -> Text -> Text
mkPrimitiveArgExpr kt q d paramName
  -- Known enum types
  | Just ed <- lookupEnumByQualType kt q d
    = let argFn = enumArgFunction ed
      in argFn <> " (coerce " <> paramName <> ")"
  -- __kindof
  | "__kindof " `T.isPrefixOf` d
    = "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  -- Struct types mis-parsed as primitives
  | q == d && isStructLikePrimitive d
    = "arg" <> d <> " " <> paramName
  -- Function pointers
  | isFuncPtrDesugared d  = "argPtr " <> paramName
  -- Mis-parsed id
  | isIdDesugared d       = "argPtr (castPtr (unRawId " <> paramName <> ") :: Ptr ())"
  -- Struct pointer
  | isObjPtrDesugared d
  , let pointee = T.strip (T.dropEnd 1 d)
  , Set.member pointee (ktStructs kt)
    = "argPtr " <> paramName
  -- Object pointer → managed or raw
  | isObjPtrDesugared d
  , let clsName = extractClassName (T.strip (T.dropEnd 1 d))
  , Set.member clsName (ktClasses kt)
    = "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  | isObjPtrDesugared d
    = "argPtr (castPtr (unRawId " <> paramName <> ") :: Ptr ())"
  -- Mis-parsed SEL
  | isSELDesugared d      = "argPtr (unSelector " <> paramName <> ")"
  -- Mis-parsed Class
  | d == "Class"          = "argPtr (unClass " <> paramName <> ")"
  -- Unknown enum
  | "enum " `T.isPrefixOf` d = "argCInt (fromIntegral " <> paramName <> ")"
  -- Block types
  | isBlockDesugared d    = "argPtr (castPtr " <> paramName <> " :: Ptr ())"
  -- Delegate to PrimitiveTypes table
  | Just info <- lookupPrimitive q d
    = ptArgFunction info <> " (fromIntegral " <> paramName <> ")"
  -- Conservative fallback
  | otherwise = "argCInt (fromIntegral " <> paramName <> ")"
