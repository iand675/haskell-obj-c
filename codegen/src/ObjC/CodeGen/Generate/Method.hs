{-# LANGUAGE OverloadedStrings #-}

-- | Method body generation using type-safe message sending.
--
-- This module provides:
--
-- * 'generateMethod' — per-method wrapper function generation
-- * 'generateMethodBody' — body generation using
--   @sendMessage@\/@sendOwnedMessage@\/@sendClassMessage@\/@sendOwnedClassMessage@
-- * 'generateSelectors' — top-level @Selector@ binding generation
module ObjC.CodeGen.Generate.Method
  ( -- * Per-method generation
    generateMethod
  , generateMethods
  , generateSelectors
    -- * Unified method body
  , ReceiverKind(..)
  , MethodBodyConfig(..)
  , generateMethodBody
    -- * Selector type text (for protocol modules)
  , selectorTypeText
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR
import ObjC.CodeGen.TypeMap
import ObjC.CodeGen.Generate.Naming
import ObjC.CodeGen.Generate.Shared

-- ---------------------------------------------------------------------------
-- Method list generation
-- ---------------------------------------------------------------------------

-- | Generate all method bindings for a class.
generateMethods :: KnownTypes -> ClassHierarchy -> ObjCClass -> Set Text -> Set Text -> (ObjCMethod -> Bool) -> [Text]
generateMethods allKnown _hierarchy cls importable allKnownClasses originFilter =
  let methods = filter (\m -> not (methodIsImplicit m) && originFilter m
                              && isMethodSupported allKnown m)
                       (allClassMethods importable allKnownClasses cls)
      dedupedByName = dedupByHsName cls methods
      instNames = instanceMethodNameSet cls dedupedByName
  in concatMap (generateMethod allKnown cls instNames) dedupedByName

-- | Generate top-level @Selector@ bindings with typed signatures.
--
-- Each method gets its own binding, named via 'methodSelectorName'.
-- When a class method and instance method share the same ObjC selector
-- with different types, they receive separate bindings (the class
-- method's binding is prefixed with the lowercased class name).
--
-- @
-- initWithString_Selector :: Selector '[Id NSString] (Id NSFoo)
-- initWithString_Selector = mkSelector "initWithString:"
-- @
generateSelectors :: KnownTypes -> ObjCClass -> Set Text -> Set Text -> (ObjCMethod -> Bool) -> [Text]
generateSelectors allKnown cls importable allKnownClasses originFilter =
  let methods = filter (\m -> not (methodIsImplicit m) && originFilter m
                              && isMethodSupported allKnown m)
                       (allClassMethods importable allKnownClasses cls)
      dedupedByName = dedupByHsName cls methods
      instNames = instanceMethodNameSet cls dedupedByName
      -- Dedup by the generated Haskell selector binding name
      uniqueSels = dedup Set.empty dedupedByName
      dedup _ [] = []
      dedup seenHs (m:ms)
        | Set.member hs seenHs = dedup seenHs ms
        | otherwise            = m : dedup (Set.insert hs seenHs) ms
        where hs = methodSelectorName cls instNames m
  in concatMap (genSel allKnown cls instNames) uniqueSels

-- | Render a single typed selector binding.
genSel :: KnownTypes -> ObjCClass -> Set Text -> ObjCMethod -> [Text]
genSel known cls instNames method =
  let sel    = methodSelector method
      hsName = methodSelectorName cls instNames method
      selStr = T.pack (show (T.unpack sel))
      selTy  = selectorTypeText known (className cls) method
  in [ "-- | @Selector@ for @" <> sel <> "@"
     , hsName <> " :: " <> selTy
     , hsName <> " = mkSelector " <> selStr
     , ""
     ]

-- | Compute the Haskell type text for a typed @Selector@ binding.
--
-- Produces e.g. @Selector \'[Id NSString, Bool] (Id NSFoo)@.
-- The @IO@ is implied by the @Selector@ type and omitted.
selectorTypeText :: KnownTypes -> Text -> ObjCMethod -> Text
selectorTypeText known clsName method =
  let argTypes = fmap (mapArgType known . snd) (methodParams method)
      retType  = mapRetType known clsName (methodReturnType method)
      argList  = "'[" <> T.intercalate ", " argTypes <> "]"
  in "Selector " <> argList <> " " <> parensIfNeeded retType

-- | Map an ObjC parameter type to its Haskell representation for the
-- type-level selector signature.
mapArgType :: KnownTypes -> ObjCType -> Text
mapArgType known ty = hsTypeToText (mapType known (stripNullab ty))

-- | Map an ObjC return type to its Haskell representation for the
-- type-level selector signature.  @IO@ is omitted because it is
-- implied by the @Selector@ type.
mapRetType :: KnownTypes -> Text -> ObjCType -> Text
mapRetType known clsName ty = case ty of
  ObjCInstancetype -> "Id " <> clsName
  ObjCVoid         -> "()"
  _                -> hsTypeToText (mapType known (stripNullab ty))

-- | Wrap in parens if the text contains spaces.
parensIfNeeded :: Text -> Text
parensIfNeeded t
  | T.any (== ' ') t = "(" <> t <> ")"
  | otherwise         = t

-- ---------------------------------------------------------------------------
-- Per-method generation
-- ---------------------------------------------------------------------------

-- | Generate a single method binding (type sig + body).
--
-- Object parameters (@Id ClassName@) use polymorphic type variables
-- with @IsClassName@ constraints, and are converted via @toClassName@
-- in the body before being passed to @sendMessage@.  The receiver
-- also uses a type class constraint for subclass polymorphism.
-- Non-object parameters (primitives, pointers, etc.) remain concrete.
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

    -- Return type (always wrapped in IO)
    retHsType = HsTyIO (mapType known (stripNullab (methodReturnType method)))
    concreteInstancetypeRet = HsTyIO (HsTyApp (HsTyCon "Id") (HsTyCon name))

    finalRetHsType
      | isInstancetype (methodReturnType method) = concreteInstancetypeRet
      | otherwise = retHsType

    isInstancetype ObjCInstancetype = True
    isInstancetype _ = False

    -- Parameters with polymorphism classification
    params = dedupParamNames (methodParams method)

    -- (paramName, hsType, Just className) for Id ClassName params
    -- (paramName, hsType, Nothing) for everything else
    classifiedParams = fmap classifyParam params

    classifyParam (pName, pType) =
      let hsType = mapType known (stripNullab pType)
      in case hsType of
        HsTyApp (HsTyCon "Id") (HsTyCon cn) -> (pName, hsType, Just cn)
        _ -> (pName, hsType, Nothing)

    -- Constraints: self + polymorphic object args
    selfConstraint
      | methodIsClass method = []
      | otherwise = ["Is" <> name <> " " <> selfVar]

    paramConstraints =
      [ "Is" <> cn <> " " <> pn
      | (pn, _, Just cn) <- classifiedParams
      ]

    allConstraints = selfConstraint ++ paramConstraints

    constraintText
      | null allConstraints = ""
      | [c] <- allConstraints = c <> " => "
      | otherwise = "(" <> T.intercalate ", " allConstraints <> ") => "

    -- Type signature parts
    selfTypePart
      | methodIsClass method = []
      | otherwise = [selfVar]

    paramTypeParts = fmap paramTypeText classifiedParams

    paramTypeText (pn, hsType, mc) = case mc of
      Just _  -> pn               -- type variable
      Nothing -> hsTypeToText hsType  -- concrete type

    typeSig =
      let allParts = selfTypePart ++ paramTypeParts ++ [hsTypeToText finalRetHsType]
      in constraintText <> T.intercalate " -> " allParts

    argPatterns =
      let selfPat
            | methodIsClass method = ""
            | otherwise = selfVar
          paramPats = fmap (\(pn, _, _) -> pn) classifiedParams
      in T.unwords (filter (not . T.null) (selfPat : paramPats))

    -- Arg expressions: polymorphic params get toClassName conversion
    argExprs = fmap mkArgExpr classifiedParams

    mkArgExpr (pn, _, Just cn)  = "(to" <> cn <> " " <> pn <> ")"
    mkArgExpr (pn, _, Nothing) = pn

    -- Reference the top-level selector binding
    selName = methodSelectorName cls instNames method

    body = generateMethodBody
      MethodBodyConfig
        { mbcReceiverKind = if methodIsClass method
                            then ClassReceiver name
                            else InstanceReceiver selfVar
        , mbcBaseIndent   = if methodIsClass method then 2 else 0
        , mbcSelectorExpr = selName
        , mbcArgExprs     = argExprs
        , mbcIsOwned      = ownershipWrapper sel == "ownedObject"
        }

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
  { mbcReceiverKind   :: ReceiverKind
  , mbcBaseIndent     :: Int
  , mbcSelectorExpr   :: Text
    -- ^ Expression to use for the selector in the @sendMessage@ call.
    -- Typically a top-level selector name (e.g., @\"fooSelector\"@)
    -- or an inline @\"(mkSelector \\\"foo:\\\" :: Selector ... ...)\"@.
  , mbcArgExprs       :: [Text]
    -- ^ Argument expressions to pass after the selector.
    -- These should already include any conversions (e.g., @toNSString x@).
  , mbcIsOwned        :: Bool
    -- ^ Whether to use the @sendOwned*@ variant (for +1 ownership).
  }

-- | Generate the body lines for a method call.
--
-- Uses the type-safe @sendMessage@ family from @ObjC.Runtime.Message@.
generateMethodBody :: MethodBodyConfig -> [Text]
generateMethodBody cfg =
  let selExpr    = mbcSelectorExpr cfg
      argsPart   = if null (mbcArgExprs cfg) then "" else " " <> T.unwords (mbcArgExprs cfg)
      baseIndent = T.replicate (mbcBaseIndent cfg) " "
      isOwned    = mbcIsOwned cfg
  in case mbcReceiverKind cfg of
    ClassReceiver clsName ->
      let sendFn = if isOwned then "sendOwnedClassMessage" else "sendClassMessage"
      in [ baseIndent <> "do"
         , baseIndent <> "  cls' <- getRequiredClass " <> T.pack (show (T.unpack clsName))
         , baseIndent <> "  " <> sendFn <> " cls' " <> selExpr <> argsPart
         ]

    InstanceReceiver selfVar ->
      let sendFn = if isOwned then "sendOwnedMessage" else "sendMessage"
      in [ baseIndent <> "  " <> sendFn <> " " <> selfVar <> " " <> selExpr <> argsPart ]

    DirectReceiver recvExpr ->
      let sendFn = if isOwned then "sendOwnedMessage" else "sendMessage"
      in [ baseIndent <> "  " <> sendFn <> " " <> recvExpr <> " " <> selExpr <> argsPart ]
