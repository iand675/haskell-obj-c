{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser for clang's @-ast-dump=json@ output.
--
-- Extracts Objective-C interface declarations, categories, protocols,
-- methods, and properties from the JSON AST into the IR types defined
-- in "ObjC.CodeGen.IR".
module ObjC.CodeGen.ClangAST
  ( parseClangAST
  , ParsedAST(..)
  , frameworkFromPath
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Char as Char
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import ObjC.CodeGen.IR
import ObjC.CodeGen.QualType (parseQualType)

-- | The result of parsing a clang AST JSON dump.
data ParsedAST = ParsedAST
  { parsedInterfaces :: [ObjCInterface]
  , parsedCategories :: [ObjCCategory]
  , parsedProtocols  :: [ObjCProtocolDecl]
  , parsedStructDefs :: [StructDef]
    -- ^ Struct definitions (each typedef that aliases a C struct, with fields).
  , parsedTypedefs   :: Map Text Text
    -- ^ Typedef name -> underlying struct C name (e.g., "NSRange" -> "_NSRange").
  , parsedEnumDefs   :: [EnumDef]
    -- ^ Enum definitions (each named enum with constants).
  } deriving (Show)

-- | Parse the JSON AST from clang's @-ast-dump=json@ output.
parseClangAST :: BSL.ByteString -> Either String ParsedAST
parseClangAST bs = case Aeson.eitherDecode bs of
  Left err  -> Left ("Failed to parse JSON: " <> err)
  Right val -> Right (extractDecls val)

-- ---------------------------------------------------------------------------
-- Top-level extraction
-- ---------------------------------------------------------------------------

extractDecls :: Value -> ParsedAST
extractDecls root = ParsedAST
  { parsedInterfaces = mapMaybe extractIfaceWithFw annotated
  , parsedCategories = mapMaybe extractCatWithFw annotated
  , parsedProtocols  = mapMaybe extractProtoWithFw annotated
  , parsedStructDefs = structDefs
  , parsedTypedefs   = typedefMap
  , parsedEnumDefs   = enumDefs
  }
  where
    topLevel = getInner root
    -- Annotate each top-level node with its resolved framework.
    -- Clang omits loc.file when it hasn't changed since the previous
    -- declaration, so we track the "current file" across the list.
    annotated = resolveFrameworks topLevel

    extractIfaceWithFw (fw, val) =
      fmap (\i -> i { ifaceFramework  = fw
                     , ifaceMethods    = stampMethods fw (ifaceMethods i)
                     , ifaceProperties = stampProps fw (ifaceProperties i)
                     }) (extractInterface val)
    extractCatWithFw (fw, val) =
      fmap (\c -> c { catFramework   = fw
                     , catMethods     = stampMethods fw (catMethods c)
                     , catProperties  = stampProps fw (catProperties c)
                     }) (extractCategory val)
    extractProtoWithFw (fw, val) =
      fmap (\p -> p { protoDeclFramework = fw
                     , protoDeclRequired  = stampMethods fw (protoDeclRequired p)
                     , protoDeclOptional  = stampMethods fw (protoDeclOptional p)
                     , protoDeclReqProps  = stampProps fw (protoDeclReqProps p)
                     , protoDeclOptProps  = stampProps fw (protoDeclOptProps p)
                     }) (extractProtocol val)

    -- Stamp the origin framework on every method in a list.
    stampMethods :: Maybe Text -> [ObjCMethod] -> [ObjCMethod]
    stampMethods fw = fmap (\m -> m { methodOriginFramework = fw })

    -- Stamp the origin framework on every property in a list.
    stampProps :: Maybe Text -> [ObjCProperty] -> [ObjCProperty]
    stampProps fw = fmap (\p -> p { propOriginFramework = fw })

    -- Struct extraction: parse RecordDecls and TypedefDecls, then combine.
    -- We use the typedef's framework (not the RecordDecl's) since the typedef
    -- location is more reliable for determining which framework owns a struct.
    rawRecords = mapMaybe (extractRecordDecl . snd) annotated
    recordMap = Map.fromList
      [ (rdCName rd, rd) | rd <- rawRecords ]
    typedefTriples = mapMaybe extractStructTypedef annotated
    typedefMap = Map.fromList
      [ (tdName, cName) | (tdName, cName, _) <- typedefTriples ]
    structDefs =
      [ StructDef
          { structTypedefName = tdName
          , structCName       = cName
          , structFields      = rdFields rd
          , structFramework   = tdFw
          , structDoc         = rdDoc rd
          }
      | (tdName, cName, tdFw) <- typedefTriples
      -- Only include structs whose typedef is inside a .framework bundle.
      -- This filters out system types like pthread_mutex_t, extended80, etc.
      , Just _ <- [tdFw]
      -- Haskell data constructors must start with an uppercase letter.
      -- Skip C-style lowercase typedefs (mach_port_status_t, wide, etc.)
      , Just (c, _) <- [T.uncons tdName]
      , Char.isUpper c
      , Just rd <- [Map.lookup cName recordMap]
      -- Only include structs that have fields (skip opaque forward decls)
      , not (null (rdFields rd))
      ]

    -- Enum extraction: parse EnumDecl nodes and stamp frameworks.
    enumDefs = mapMaybe extractEnumWithFw annotated

    extractEnumWithFw (fw, val) =
      fmap (\e -> e { enumFramework = fw }) (extractEnumDecl val)

-- | Annotate each top-level declaration with its source framework.
--
-- Framework resolution uses three sources in priority order:
--
-- 1. Direct @loc.file@ — the actual source file (only present when
--    it changes from the previous declaration)
-- 2. @loc.includedFrom.file@ — the header that included this one;
--    useful when @loc.file@ is absent or points to a non-framework
--    path (e.g., @\/usr\/include\/objc\/NSObject.h@)
-- 3. Delta-tracked file from a previous declaration (last resort)
resolveFrameworks :: [Value] -> [(Maybe Text, Value)]
resolveFrameworks = go Nothing
  where
    go _ [] = []
    go lastFile (val : rest) =
      let -- 1. Try the direct loc.file on this node
          directFile = extractLocFile val
          -- Update file tracking for delta encoding
          trackedFile = case directFile of
            Just _  -> directFile
            Nothing -> lastFile
          -- 2. Try includedFrom.file (the including header chain)
          incFile = extractIncludedFrom val
          -- Resolve framework in priority order:
          -- direct file > includedFrom > delta-tracked file
          fwFromDirect = directFile >>= frameworkFromPath
          fwFromInc    = incFile >>= frameworkFromPath
          fwFromTrack  = trackedFile >>= frameworkFromPath
          fw = fwFromDirect <|> fwFromInc <|> fwFromTrack
      in (fw, val) : go trackedFile rest

-- | Extract the @includedFrom.file@ path from a declaration's @loc@.
--
-- This gives us the header that included the current file, which often
-- belongs to the correct framework even when the actual source file
-- is in a non-framework location (e.g., system headers).
extractIncludedFrom :: Value -> Maybe Text
extractIncludedFrom (Object o) = case KM.lookup "loc" o of
  Just (Object locObj) -> getIncludedFromFile locObj
  _ -> Nothing
  where
    getIncludedFromFile locObj = case KM.lookup "includedFrom" locObj of
      Just (Object ifObj) -> case KM.lookup "file" ifObj of
        Just (String s) -> Just s
        _ -> Nothing
      _ -> Nothing
extractIncludedFrom _ = Nothing

-- | Get the @"inner"@ array from a JSON object, or empty if absent.
getInner :: Value -> [Value]
getInner (Object o) = case KM.lookup "inner" o of
  Just (Array arr) -> V.toList arr
  _                -> []
getInner _ = []

-- | Get a text field from a JSON object.
getText :: Text -> Value -> Maybe Text
getText key (Object o) = case KM.lookup (Key.fromText key) o of
  Just (String s) -> Just s
  _               -> Nothing
getText _ _ = Nothing

-- | Get a boolean field from a JSON object, defaulting to False.
getBool :: Text -> Value -> Bool
getBool key (Object o) = case KM.lookup (Key.fromText key) o of
  Just (Aeson.Bool b) -> b
  _                   -> False
getBool _ _ = False

-- | Get the @"kind"@ field.
getKind :: Value -> Maybe Text
getKind = getText "kind"

-- | Get the @"name"@ field.
getName :: Value -> Maybe Text
getName = getText "name"

-- ---------------------------------------------------------------------------
-- Documentation comment extraction
-- ---------------------------------------------------------------------------

-- | Extract a documentation comment from a declaration's @inner@ children.
--
-- Clang represents doc comments as @FullComment@ nodes inside a
-- declaration's @inner@ array.  This function finds the first such
-- node and renders it to plain text suitable for inclusion in Haddock.
extractDocComment :: Value -> Maybe Text
extractDocComment val =
  let children = getInner val
      fullComments = filter isFullComment children
  in case fullComments of
       (fc : _) -> let rendered = T.strip (renderComment fc)
                   in if T.null rendered then Nothing else Just rendered
       []       -> Nothing
  where
    isFullComment v = getKind v == Just "FullComment"

-- | Render a Clang comment AST node to plain text.
--
-- Handles the various comment node kinds that appear inside @FullComment@:
--
-- * @ParagraphComment@ — a paragraph of text
-- * @TextComment@ — a leaf text node
-- * @ParamCommandComment@ — @\\param name description@
-- * @BlockCommandComment@ — @\\abstract@, @\\return@, @\\discussion@, etc.
-- * @InlineCommandComment@ — inline markup like @\\c code@, @\\e emphasis@
-- * @VerbatimBlockComment@ / @VerbatimBlockLineComment@ — code blocks
-- * @VerbatimLineComment@ — a single verbatim line
-- * @HTMLStartTagComment@ / @HTMLEndTagComment@ — stripped
renderComment :: Value -> Text
renderComment val = case getKind val of
  Just "TextComment" ->
    escapeHaddock (fromMaybe "" (getText "text" val))

  Just "ParagraphComment" ->
    let parts = fmap renderComment (getInner val)
        joined = T.concat parts
    in T.strip joined

  Just "FullComment" ->
    let children = getInner val
        -- Separate paragraphs from param/block commands
        rendered = fmap renderTopLevelChild children
        -- Filter out empty sections
        nonEmpty = filter (not . T.null . T.strip) rendered
    in T.intercalate "\n\n" nonEmpty

  Just "ParamCommandComment" ->
    let paramName = fromMaybe "?" (getText "param" val)
        body = T.strip (T.concat (fmap renderComment (getInner val)))
    in "@" <> paramName <> "@ — " <> body

  Just "BlockCommandComment" ->
    let cmdName = fromMaybe "" (getText "name" val)
        body = T.strip (T.concat (fmap renderComment (getInner val)))
    in case cmdName of
         -- @abstract and @brief are lead paragraphs — just emit the body
         "abstract" -> body
         "brief"    -> body
         -- @return / @result get a label
         "return"   -> "Returns: " <> body
         "result"   -> "Returns: " <> body
         -- @discussion is supplementary prose
         "discussion" -> body
         -- @note, @warning, @important get a label
         "note"      -> "Note: " <> body
         "warning"   -> "Warning: " <> body
         "important" -> "Important: " <> body
         -- @see → See also
         "see"       -> "See: " <> body
         -- Everything else: just emit the body
         _           -> body

  Just "InlineCommandComment" ->
    let cmdName = fromMaybe "" (getText "name" val)
        args = getInlineArgs val
        joined = T.unwords args
    in if T.null joined
       -- Empty args: skip (the content is typically in an adjacent
       -- TextComment sibling, as with \field commands)
       then ""
       else case cmdName of
         -- \c and \p render as code (@...@ in Haddock)
         "c" -> "@" <> joined <> "@"
         "p" -> "@" <> joined <> "@"
         -- \e and \em render as emphasis (/.../ in Haddock)
         "e"  -> "/" <> joined <> "/"
         "em" -> "/" <> joined <> "/"
         -- \b renders as bold (__...__ in Haddock)
         "b" -> "__" <> joined <> "__"
         -- Anything else: just the text
         _   -> joined

  Just "VerbatimBlockComment" ->
    let bodyLines = fmap renderComment (getInner val)
    in T.intercalate "\n" bodyLines

  Just "VerbatimBlockLineComment" ->
    fromMaybe "" (getText "text" val)

  Just "VerbatimLineComment" ->
    fromMaybe "" (getText "text" val)

  -- HTML tags: strip them
  Just "HTMLStartTagComment" -> ""
  Just "HTMLEndTagComment"   -> ""

  -- Fallback: recurse into children
  _ -> T.concat (fmap renderComment (getInner val))

-- | Render a top-level child of @FullComment@ (paragraph or command).
renderTopLevelChild :: Value -> Text
renderTopLevelChild val = case getKind val of
  Just "ParagraphComment" -> T.strip (renderComment val)
  _                       -> renderComment val

-- | Get the @"args"@ array from an @InlineCommandComment@ node.
--
-- Clang represents inline command arguments either as plain strings
-- (e.g., @["NSArray"]@) or as objects with a @"value"@ field.
-- We handle both formats.
getInlineArgs :: Value -> [Text]
getInlineArgs (Object o) = case KM.lookup "args" o of
  Just (Array arr) -> mapMaybe extractArgValue (V.toList arr)
  _                -> []
  where
    -- Plain string: the common Clang format
    extractArgValue (String s) = Just s
    -- Object with "value" key: alternative format
    extractArgValue (Object ao) = case KM.lookup "value" ao of
      Just (String s) -> Just s
      _               -> Nothing
    extractArgValue _ = Nothing
getInlineArgs _ = []

-- | Escape Haddock-special characters in raw documentation text and
-- convert markdown-style backtick code quotes to Haddock @code@ syntax.
--
-- Haddock interprets several characters as markup:
--
-- * @\\@ — escape character (must be doubled to produce a literal backslash)
-- * @\@@ — inline code delimiter (unmatched @\@@ breaks rendering)
-- * @\/@ — emphasis delimiter (when surrounding a word: @\/word\/@)
--
-- Additionally, modern Apple documentation uses backtick-quoted code
-- (@\`identifier\`@) which we convert to Haddock's @\@identifier\@@ syntax.
escapeHaddock :: Text -> Text
escapeHaddock = convertBacktickCode . escapeSpecialChars
  where
    -- Escape \, @, and / in raw text.
    -- We use \ as the Haddock escape character for @ and /.
    -- For \ itself, we double it.
    escapeSpecialChars :: Text -> Text
    escapeSpecialChars t = T.concatMap escapeChar t

    escapeChar :: Char -> Text
    escapeChar '\\' = "\\\\"
    escapeChar '@'  = "\\@"
    escapeChar c    = T.singleton c

    -- Convert `code` to @code@ (Haddock inline code).
    -- Handles the common pattern from modern Apple docs.
    convertBacktickCode :: Text -> Text
    convertBacktickCode t = case T.breakOn "`" t of
      (before, rest)
        | T.null rest -> t  -- no backtick found
        | otherwise   ->
            let afterOpen = T.drop 1 rest  -- drop the opening backtick
            in case T.breakOn "`" afterOpen of
                 (code, rest2)
                   | T.null rest2 -> t  -- unmatched backtick, leave as-is
                   | T.null code  -> before <> T.take 2 rest <> convertBacktickCode (T.drop 2 rest)  -- empty `` pair, leave
                   | otherwise    -> before <> "@" <> code <> "@" <> convertBacktickCode (T.drop 1 rest2)

-- ---------------------------------------------------------------------------
-- Source location / framework extraction
-- ---------------------------------------------------------------------------

-- | Extract the source file path from a declaration's @"loc"@ field.
--
-- Handles both direct @loc.file@ and the @loc.spellingLoc.file@ /
-- @loc.expansionLoc.file@ variants that clang uses for macro expansions.
extractLocFile :: Value -> Maybe Text
extractLocFile (Object o) = case KM.lookup "loc" o of
  Just (Object locObj) ->
    -- Try direct "file" field first
    case KM.lookup "file" locObj of
      Just (String s) -> Just s
      _ -> -- Try spellingLoc, then expansionLoc
        case KM.lookup "spellingLoc" locObj of
          Just (Object slObj) -> case KM.lookup "file" slObj of
            Just (String s) -> Just s
            _ -> tryExpansionLoc locObj
          _ -> tryExpansionLoc locObj
  _ -> Nothing
  where
    tryExpansionLoc locObj = case KM.lookup "expansionLoc" locObj of
      Just (Object elObj) -> case KM.lookup "file" elObj of
        Just (String s) -> Just s
        _ -> Nothing
      _ -> Nothing
extractLocFile _ = Nothing

-- | Extract a framework name from a header file path.
--
-- Matches the pattern @.../SomeFramework.framework/...@ and returns
-- the framework name.
--
-- >>> frameworkFromPath "/SDK/System/Library/Frameworks/Foundation.framework/Headers/NSString.h"
-- Just "Foundation"
-- >>> frameworkFromPath "/usr/include/stdio.h"
-- Nothing
frameworkFromPath :: Text -> Maybe Text
frameworkFromPath path = do
  let (before, after) = T.breakOn ".framework/" path
  guard (not (T.null after))
  -- The framework name is the last path component before ".framework/"
  -- e.g., ".../Foundation" -> "Foundation"
  let (_, fwName) = T.breakOnEnd "/" before
  guard (not (T.null fwName))
  pure fwName

-- ---------------------------------------------------------------------------
-- Interface extraction
-- ---------------------------------------------------------------------------

extractInterface :: Value -> Maybe ObjCInterface
extractInterface val = do
  kind <- getKind val
  guard (kind == "ObjCInterfaceDecl")
  name <- getName val
  let children = getInner val
  -- Skip empty forward declarations
  guard (not (null children))
  let methods = mapMaybe extractMethod children
      props   = mapMaybe extractProperty children
      -- Protocol conformance from inner children (older Clang format)
      innerProtos  = mapMaybe extractProtocolRef children
      -- Protocol conformance from the top-level "protocols" field
      fieldProtos  = extractProtocolsField val
      protos = innerProtos ++ fieldProtos
      tparams = mapMaybe extractTypeParam children
      super   = extractSuperclass val
      fw      = extractLocFile val >>= frameworkFromPath
  let doc = extractDocComment val
  Just ObjCInterface
    { ifaceName       = name
    , ifaceSuperclass = super
    , ifaceTypeParams = tparams
    , ifaceProtocols  = protos
    , ifaceMethods    = methods
    , ifaceProperties = props
    , ifaceFramework  = fw
    , ifaceDoc        = doc
    }

-- | Extract the superclass name from a @"super"@ field.
extractSuperclass :: Value -> Maybe Text
extractSuperclass (Object o) = case KM.lookup "super" o of
  Just (Object superObj) -> case KM.lookup "name" superObj of
    Just (String s) -> Just s
    _               -> Nothing
  _ -> Nothing
extractSuperclass _ = Nothing

-- | Extract a protocol reference from nodes in the interface's inner list.
-- Protocol conformance is indicated by @ObjCProtocol@ nodes (not full decls)
-- appearing inside the interface's inner array.
extractProtocolRef :: Value -> Maybe Text
extractProtocolRef val = do
  kind <- getKind val
  guard (kind == "ObjCProtocol")
  getName val

-- | Extract protocol names from the top-level @\"protocols\"@ field.
--
-- In modern Clang AST JSON, protocol conformance is represented as:
-- @{ "protocols": [{ "kind": "ObjCProtocolDecl", "name": "NSCoding" }, ...] }@
extractProtocolsField :: Value -> [Text]
extractProtocolsField (Object o) = case KM.lookup "protocols" o of
  Just (Array arr) -> mapMaybe getName (V.toList arr)
  _                -> []
extractProtocolsField _ = []

-- | Extract a type parameter name from an @ObjCTypeParamDecl@ node.
extractTypeParam :: Value -> Maybe Text
extractTypeParam val = do
  kind <- getKind val
  guard (kind == "ObjCTypeParamDecl")
  getName val

-- ---------------------------------------------------------------------------
-- Category extraction
-- ---------------------------------------------------------------------------

extractCategory :: Value -> Maybe ObjCCategory
extractCategory val = do
  kind <- getKind val
  guard (kind == "ObjCCategoryDecl")
  clsName <- extractCategoryInterface val
  let children = getInner val
      methods  = mapMaybe extractMethod children
      props    = mapMaybe extractProperty children
      -- Protocol conformance from inner children (older Clang format)
      innerProtos  = mapMaybe extractProtocolRef children
      -- Protocol conformance from the top-level "protocols" field
      fieldProtos  = extractProtocolsField val
      protos = innerProtos ++ fieldProtos
      catNm    = getName val
      fw       = extractLocFile val >>= frameworkFromPath
  let doc = extractDocComment val
  Just ObjCCategory
    { catClassName  = clsName
    , catName       = catNm
    , catProtocols  = protos
    , catMethods    = methods
    , catProperties = props
    , catFramework  = fw
    , catDoc        = doc
    }

-- | Get the class name from the @"interface"@ field of a category.
extractCategoryInterface :: Value -> Maybe Text
extractCategoryInterface (Object o) = case KM.lookup "interface" o of
  Just (Object ifObj) -> case KM.lookup "name" ifObj of
    Just (String s) -> Just s
    _               -> Nothing
  _ -> Nothing
extractCategoryInterface _ = Nothing

-- ---------------------------------------------------------------------------
-- Protocol extraction
-- ---------------------------------------------------------------------------

extractProtocol :: Value -> Maybe ObjCProtocolDecl
extractProtocol val = do
  kind <- getKind val
  guard (kind == "ObjCProtocolDecl")
  name <- getName val
  let children = getInner val
      fieldAdopted = extractProtocolsField val
  -- Skip forward declarations (no children AND no protocols field)
  guard (not (null children) || not (null fieldAdopted))
  let methods = mapMaybe extractMethod children
      props   = mapMaybe extractProperty children
      innerAdopted = mapMaybe extractProtocolRef children
      adopted = innerAdopted ++ fieldAdopted
      fw      = extractLocFile val >>= frameworkFromPath
  let doc = extractDocComment val
  Just ObjCProtocolDecl
    { protoDeclName     = name
    , protoDeclAdopted  = adopted
    , protoDeclRequired = methods
    , protoDeclOptional = []
    , protoDeclReqProps = props
    , protoDeclOptProps = []
    , protoDeclFramework = fw
    , protoDeclDoc       = doc
    }

-- ---------------------------------------------------------------------------
-- Method extraction
-- ---------------------------------------------------------------------------

extractMethod :: Value -> Maybe ObjCMethod
extractMethod val = do
  kind <- getKind val
  guard (kind == "ObjCMethodDecl")
  sel <- getName val
  let isImplicit = getBool "isImplicit" val
  -- Skip implicit (compiler-generated) methods
  guard (not isImplicit)
  let isInstance = getBool "instance" val
      children   = getInner val
      params     = mapMaybe extractParam children
      avail      = mapMaybe extractAvailability children
  retType <- extractReturnType val
  let doc = extractDocComment val
  Just ObjCMethod
    { methodSelector        = sel
    , methodReturnType      = retType
    , methodParams          = params
    , methodIsClass         = not isInstance
    , methodIsImplicit      = isImplicit
    , methodAvailability    = avail
    , methodDoc             = doc
    , methodOriginFramework = Nothing  -- set later by resolveFrameworks
    }

-- | Extract the return type from a method declaration.
extractReturnType :: Value -> Maybe ObjCType
extractReturnType (Object o) = case KM.lookup "returnType" o of
  Just (Object retObj) ->
    let qualType = case KM.lookup "qualType" retObj of
          Just (String s) -> s
          _               -> "void"
        desugared = case KM.lookup "desugaredQualType" retObj of
          Just (String s) -> Just s
          _               -> Nothing
    in Just (parseQualType qualType desugared)
  _ -> Nothing
extractReturnType _ = Nothing

-- | Extract a parameter declaration.
extractParam :: Value -> Maybe (Text, ObjCType)
extractParam val = do
  kind <- getKind val
  guard (kind == "ParmVarDecl")
  let paramName = fromMaybe "_" (getName val)
  paramType <- extractParamType val
  Just (paramName, paramType)

-- | Extract the type from a parameter declaration.
extractParamType :: Value -> Maybe ObjCType
extractParamType (Object o) = case KM.lookup "type" o of
  Just (Object typeObj) ->
    let qualType = case KM.lookup "qualType" typeObj of
          Just (String s) -> s
          _               -> "id"
        desugared = case KM.lookup "desugaredQualType" typeObj of
          Just (String s) -> Just s
          _               -> Nothing
    in Just (parseQualType qualType desugared)
  _ -> Nothing
extractParamType _ = Nothing

-- ---------------------------------------------------------------------------
-- Property extraction
-- ---------------------------------------------------------------------------

extractProperty :: Value -> Maybe ObjCProperty
extractProperty val = do
  kind <- getKind val
  guard (kind == "ObjCPropertyDecl")
  name <- getName val
  propTy <- extractPropertyType val
  let ro = getBool "readonly" val
      cp = getBool "copy" val
      na = getBool "nonatomic" val
      cl = getBool "class" val
      nullab = extractPropertyNullability propTy
  let doc = extractDocComment val
  Just ObjCProperty
    { propName            = name
    , propType            = propTy
    , propReadonly        = ro
    , propCopy            = cp
    , propNonatomic       = na
    , propIsClass         = cl
    , propNullability     = nullab
    , propDoc             = doc
    , propOriginFramework = Nothing  -- set later by resolveFrameworks
    }

-- | Extract the type from a property declaration.
extractPropertyType :: Value -> Maybe ObjCType
extractPropertyType (Object o) = case KM.lookup "type" o of
  Just (Object typeObj) ->
    let qualType = case KM.lookup "qualType" typeObj of
          Just (String s) -> s
          _               -> "id"
        desugared = case KM.lookup "desugaredQualType" typeObj of
          Just (String s) -> Just s
          _               -> Nothing
    in Just (parseQualType qualType desugared)
  _ -> Nothing
extractPropertyType _ = Nothing

-- | Determine nullability from the parsed type.
extractPropertyNullability :: ObjCType -> Nullability
extractPropertyNullability (ObjCId _ n) = n
extractPropertyNullability (ObjCGeneric _ _ n) = n
extractPropertyNullability _ = Unspecified

-- ---------------------------------------------------------------------------
-- Availability extraction
-- ---------------------------------------------------------------------------

extractAvailability :: Value -> Maybe Availability
extractAvailability val = do
  kind <- getKind val
  guard (kind == "AvailabilityAttr")
  platform <- getText "platform" val
  let intro = getText "introduced" val
      depr  = getText "deprecated" val
  Just Availability
    { availPlatform   = platform
    , availIntroduced = intro
    , availDeprecated = depr
    }

-- ---------------------------------------------------------------------------
-- Struct (RecordDecl) extraction
-- ---------------------------------------------------------------------------

-- | Raw record (struct) data before combining with typedefs.
data RawRecordDecl = RawRecordDecl
  { rdCName     :: Text            -- ^ C struct name (e.g., "_NSRange")
  , rdFields    :: [(Text, ObjCType)]
  , rdFramework :: Maybe Text
  , rdDoc       :: Maybe Text      -- ^ Documentation comment, if any.
  } deriving (Show)

-- | Extract a RecordDecl (struct definition) from a clang AST node.
--
-- Clang represents structs as:
-- @
-- { "kind": "RecordDecl", "tagUsed": "struct", "name": "_NSRange",
--   "inner": [ { "kind": "FieldDecl", "name": "location", "type": { ... } }, ... ] }
-- @
extractRecordDecl :: Value -> Maybe RawRecordDecl
extractRecordDecl val = do
  kind <- getKind val
  guard (kind == "RecordDecl")
  tag <- getText "tagUsed" val
  guard (tag == "struct")
  name <- getName val
  let children = getInner val
      fields = mapMaybe extractFieldDecl children
  -- Only produce a record for definitions (that have fields),
  -- not forward declarations.
  guard (not (null fields))
  let fw = extractLocFile val >>= frameworkFromPath
      doc = extractDocComment val
  Just RawRecordDecl
    { rdCName     = name
    , rdFields    = fields
    , rdFramework = fw
    , rdDoc       = doc
    }

-- | Extract a FieldDecl from inside a RecordDecl.
extractFieldDecl :: Value -> Maybe (Text, ObjCType)
extractFieldDecl val = do
  kind <- getKind val
  guard (kind == "FieldDecl")
  name <- getName val
  ty <- extractFieldType val
  Just (name, ty)

-- | Extract the type from a FieldDecl's @"type"@ object.
extractFieldType :: Value -> Maybe ObjCType
extractFieldType (Object o) = case KM.lookup "type" o of
  Just (Object typeObj) ->
    let qualType = case KM.lookup "qualType" typeObj of
          Just (String s) -> s
          _               -> "int"
        desugared = case KM.lookup "desugaredQualType" typeObj of
          Just (String s) -> Just s
          _               -> Nothing
    in Just (parseQualType qualType desugared)
  _ -> Nothing
extractFieldType _ = Nothing

-- | Extract a TypedefDecl that aliases a struct type.
--
-- We look for typedefs whose @desugaredQualType@ starts with @"struct "@ —
-- these are the public names for C structs (e.g., @NSRange@ → @struct _NSRange@).
--
-- Returns @(typedefName, structCName, framework)@ if the typedef points to a struct.
extractStructTypedef :: (Maybe Text, Value) -> Maybe (Text, Text, Maybe Text)
extractStructTypedef (fw, val) = do
  kind <- getKind val
  guard (kind == "TypedefDecl")
  name <- getName val
  -- Get the desugared type to find the underlying struct name
  desugared <- extractTypedefDesugared val
  -- Check if it references a struct
  cName <- T.stripPrefix "struct " desugared
  -- Ignore reserved/internal typedefs
  guard (not (T.isPrefixOf "__" name))
  -- Ignore typedefs that point to anonymous structs (empty cName after strip)
  guard (not (T.null (T.strip cName)))
  Just (name, T.strip cName, fw)

-- | Get the underlying type from a TypedefDecl's "type" object.
-- Prefers @desugaredQualType@, falls back to @qualType@ (which is the
-- desugared form when clang omits @desugaredQualType@, e.g., for
-- @typedef struct _NSRange NSRange@).
extractTypedefDesugared :: Value -> Maybe Text
extractTypedefDesugared (Object o) = case KM.lookup "type" o of
  Just (Object typeObj) ->
    case KM.lookup "desugaredQualType" typeObj of
      Just (String s) -> Just s
      _ -> case KM.lookup "qualType" typeObj of
        Just (String s) -> Just s
        _ -> Nothing
  _ -> Nothing
extractTypedefDesugared _ = Nothing

-- ---------------------------------------------------------------------------
-- Enum (EnumDecl) extraction
-- ---------------------------------------------------------------------------

-- | Extract an EnumDecl from a clang AST node.
--
-- Clang represents enums (including @NS_ENUM@ and @NS_OPTIONS@) as:
--
-- @
-- { "kind": "EnumDecl", "name": "NSWindowStyleMask",
--   "fixedUnderlyingType": { "qualType": "NSUInteger",
--                            "desugaredQualType": "unsigned long" },
--   "inner": [
--     { "kind": "FlagEnumAttr" },          -- present for NS_OPTIONS
--     { "kind": "EnumConstantDecl", "name": "NSWindowStyleMaskTitled",
--       "inner": [{ "kind": "ConstantExpr", "value": "1", ... }] },
--     ...
--   ] }
-- @
extractEnumDecl :: Value -> Maybe EnumDef
extractEnumDecl val = do
  kind <- getKind val
  guard (kind == "EnumDecl")
  name <- getName val
  -- Skip anonymous enums
  guard (not (T.null name))
  -- Haskell type constructors must start with an uppercase letter
  guard (maybe False (Char.isUpper . fst) (T.uncons name))
  let children = getInner val
  -- Skip forward declarations (no children)
  guard (not (null children))
  let (underlyingQ, underlyingD) = extractFixedUnderlyingType val
      isOptions = any isFlagEnumAttr children
      constants = extractEnumConstants children
      doc = extractDocComment val
  -- Skip enums with no extractable constants
  guard (not (null constants))
  Just EnumDef
    { enumName               = name
    , enumUnderlyingQual     = underlyingQ
    , enumUnderlyingDesugared = underlyingD
    , enumIsOptions          = isOptions
    , enumConstants          = constants
    , enumFramework          = Nothing  -- set later by extractEnumWithFw
    , enumDoc                = doc
    }

-- | Extract the @fixedUnderlyingType@ from an EnumDecl.
-- Returns @(qualType, desugaredQualType)@; defaults to @("int", "int")@.
extractFixedUnderlyingType :: Value -> (Text, Text)
extractFixedUnderlyingType (Object o) = case KM.lookup "fixedUnderlyingType" o of
  Just (Object ftObj) ->
    let q = case KM.lookup "qualType" ftObj of
              Just (String s) -> s
              _               -> "int"
        d = case KM.lookup "desugaredQualType" ftObj of
              Just (String s) -> s
              _               -> q  -- fall back to qualType
    in (q, d)
  _ -> ("int", "int")
extractFixedUnderlyingType _ = ("int", "int")

-- | Check if a child node is a @FlagEnumAttr@ (indicates NS_OPTIONS).
isFlagEnumAttr :: Value -> Bool
isFlagEnumAttr v = getKind v == Just "FlagEnumAttr"

-- | Extract all EnumConstantDecl children, preserving declaration order.
-- Uses a fold to track the previous constant's value for auto-increment
-- when a constant has no explicit value expression.
extractEnumConstants :: [Value] -> [EnumConstant]
extractEnumConstants children =
  let constDecls = filter (\v -> getKind v == Just "EnumConstantDecl") children
  in snd (foldl go (0, []) constDecls)
  where
    go :: (Integer, [EnumConstant]) -> Value -> (Integer, [EnumConstant])
    go (nextVal, acc) v =
      case getName v of
        Nothing -> (nextVal, acc)
        Just cName ->
          let explicitVal = extractConstantValue v
              val = fromMaybe nextVal explicitVal
              ec = EnumConstant { ecName = cName, ecValue = val }
          in (val + 1, acc ++ [ec])

-- | Extract the integer value from an EnumConstantDecl.
--
-- Looks for:
--
-- 1. A @ConstantExpr@ child with a @\"value\"@ field (modern clang)
-- 2. An @IntegerLiteral@ child with a @\"value\"@ field
-- 3. A @ImplicitValueInitExpr@ (value 0)
--
-- Returns 'Nothing' if no value could be extracted (will auto-increment).
extractConstantValue :: Value -> Maybe Integer
extractConstantValue val =
  let children = getInner val
  in findConstantExprValue children
     <|> findIntegerLiteralValue children

-- | Find a ConstantExpr with a "value" field anywhere in the tree.
-- ConstantExpr nodes in modern clang wrap the expression tree and carry
-- the pre-evaluated integer in a "value" field.  Recurses into wrapping
-- nodes like ImplicitCastExpr.
findConstantExprValue :: [Value] -> Maybe Integer
findConstantExprValue [] = Nothing
findConstantExprValue (v : vs) = case getKind v of
  Just "ConstantExpr" -> parseValueField v <|> findConstantExprValue vs
  _ ->
    -- Recurse into children to handle wrappers like ImplicitCastExpr
    let childResult = findConstantExprValue (getInner v)
    in childResult <|> findConstantExprValue vs

-- | Find an IntegerLiteral with a "value" field.
findIntegerLiteralValue :: [Value] -> Maybe Integer
findIntegerLiteralValue [] = Nothing
findIntegerLiteralValue (v : vs) = case getKind v of
  Just "IntegerLiteral" -> parseValueField v <|> findIntegerLiteralValue vs
  _ ->
    -- Recurse into children to handle expressions like
    -- ImplicitCastExpr > IntegerLiteral
    let childResult = findIntegerLiteralValue (getInner v)
    in childResult <|> findIntegerLiteralValue vs

-- | Parse the "value" field from a node as an Integer.
-- Handles decimal and hex (0x...) formats.
parseValueField :: Value -> Maybe Integer
parseValueField v = do
  valText <- getText "value" v
  parseIntegerText valText

-- | Parse a text representation of an integer (decimal or hex).
parseIntegerText :: Text -> Maybe Integer
parseIntegerText t =
  let s = T.unpack (T.strip t)
  in case s of
    ('-' : rest) -> fmap negate (parsePositive rest)
    _            -> parsePositive s
  where
    parsePositive :: String -> Maybe Integer
    parsePositive ('0' : 'x' : hex) = readMaybe16 hex
    parsePositive ('0' : 'X' : hex) = readMaybe16 hex
    parsePositive digits             = readMaybe10 digits

    readMaybe10 :: String -> Maybe Integer
    readMaybe10 ds
      | null ds = Nothing
      | all Char.isDigit ds = Just (read ds)
      | otherwise = Nothing

    readMaybe16 :: String -> Maybe Integer
    readMaybe16 ds
      | null ds = Nothing
      | all Char.isHexDigit ds = Just (read ("0x" ++ ds))
      | otherwise = Nothing
