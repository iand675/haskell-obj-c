{-# LANGUAGE OverloadedStrings #-}

-- | Haskell name generation, escaping, and Haddock formatting.
--
-- Pure utility functions used by every generation sub-module.
module ObjC.CodeGen.Generate.Naming
  ( -- * CamelCase helpers
    lowerFirst
  , upperFirst
    -- * ObjC → Haskell name mapping
  , methodHaskellName
  , selectorHaskellName
  , instanceMethodNameSet
  , sanitizeParamName
    -- * Reserved words
  , escapeReserved
  , reservedNames
    -- * Haddock
  , formatHaddock
  ) where

import Data.Char (isUpper)
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import ObjC.CodeGen.IR (ObjCClass(..), ObjCMethod(..))

-- ---------------------------------------------------------------------------
-- CamelCase helpers
-- ---------------------------------------------------------------------------

-- | Lower the first \"word\" of a CamelCase identifier, handling
-- acronym prefixes like NS, UI, etc.
--
-- >>> lowerFirst "NSString"
-- "nsString"
-- >>> lowerFirst "URL"
-- "url"
-- >>> lowerFirst "a"
-- "a"
lowerFirst :: Text -> Text
lowerFirst t =
  let (caps, rest) = T.span isUpper t
  in case T.length caps of
    0 -> t
    1 -> T.cons (Char.toLower (T.head caps)) rest
    _ | T.null rest ->
          T.map Char.toLower caps
      | otherwise ->
          T.map Char.toLower (T.init caps) <> T.cons (T.last caps) rest

-- | Capitalise the first character of a text.
upperFirst :: Text -> Text
upperFirst t
  | T.null t  = t
  | otherwise = T.cons (Char.toUpper (T.head t)) (T.tail t)

-- ---------------------------------------------------------------------------
-- ObjC → Haskell name mapping
-- ---------------------------------------------------------------------------

-- | Compute the Haskell name for a method.
--
-- Instance methods: @lowerFirst baseName@ (e.g., @\"addTimer_forMode\"@).
-- Class methods: same, unless there is a collision with an instance
-- method sharing the same name, in which case the class method is
-- prefixed with @lowerFirst className@.
methodHaskellName :: ObjCClass -> Set Text -> ObjCMethod -> Text
methodHaskellName cls instanceMethodNames method =
  let sel = methodSelector method
      baseName = T.replace ":" "_" (T.dropWhileEnd (== ':') sel)
      raw = lowerFirst baseName
      escaped = escapeReserved raw
  in if methodIsClass method && Set.member escaped instanceMethodNames
     then escapeReserved (lowerFirst (className cls) <> upperFirst raw)
     else escaped

-- | Generate a Haskell name for a top-level @Selector@ binding.
--
-- >>> selectorHaskellName "terminate:"
-- "terminateSelector"
selectorHaskellName :: Text -> Text
selectorHaskellName sel =
  let baseName = T.replace ":" "_" (T.dropWhileEnd (== ':') sel)
  in escapeReserved (lowerFirst baseName <> "Selector")

-- | Compute the set of instance method Haskell names for collision detection.
instanceMethodNameSet :: ObjCClass -> [ObjCMethod] -> Set Text
instanceMethodNameSet _cls methods =
  let instMethods = filter (not . methodIsClass) methods
  in Set.fromList (fmap (\m ->
       let sel = methodSelector m
           baseName = T.replace ":" "_" (T.dropWhileEnd (== ':') sel)
       in escapeReserved (lowerFirst baseName)) instMethods)

-- | Sanitize an ObjC parameter name for use as a Haskell variable.
sanitizeParamName :: Text -> Text
sanitizeParamName name
  | T.null name = "arg_"
  | isUpper (T.head name) = sanitizeParamName (lowerFirst name)
  | Set.member name reservedNames || name == "obj" || name == "pure" = name <> "_"
  | otherwise = name

-- ---------------------------------------------------------------------------
-- Reserved words
-- ---------------------------------------------------------------------------

-- | Append an underscore to names that clash with Haskell reserved
-- words or common Prelude identifiers.
escapeReserved :: Text -> Text
escapeReserved name
  | Set.member name reservedNames = name <> "_"
  | otherwise = name

-- | Haskell keywords and Prelude names commonly shadowed by ObjC selectors.
reservedNames :: Set Text
reservedNames = Set.fromList
  [ -- Haskell keywords
    "as", "case", "class", "data", "default", "deriving", "do", "else"
  , "forall", "foreign", "hiding", "if", "import", "in", "infix", "infixl"
  , "infixr", "instance", "let", "module", "newtype", "of", "qualified"
  , "then", "type", "where"
  -- GHC extension keywords
  , "pattern", "role", "family", "stock", "anyclass", "via"
  -- Common Prelude identifiers
  , "init", "error", "fail", "id", "map", "filter", "length", "head", "tail"
  , "last", "null", "read", "show", "print", "return", "sequence"
  , "compare", "min", "max", "minimum", "maximum"
  , "not", "and", "or", "any", "all"
  , "concat", "sum", "product", "elem", "repeat", "replicate"
  , "take", "drop", "reverse", "lookup", "words", "lines", "unwords"
  , "unlines", "otherwise", "undefined", "seq", "subtract"
  , "div", "mod", "rem", "quot", "negate", "abs", "signum"
  ]

-- ---------------------------------------------------------------------------
-- Haddock formatting
-- ---------------------------------------------------------------------------

-- | Format a raw documentation string as Haddock comment lines.
--
-- The first line is prefixed with @-- | @, continuation lines with @-- @.
-- Empty lines become bare @--@ to separate Haddock paragraphs.
formatHaddock :: Text -> [Text]
formatHaddock raw =
  let cleaned = T.strip raw
      rawLines = T.lines cleaned
      collapsed = collapseBlankLines rawLines
  in case collapsed of
       []     -> []
       (l:ls) -> ("-- | " <> T.strip l) : fmap mkContinuation ls
  where
    mkContinuation line
      | T.null (T.strip line) = "--"
      | otherwise              = "-- " <> T.strip line

    collapseBlankLines :: [Text] -> [Text]
    collapseBlankLines = go False
      where
        go _ [] = []
        go prevBlank (x : xs)
          | T.null (T.strip x) =
              if prevBlank then go True xs else "" : go True xs
          | otherwise = x : go False xs
