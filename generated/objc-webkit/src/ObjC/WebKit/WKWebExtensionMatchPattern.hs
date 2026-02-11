{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionMatchPattern`` object represents a way to specify groups of URLs.
--
-- All match patterns are specified as strings. Apart from the special `<all_urls>` pattern, match patterns consist of three parts: scheme, host, and path.
--
-- Generated bindings for @WKWebExtensionMatchPattern@.
module ObjC.WebKit.WKWebExtensionMatchPattern
  ( WKWebExtensionMatchPattern
  , IsWKWebExtensionMatchPattern(..)
  , new
  , init_
  , registerCustomURLScheme
  , allURLsMatchPattern
  , allHostsAndSchemesMatchPattern
  , matchPatternWithString
  , matchPatternWithScheme_host_path
  , initWithString_error
  , initWithScheme_host_path_error
  , matchesURL
  , matchesURL_options
  , matchesPattern
  , matchesPattern_options
  , string
  , scheme
  , host
  , path
  , matchesAllURLs
  , matchesAllHosts
  , newSelector
  , initSelector
  , registerCustomURLSchemeSelector
  , allURLsMatchPatternSelector
  , allHostsAndSchemesMatchPatternSelector
  , matchPatternWithStringSelector
  , matchPatternWithScheme_host_pathSelector
  , initWithString_errorSelector
  , initWithScheme_host_path_errorSelector
  , matchesURLSelector
  , matchesURL_optionsSelector
  , matchesPatternSelector
  , matchesPattern_optionsSelector
  , stringSelector
  , schemeSelector
  , hostSelector
  , pathSelector
  , matchesAllURLsSelector
  , matchesAllHostsSelector

  -- * Enum types
  , WKWebExtensionMatchPatternOptions(WKWebExtensionMatchPatternOptions)
  , pattern WKWebExtensionMatchPatternOptionsNone
  , pattern WKWebExtensionMatchPatternOptionsIgnoreSchemes
  , pattern WKWebExtensionMatchPatternOptionsIgnorePaths
  , pattern WKWebExtensionMatchPatternOptionsMatchBidirectionally

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionMatchPattern)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id WKWebExtensionMatchPattern)
init_ wkWebExtensionMatchPattern  =
  sendMsg wkWebExtensionMatchPattern (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Registers a custom URL scheme that can be used in match patterns.
--
-- This method should be used to register any custom URL schemes used by the app for the extension base URLs, other than @webkit-extension@, or if extensions should have access to other supported URL schemes when using `<all_urls>`.
--
-- @urlScheme@ — The custom URL scheme to register.
--
-- ObjC selector: @+ registerCustomURLScheme:@
registerCustomURLScheme :: IsNSString urlScheme => urlScheme -> IO ()
registerCustomURLScheme urlScheme =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    withObjCPtr urlScheme $ \raw_urlScheme ->
      sendClassMsg cls' (mkSelector "registerCustomURLScheme:") retVoid [argPtr (castPtr raw_urlScheme :: Ptr ())]

-- | Returns a pattern object for `<all_urls>`.
--
-- ObjC selector: @+ allURLsMatchPattern@
allURLsMatchPattern :: IO (Id WKWebExtensionMatchPattern)
allURLsMatchPattern  =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    sendClassMsg cls' (mkSelector "allURLsMatchPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a pattern object that has @*@ for scheme, host, and path.
--
-- ObjC selector: @+ allHostsAndSchemesMatchPattern@
allHostsAndSchemesMatchPattern :: IO (Id WKWebExtensionMatchPattern)
allHostsAndSchemesMatchPattern  =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    sendClassMsg cls' (mkSelector "allHostsAndSchemesMatchPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a pattern object for the specified pattern string.
--
-- Returns: Returns @nil@ if the pattern string is invalid.
--
-- initWithString:error:
--
-- ObjC selector: @+ matchPatternWithString:@
matchPatternWithString :: IsNSString string => string -> IO (Id WKWebExtensionMatchPattern)
matchPatternWithString string =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "matchPatternWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a pattern object for the specified scheme, host, and path strings.
--
-- Returns: A pattern object, or @nil@ if any of the strings are invalid.
--
-- initWithScheme:host:path:error:
--
-- ObjC selector: @+ matchPatternWithScheme:host:path:@
matchPatternWithScheme_host_path :: (IsNSString scheme, IsNSString host, IsNSString path) => scheme -> host -> path -> IO (Id WKWebExtensionMatchPattern)
matchPatternWithScheme_host_path scheme host path =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    withObjCPtr scheme $ \raw_scheme ->
      withObjCPtr host $ \raw_host ->
        withObjCPtr path $ \raw_path ->
          sendClassMsg cls' (mkSelector "matchPatternWithScheme:host:path:") (retPtr retVoid) [argPtr (castPtr raw_scheme :: Ptr ()), argPtr (castPtr raw_host :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a pattern object for the specified pattern string.
--
-- @error@ — Set to @nil@ or an error instance if an error occurred.
--
-- Returns: A pattern object, or @nil@ if the pattern string is invalid and an error will be set.
--
-- initWithString:
--
-- ObjC selector: @- initWithString:error:@
initWithString_error :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsNSString string, IsNSError error_) => wkWebExtensionMatchPattern -> string -> error_ -> IO (Id WKWebExtensionMatchPattern)
initWithString_error wkWebExtensionMatchPattern  string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg wkWebExtensionMatchPattern (mkSelector "initWithString:error:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a pattern object for the specified scheme, host, and path strings.
--
-- @error@ — Set to @nil@ or an error instance if an error occurred.
--
-- Returns: A pattern object, or @nil@ if any of the strings are invalid and an error will be set.
--
-- initWithScheme:host:path:
--
-- ObjC selector: @- initWithScheme:host:path:error:@
initWithScheme_host_path_error :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsNSString scheme, IsNSString host, IsNSString path, IsNSError error_) => wkWebExtensionMatchPattern -> scheme -> host -> path -> error_ -> IO (Id WKWebExtensionMatchPattern)
initWithScheme_host_path_error wkWebExtensionMatchPattern  scheme host path error_ =
withObjCPtr scheme $ \raw_scheme ->
  withObjCPtr host $ \raw_host ->
    withObjCPtr path $ \raw_path ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg wkWebExtensionMatchPattern (mkSelector "initWithScheme:host:path:error:") (retPtr retVoid) [argPtr (castPtr raw_scheme :: Ptr ()), argPtr (castPtr raw_host :: Ptr ()), argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Matches the reciever pattern against the specified URL.
--
-- @url@ — The URL to match the against the reciever pattern.
--
-- Returns: A Boolean value indicating if pattern matches the specified URL.
--
-- matchesURL:options:
--
-- ObjC selector: @- matchesURL:@
matchesURL :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsNSURL url) => wkWebExtensionMatchPattern -> url -> IO Bool
matchesURL wkWebExtensionMatchPattern  url =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | Matches the reciever pattern against the specified URL with options.
--
-- @url@ — The URL to match the against the reciever pattern.
--
-- @options@ — The options to use while matching.
--
-- Returns: A Boolean value indicating if pattern matches the specified URL.
--
-- matchesURL:
--
-- ObjC selector: @- matchesURL:options:@
matchesURL_options :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsNSURL url) => wkWebExtensionMatchPattern -> url -> WKWebExtensionMatchPatternOptions -> IO Bool
matchesURL_options wkWebExtensionMatchPattern  url options =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesURL:options:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options)]

-- | Matches the receiver pattern against the specified pattern.
--
-- @pattern@ — The pattern to match against the receiver pattern.
--
-- Returns: A Boolean value indicating if receiver pattern matches the specified pattern.
--
-- matchesPattern:options:
--
-- ObjC selector: @- matchesPattern:@
matchesPattern :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsWKWebExtensionMatchPattern pattern_) => wkWebExtensionMatchPattern -> pattern_ -> IO Bool
matchesPattern wkWebExtensionMatchPattern  pattern_ =
withObjCPtr pattern_ $ \raw_pattern_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesPattern:") retCULong [argPtr (castPtr raw_pattern_ :: Ptr ())]

-- | Matches the receiver pattern against the specified pattern with options.
--
-- @pattern@ — The pattern to match against the receiver pattern.
--
-- @options@ — The options to use while matching.
--
-- Returns: A Boolean value indicating if receiver pattern matches the specified pattern.
--
-- matchesPattern:
--
-- ObjC selector: @- matchesPattern:options:@
matchesPattern_options :: (IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern, IsWKWebExtensionMatchPattern pattern_) => wkWebExtensionMatchPattern -> pattern_ -> WKWebExtensionMatchPatternOptions -> IO Bool
matchesPattern_options wkWebExtensionMatchPattern  pattern_ options =
withObjCPtr pattern_ $ \raw_pattern_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesPattern:options:") retCULong [argPtr (castPtr raw_pattern_ :: Ptr ()), argCULong (coerce options)]

-- | The original pattern string.
--
-- ObjC selector: @- string@
string :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
string wkWebExtensionMatchPattern  =
  sendMsg wkWebExtensionMatchPattern (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scheme part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- scheme@
scheme :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
scheme wkWebExtensionMatchPattern  =
  sendMsg wkWebExtensionMatchPattern (mkSelector "scheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The host part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- host@
host :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
host wkWebExtensionMatchPattern  =
  sendMsg wkWebExtensionMatchPattern (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The path part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- path@
path :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
path wkWebExtensionMatchPattern  =
  sendMsg wkWebExtensionMatchPattern (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the pattern is `<all_urls>`.
--
-- ObjC selector: @- matchesAllURLs@
matchesAllURLs :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO Bool
matchesAllURLs wkWebExtensionMatchPattern  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesAllURLs") retCULong []

-- | If the pattern is `<all_urls>@ or has @*` as the host.
--
-- ObjC selector: @- matchesAllHosts@
matchesAllHosts :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO Bool
matchesAllHosts wkWebExtensionMatchPattern  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionMatchPattern (mkSelector "matchesAllHosts") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @registerCustomURLScheme:@
registerCustomURLSchemeSelector :: Selector
registerCustomURLSchemeSelector = mkSelector "registerCustomURLScheme:"

-- | @Selector@ for @allURLsMatchPattern@
allURLsMatchPatternSelector :: Selector
allURLsMatchPatternSelector = mkSelector "allURLsMatchPattern"

-- | @Selector@ for @allHostsAndSchemesMatchPattern@
allHostsAndSchemesMatchPatternSelector :: Selector
allHostsAndSchemesMatchPatternSelector = mkSelector "allHostsAndSchemesMatchPattern"

-- | @Selector@ for @matchPatternWithString:@
matchPatternWithStringSelector :: Selector
matchPatternWithStringSelector = mkSelector "matchPatternWithString:"

-- | @Selector@ for @matchPatternWithScheme:host:path:@
matchPatternWithScheme_host_pathSelector :: Selector
matchPatternWithScheme_host_pathSelector = mkSelector "matchPatternWithScheme:host:path:"

-- | @Selector@ for @initWithString:error:@
initWithString_errorSelector :: Selector
initWithString_errorSelector = mkSelector "initWithString:error:"

-- | @Selector@ for @initWithScheme:host:path:error:@
initWithScheme_host_path_errorSelector :: Selector
initWithScheme_host_path_errorSelector = mkSelector "initWithScheme:host:path:error:"

-- | @Selector@ for @matchesURL:@
matchesURLSelector :: Selector
matchesURLSelector = mkSelector "matchesURL:"

-- | @Selector@ for @matchesURL:options:@
matchesURL_optionsSelector :: Selector
matchesURL_optionsSelector = mkSelector "matchesURL:options:"

-- | @Selector@ for @matchesPattern:@
matchesPatternSelector :: Selector
matchesPatternSelector = mkSelector "matchesPattern:"

-- | @Selector@ for @matchesPattern:options:@
matchesPattern_optionsSelector :: Selector
matchesPattern_optionsSelector = mkSelector "matchesPattern:options:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @scheme@
schemeSelector :: Selector
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @matchesAllURLs@
matchesAllURLsSelector :: Selector
matchesAllURLsSelector = mkSelector "matchesAllURLs"

-- | @Selector@ for @matchesAllHosts@
matchesAllHostsSelector :: Selector
matchesAllHostsSelector = mkSelector "matchesAllHosts"

