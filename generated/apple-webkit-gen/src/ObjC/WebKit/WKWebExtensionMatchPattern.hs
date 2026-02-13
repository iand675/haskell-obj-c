{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allHostsAndSchemesMatchPatternSelector
  , allURLsMatchPatternSelector
  , hostSelector
  , initSelector
  , initWithScheme_host_path_errorSelector
  , initWithString_errorSelector
  , matchPatternWithScheme_host_pathSelector
  , matchPatternWithStringSelector
  , matchesAllHostsSelector
  , matchesAllURLsSelector
  , matchesPatternSelector
  , matchesPattern_optionsSelector
  , matchesURLSelector
  , matchesURL_optionsSelector
  , newSelector
  , pathSelector
  , registerCustomURLSchemeSelector
  , schemeSelector
  , stringSelector

  -- * Enum types
  , WKWebExtensionMatchPatternOptions(WKWebExtensionMatchPatternOptions)
  , pattern WKWebExtensionMatchPatternOptionsNone
  , pattern WKWebExtensionMatchPatternOptionsIgnoreSchemes
  , pattern WKWebExtensionMatchPatternOptionsIgnorePaths
  , pattern WKWebExtensionMatchPatternOptionsMatchBidirectionally

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id WKWebExtensionMatchPattern)
init_ wkWebExtensionMatchPattern =
  sendOwnedMessage wkWebExtensionMatchPattern initSelector

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
    sendClassMessage cls' registerCustomURLSchemeSelector (toNSString urlScheme)

-- | Returns a pattern object for `<all_urls>`.
--
-- ObjC selector: @+ allURLsMatchPattern@
allURLsMatchPattern :: IO (Id WKWebExtensionMatchPattern)
allURLsMatchPattern  =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    sendClassMessage cls' allURLsMatchPatternSelector

-- | Returns a pattern object that has @*@ for scheme, host, and path.
--
-- ObjC selector: @+ allHostsAndSchemesMatchPattern@
allHostsAndSchemesMatchPattern :: IO (Id WKWebExtensionMatchPattern)
allHostsAndSchemesMatchPattern  =
  do
    cls' <- getRequiredClass "WKWebExtensionMatchPattern"
    sendClassMessage cls' allHostsAndSchemesMatchPatternSelector

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
    sendClassMessage cls' matchPatternWithStringSelector (toNSString string)

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
    sendClassMessage cls' matchPatternWithScheme_host_pathSelector (toNSString scheme) (toNSString host) (toNSString path)

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
initWithString_error wkWebExtensionMatchPattern string error_ =
  sendOwnedMessage wkWebExtensionMatchPattern initWithString_errorSelector (toNSString string) (toNSError error_)

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
initWithScheme_host_path_error wkWebExtensionMatchPattern scheme host path error_ =
  sendOwnedMessage wkWebExtensionMatchPattern initWithScheme_host_path_errorSelector (toNSString scheme) (toNSString host) (toNSString path) (toNSError error_)

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
matchesURL wkWebExtensionMatchPattern url =
  sendMessage wkWebExtensionMatchPattern matchesURLSelector (toNSURL url)

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
matchesURL_options wkWebExtensionMatchPattern url options =
  sendMessage wkWebExtensionMatchPattern matchesURL_optionsSelector (toNSURL url) options

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
matchesPattern wkWebExtensionMatchPattern pattern_ =
  sendMessage wkWebExtensionMatchPattern matchesPatternSelector (toWKWebExtensionMatchPattern pattern_)

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
matchesPattern_options wkWebExtensionMatchPattern pattern_ options =
  sendMessage wkWebExtensionMatchPattern matchesPattern_optionsSelector (toWKWebExtensionMatchPattern pattern_) options

-- | The original pattern string.
--
-- ObjC selector: @- string@
string :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
string wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern stringSelector

-- | The scheme part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- scheme@
scheme :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
scheme wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern schemeSelector

-- | The host part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- host@
host :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
host wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern hostSelector

-- | The path part of the pattern string, unless ``matchesAllURLs`` is @YES@.
--
-- ObjC selector: @- path@
path :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO (Id NSString)
path wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern pathSelector

-- | If the pattern is `<all_urls>`.
--
-- ObjC selector: @- matchesAllURLs@
matchesAllURLs :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO Bool
matchesAllURLs wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern matchesAllURLsSelector

-- | If the pattern is `<all_urls>@ or has @*` as the host.
--
-- ObjC selector: @- matchesAllHosts@
matchesAllHosts :: IsWKWebExtensionMatchPattern wkWebExtensionMatchPattern => wkWebExtensionMatchPattern -> IO Bool
matchesAllHosts wkWebExtensionMatchPattern =
  sendMessage wkWebExtensionMatchPattern matchesAllHostsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionMatchPattern)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionMatchPattern)
initSelector = mkSelector "init"

-- | @Selector@ for @registerCustomURLScheme:@
registerCustomURLSchemeSelector :: Selector '[Id NSString] ()
registerCustomURLSchemeSelector = mkSelector "registerCustomURLScheme:"

-- | @Selector@ for @allURLsMatchPattern@
allURLsMatchPatternSelector :: Selector '[] (Id WKWebExtensionMatchPattern)
allURLsMatchPatternSelector = mkSelector "allURLsMatchPattern"

-- | @Selector@ for @allHostsAndSchemesMatchPattern@
allHostsAndSchemesMatchPatternSelector :: Selector '[] (Id WKWebExtensionMatchPattern)
allHostsAndSchemesMatchPatternSelector = mkSelector "allHostsAndSchemesMatchPattern"

-- | @Selector@ for @matchPatternWithString:@
matchPatternWithStringSelector :: Selector '[Id NSString] (Id WKWebExtensionMatchPattern)
matchPatternWithStringSelector = mkSelector "matchPatternWithString:"

-- | @Selector@ for @matchPatternWithScheme:host:path:@
matchPatternWithScheme_host_pathSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id WKWebExtensionMatchPattern)
matchPatternWithScheme_host_pathSelector = mkSelector "matchPatternWithScheme:host:path:"

-- | @Selector@ for @initWithString:error:@
initWithString_errorSelector :: Selector '[Id NSString, Id NSError] (Id WKWebExtensionMatchPattern)
initWithString_errorSelector = mkSelector "initWithString:error:"

-- | @Selector@ for @initWithScheme:host:path:error:@
initWithScheme_host_path_errorSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSError] (Id WKWebExtensionMatchPattern)
initWithScheme_host_path_errorSelector = mkSelector "initWithScheme:host:path:error:"

-- | @Selector@ for @matchesURL:@
matchesURLSelector :: Selector '[Id NSURL] Bool
matchesURLSelector = mkSelector "matchesURL:"

-- | @Selector@ for @matchesURL:options:@
matchesURL_optionsSelector :: Selector '[Id NSURL, WKWebExtensionMatchPatternOptions] Bool
matchesURL_optionsSelector = mkSelector "matchesURL:options:"

-- | @Selector@ for @matchesPattern:@
matchesPatternSelector :: Selector '[Id WKWebExtensionMatchPattern] Bool
matchesPatternSelector = mkSelector "matchesPattern:"

-- | @Selector@ for @matchesPattern:options:@
matchesPattern_optionsSelector :: Selector '[Id WKWebExtensionMatchPattern, WKWebExtensionMatchPatternOptions] Bool
matchesPattern_optionsSelector = mkSelector "matchesPattern:options:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @scheme@
schemeSelector :: Selector '[] (Id NSString)
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @matchesAllURLs@
matchesAllURLsSelector :: Selector '[] Bool
matchesAllURLsSelector = mkSelector "matchesAllURLs"

-- | @Selector@ for @matchesAllHosts@
matchesAllHostsSelector :: Selector '[] Bool
matchesAllHostsSelector = mkSelector "matchesAllHosts"

