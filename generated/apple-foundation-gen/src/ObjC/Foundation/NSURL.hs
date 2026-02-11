{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURL@.
module ObjC.Foundation.NSURL
  ( NSURL
  , IsNSURL(..)
  , initWithScheme_host_path
  , initFileURLWithPath_isDirectory_relativeToURL
  , initFileURLWithPath_relativeToURL
  , initFileURLWithPath_isDirectory
  , initFileURLWithPath
  , fileURLWithPath_isDirectory_relativeToURL
  , fileURLWithPath_relativeToURL
  , fileURLWithPath_isDirectory
  , fileURLWithPath
  , initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL
  , fileURLWithFileSystemRepresentation_isDirectory_relativeToURL
  , initWithString
  , initWithString_relativeToURL
  , urlWithString
  , urlWithString_relativeToURL
  , initWithString_encodingInvalidCharacters
  , urlWithString_encodingInvalidCharacters
  , initWithDataRepresentation_relativeToURL
  , urlWithDataRepresentation_relativeToURL
  , initAbsoluteURLWithDataRepresentation_relativeToURL
  , absoluteURLWithDataRepresentation_relativeToURL
  , getFileSystemRepresentation_maxLength
  , isFileReferenceURL
  , fileReferenceURL
  , getResourceValue_forKey_error
  , resourceValuesForKeys_error
  , setResourceValue_forKey_error
  , setResourceValues_error
  , removeCachedResourceValueForKey
  , removeAllCachedResourceValues
  , setTemporaryResourceValue_forKey
  , bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error
  , initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error
  , urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error
  , resourceValuesForKeys_fromBookmarkData
  , writeBookmarkData_toURL_options_error
  , bookmarkDataWithContentsOfURL_error
  , urlByResolvingAliasFileAtURL_options_error
  , startAccessingSecurityScopedResource
  , stopAccessingSecurityScopedResource
  , resourceDataUsingCache
  , loadResourceDataNotifyingClient_usingCache
  , propertyForKey
  , setResourceData
  , setProperty_forKey
  , urlHandleUsingCache
  , fileURLWithPathComponents
  , urlByAppendingPathComponent
  , urlByAppendingPathComponent_isDirectory
  , urlByAppendingPathExtension
  , checkResourceIsReachableAndReturnError
  , getPromisedItemResourceValue_forKey_error
  , promisedItemResourceValuesForKeys_error
  , checkPromisedItemIsReachableAndReturnError
  , dataRepresentation
  , absoluteString
  , relativeString
  , baseURL
  , absoluteURL
  , scheme
  , resourceSpecifier
  , host
  , port
  , user
  , password
  , path
  , fragment
  , parameterString
  , query
  , relativePath
  , hasDirectoryPath
  , fileSystemRepresentation
  , fileURL
  , standardizedURL
  , filePathURL
  , pathComponents
  , lastPathComponent
  , pathExtension
  , urlByDeletingLastPathComponent
  , urlByDeletingPathExtension
  , urlByStandardizingPath
  , urlByResolvingSymlinksInPath
  , initWithScheme_host_pathSelector
  , initFileURLWithPath_isDirectory_relativeToURLSelector
  , initFileURLWithPath_relativeToURLSelector
  , initFileURLWithPath_isDirectorySelector
  , initFileURLWithPathSelector
  , fileURLWithPath_isDirectory_relativeToURLSelector
  , fileURLWithPath_relativeToURLSelector
  , fileURLWithPath_isDirectorySelector
  , fileURLWithPathSelector
  , initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector
  , fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector
  , initWithStringSelector
  , initWithString_relativeToURLSelector
  , urlWithStringSelector
  , urlWithString_relativeToURLSelector
  , initWithString_encodingInvalidCharactersSelector
  , urlWithString_encodingInvalidCharactersSelector
  , initWithDataRepresentation_relativeToURLSelector
  , urlWithDataRepresentation_relativeToURLSelector
  , initAbsoluteURLWithDataRepresentation_relativeToURLSelector
  , absoluteURLWithDataRepresentation_relativeToURLSelector
  , getFileSystemRepresentation_maxLengthSelector
  , isFileReferenceURLSelector
  , fileReferenceURLSelector
  , getResourceValue_forKey_errorSelector
  , resourceValuesForKeys_errorSelector
  , setResourceValue_forKey_errorSelector
  , setResourceValues_errorSelector
  , removeCachedResourceValueForKeySelector
  , removeAllCachedResourceValuesSelector
  , setTemporaryResourceValue_forKeySelector
  , bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector
  , initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector
  , urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector
  , resourceValuesForKeys_fromBookmarkDataSelector
  , writeBookmarkData_toURL_options_errorSelector
  , bookmarkDataWithContentsOfURL_errorSelector
  , urlByResolvingAliasFileAtURL_options_errorSelector
  , startAccessingSecurityScopedResourceSelector
  , stopAccessingSecurityScopedResourceSelector
  , resourceDataUsingCacheSelector
  , loadResourceDataNotifyingClient_usingCacheSelector
  , propertyForKeySelector
  , setResourceDataSelector
  , setProperty_forKeySelector
  , urlHandleUsingCacheSelector
  , fileURLWithPathComponentsSelector
  , urlByAppendingPathComponentSelector
  , urlByAppendingPathComponent_isDirectorySelector
  , urlByAppendingPathExtensionSelector
  , checkResourceIsReachableAndReturnErrorSelector
  , getPromisedItemResourceValue_forKey_errorSelector
  , promisedItemResourceValuesForKeys_errorSelector
  , checkPromisedItemIsReachableAndReturnErrorSelector
  , dataRepresentationSelector
  , absoluteStringSelector
  , relativeStringSelector
  , baseURLSelector
  , absoluteURLSelector
  , schemeSelector
  , resourceSpecifierSelector
  , hostSelector
  , portSelector
  , userSelector
  , passwordSelector
  , pathSelector
  , fragmentSelector
  , parameterStringSelector
  , querySelector
  , relativePathSelector
  , hasDirectoryPathSelector
  , fileSystemRepresentationSelector
  , fileURLSelector
  , standardizedURLSelector
  , filePathURLSelector
  , pathComponentsSelector
  , lastPathComponentSelector
  , pathExtensionSelector
  , urlByDeletingLastPathComponentSelector
  , urlByDeletingPathExtensionSelector
  , urlByStandardizingPathSelector
  , urlByResolvingSymlinksInPathSelector

  -- * Enum types
  , NSURLBookmarkCreationOptions(NSURLBookmarkCreationOptions)
  , pattern NSURLBookmarkCreationPreferFileIDResolution
  , pattern NSURLBookmarkCreationMinimalBookmark
  , pattern NSURLBookmarkCreationSuitableForBookmarkFile
  , pattern NSURLBookmarkCreationWithSecurityScope
  , pattern NSURLBookmarkCreationSecurityScopeAllowOnlyReadAccess
  , pattern NSURLBookmarkCreationWithoutImplicitSecurityScope
  , NSURLBookmarkResolutionOptions(NSURLBookmarkResolutionOptions)
  , pattern NSURLBookmarkResolutionWithoutUI
  , pattern NSURLBookmarkResolutionWithoutMounting
  , pattern NSURLBookmarkResolutionWithSecurityScope
  , pattern NSURLBookmarkResolutionWithoutImplicitStartAccessing

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithScheme:host:path:@
initWithScheme_host_path :: (IsNSURL nsurl, IsNSString scheme, IsNSString host, IsNSString path) => nsurl -> scheme -> host -> path -> IO (Id NSURL)
initWithScheme_host_path nsurl  scheme host path =
  withObjCPtr scheme $ \raw_scheme ->
    withObjCPtr host $ \raw_host ->
      withObjCPtr path $ \raw_path ->
          sendMsg nsurl (mkSelector "initWithScheme:host:path:") (retPtr retVoid) [argPtr (castPtr raw_scheme :: Ptr ()), argPtr (castPtr raw_host :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- initFileURLWithPath:isDirectory:relativeToURL:@
initFileURLWithPath_isDirectory_relativeToURL :: (IsNSURL nsurl, IsNSString path, IsNSURL baseURL) => nsurl -> path -> Bool -> baseURL -> IO (Id NSURL)
initFileURLWithPath_isDirectory_relativeToURL nsurl  path isDir baseURL =
  withObjCPtr path $ \raw_path ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg nsurl (mkSelector "initFileURLWithPath:isDirectory:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if isDir then 1 else 0), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- initFileURLWithPath:relativeToURL:@
initFileURLWithPath_relativeToURL :: (IsNSURL nsurl, IsNSString path, IsNSURL baseURL) => nsurl -> path -> baseURL -> IO (Id NSURL)
initFileURLWithPath_relativeToURL nsurl  path baseURL =
  withObjCPtr path $ \raw_path ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg nsurl (mkSelector "initFileURLWithPath:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- initFileURLWithPath:isDirectory:@
initFileURLWithPath_isDirectory :: (IsNSURL nsurl, IsNSString path) => nsurl -> path -> Bool -> IO (Id NSURL)
initFileURLWithPath_isDirectory nsurl  path isDir =
  withObjCPtr path $ \raw_path ->
      sendMsg nsurl (mkSelector "initFileURLWithPath:isDirectory:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if isDir then 1 else 0)] >>= ownedObject . castPtr

-- | @- initFileURLWithPath:@
initFileURLWithPath :: (IsNSURL nsurl, IsNSString path) => nsurl -> path -> IO (Id NSURL)
initFileURLWithPath nsurl  path =
  withObjCPtr path $ \raw_path ->
      sendMsg nsurl (mkSelector "initFileURLWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @+ fileURLWithPath:isDirectory:relativeToURL:@
fileURLWithPath_isDirectory_relativeToURL :: (IsNSString path, IsNSURL baseURL) => path -> Bool -> baseURL -> IO (Id NSURL)
fileURLWithPath_isDirectory_relativeToURL path isDir baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr path $ \raw_path ->
      withObjCPtr baseURL $ \raw_baseURL ->
        sendClassMsg cls' (mkSelector "fileURLWithPath:isDirectory:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if isDir then 1 else 0), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileURLWithPath:relativeToURL:@
fileURLWithPath_relativeToURL :: (IsNSString path, IsNSURL baseURL) => path -> baseURL -> IO (Id NSURL)
fileURLWithPath_relativeToURL path baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr path $ \raw_path ->
      withObjCPtr baseURL $ \raw_baseURL ->
        sendClassMsg cls' (mkSelector "fileURLWithPath:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fileURLWithPath:isDirectory:@
fileURLWithPath_isDirectory :: IsNSString path => path -> Bool -> IO (Id NSURL)
fileURLWithPath_isDirectory path isDir =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileURLWithPath:isDirectory:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if isDir then 1 else 0)] >>= retainedObject . castPtr

-- | @+ fileURLWithPath:@
fileURLWithPath :: IsNSString path => path -> IO (Id NSURL)
fileURLWithPath path =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileURLWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL :: (IsNSURL nsurl, IsNSURL baseURL) => nsurl -> Const (Ptr CChar) -> Bool -> baseURL -> IO (Id NSURL)
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL nsurl  path isDir baseURL =
  withObjCPtr baseURL $ \raw_baseURL ->
      sendMsg nsurl (mkSelector "initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:") (retPtr retVoid) [argPtr (unConst path), argCULong (if isDir then 1 else 0), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @+ fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
fileURLWithFileSystemRepresentation_isDirectory_relativeToURL :: IsNSURL baseURL => Const (Ptr CChar) -> Bool -> baseURL -> IO (Id NSURL)
fileURLWithFileSystemRepresentation_isDirectory_relativeToURL path isDir baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr baseURL $ \raw_baseURL ->
      sendClassMsg cls' (mkSelector "fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:") (retPtr retVoid) [argPtr (unConst path), argCULong (if isDir then 1 else 0), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSURL nsurl, IsNSString urlString) => nsurl -> urlString -> IO (Id NSURL)
initWithString nsurl  urlString =
  withObjCPtr urlString $ \raw_urlString ->
      sendMsg nsurl (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithString:relativeToURL:@
initWithString_relativeToURL :: (IsNSURL nsurl, IsNSString urlString, IsNSURL baseURL) => nsurl -> urlString -> baseURL -> IO (Id NSURL)
initWithString_relativeToURL nsurl  urlString baseURL =
  withObjCPtr urlString $ \raw_urlString ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg nsurl (mkSelector "initWithString:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @+ URLWithString:@
urlWithString :: IsNSString urlString => urlString -> IO (Id NSURL)
urlWithString urlString =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr urlString $ \raw_urlString ->
      sendClassMsg cls' (mkSelector "URLWithString:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ URLWithString:relativeToURL:@
urlWithString_relativeToURL :: (IsNSString urlString, IsNSURL baseURL) => urlString -> baseURL -> IO (Id NSURL)
urlWithString_relativeToURL urlString baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr urlString $ \raw_urlString ->
      withObjCPtr baseURL $ \raw_baseURL ->
        sendClassMsg cls' (mkSelector "URLWithString:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an @NSURL@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURL@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURL@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURL@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @- initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharacters :: (IsNSURL nsurl, IsNSString urlString) => nsurl -> urlString -> Bool -> IO (Id NSURL)
initWithString_encodingInvalidCharacters nsurl  urlString encodingInvalidCharacters =
  withObjCPtr urlString $ \raw_urlString ->
      sendMsg nsurl (mkSelector "initWithString:encodingInvalidCharacters:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argCULong (if encodingInvalidCharacters then 1 else 0)] >>= ownedObject . castPtr

-- | Initializes and returns a newly created @NSURL@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURL@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURL@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURL@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @+ URLWithString:encodingInvalidCharacters:@
urlWithString_encodingInvalidCharacters :: IsNSString urlString => urlString -> Bool -> IO (Id NSURL)
urlWithString_encodingInvalidCharacters urlString encodingInvalidCharacters =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr urlString $ \raw_urlString ->
      sendClassMsg cls' (mkSelector "URLWithString:encodingInvalidCharacters:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argCULong (if encodingInvalidCharacters then 1 else 0)] >>= retainedObject . castPtr

-- | @- initWithDataRepresentation:relativeToURL:@
initWithDataRepresentation_relativeToURL :: (IsNSURL nsurl, IsNSData data_, IsNSURL baseURL) => nsurl -> data_ -> baseURL -> IO (Id NSURL)
initWithDataRepresentation_relativeToURL nsurl  data_ baseURL =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg nsurl (mkSelector "initWithDataRepresentation:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @+ URLWithDataRepresentation:relativeToURL:@
urlWithDataRepresentation_relativeToURL :: (IsNSData data_, IsNSURL baseURL) => data_ -> baseURL -> IO (Id NSURL)
urlWithDataRepresentation_relativeToURL data_ baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr baseURL $ \raw_baseURL ->
        sendClassMsg cls' (mkSelector "URLWithDataRepresentation:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- initAbsoluteURLWithDataRepresentation:relativeToURL:@
initAbsoluteURLWithDataRepresentation_relativeToURL :: (IsNSURL nsurl, IsNSData data_, IsNSURL baseURL) => nsurl -> data_ -> baseURL -> IO (Id NSURL)
initAbsoluteURLWithDataRepresentation_relativeToURL nsurl  data_ baseURL =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr baseURL $ \raw_baseURL ->
        sendMsg nsurl (mkSelector "initAbsoluteURLWithDataRepresentation:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= ownedObject . castPtr

-- | @+ absoluteURLWithDataRepresentation:relativeToURL:@
absoluteURLWithDataRepresentation_relativeToURL :: (IsNSData data_, IsNSURL baseURL) => data_ -> baseURL -> IO (Id NSURL)
absoluteURLWithDataRepresentation_relativeToURL data_ baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr baseURL $ \raw_baseURL ->
        sendClassMsg cls' (mkSelector "absoluteURLWithDataRepresentation:relativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLength :: IsNSURL nsurl => nsurl -> Ptr CChar -> CULong -> IO Bool
getFileSystemRepresentation_maxLength nsurl  buffer maxBufferLength =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "getFileSystemRepresentation:maxLength:") retCULong [argPtr buffer, argCULong maxBufferLength]

-- | @- isFileReferenceURL@
isFileReferenceURL :: IsNSURL nsurl => nsurl -> IO Bool
isFileReferenceURL nsurl  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "isFileReferenceURL") retCULong []

-- | @- fileReferenceURL@
fileReferenceURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
fileReferenceURL nsurl  =
    sendMsg nsurl (mkSelector "fileReferenceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getResourceValue:forKey:error:@
getResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> Ptr RawId -> key -> error_ -> IO Bool
getResourceValue_forKey_error nsurl  value key error_ =
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "getResourceValue:forKey:error:") retCULong [argPtr value, argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- resourceValuesForKeys:error:@
resourceValuesForKeys_error :: (IsNSURL nsurl, IsNSArray keys, IsNSError error_) => nsurl -> keys -> error_ -> IO (Id NSDictionary)
resourceValuesForKeys_error nsurl  keys error_ =
  withObjCPtr keys $ \raw_keys ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsurl (mkSelector "resourceValuesForKeys:error:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- setResourceValue:forKey:error:@
setResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> RawId -> key -> error_ -> IO Bool
setResourceValue_forKey_error nsurl  value key error_ =
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "setResourceValue:forKey:error:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setResourceValues:error:@
setResourceValues_error :: (IsNSURL nsurl, IsNSDictionary keyedValues, IsNSError error_) => nsurl -> keyedValues -> error_ -> IO Bool
setResourceValues_error nsurl  keyedValues error_ =
  withObjCPtr keyedValues $ \raw_keyedValues ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "setResourceValues:error:") retCULong [argPtr (castPtr raw_keyedValues :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- removeCachedResourceValueForKey:@
removeCachedResourceValueForKey :: (IsNSURL nsurl, IsNSString key) => nsurl -> key -> IO ()
removeCachedResourceValueForKey nsurl  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsurl (mkSelector "removeCachedResourceValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- removeAllCachedResourceValues@
removeAllCachedResourceValues :: IsNSURL nsurl => nsurl -> IO ()
removeAllCachedResourceValues nsurl  =
    sendMsg nsurl (mkSelector "removeAllCachedResourceValues") retVoid []

-- | @- setTemporaryResourceValue:forKey:@
setTemporaryResourceValue_forKey :: (IsNSURL nsurl, IsNSString key) => nsurl -> RawId -> key -> IO ()
setTemporaryResourceValue_forKey nsurl  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsurl (mkSelector "setTemporaryResourceValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:@
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error :: (IsNSURL nsurl, IsNSArray keys, IsNSURL relativeURL, IsNSError error_) => nsurl -> NSURLBookmarkCreationOptions -> keys -> relativeURL -> error_ -> IO (Id NSData)
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error nsurl  options keys relativeURL error_ =
  withObjCPtr keys $ \raw_keys ->
    withObjCPtr relativeURL $ \raw_relativeURL ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsurl (mkSelector "bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:") (retPtr retVoid) [argCULong (coerce options), argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_relativeURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error :: (IsNSURL nsurl, IsNSData bookmarkData, IsNSURL relativeURL, IsNSError error_) => nsurl -> bookmarkData -> NSURLBookmarkResolutionOptions -> relativeURL -> Ptr Bool -> error_ -> IO (Id NSURL)
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error nsurl  bookmarkData options relativeURL isStale error_ =
  withObjCPtr bookmarkData $ \raw_bookmarkData ->
    withObjCPtr relativeURL $ \raw_relativeURL ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsurl (mkSelector "initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:") (retPtr retVoid) [argPtr (castPtr raw_bookmarkData :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_relativeURL :: Ptr ()), argPtr isStale, argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error :: (IsNSData bookmarkData, IsNSURL relativeURL, IsNSError error_) => bookmarkData -> NSURLBookmarkResolutionOptions -> relativeURL -> Ptr Bool -> error_ -> IO (Id NSURL)
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error bookmarkData options relativeURL isStale error_ =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr bookmarkData $ \raw_bookmarkData ->
      withObjCPtr relativeURL $ \raw_relativeURL ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:") (retPtr retVoid) [argPtr (castPtr raw_bookmarkData :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_relativeURL :: Ptr ()), argPtr isStale, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ resourceValuesForKeys:fromBookmarkData:@
resourceValuesForKeys_fromBookmarkData :: (IsNSArray keys, IsNSData bookmarkData) => keys -> bookmarkData -> IO (Id NSDictionary)
resourceValuesForKeys_fromBookmarkData keys bookmarkData =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr keys $ \raw_keys ->
      withObjCPtr bookmarkData $ \raw_bookmarkData ->
        sendClassMsg cls' (mkSelector "resourceValuesForKeys:fromBookmarkData:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_bookmarkData :: Ptr ())] >>= retainedObject . castPtr

-- | @+ writeBookmarkData:toURL:options:error:@
writeBookmarkData_toURL_options_error :: (IsNSData bookmarkData, IsNSURL bookmarkFileURL, IsNSError error_) => bookmarkData -> bookmarkFileURL -> CULong -> error_ -> IO Bool
writeBookmarkData_toURL_options_error bookmarkData bookmarkFileURL options error_ =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr bookmarkData $ \raw_bookmarkData ->
      withObjCPtr bookmarkFileURL $ \raw_bookmarkFileURL ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "writeBookmarkData:toURL:options:error:") retCULong [argPtr (castPtr raw_bookmarkData :: Ptr ()), argPtr (castPtr raw_bookmarkFileURL :: Ptr ()), argCULong options, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ bookmarkDataWithContentsOfURL:error:@
bookmarkDataWithContentsOfURL_error :: (IsNSURL bookmarkFileURL, IsNSError error_) => bookmarkFileURL -> error_ -> IO (Id NSData)
bookmarkDataWithContentsOfURL_error bookmarkFileURL error_ =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr bookmarkFileURL $ \raw_bookmarkFileURL ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "bookmarkDataWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_bookmarkFileURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ URLByResolvingAliasFileAtURL:options:error:@
urlByResolvingAliasFileAtURL_options_error :: (IsNSURL url, IsNSError error_) => url -> NSURLBookmarkResolutionOptions -> error_ -> IO (Id NSURL)
urlByResolvingAliasFileAtURL_options_error url options error_ =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "URLByResolvingAliasFileAtURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- startAccessingSecurityScopedResource@
startAccessingSecurityScopedResource :: IsNSURL nsurl => nsurl -> IO Bool
startAccessingSecurityScopedResource nsurl  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "startAccessingSecurityScopedResource") retCULong []

-- | @- stopAccessingSecurityScopedResource@
stopAccessingSecurityScopedResource :: IsNSURL nsurl => nsurl -> IO ()
stopAccessingSecurityScopedResource nsurl  =
    sendMsg nsurl (mkSelector "stopAccessingSecurityScopedResource") retVoid []

-- | @- resourceDataUsingCache:@
resourceDataUsingCache :: IsNSURL nsurl => nsurl -> Bool -> IO (Id NSData)
resourceDataUsingCache nsurl  shouldUseCache =
    sendMsg nsurl (mkSelector "resourceDataUsingCache:") (retPtr retVoid) [argCULong (if shouldUseCache then 1 else 0)] >>= retainedObject . castPtr

-- | @- loadResourceDataNotifyingClient:usingCache:@
loadResourceDataNotifyingClient_usingCache :: IsNSURL nsurl => nsurl -> RawId -> Bool -> IO ()
loadResourceDataNotifyingClient_usingCache nsurl  client shouldUseCache =
    sendMsg nsurl (mkSelector "loadResourceDataNotifyingClient:usingCache:") retVoid [argPtr (castPtr (unRawId client) :: Ptr ()), argCULong (if shouldUseCache then 1 else 0)]

-- | @- propertyForKey:@
propertyForKey :: (IsNSURL nsurl, IsNSString propertyKey) => nsurl -> propertyKey -> IO RawId
propertyForKey nsurl  propertyKey =
  withObjCPtr propertyKey $ \raw_propertyKey ->
      fmap (RawId . castPtr) $ sendMsg nsurl (mkSelector "propertyForKey:") (retPtr retVoid) [argPtr (castPtr raw_propertyKey :: Ptr ())]

-- | @- setResourceData:@
setResourceData :: (IsNSURL nsurl, IsNSData data_) => nsurl -> data_ -> IO Bool
setResourceData nsurl  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "setResourceData:") retCULong [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- setProperty:forKey:@
setProperty_forKey :: (IsNSURL nsurl, IsNSString propertyKey) => nsurl -> RawId -> propertyKey -> IO Bool
setProperty_forKey nsurl  property propertyKey =
  withObjCPtr propertyKey $ \raw_propertyKey ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "setProperty:forKey:") retCULong [argPtr (castPtr (unRawId property) :: Ptr ()), argPtr (castPtr raw_propertyKey :: Ptr ())]

-- | @- URLHandleUsingCache:@
urlHandleUsingCache :: IsNSURL nsurl => nsurl -> Bool -> IO (Id NSURLHandle)
urlHandleUsingCache nsurl  shouldUseCache =
    sendMsg nsurl (mkSelector "URLHandleUsingCache:") (retPtr retVoid) [argCULong (if shouldUseCache then 1 else 0)] >>= retainedObject . castPtr

-- | @+ fileURLWithPathComponents:@
fileURLWithPathComponents :: IsNSArray components => components -> IO (Id NSURL)
fileURLWithPathComponents components =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "fileURLWithPathComponents:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLByAppendingPathComponent:@
urlByAppendingPathComponent :: (IsNSURL nsurl, IsNSString pathComponent) => nsurl -> pathComponent -> IO (Id NSURL)
urlByAppendingPathComponent nsurl  pathComponent =
  withObjCPtr pathComponent $ \raw_pathComponent ->
      sendMsg nsurl (mkSelector "URLByAppendingPathComponent:") (retPtr retVoid) [argPtr (castPtr raw_pathComponent :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLByAppendingPathComponent:isDirectory:@
urlByAppendingPathComponent_isDirectory :: (IsNSURL nsurl, IsNSString pathComponent) => nsurl -> pathComponent -> Bool -> IO (Id NSURL)
urlByAppendingPathComponent_isDirectory nsurl  pathComponent isDirectory =
  withObjCPtr pathComponent $ \raw_pathComponent ->
      sendMsg nsurl (mkSelector "URLByAppendingPathComponent:isDirectory:") (retPtr retVoid) [argPtr (castPtr raw_pathComponent :: Ptr ()), argCULong (if isDirectory then 1 else 0)] >>= retainedObject . castPtr

-- | @- URLByAppendingPathExtension:@
urlByAppendingPathExtension :: (IsNSURL nsurl, IsNSString pathExtension) => nsurl -> pathExtension -> IO (Id NSURL)
urlByAppendingPathExtension nsurl  pathExtension =
  withObjCPtr pathExtension $ \raw_pathExtension ->
      sendMsg nsurl (mkSelector "URLByAppendingPathExtension:") (retPtr retVoid) [argPtr (castPtr raw_pathExtension :: Ptr ())] >>= retainedObject . castPtr

-- | @- checkResourceIsReachableAndReturnError:@
checkResourceIsReachableAndReturnError :: (IsNSURL nsurl, IsNSError error_) => nsurl -> error_ -> IO Bool
checkResourceIsReachableAndReturnError nsurl  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "checkResourceIsReachableAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- getPromisedItemResourceValue:forKey:error:@
getPromisedItemResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> Ptr RawId -> key -> error_ -> IO Bool
getPromisedItemResourceValue_forKey_error nsurl  value key error_ =
  withObjCPtr key $ \raw_key ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "getPromisedItemResourceValue:forKey:error:") retCULong [argPtr value, argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- promisedItemResourceValuesForKeys:error:@
promisedItemResourceValuesForKeys_error :: (IsNSURL nsurl, IsNSArray keys, IsNSError error_) => nsurl -> keys -> error_ -> IO (Id NSDictionary)
promisedItemResourceValuesForKeys_error nsurl  keys error_ =
  withObjCPtr keys $ \raw_keys ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsurl (mkSelector "promisedItemResourceValuesForKeys:error:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- checkPromisedItemIsReachableAndReturnError:@
checkPromisedItemIsReachableAndReturnError :: (IsNSURL nsurl, IsNSError error_) => nsurl -> error_ -> IO Bool
checkPromisedItemIsReachableAndReturnError nsurl  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "checkPromisedItemIsReachableAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- dataRepresentation@
dataRepresentation :: IsNSURL nsurl => nsurl -> IO (Id NSData)
dataRepresentation nsurl  =
    sendMsg nsurl (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteString@
absoluteString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
absoluteString nsurl  =
    sendMsg nsurl (mkSelector "absoluteString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativeString@
relativeString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
relativeString nsurl  =
    sendMsg nsurl (mkSelector "relativeString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- baseURL@
baseURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
baseURL nsurl  =
    sendMsg nsurl (mkSelector "baseURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteURL@
absoluteURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
absoluteURL nsurl  =
    sendMsg nsurl (mkSelector "absoluteURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scheme@
scheme :: IsNSURL nsurl => nsurl -> IO (Id NSString)
scheme nsurl  =
    sendMsg nsurl (mkSelector "scheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resourceSpecifier@
resourceSpecifier :: IsNSURL nsurl => nsurl -> IO (Id NSString)
resourceSpecifier nsurl  =
    sendMsg nsurl (mkSelector "resourceSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- host@
host :: IsNSURL nsurl => nsurl -> IO (Id NSString)
host nsurl  =
    sendMsg nsurl (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- port@
port :: IsNSURL nsurl => nsurl -> IO (Id NSNumber)
port nsurl  =
    sendMsg nsurl (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- user@
user :: IsNSURL nsurl => nsurl -> IO (Id NSString)
user nsurl  =
    sendMsg nsurl (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- password@
password :: IsNSURL nsurl => nsurl -> IO (Id NSString)
password nsurl  =
    sendMsg nsurl (mkSelector "password") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- path@
path :: IsNSURL nsurl => nsurl -> IO (Id NSString)
path nsurl  =
    sendMsg nsurl (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fragment@
fragment :: IsNSURL nsurl => nsurl -> IO (Id NSString)
fragment nsurl  =
    sendMsg nsurl (mkSelector "fragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parameterString@
parameterString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
parameterString nsurl  =
    sendMsg nsurl (mkSelector "parameterString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- query@
query :: IsNSURL nsurl => nsurl -> IO (Id NSString)
query nsurl  =
    sendMsg nsurl (mkSelector "query") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relativePath@
relativePath :: IsNSURL nsurl => nsurl -> IO (Id NSString)
relativePath nsurl  =
    sendMsg nsurl (mkSelector "relativePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasDirectoryPath@
hasDirectoryPath :: IsNSURL nsurl => nsurl -> IO Bool
hasDirectoryPath nsurl  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "hasDirectoryPath") retCULong []

-- | @- fileSystemRepresentation@
fileSystemRepresentation :: IsNSURL nsurl => nsurl -> IO (Ptr CChar)
fileSystemRepresentation nsurl  =
    fmap castPtr $ sendMsg nsurl (mkSelector "fileSystemRepresentation") (retPtr retVoid) []

-- | @- fileURL@
fileURL :: IsNSURL nsurl => nsurl -> IO Bool
fileURL nsurl  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurl (mkSelector "fileURL") retCULong []

-- | @- standardizedURL@
standardizedURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
standardizedURL nsurl  =
    sendMsg nsurl (mkSelector "standardizedURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- filePathURL@
filePathURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
filePathURL nsurl  =
    sendMsg nsurl (mkSelector "filePathURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathComponents@
pathComponents :: IsNSURL nsurl => nsurl -> IO (Id NSArray)
pathComponents nsurl  =
    sendMsg nsurl (mkSelector "pathComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastPathComponent@
lastPathComponent :: IsNSURL nsurl => nsurl -> IO (Id NSString)
lastPathComponent nsurl  =
    sendMsg nsurl (mkSelector "lastPathComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathExtension@
pathExtension :: IsNSURL nsurl => nsurl -> IO (Id NSString)
pathExtension nsurl  =
    sendMsg nsurl (mkSelector "pathExtension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URLByDeletingLastPathComponent@
urlByDeletingLastPathComponent :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByDeletingLastPathComponent nsurl  =
    sendMsg nsurl (mkSelector "URLByDeletingLastPathComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URLByDeletingPathExtension@
urlByDeletingPathExtension :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByDeletingPathExtension nsurl  =
    sendMsg nsurl (mkSelector "URLByDeletingPathExtension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URLByStandardizingPath@
urlByStandardizingPath :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByStandardizingPath nsurl  =
    sendMsg nsurl (mkSelector "URLByStandardizingPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URLByResolvingSymlinksInPath@
urlByResolvingSymlinksInPath :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByResolvingSymlinksInPath nsurl  =
    sendMsg nsurl (mkSelector "URLByResolvingSymlinksInPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScheme:host:path:@
initWithScheme_host_pathSelector :: Selector
initWithScheme_host_pathSelector = mkSelector "initWithScheme:host:path:"

-- | @Selector@ for @initFileURLWithPath:isDirectory:relativeToURL:@
initFileURLWithPath_isDirectory_relativeToURLSelector :: Selector
initFileURLWithPath_isDirectory_relativeToURLSelector = mkSelector "initFileURLWithPath:isDirectory:relativeToURL:"

-- | @Selector@ for @initFileURLWithPath:relativeToURL:@
initFileURLWithPath_relativeToURLSelector :: Selector
initFileURLWithPath_relativeToURLSelector = mkSelector "initFileURLWithPath:relativeToURL:"

-- | @Selector@ for @initFileURLWithPath:isDirectory:@
initFileURLWithPath_isDirectorySelector :: Selector
initFileURLWithPath_isDirectorySelector = mkSelector "initFileURLWithPath:isDirectory:"

-- | @Selector@ for @initFileURLWithPath:@
initFileURLWithPathSelector :: Selector
initFileURLWithPathSelector = mkSelector "initFileURLWithPath:"

-- | @Selector@ for @fileURLWithPath:isDirectory:relativeToURL:@
fileURLWithPath_isDirectory_relativeToURLSelector :: Selector
fileURLWithPath_isDirectory_relativeToURLSelector = mkSelector "fileURLWithPath:isDirectory:relativeToURL:"

-- | @Selector@ for @fileURLWithPath:relativeToURL:@
fileURLWithPath_relativeToURLSelector :: Selector
fileURLWithPath_relativeToURLSelector = mkSelector "fileURLWithPath:relativeToURL:"

-- | @Selector@ for @fileURLWithPath:isDirectory:@
fileURLWithPath_isDirectorySelector :: Selector
fileURLWithPath_isDirectorySelector = mkSelector "fileURLWithPath:isDirectory:"

-- | @Selector@ for @fileURLWithPath:@
fileURLWithPathSelector :: Selector
fileURLWithPathSelector = mkSelector "fileURLWithPath:"

-- | @Selector@ for @initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector :: Selector
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector = mkSelector "initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:"

-- | @Selector@ for @fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector :: Selector
fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector = mkSelector "fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:relativeToURL:@
initWithString_relativeToURLSelector :: Selector
initWithString_relativeToURLSelector = mkSelector "initWithString:relativeToURL:"

-- | @Selector@ for @URLWithString:@
urlWithStringSelector :: Selector
urlWithStringSelector = mkSelector "URLWithString:"

-- | @Selector@ for @URLWithString:relativeToURL:@
urlWithString_relativeToURLSelector :: Selector
urlWithString_relativeToURLSelector = mkSelector "URLWithString:relativeToURL:"

-- | @Selector@ for @initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharactersSelector :: Selector
initWithString_encodingInvalidCharactersSelector = mkSelector "initWithString:encodingInvalidCharacters:"

-- | @Selector@ for @URLWithString:encodingInvalidCharacters:@
urlWithString_encodingInvalidCharactersSelector :: Selector
urlWithString_encodingInvalidCharactersSelector = mkSelector "URLWithString:encodingInvalidCharacters:"

-- | @Selector@ for @initWithDataRepresentation:relativeToURL:@
initWithDataRepresentation_relativeToURLSelector :: Selector
initWithDataRepresentation_relativeToURLSelector = mkSelector "initWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @URLWithDataRepresentation:relativeToURL:@
urlWithDataRepresentation_relativeToURLSelector :: Selector
urlWithDataRepresentation_relativeToURLSelector = mkSelector "URLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @initAbsoluteURLWithDataRepresentation:relativeToURL:@
initAbsoluteURLWithDataRepresentation_relativeToURLSelector :: Selector
initAbsoluteURLWithDataRepresentation_relativeToURLSelector = mkSelector "initAbsoluteURLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @absoluteURLWithDataRepresentation:relativeToURL:@
absoluteURLWithDataRepresentation_relativeToURLSelector :: Selector
absoluteURLWithDataRepresentation_relativeToURLSelector = mkSelector "absoluteURLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLengthSelector :: Selector
getFileSystemRepresentation_maxLengthSelector = mkSelector "getFileSystemRepresentation:maxLength:"

-- | @Selector@ for @isFileReferenceURL@
isFileReferenceURLSelector :: Selector
isFileReferenceURLSelector = mkSelector "isFileReferenceURL"

-- | @Selector@ for @fileReferenceURL@
fileReferenceURLSelector :: Selector
fileReferenceURLSelector = mkSelector "fileReferenceURL"

-- | @Selector@ for @getResourceValue:forKey:error:@
getResourceValue_forKey_errorSelector :: Selector
getResourceValue_forKey_errorSelector = mkSelector "getResourceValue:forKey:error:"

-- | @Selector@ for @resourceValuesForKeys:error:@
resourceValuesForKeys_errorSelector :: Selector
resourceValuesForKeys_errorSelector = mkSelector "resourceValuesForKeys:error:"

-- | @Selector@ for @setResourceValue:forKey:error:@
setResourceValue_forKey_errorSelector :: Selector
setResourceValue_forKey_errorSelector = mkSelector "setResourceValue:forKey:error:"

-- | @Selector@ for @setResourceValues:error:@
setResourceValues_errorSelector :: Selector
setResourceValues_errorSelector = mkSelector "setResourceValues:error:"

-- | @Selector@ for @removeCachedResourceValueForKey:@
removeCachedResourceValueForKeySelector :: Selector
removeCachedResourceValueForKeySelector = mkSelector "removeCachedResourceValueForKey:"

-- | @Selector@ for @removeAllCachedResourceValues@
removeAllCachedResourceValuesSelector :: Selector
removeAllCachedResourceValuesSelector = mkSelector "removeAllCachedResourceValues"

-- | @Selector@ for @setTemporaryResourceValue:forKey:@
setTemporaryResourceValue_forKeySelector :: Selector
setTemporaryResourceValue_forKeySelector = mkSelector "setTemporaryResourceValue:forKey:"

-- | @Selector@ for @bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:@
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector :: Selector
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector = mkSelector "bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:"

-- | @Selector@ for @initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector :: Selector
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector = mkSelector "initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:"

-- | @Selector@ for @URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector :: Selector
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector = mkSelector "URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:"

-- | @Selector@ for @resourceValuesForKeys:fromBookmarkData:@
resourceValuesForKeys_fromBookmarkDataSelector :: Selector
resourceValuesForKeys_fromBookmarkDataSelector = mkSelector "resourceValuesForKeys:fromBookmarkData:"

-- | @Selector@ for @writeBookmarkData:toURL:options:error:@
writeBookmarkData_toURL_options_errorSelector :: Selector
writeBookmarkData_toURL_options_errorSelector = mkSelector "writeBookmarkData:toURL:options:error:"

-- | @Selector@ for @bookmarkDataWithContentsOfURL:error:@
bookmarkDataWithContentsOfURL_errorSelector :: Selector
bookmarkDataWithContentsOfURL_errorSelector = mkSelector "bookmarkDataWithContentsOfURL:error:"

-- | @Selector@ for @URLByResolvingAliasFileAtURL:options:error:@
urlByResolvingAliasFileAtURL_options_errorSelector :: Selector
urlByResolvingAliasFileAtURL_options_errorSelector = mkSelector "URLByResolvingAliasFileAtURL:options:error:"

-- | @Selector@ for @startAccessingSecurityScopedResource@
startAccessingSecurityScopedResourceSelector :: Selector
startAccessingSecurityScopedResourceSelector = mkSelector "startAccessingSecurityScopedResource"

-- | @Selector@ for @stopAccessingSecurityScopedResource@
stopAccessingSecurityScopedResourceSelector :: Selector
stopAccessingSecurityScopedResourceSelector = mkSelector "stopAccessingSecurityScopedResource"

-- | @Selector@ for @resourceDataUsingCache:@
resourceDataUsingCacheSelector :: Selector
resourceDataUsingCacheSelector = mkSelector "resourceDataUsingCache:"

-- | @Selector@ for @loadResourceDataNotifyingClient:usingCache:@
loadResourceDataNotifyingClient_usingCacheSelector :: Selector
loadResourceDataNotifyingClient_usingCacheSelector = mkSelector "loadResourceDataNotifyingClient:usingCache:"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @setResourceData:@
setResourceDataSelector :: Selector
setResourceDataSelector = mkSelector "setResourceData:"

-- | @Selector@ for @setProperty:forKey:@
setProperty_forKeySelector :: Selector
setProperty_forKeySelector = mkSelector "setProperty:forKey:"

-- | @Selector@ for @URLHandleUsingCache:@
urlHandleUsingCacheSelector :: Selector
urlHandleUsingCacheSelector = mkSelector "URLHandleUsingCache:"

-- | @Selector@ for @fileURLWithPathComponents:@
fileURLWithPathComponentsSelector :: Selector
fileURLWithPathComponentsSelector = mkSelector "fileURLWithPathComponents:"

-- | @Selector@ for @URLByAppendingPathComponent:@
urlByAppendingPathComponentSelector :: Selector
urlByAppendingPathComponentSelector = mkSelector "URLByAppendingPathComponent:"

-- | @Selector@ for @URLByAppendingPathComponent:isDirectory:@
urlByAppendingPathComponent_isDirectorySelector :: Selector
urlByAppendingPathComponent_isDirectorySelector = mkSelector "URLByAppendingPathComponent:isDirectory:"

-- | @Selector@ for @URLByAppendingPathExtension:@
urlByAppendingPathExtensionSelector :: Selector
urlByAppendingPathExtensionSelector = mkSelector "URLByAppendingPathExtension:"

-- | @Selector@ for @checkResourceIsReachableAndReturnError:@
checkResourceIsReachableAndReturnErrorSelector :: Selector
checkResourceIsReachableAndReturnErrorSelector = mkSelector "checkResourceIsReachableAndReturnError:"

-- | @Selector@ for @getPromisedItemResourceValue:forKey:error:@
getPromisedItemResourceValue_forKey_errorSelector :: Selector
getPromisedItemResourceValue_forKey_errorSelector = mkSelector "getPromisedItemResourceValue:forKey:error:"

-- | @Selector@ for @promisedItemResourceValuesForKeys:error:@
promisedItemResourceValuesForKeys_errorSelector :: Selector
promisedItemResourceValuesForKeys_errorSelector = mkSelector "promisedItemResourceValuesForKeys:error:"

-- | @Selector@ for @checkPromisedItemIsReachableAndReturnError:@
checkPromisedItemIsReachableAndReturnErrorSelector :: Selector
checkPromisedItemIsReachableAndReturnErrorSelector = mkSelector "checkPromisedItemIsReachableAndReturnError:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @absoluteString@
absoluteStringSelector :: Selector
absoluteStringSelector = mkSelector "absoluteString"

-- | @Selector@ for @relativeString@
relativeStringSelector :: Selector
relativeStringSelector = mkSelector "relativeString"

-- | @Selector@ for @baseURL@
baseURLSelector :: Selector
baseURLSelector = mkSelector "baseURL"

-- | @Selector@ for @absoluteURL@
absoluteURLSelector :: Selector
absoluteURLSelector = mkSelector "absoluteURL"

-- | @Selector@ for @scheme@
schemeSelector :: Selector
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @resourceSpecifier@
resourceSpecifierSelector :: Selector
resourceSpecifierSelector = mkSelector "resourceSpecifier"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @password@
passwordSelector :: Selector
passwordSelector = mkSelector "password"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @fragment@
fragmentSelector :: Selector
fragmentSelector = mkSelector "fragment"

-- | @Selector@ for @parameterString@
parameterStringSelector :: Selector
parameterStringSelector = mkSelector "parameterString"

-- | @Selector@ for @query@
querySelector :: Selector
querySelector = mkSelector "query"

-- | @Selector@ for @relativePath@
relativePathSelector :: Selector
relativePathSelector = mkSelector "relativePath"

-- | @Selector@ for @hasDirectoryPath@
hasDirectoryPathSelector :: Selector
hasDirectoryPathSelector = mkSelector "hasDirectoryPath"

-- | @Selector@ for @fileSystemRepresentation@
fileSystemRepresentationSelector :: Selector
fileSystemRepresentationSelector = mkSelector "fileSystemRepresentation"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @standardizedURL@
standardizedURLSelector :: Selector
standardizedURLSelector = mkSelector "standardizedURL"

-- | @Selector@ for @filePathURL@
filePathURLSelector :: Selector
filePathURLSelector = mkSelector "filePathURL"

-- | @Selector@ for @pathComponents@
pathComponentsSelector :: Selector
pathComponentsSelector = mkSelector "pathComponents"

-- | @Selector@ for @lastPathComponent@
lastPathComponentSelector :: Selector
lastPathComponentSelector = mkSelector "lastPathComponent"

-- | @Selector@ for @pathExtension@
pathExtensionSelector :: Selector
pathExtensionSelector = mkSelector "pathExtension"

-- | @Selector@ for @URLByDeletingLastPathComponent@
urlByDeletingLastPathComponentSelector :: Selector
urlByDeletingLastPathComponentSelector = mkSelector "URLByDeletingLastPathComponent"

-- | @Selector@ for @URLByDeletingPathExtension@
urlByDeletingPathExtensionSelector :: Selector
urlByDeletingPathExtensionSelector = mkSelector "URLByDeletingPathExtension"

-- | @Selector@ for @URLByStandardizingPath@
urlByStandardizingPathSelector :: Selector
urlByStandardizingPathSelector = mkSelector "URLByStandardizingPath"

-- | @Selector@ for @URLByResolvingSymlinksInPath@
urlByResolvingSymlinksInPathSelector :: Selector
urlByResolvingSymlinksInPathSelector = mkSelector "URLByResolvingSymlinksInPath"

