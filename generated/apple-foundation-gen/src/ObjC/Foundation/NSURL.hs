{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , absoluteStringSelector
  , absoluteURLSelector
  , absoluteURLWithDataRepresentation_relativeToURLSelector
  , baseURLSelector
  , bookmarkDataWithContentsOfURL_errorSelector
  , bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector
  , checkPromisedItemIsReachableAndReturnErrorSelector
  , checkResourceIsReachableAndReturnErrorSelector
  , dataRepresentationSelector
  , filePathURLSelector
  , fileReferenceURLSelector
  , fileSystemRepresentationSelector
  , fileURLSelector
  , fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector
  , fileURLWithPathComponentsSelector
  , fileURLWithPathSelector
  , fileURLWithPath_isDirectorySelector
  , fileURLWithPath_isDirectory_relativeToURLSelector
  , fileURLWithPath_relativeToURLSelector
  , fragmentSelector
  , getFileSystemRepresentation_maxLengthSelector
  , getPromisedItemResourceValue_forKey_errorSelector
  , getResourceValue_forKey_errorSelector
  , hasDirectoryPathSelector
  , hostSelector
  , initAbsoluteURLWithDataRepresentation_relativeToURLSelector
  , initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector
  , initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector
  , initFileURLWithPathSelector
  , initFileURLWithPath_isDirectorySelector
  , initFileURLWithPath_isDirectory_relativeToURLSelector
  , initFileURLWithPath_relativeToURLSelector
  , initWithDataRepresentation_relativeToURLSelector
  , initWithScheme_host_pathSelector
  , initWithStringSelector
  , initWithString_encodingInvalidCharactersSelector
  , initWithString_relativeToURLSelector
  , isFileReferenceURLSelector
  , lastPathComponentSelector
  , loadResourceDataNotifyingClient_usingCacheSelector
  , parameterStringSelector
  , passwordSelector
  , pathComponentsSelector
  , pathExtensionSelector
  , pathSelector
  , portSelector
  , promisedItemResourceValuesForKeys_errorSelector
  , propertyForKeySelector
  , querySelector
  , relativePathSelector
  , relativeStringSelector
  , removeAllCachedResourceValuesSelector
  , removeCachedResourceValueForKeySelector
  , resourceDataUsingCacheSelector
  , resourceSpecifierSelector
  , resourceValuesForKeys_errorSelector
  , resourceValuesForKeys_fromBookmarkDataSelector
  , schemeSelector
  , setProperty_forKeySelector
  , setResourceDataSelector
  , setResourceValue_forKey_errorSelector
  , setResourceValues_errorSelector
  , setTemporaryResourceValue_forKeySelector
  , standardizedURLSelector
  , startAccessingSecurityScopedResourceSelector
  , stopAccessingSecurityScopedResourceSelector
  , urlByAppendingPathComponentSelector
  , urlByAppendingPathComponent_isDirectorySelector
  , urlByAppendingPathExtensionSelector
  , urlByDeletingLastPathComponentSelector
  , urlByDeletingPathExtensionSelector
  , urlByResolvingAliasFileAtURL_options_errorSelector
  , urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector
  , urlByResolvingSymlinksInPathSelector
  , urlByStandardizingPathSelector
  , urlHandleUsingCacheSelector
  , urlWithDataRepresentation_relativeToURLSelector
  , urlWithStringSelector
  , urlWithString_encodingInvalidCharactersSelector
  , urlWithString_relativeToURLSelector
  , userSelector
  , writeBookmarkData_toURL_options_errorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithScheme:host:path:@
initWithScheme_host_path :: (IsNSURL nsurl, IsNSString scheme, IsNSString host, IsNSString path) => nsurl -> scheme -> host -> path -> IO (Id NSURL)
initWithScheme_host_path nsurl scheme host path =
  sendOwnedMessage nsurl initWithScheme_host_pathSelector (toNSString scheme) (toNSString host) (toNSString path)

-- | @- initFileURLWithPath:isDirectory:relativeToURL:@
initFileURLWithPath_isDirectory_relativeToURL :: (IsNSURL nsurl, IsNSString path, IsNSURL baseURL) => nsurl -> path -> Bool -> baseURL -> IO (Id NSURL)
initFileURLWithPath_isDirectory_relativeToURL nsurl path isDir baseURL =
  sendOwnedMessage nsurl initFileURLWithPath_isDirectory_relativeToURLSelector (toNSString path) isDir (toNSURL baseURL)

-- | @- initFileURLWithPath:relativeToURL:@
initFileURLWithPath_relativeToURL :: (IsNSURL nsurl, IsNSString path, IsNSURL baseURL) => nsurl -> path -> baseURL -> IO (Id NSURL)
initFileURLWithPath_relativeToURL nsurl path baseURL =
  sendOwnedMessage nsurl initFileURLWithPath_relativeToURLSelector (toNSString path) (toNSURL baseURL)

-- | @- initFileURLWithPath:isDirectory:@
initFileURLWithPath_isDirectory :: (IsNSURL nsurl, IsNSString path) => nsurl -> path -> Bool -> IO (Id NSURL)
initFileURLWithPath_isDirectory nsurl path isDir =
  sendOwnedMessage nsurl initFileURLWithPath_isDirectorySelector (toNSString path) isDir

-- | @- initFileURLWithPath:@
initFileURLWithPath :: (IsNSURL nsurl, IsNSString path) => nsurl -> path -> IO (Id NSURL)
initFileURLWithPath nsurl path =
  sendOwnedMessage nsurl initFileURLWithPathSelector (toNSString path)

-- | @+ fileURLWithPath:isDirectory:relativeToURL:@
fileURLWithPath_isDirectory_relativeToURL :: (IsNSString path, IsNSURL baseURL) => path -> Bool -> baseURL -> IO (Id NSURL)
fileURLWithPath_isDirectory_relativeToURL path isDir baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithPath_isDirectory_relativeToURLSelector (toNSString path) isDir (toNSURL baseURL)

-- | @+ fileURLWithPath:relativeToURL:@
fileURLWithPath_relativeToURL :: (IsNSString path, IsNSURL baseURL) => path -> baseURL -> IO (Id NSURL)
fileURLWithPath_relativeToURL path baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithPath_relativeToURLSelector (toNSString path) (toNSURL baseURL)

-- | @+ fileURLWithPath:isDirectory:@
fileURLWithPath_isDirectory :: IsNSString path => path -> Bool -> IO (Id NSURL)
fileURLWithPath_isDirectory path isDir =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithPath_isDirectorySelector (toNSString path) isDir

-- | @+ fileURLWithPath:@
fileURLWithPath :: IsNSString path => path -> IO (Id NSURL)
fileURLWithPath path =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithPathSelector (toNSString path)

-- | @- initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL :: (IsNSURL nsurl, IsNSURL baseURL) => nsurl -> Const (Ptr CChar) -> Bool -> baseURL -> IO (Id NSURL)
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURL nsurl path isDir baseURL =
  sendOwnedMessage nsurl initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector path isDir (toNSURL baseURL)

-- | @+ fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
fileURLWithFileSystemRepresentation_isDirectory_relativeToURL :: IsNSURL baseURL => Const (Ptr CChar) -> Bool -> baseURL -> IO (Id NSURL)
fileURLWithFileSystemRepresentation_isDirectory_relativeToURL path isDir baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector path isDir (toNSURL baseURL)

-- | @- initWithString:@
initWithString :: (IsNSURL nsurl, IsNSString urlString) => nsurl -> urlString -> IO (Id NSURL)
initWithString nsurl urlString =
  sendOwnedMessage nsurl initWithStringSelector (toNSString urlString)

-- | @- initWithString:relativeToURL:@
initWithString_relativeToURL :: (IsNSURL nsurl, IsNSString urlString, IsNSURL baseURL) => nsurl -> urlString -> baseURL -> IO (Id NSURL)
initWithString_relativeToURL nsurl urlString baseURL =
  sendOwnedMessage nsurl initWithString_relativeToURLSelector (toNSString urlString) (toNSURL baseURL)

-- | @+ URLWithString:@
urlWithString :: IsNSString urlString => urlString -> IO (Id NSURL)
urlWithString urlString =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlWithStringSelector (toNSString urlString)

-- | @+ URLWithString:relativeToURL:@
urlWithString_relativeToURL :: (IsNSString urlString, IsNSURL baseURL) => urlString -> baseURL -> IO (Id NSURL)
urlWithString_relativeToURL urlString baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlWithString_relativeToURLSelector (toNSString urlString) (toNSURL baseURL)

-- | Initializes an @NSURL@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURL@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURL@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURL@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @- initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharacters :: (IsNSURL nsurl, IsNSString urlString) => nsurl -> urlString -> Bool -> IO (Id NSURL)
initWithString_encodingInvalidCharacters nsurl urlString encodingInvalidCharacters =
  sendOwnedMessage nsurl initWithString_encodingInvalidCharactersSelector (toNSString urlString) encodingInvalidCharacters

-- | Initializes and returns a newly created @NSURL@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURL@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURL@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURL@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @+ URLWithString:encodingInvalidCharacters:@
urlWithString_encodingInvalidCharacters :: IsNSString urlString => urlString -> Bool -> IO (Id NSURL)
urlWithString_encodingInvalidCharacters urlString encodingInvalidCharacters =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlWithString_encodingInvalidCharactersSelector (toNSString urlString) encodingInvalidCharacters

-- | @- initWithDataRepresentation:relativeToURL:@
initWithDataRepresentation_relativeToURL :: (IsNSURL nsurl, IsNSData data_, IsNSURL baseURL) => nsurl -> data_ -> baseURL -> IO (Id NSURL)
initWithDataRepresentation_relativeToURL nsurl data_ baseURL =
  sendOwnedMessage nsurl initWithDataRepresentation_relativeToURLSelector (toNSData data_) (toNSURL baseURL)

-- | @+ URLWithDataRepresentation:relativeToURL:@
urlWithDataRepresentation_relativeToURL :: (IsNSData data_, IsNSURL baseURL) => data_ -> baseURL -> IO (Id NSURL)
urlWithDataRepresentation_relativeToURL data_ baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlWithDataRepresentation_relativeToURLSelector (toNSData data_) (toNSURL baseURL)

-- | @- initAbsoluteURLWithDataRepresentation:relativeToURL:@
initAbsoluteURLWithDataRepresentation_relativeToURL :: (IsNSURL nsurl, IsNSData data_, IsNSURL baseURL) => nsurl -> data_ -> baseURL -> IO (Id NSURL)
initAbsoluteURLWithDataRepresentation_relativeToURL nsurl data_ baseURL =
  sendOwnedMessage nsurl initAbsoluteURLWithDataRepresentation_relativeToURLSelector (toNSData data_) (toNSURL baseURL)

-- | @+ absoluteURLWithDataRepresentation:relativeToURL:@
absoluteURLWithDataRepresentation_relativeToURL :: (IsNSData data_, IsNSURL baseURL) => data_ -> baseURL -> IO (Id NSURL)
absoluteURLWithDataRepresentation_relativeToURL data_ baseURL =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' absoluteURLWithDataRepresentation_relativeToURLSelector (toNSData data_) (toNSURL baseURL)

-- | @- getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLength :: IsNSURL nsurl => nsurl -> Ptr CChar -> CULong -> IO Bool
getFileSystemRepresentation_maxLength nsurl buffer maxBufferLength =
  sendMessage nsurl getFileSystemRepresentation_maxLengthSelector buffer maxBufferLength

-- | @- isFileReferenceURL@
isFileReferenceURL :: IsNSURL nsurl => nsurl -> IO Bool
isFileReferenceURL nsurl =
  sendMessage nsurl isFileReferenceURLSelector

-- | @- fileReferenceURL@
fileReferenceURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
fileReferenceURL nsurl =
  sendMessage nsurl fileReferenceURLSelector

-- | @- getResourceValue:forKey:error:@
getResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> Ptr RawId -> key -> error_ -> IO Bool
getResourceValue_forKey_error nsurl value key error_ =
  sendMessage nsurl getResourceValue_forKey_errorSelector value (toNSString key) (toNSError error_)

-- | @- resourceValuesForKeys:error:@
resourceValuesForKeys_error :: (IsNSURL nsurl, IsNSArray keys, IsNSError error_) => nsurl -> keys -> error_ -> IO (Id NSDictionary)
resourceValuesForKeys_error nsurl keys error_ =
  sendMessage nsurl resourceValuesForKeys_errorSelector (toNSArray keys) (toNSError error_)

-- | @- setResourceValue:forKey:error:@
setResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> RawId -> key -> error_ -> IO Bool
setResourceValue_forKey_error nsurl value key error_ =
  sendMessage nsurl setResourceValue_forKey_errorSelector value (toNSString key) (toNSError error_)

-- | @- setResourceValues:error:@
setResourceValues_error :: (IsNSURL nsurl, IsNSDictionary keyedValues, IsNSError error_) => nsurl -> keyedValues -> error_ -> IO Bool
setResourceValues_error nsurl keyedValues error_ =
  sendMessage nsurl setResourceValues_errorSelector (toNSDictionary keyedValues) (toNSError error_)

-- | @- removeCachedResourceValueForKey:@
removeCachedResourceValueForKey :: (IsNSURL nsurl, IsNSString key) => nsurl -> key -> IO ()
removeCachedResourceValueForKey nsurl key =
  sendMessage nsurl removeCachedResourceValueForKeySelector (toNSString key)

-- | @- removeAllCachedResourceValues@
removeAllCachedResourceValues :: IsNSURL nsurl => nsurl -> IO ()
removeAllCachedResourceValues nsurl =
  sendMessage nsurl removeAllCachedResourceValuesSelector

-- | @- setTemporaryResourceValue:forKey:@
setTemporaryResourceValue_forKey :: (IsNSURL nsurl, IsNSString key) => nsurl -> RawId -> key -> IO ()
setTemporaryResourceValue_forKey nsurl value key =
  sendMessage nsurl setTemporaryResourceValue_forKeySelector value (toNSString key)

-- | @- bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:@
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error :: (IsNSURL nsurl, IsNSArray keys, IsNSURL relativeURL, IsNSError error_) => nsurl -> NSURLBookmarkCreationOptions -> keys -> relativeURL -> error_ -> IO (Id NSData)
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_error nsurl options keys relativeURL error_ =
  sendMessage nsurl bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector options (toNSArray keys) (toNSURL relativeURL) (toNSError error_)

-- | @- initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error :: (IsNSURL nsurl, IsNSData bookmarkData, IsNSURL relativeURL, IsNSError error_) => nsurl -> bookmarkData -> NSURLBookmarkResolutionOptions -> relativeURL -> Ptr Bool -> error_ -> IO (Id NSURL)
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error nsurl bookmarkData options relativeURL isStale error_ =
  sendOwnedMessage nsurl initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector (toNSData bookmarkData) options (toNSURL relativeURL) isStale (toNSError error_)

-- | @+ URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error :: (IsNSData bookmarkData, IsNSURL relativeURL, IsNSError error_) => bookmarkData -> NSURLBookmarkResolutionOptions -> relativeURL -> Ptr Bool -> error_ -> IO (Id NSURL)
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_error bookmarkData options relativeURL isStale error_ =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector (toNSData bookmarkData) options (toNSURL relativeURL) isStale (toNSError error_)

-- | @+ resourceValuesForKeys:fromBookmarkData:@
resourceValuesForKeys_fromBookmarkData :: (IsNSArray keys, IsNSData bookmarkData) => keys -> bookmarkData -> IO (Id NSDictionary)
resourceValuesForKeys_fromBookmarkData keys bookmarkData =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' resourceValuesForKeys_fromBookmarkDataSelector (toNSArray keys) (toNSData bookmarkData)

-- | @+ writeBookmarkData:toURL:options:error:@
writeBookmarkData_toURL_options_error :: (IsNSData bookmarkData, IsNSURL bookmarkFileURL, IsNSError error_) => bookmarkData -> bookmarkFileURL -> CULong -> error_ -> IO Bool
writeBookmarkData_toURL_options_error bookmarkData bookmarkFileURL options error_ =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' writeBookmarkData_toURL_options_errorSelector (toNSData bookmarkData) (toNSURL bookmarkFileURL) options (toNSError error_)

-- | @+ bookmarkDataWithContentsOfURL:error:@
bookmarkDataWithContentsOfURL_error :: (IsNSURL bookmarkFileURL, IsNSError error_) => bookmarkFileURL -> error_ -> IO (Id NSData)
bookmarkDataWithContentsOfURL_error bookmarkFileURL error_ =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' bookmarkDataWithContentsOfURL_errorSelector (toNSURL bookmarkFileURL) (toNSError error_)

-- | @+ URLByResolvingAliasFileAtURL:options:error:@
urlByResolvingAliasFileAtURL_options_error :: (IsNSURL url, IsNSError error_) => url -> NSURLBookmarkResolutionOptions -> error_ -> IO (Id NSURL)
urlByResolvingAliasFileAtURL_options_error url options error_ =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlByResolvingAliasFileAtURL_options_errorSelector (toNSURL url) options (toNSError error_)

-- | @- startAccessingSecurityScopedResource@
startAccessingSecurityScopedResource :: IsNSURL nsurl => nsurl -> IO Bool
startAccessingSecurityScopedResource nsurl =
  sendMessage nsurl startAccessingSecurityScopedResourceSelector

-- | @- stopAccessingSecurityScopedResource@
stopAccessingSecurityScopedResource :: IsNSURL nsurl => nsurl -> IO ()
stopAccessingSecurityScopedResource nsurl =
  sendMessage nsurl stopAccessingSecurityScopedResourceSelector

-- | @- resourceDataUsingCache:@
resourceDataUsingCache :: IsNSURL nsurl => nsurl -> Bool -> IO (Id NSData)
resourceDataUsingCache nsurl shouldUseCache =
  sendMessage nsurl resourceDataUsingCacheSelector shouldUseCache

-- | @- loadResourceDataNotifyingClient:usingCache:@
loadResourceDataNotifyingClient_usingCache :: IsNSURL nsurl => nsurl -> RawId -> Bool -> IO ()
loadResourceDataNotifyingClient_usingCache nsurl client shouldUseCache =
  sendMessage nsurl loadResourceDataNotifyingClient_usingCacheSelector client shouldUseCache

-- | @- propertyForKey:@
propertyForKey :: (IsNSURL nsurl, IsNSString propertyKey) => nsurl -> propertyKey -> IO RawId
propertyForKey nsurl propertyKey =
  sendMessage nsurl propertyForKeySelector (toNSString propertyKey)

-- | @- setResourceData:@
setResourceData :: (IsNSURL nsurl, IsNSData data_) => nsurl -> data_ -> IO Bool
setResourceData nsurl data_ =
  sendMessage nsurl setResourceDataSelector (toNSData data_)

-- | @- setProperty:forKey:@
setProperty_forKey :: (IsNSURL nsurl, IsNSString propertyKey) => nsurl -> RawId -> propertyKey -> IO Bool
setProperty_forKey nsurl property propertyKey =
  sendMessage nsurl setProperty_forKeySelector property (toNSString propertyKey)

-- | @- URLHandleUsingCache:@
urlHandleUsingCache :: IsNSURL nsurl => nsurl -> Bool -> IO (Id NSURLHandle)
urlHandleUsingCache nsurl shouldUseCache =
  sendMessage nsurl urlHandleUsingCacheSelector shouldUseCache

-- | @+ fileURLWithPathComponents:@
fileURLWithPathComponents :: IsNSArray components => components -> IO (Id NSURL)
fileURLWithPathComponents components =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' fileURLWithPathComponentsSelector (toNSArray components)

-- | @- URLByAppendingPathComponent:@
urlByAppendingPathComponent :: (IsNSURL nsurl, IsNSString pathComponent) => nsurl -> pathComponent -> IO (Id NSURL)
urlByAppendingPathComponent nsurl pathComponent =
  sendMessage nsurl urlByAppendingPathComponentSelector (toNSString pathComponent)

-- | @- URLByAppendingPathComponent:isDirectory:@
urlByAppendingPathComponent_isDirectory :: (IsNSURL nsurl, IsNSString pathComponent) => nsurl -> pathComponent -> Bool -> IO (Id NSURL)
urlByAppendingPathComponent_isDirectory nsurl pathComponent isDirectory =
  sendMessage nsurl urlByAppendingPathComponent_isDirectorySelector (toNSString pathComponent) isDirectory

-- | @- URLByAppendingPathExtension:@
urlByAppendingPathExtension :: (IsNSURL nsurl, IsNSString pathExtension) => nsurl -> pathExtension -> IO (Id NSURL)
urlByAppendingPathExtension nsurl pathExtension =
  sendMessage nsurl urlByAppendingPathExtensionSelector (toNSString pathExtension)

-- | @- checkResourceIsReachableAndReturnError:@
checkResourceIsReachableAndReturnError :: (IsNSURL nsurl, IsNSError error_) => nsurl -> error_ -> IO Bool
checkResourceIsReachableAndReturnError nsurl error_ =
  sendMessage nsurl checkResourceIsReachableAndReturnErrorSelector (toNSError error_)

-- | @- getPromisedItemResourceValue:forKey:error:@
getPromisedItemResourceValue_forKey_error :: (IsNSURL nsurl, IsNSString key, IsNSError error_) => nsurl -> Ptr RawId -> key -> error_ -> IO Bool
getPromisedItemResourceValue_forKey_error nsurl value key error_ =
  sendMessage nsurl getPromisedItemResourceValue_forKey_errorSelector value (toNSString key) (toNSError error_)

-- | @- promisedItemResourceValuesForKeys:error:@
promisedItemResourceValuesForKeys_error :: (IsNSURL nsurl, IsNSArray keys, IsNSError error_) => nsurl -> keys -> error_ -> IO (Id NSDictionary)
promisedItemResourceValuesForKeys_error nsurl keys error_ =
  sendMessage nsurl promisedItemResourceValuesForKeys_errorSelector (toNSArray keys) (toNSError error_)

-- | @- checkPromisedItemIsReachableAndReturnError:@
checkPromisedItemIsReachableAndReturnError :: (IsNSURL nsurl, IsNSError error_) => nsurl -> error_ -> IO Bool
checkPromisedItemIsReachableAndReturnError nsurl error_ =
  sendMessage nsurl checkPromisedItemIsReachableAndReturnErrorSelector (toNSError error_)

-- | @- dataRepresentation@
dataRepresentation :: IsNSURL nsurl => nsurl -> IO (Id NSData)
dataRepresentation nsurl =
  sendMessage nsurl dataRepresentationSelector

-- | @- absoluteString@
absoluteString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
absoluteString nsurl =
  sendMessage nsurl absoluteStringSelector

-- | @- relativeString@
relativeString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
relativeString nsurl =
  sendMessage nsurl relativeStringSelector

-- | @- baseURL@
baseURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
baseURL nsurl =
  sendMessage nsurl baseURLSelector

-- | @- absoluteURL@
absoluteURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
absoluteURL nsurl =
  sendMessage nsurl absoluteURLSelector

-- | @- scheme@
scheme :: IsNSURL nsurl => nsurl -> IO (Id NSString)
scheme nsurl =
  sendMessage nsurl schemeSelector

-- | @- resourceSpecifier@
resourceSpecifier :: IsNSURL nsurl => nsurl -> IO (Id NSString)
resourceSpecifier nsurl =
  sendMessage nsurl resourceSpecifierSelector

-- | @- host@
host :: IsNSURL nsurl => nsurl -> IO (Id NSString)
host nsurl =
  sendMessage nsurl hostSelector

-- | @- port@
port :: IsNSURL nsurl => nsurl -> IO (Id NSNumber)
port nsurl =
  sendMessage nsurl portSelector

-- | @- user@
user :: IsNSURL nsurl => nsurl -> IO (Id NSString)
user nsurl =
  sendMessage nsurl userSelector

-- | @- password@
password :: IsNSURL nsurl => nsurl -> IO (Id NSString)
password nsurl =
  sendMessage nsurl passwordSelector

-- | @- path@
path :: IsNSURL nsurl => nsurl -> IO (Id NSString)
path nsurl =
  sendMessage nsurl pathSelector

-- | @- fragment@
fragment :: IsNSURL nsurl => nsurl -> IO (Id NSString)
fragment nsurl =
  sendMessage nsurl fragmentSelector

-- | @- parameterString@
parameterString :: IsNSURL nsurl => nsurl -> IO (Id NSString)
parameterString nsurl =
  sendMessage nsurl parameterStringSelector

-- | @- query@
query :: IsNSURL nsurl => nsurl -> IO (Id NSString)
query nsurl =
  sendMessage nsurl querySelector

-- | @- relativePath@
relativePath :: IsNSURL nsurl => nsurl -> IO (Id NSString)
relativePath nsurl =
  sendMessage nsurl relativePathSelector

-- | @- hasDirectoryPath@
hasDirectoryPath :: IsNSURL nsurl => nsurl -> IO Bool
hasDirectoryPath nsurl =
  sendMessage nsurl hasDirectoryPathSelector

-- | @- fileSystemRepresentation@
fileSystemRepresentation :: IsNSURL nsurl => nsurl -> IO (Ptr CChar)
fileSystemRepresentation nsurl =
  sendMessage nsurl fileSystemRepresentationSelector

-- | @- fileURL@
fileURL :: IsNSURL nsurl => nsurl -> IO Bool
fileURL nsurl =
  sendMessage nsurl fileURLSelector

-- | @- standardizedURL@
standardizedURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
standardizedURL nsurl =
  sendMessage nsurl standardizedURLSelector

-- | @- filePathURL@
filePathURL :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
filePathURL nsurl =
  sendMessage nsurl filePathURLSelector

-- | @- pathComponents@
pathComponents :: IsNSURL nsurl => nsurl -> IO (Id NSArray)
pathComponents nsurl =
  sendMessage nsurl pathComponentsSelector

-- | @- lastPathComponent@
lastPathComponent :: IsNSURL nsurl => nsurl -> IO (Id NSString)
lastPathComponent nsurl =
  sendMessage nsurl lastPathComponentSelector

-- | @- pathExtension@
pathExtension :: IsNSURL nsurl => nsurl -> IO (Id NSString)
pathExtension nsurl =
  sendMessage nsurl pathExtensionSelector

-- | @- URLByDeletingLastPathComponent@
urlByDeletingLastPathComponent :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByDeletingLastPathComponent nsurl =
  sendMessage nsurl urlByDeletingLastPathComponentSelector

-- | @- URLByDeletingPathExtension@
urlByDeletingPathExtension :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByDeletingPathExtension nsurl =
  sendMessage nsurl urlByDeletingPathExtensionSelector

-- | @- URLByStandardizingPath@
urlByStandardizingPath :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByStandardizingPath nsurl =
  sendMessage nsurl urlByStandardizingPathSelector

-- | @- URLByResolvingSymlinksInPath@
urlByResolvingSymlinksInPath :: IsNSURL nsurl => nsurl -> IO (Id NSURL)
urlByResolvingSymlinksInPath nsurl =
  sendMessage nsurl urlByResolvingSymlinksInPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScheme:host:path:@
initWithScheme_host_pathSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSURL)
initWithScheme_host_pathSelector = mkSelector "initWithScheme:host:path:"

-- | @Selector@ for @initFileURLWithPath:isDirectory:relativeToURL:@
initFileURLWithPath_isDirectory_relativeToURLSelector :: Selector '[Id NSString, Bool, Id NSURL] (Id NSURL)
initFileURLWithPath_isDirectory_relativeToURLSelector = mkSelector "initFileURLWithPath:isDirectory:relativeToURL:"

-- | @Selector@ for @initFileURLWithPath:relativeToURL:@
initFileURLWithPath_relativeToURLSelector :: Selector '[Id NSString, Id NSURL] (Id NSURL)
initFileURLWithPath_relativeToURLSelector = mkSelector "initFileURLWithPath:relativeToURL:"

-- | @Selector@ for @initFileURLWithPath:isDirectory:@
initFileURLWithPath_isDirectorySelector :: Selector '[Id NSString, Bool] (Id NSURL)
initFileURLWithPath_isDirectorySelector = mkSelector "initFileURLWithPath:isDirectory:"

-- | @Selector@ for @initFileURLWithPath:@
initFileURLWithPathSelector :: Selector '[Id NSString] (Id NSURL)
initFileURLWithPathSelector = mkSelector "initFileURLWithPath:"

-- | @Selector@ for @fileURLWithPath:isDirectory:relativeToURL:@
fileURLWithPath_isDirectory_relativeToURLSelector :: Selector '[Id NSString, Bool, Id NSURL] (Id NSURL)
fileURLWithPath_isDirectory_relativeToURLSelector = mkSelector "fileURLWithPath:isDirectory:relativeToURL:"

-- | @Selector@ for @fileURLWithPath:relativeToURL:@
fileURLWithPath_relativeToURLSelector :: Selector '[Id NSString, Id NSURL] (Id NSURL)
fileURLWithPath_relativeToURLSelector = mkSelector "fileURLWithPath:relativeToURL:"

-- | @Selector@ for @fileURLWithPath:isDirectory:@
fileURLWithPath_isDirectorySelector :: Selector '[Id NSString, Bool] (Id NSURL)
fileURLWithPath_isDirectorySelector = mkSelector "fileURLWithPath:isDirectory:"

-- | @Selector@ for @fileURLWithPath:@
fileURLWithPathSelector :: Selector '[Id NSString] (Id NSURL)
fileURLWithPathSelector = mkSelector "fileURLWithPath:"

-- | @Selector@ for @initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector :: Selector '[Const (Ptr CChar), Bool, Id NSURL] (Id NSURL)
initFileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector = mkSelector "initFileURLWithFileSystemRepresentation:isDirectory:relativeToURL:"

-- | @Selector@ for @fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:@
fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector :: Selector '[Const (Ptr CChar), Bool, Id NSURL] (Id NSURL)
fileURLWithFileSystemRepresentation_isDirectory_relativeToURLSelector = mkSelector "fileURLWithFileSystemRepresentation:isDirectory:relativeToURL:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSURL)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:relativeToURL:@
initWithString_relativeToURLSelector :: Selector '[Id NSString, Id NSURL] (Id NSURL)
initWithString_relativeToURLSelector = mkSelector "initWithString:relativeToURL:"

-- | @Selector@ for @URLWithString:@
urlWithStringSelector :: Selector '[Id NSString] (Id NSURL)
urlWithStringSelector = mkSelector "URLWithString:"

-- | @Selector@ for @URLWithString:relativeToURL:@
urlWithString_relativeToURLSelector :: Selector '[Id NSString, Id NSURL] (Id NSURL)
urlWithString_relativeToURLSelector = mkSelector "URLWithString:relativeToURL:"

-- | @Selector@ for @initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharactersSelector :: Selector '[Id NSString, Bool] (Id NSURL)
initWithString_encodingInvalidCharactersSelector = mkSelector "initWithString:encodingInvalidCharacters:"

-- | @Selector@ for @URLWithString:encodingInvalidCharacters:@
urlWithString_encodingInvalidCharactersSelector :: Selector '[Id NSString, Bool] (Id NSURL)
urlWithString_encodingInvalidCharactersSelector = mkSelector "URLWithString:encodingInvalidCharacters:"

-- | @Selector@ for @initWithDataRepresentation:relativeToURL:@
initWithDataRepresentation_relativeToURLSelector :: Selector '[Id NSData, Id NSURL] (Id NSURL)
initWithDataRepresentation_relativeToURLSelector = mkSelector "initWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @URLWithDataRepresentation:relativeToURL:@
urlWithDataRepresentation_relativeToURLSelector :: Selector '[Id NSData, Id NSURL] (Id NSURL)
urlWithDataRepresentation_relativeToURLSelector = mkSelector "URLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @initAbsoluteURLWithDataRepresentation:relativeToURL:@
initAbsoluteURLWithDataRepresentation_relativeToURLSelector :: Selector '[Id NSData, Id NSURL] (Id NSURL)
initAbsoluteURLWithDataRepresentation_relativeToURLSelector = mkSelector "initAbsoluteURLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @absoluteURLWithDataRepresentation:relativeToURL:@
absoluteURLWithDataRepresentation_relativeToURLSelector :: Selector '[Id NSData, Id NSURL] (Id NSURL)
absoluteURLWithDataRepresentation_relativeToURLSelector = mkSelector "absoluteURLWithDataRepresentation:relativeToURL:"

-- | @Selector@ for @getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLengthSelector :: Selector '[Ptr CChar, CULong] Bool
getFileSystemRepresentation_maxLengthSelector = mkSelector "getFileSystemRepresentation:maxLength:"

-- | @Selector@ for @isFileReferenceURL@
isFileReferenceURLSelector :: Selector '[] Bool
isFileReferenceURLSelector = mkSelector "isFileReferenceURL"

-- | @Selector@ for @fileReferenceURL@
fileReferenceURLSelector :: Selector '[] (Id NSURL)
fileReferenceURLSelector = mkSelector "fileReferenceURL"

-- | @Selector@ for @getResourceValue:forKey:error:@
getResourceValue_forKey_errorSelector :: Selector '[Ptr RawId, Id NSString, Id NSError] Bool
getResourceValue_forKey_errorSelector = mkSelector "getResourceValue:forKey:error:"

-- | @Selector@ for @resourceValuesForKeys:error:@
resourceValuesForKeys_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSDictionary)
resourceValuesForKeys_errorSelector = mkSelector "resourceValuesForKeys:error:"

-- | @Selector@ for @setResourceValue:forKey:error:@
setResourceValue_forKey_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
setResourceValue_forKey_errorSelector = mkSelector "setResourceValue:forKey:error:"

-- | @Selector@ for @setResourceValues:error:@
setResourceValues_errorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setResourceValues_errorSelector = mkSelector "setResourceValues:error:"

-- | @Selector@ for @removeCachedResourceValueForKey:@
removeCachedResourceValueForKeySelector :: Selector '[Id NSString] ()
removeCachedResourceValueForKeySelector = mkSelector "removeCachedResourceValueForKey:"

-- | @Selector@ for @removeAllCachedResourceValues@
removeAllCachedResourceValuesSelector :: Selector '[] ()
removeAllCachedResourceValuesSelector = mkSelector "removeAllCachedResourceValues"

-- | @Selector@ for @setTemporaryResourceValue:forKey:@
setTemporaryResourceValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setTemporaryResourceValue_forKeySelector = mkSelector "setTemporaryResourceValue:forKey:"

-- | @Selector@ for @bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:@
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector :: Selector '[NSURLBookmarkCreationOptions, Id NSArray, Id NSURL, Id NSError] (Id NSData)
bookmarkDataWithOptions_includingResourceValuesForKeys_relativeToURL_errorSelector = mkSelector "bookmarkDataWithOptions:includingResourceValuesForKeys:relativeToURL:error:"

-- | @Selector@ for @initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector :: Selector '[Id NSData, NSURLBookmarkResolutionOptions, Id NSURL, Ptr Bool, Id NSError] (Id NSURL)
initByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector = mkSelector "initByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:"

-- | @Selector@ for @URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:@
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector :: Selector '[Id NSData, NSURLBookmarkResolutionOptions, Id NSURL, Ptr Bool, Id NSError] (Id NSURL)
urlByResolvingBookmarkData_options_relativeToURL_bookmarkDataIsStale_errorSelector = mkSelector "URLByResolvingBookmarkData:options:relativeToURL:bookmarkDataIsStale:error:"

-- | @Selector@ for @resourceValuesForKeys:fromBookmarkData:@
resourceValuesForKeys_fromBookmarkDataSelector :: Selector '[Id NSArray, Id NSData] (Id NSDictionary)
resourceValuesForKeys_fromBookmarkDataSelector = mkSelector "resourceValuesForKeys:fromBookmarkData:"

-- | @Selector@ for @writeBookmarkData:toURL:options:error:@
writeBookmarkData_toURL_options_errorSelector :: Selector '[Id NSData, Id NSURL, CULong, Id NSError] Bool
writeBookmarkData_toURL_options_errorSelector = mkSelector "writeBookmarkData:toURL:options:error:"

-- | @Selector@ for @bookmarkDataWithContentsOfURL:error:@
bookmarkDataWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSData)
bookmarkDataWithContentsOfURL_errorSelector = mkSelector "bookmarkDataWithContentsOfURL:error:"

-- | @Selector@ for @URLByResolvingAliasFileAtURL:options:error:@
urlByResolvingAliasFileAtURL_options_errorSelector :: Selector '[Id NSURL, NSURLBookmarkResolutionOptions, Id NSError] (Id NSURL)
urlByResolvingAliasFileAtURL_options_errorSelector = mkSelector "URLByResolvingAliasFileAtURL:options:error:"

-- | @Selector@ for @startAccessingSecurityScopedResource@
startAccessingSecurityScopedResourceSelector :: Selector '[] Bool
startAccessingSecurityScopedResourceSelector = mkSelector "startAccessingSecurityScopedResource"

-- | @Selector@ for @stopAccessingSecurityScopedResource@
stopAccessingSecurityScopedResourceSelector :: Selector '[] ()
stopAccessingSecurityScopedResourceSelector = mkSelector "stopAccessingSecurityScopedResource"

-- | @Selector@ for @resourceDataUsingCache:@
resourceDataUsingCacheSelector :: Selector '[Bool] (Id NSData)
resourceDataUsingCacheSelector = mkSelector "resourceDataUsingCache:"

-- | @Selector@ for @loadResourceDataNotifyingClient:usingCache:@
loadResourceDataNotifyingClient_usingCacheSelector :: Selector '[RawId, Bool] ()
loadResourceDataNotifyingClient_usingCacheSelector = mkSelector "loadResourceDataNotifyingClient:usingCache:"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector '[Id NSString] RawId
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @setResourceData:@
setResourceDataSelector :: Selector '[Id NSData] Bool
setResourceDataSelector = mkSelector "setResourceData:"

-- | @Selector@ for @setProperty:forKey:@
setProperty_forKeySelector :: Selector '[RawId, Id NSString] Bool
setProperty_forKeySelector = mkSelector "setProperty:forKey:"

-- | @Selector@ for @URLHandleUsingCache:@
urlHandleUsingCacheSelector :: Selector '[Bool] (Id NSURLHandle)
urlHandleUsingCacheSelector = mkSelector "URLHandleUsingCache:"

-- | @Selector@ for @fileURLWithPathComponents:@
fileURLWithPathComponentsSelector :: Selector '[Id NSArray] (Id NSURL)
fileURLWithPathComponentsSelector = mkSelector "fileURLWithPathComponents:"

-- | @Selector@ for @URLByAppendingPathComponent:@
urlByAppendingPathComponentSelector :: Selector '[Id NSString] (Id NSURL)
urlByAppendingPathComponentSelector = mkSelector "URLByAppendingPathComponent:"

-- | @Selector@ for @URLByAppendingPathComponent:isDirectory:@
urlByAppendingPathComponent_isDirectorySelector :: Selector '[Id NSString, Bool] (Id NSURL)
urlByAppendingPathComponent_isDirectorySelector = mkSelector "URLByAppendingPathComponent:isDirectory:"

-- | @Selector@ for @URLByAppendingPathExtension:@
urlByAppendingPathExtensionSelector :: Selector '[Id NSString] (Id NSURL)
urlByAppendingPathExtensionSelector = mkSelector "URLByAppendingPathExtension:"

-- | @Selector@ for @checkResourceIsReachableAndReturnError:@
checkResourceIsReachableAndReturnErrorSelector :: Selector '[Id NSError] Bool
checkResourceIsReachableAndReturnErrorSelector = mkSelector "checkResourceIsReachableAndReturnError:"

-- | @Selector@ for @getPromisedItemResourceValue:forKey:error:@
getPromisedItemResourceValue_forKey_errorSelector :: Selector '[Ptr RawId, Id NSString, Id NSError] Bool
getPromisedItemResourceValue_forKey_errorSelector = mkSelector "getPromisedItemResourceValue:forKey:error:"

-- | @Selector@ for @promisedItemResourceValuesForKeys:error:@
promisedItemResourceValuesForKeys_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSDictionary)
promisedItemResourceValuesForKeys_errorSelector = mkSelector "promisedItemResourceValuesForKeys:error:"

-- | @Selector@ for @checkPromisedItemIsReachableAndReturnError:@
checkPromisedItemIsReachableAndReturnErrorSelector :: Selector '[Id NSError] Bool
checkPromisedItemIsReachableAndReturnErrorSelector = mkSelector "checkPromisedItemIsReachableAndReturnError:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @absoluteString@
absoluteStringSelector :: Selector '[] (Id NSString)
absoluteStringSelector = mkSelector "absoluteString"

-- | @Selector@ for @relativeString@
relativeStringSelector :: Selector '[] (Id NSString)
relativeStringSelector = mkSelector "relativeString"

-- | @Selector@ for @baseURL@
baseURLSelector :: Selector '[] (Id NSURL)
baseURLSelector = mkSelector "baseURL"

-- | @Selector@ for @absoluteURL@
absoluteURLSelector :: Selector '[] (Id NSURL)
absoluteURLSelector = mkSelector "absoluteURL"

-- | @Selector@ for @scheme@
schemeSelector :: Selector '[] (Id NSString)
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @resourceSpecifier@
resourceSpecifierSelector :: Selector '[] (Id NSString)
resourceSpecifierSelector = mkSelector "resourceSpecifier"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSNumber)
portSelector = mkSelector "port"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @fragment@
fragmentSelector :: Selector '[] (Id NSString)
fragmentSelector = mkSelector "fragment"

-- | @Selector@ for @parameterString@
parameterStringSelector :: Selector '[] (Id NSString)
parameterStringSelector = mkSelector "parameterString"

-- | @Selector@ for @query@
querySelector :: Selector '[] (Id NSString)
querySelector = mkSelector "query"

-- | @Selector@ for @relativePath@
relativePathSelector :: Selector '[] (Id NSString)
relativePathSelector = mkSelector "relativePath"

-- | @Selector@ for @hasDirectoryPath@
hasDirectoryPathSelector :: Selector '[] Bool
hasDirectoryPathSelector = mkSelector "hasDirectoryPath"

-- | @Selector@ for @fileSystemRepresentation@
fileSystemRepresentationSelector :: Selector '[] (Ptr CChar)
fileSystemRepresentationSelector = mkSelector "fileSystemRepresentation"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] Bool
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @standardizedURL@
standardizedURLSelector :: Selector '[] (Id NSURL)
standardizedURLSelector = mkSelector "standardizedURL"

-- | @Selector@ for @filePathURL@
filePathURLSelector :: Selector '[] (Id NSURL)
filePathURLSelector = mkSelector "filePathURL"

-- | @Selector@ for @pathComponents@
pathComponentsSelector :: Selector '[] (Id NSArray)
pathComponentsSelector = mkSelector "pathComponents"

-- | @Selector@ for @lastPathComponent@
lastPathComponentSelector :: Selector '[] (Id NSString)
lastPathComponentSelector = mkSelector "lastPathComponent"

-- | @Selector@ for @pathExtension@
pathExtensionSelector :: Selector '[] (Id NSString)
pathExtensionSelector = mkSelector "pathExtension"

-- | @Selector@ for @URLByDeletingLastPathComponent@
urlByDeletingLastPathComponentSelector :: Selector '[] (Id NSURL)
urlByDeletingLastPathComponentSelector = mkSelector "URLByDeletingLastPathComponent"

-- | @Selector@ for @URLByDeletingPathExtension@
urlByDeletingPathExtensionSelector :: Selector '[] (Id NSURL)
urlByDeletingPathExtensionSelector = mkSelector "URLByDeletingPathExtension"

-- | @Selector@ for @URLByStandardizingPath@
urlByStandardizingPathSelector :: Selector '[] (Id NSURL)
urlByStandardizingPathSelector = mkSelector "URLByStandardizingPath"

-- | @Selector@ for @URLByResolvingSymlinksInPath@
urlByResolvingSymlinksInPathSelector :: Selector '[] (Id NSURL)
urlByResolvingSymlinksInPathSelector = mkSelector "URLByResolvingSymlinksInPath"

