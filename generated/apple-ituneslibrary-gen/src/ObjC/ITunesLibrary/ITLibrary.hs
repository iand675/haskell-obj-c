{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing an iTunes library whose metadata is being queried.
--
-- Generated bindings for @ITLibrary@.
module ObjC.ITunesLibrary.ITLibrary
  ( ITLibrary
  , IsITLibrary(..)
  , init_
  , libraryWithAPIVersion_error
  , libraryWithAPIVersion_options_error
  , initWithAPIVersion_error
  , initWithAPIVersion_options_error
  , artworkForMediaFile
  , reloadData
  , unloadData
  , applicationVersion
  , features
  , apiMajorVersion
  , apiMinorVersion
  , mediaFolderLocation
  , musicFolderLocation
  , showContentRating
  , allMediaItems
  , allPlaylists
  , initSelector
  , libraryWithAPIVersion_errorSelector
  , libraryWithAPIVersion_options_errorSelector
  , initWithAPIVersion_errorSelector
  , initWithAPIVersion_options_errorSelector
  , artworkForMediaFileSelector
  , reloadDataSelector
  , unloadDataSelector
  , applicationVersionSelector
  , featuresSelector
  , apiMajorVersionSelector
  , apiMinorVersionSelector
  , mediaFolderLocationSelector
  , musicFolderLocationSelector
  , showContentRatingSelector
  , allMediaItemsSelector
  , allPlaylistsSelector

  -- * Enum types
  , ITLibExportFeature(ITLibExportFeature)
  , pattern ITLibExportFeatureNone
  , ITLibInitOptions(ITLibInitOptions)
  , pattern ITLibInitOptionNone
  , pattern ITLibInitOptionLazyLoadData

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

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsITLibrary itLibrary => itLibrary -> IO (Id ITLibrary)
init_ itLibrary  =
    sendMsg itLibrary (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates and initializes an instance of ITLibrary which can be used to retrieve media entities.
--
-- Upon initialization of the ITLibrary class, the default iTunes database for the current user will be read and parsed. 				At this point all media entities will be cached in memory until the time the object is deallocated.
--
-- @requestedAPIVersion@ — The version of the iTunesLibrary API that the application is requesting, provide "1.0" if unknown.
--
-- @error@ — A pointer to a variable that will receive an NSError if this method fails. May be nil if caller does not care about error.
--
-- Returns: An ITLibrary instance, or nil if this method fails.
--
-- ObjC selector: @+ libraryWithAPIVersion:error:@
libraryWithAPIVersion_error :: (IsNSString requestedAPIVersion, IsNSError error_) => requestedAPIVersion -> error_ -> IO (Id ITLibrary)
libraryWithAPIVersion_error requestedAPIVersion error_ =
  do
    cls' <- getRequiredClass "ITLibrary"
    withObjCPtr requestedAPIVersion $ \raw_requestedAPIVersion ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "libraryWithAPIVersion:error:") (retPtr retVoid) [argPtr (castPtr raw_requestedAPIVersion :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and initializes an instance of ITLibrary which can be used to retrieve media entities.
--
-- Unless the ITLibInitOptionLazyLoadData option is specified, the default iTunes database for the current user will be 				read and parsed upon initialization of the ITLibrary class, and all media entities will be cached in memory.
--
-- @requestedAPIVersion@ — The version of the iTunesLibrary API that the application is requesting, provide "1.0" if unknown.
--
-- @options@ — Options that change the initialization behavior.
--
-- @error@ — A pointer to a variable that will receive an NSError if this method fails. May be nil if caller does not care about error.
--
-- Returns: An ITLibrary instance, or nil if this method fails.
--
-- ObjC selector: @+ libraryWithAPIVersion:options:error:@
libraryWithAPIVersion_options_error :: (IsNSString requestedAPIVersion, IsNSError error_) => requestedAPIVersion -> ITLibInitOptions -> error_ -> IO (Id ITLibrary)
libraryWithAPIVersion_options_error requestedAPIVersion options error_ =
  do
    cls' <- getRequiredClass "ITLibrary"
    withObjCPtr requestedAPIVersion $ \raw_requestedAPIVersion ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "libraryWithAPIVersion:options:error:") (retPtr retVoid) [argPtr (castPtr raw_requestedAPIVersion :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an instance of ITLibrary which can be used to retrieve media entities.
--
-- Upon initialization of the ITLibrary class, the default iTunes database for the current user will be read and parsed. 				At this point all media entities will be cached in memory until the time the object is deallocated.
--
-- @requestedAPIVersion@ — The version of the iTunesLibrary API that the application is requesting, provide "1.0" if unknown.
--
-- @error@ — A pointer to a variable that will receive an NSError if this method fails. May be nil if caller does not care about error.
--
-- Returns: An ITLibrary instance, or nil if this method fails.
--
-- ObjC selector: @- initWithAPIVersion:error:@
initWithAPIVersion_error :: (IsITLibrary itLibrary, IsNSString requestedAPIVersion, IsNSError error_) => itLibrary -> requestedAPIVersion -> error_ -> IO (Id ITLibrary)
initWithAPIVersion_error itLibrary  requestedAPIVersion error_ =
  withObjCPtr requestedAPIVersion $ \raw_requestedAPIVersion ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg itLibrary (mkSelector "initWithAPIVersion:error:") (retPtr retVoid) [argPtr (castPtr raw_requestedAPIVersion :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes an instance of ITLibrary which can be used to retrieve media entities.
--
-- Unless the ITLibInitOptionLazyLoadData option is specified, the default iTunes database for the current user will be 				read and parsed upon initialization of the ITLibrary class, and all media entities will be cached in memory.
--
-- @requestedAPIVersion@ — The version of the iTunesLibrary API that the application is requesting, provide "1.0" if unknown.
--
-- @options@ — Options that change the initialization behavior.
--
-- @error@ — A pointer to a variable that will receive an NSError if this method fails. May be nil if caller does not care about error.
--
-- Returns: An ITLibrary instance, or nil if this method fails.
--
-- ObjC selector: @- initWithAPIVersion:options:error:@
initWithAPIVersion_options_error :: (IsITLibrary itLibrary, IsNSString requestedAPIVersion, IsNSError error_) => itLibrary -> requestedAPIVersion -> ITLibInitOptions -> error_ -> IO (Id ITLibrary)
initWithAPIVersion_options_error itLibrary  requestedAPIVersion options error_ =
  withObjCPtr requestedAPIVersion $ \raw_requestedAPIVersion ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg itLibrary (mkSelector "initWithAPIVersion:options:error:") (retPtr retVoid) [argPtr (castPtr raw_requestedAPIVersion :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Retrieves the artwork from a media file.
--
-- @mediaFileURL@ — The URL of the media file whose artwork should be extracted.
--
-- Returns: A ITLibArtwork instance represeting the media file artwork, or nil if the artwork was not found or could not be extracted.
--
-- ObjC selector: @- artworkForMediaFile:@
artworkForMediaFile :: (IsITLibrary itLibrary, IsNSURL mediaFileURL) => itLibrary -> mediaFileURL -> IO (Id ITLibArtwork)
artworkForMediaFile itLibrary  mediaFileURL =
  withObjCPtr mediaFileURL $ \raw_mediaFileURL ->
      sendMsg itLibrary (mkSelector "artworkForMediaFile:") (retPtr retVoid) [argPtr (castPtr raw_mediaFileURL :: Ptr ())] >>= retainedObject . castPtr

-- | Refreshes the data used by the framework.
--
-- Returns: YES if the data was reloaded, false if an error occurred.
--
-- ObjC selector: @- reloadData@
reloadData :: IsITLibrary itLibrary => itLibrary -> IO Bool
reloadData itLibrary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibrary (mkSelector "reloadData") retCULong []

-- | Unloads the data used by the framework.
--
-- ObjC selector: @- unloadData@
unloadData :: IsITLibrary itLibrary => itLibrary -> IO ()
unloadData itLibrary  =
    sendMsg itLibrary (mkSelector "unloadData") retVoid []

-- | The version of iTunes being accessed.
--
-- ObjC selector: @- applicationVersion@
applicationVersion :: IsITLibrary itLibrary => itLibrary -> IO (Id NSString)
applicationVersion itLibrary  =
    sendMsg itLibrary (mkSelector "applicationVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A bitwise OR combination of the features of this library.
--
-- ObjC selector: @- features@
features :: IsITLibrary itLibrary => itLibrary -> IO ITLibExportFeature
features itLibrary  =
    fmap (coerce :: CULong -> ITLibExportFeature) $ sendMsg itLibrary (mkSelector "features") retCULong []

-- | The major version number of this API.
--
-- ObjC selector: @- apiMajorVersion@
apiMajorVersion :: IsITLibrary itLibrary => itLibrary -> IO CULong
apiMajorVersion itLibrary  =
    sendMsg itLibrary (mkSelector "apiMajorVersion") retCULong []

-- | The minor version number of this API.
--
-- ObjC selector: @- apiMinorVersion@
apiMinorVersion :: IsITLibrary itLibrary => itLibrary -> IO CULong
apiMinorVersion itLibrary  =
    sendMsg itLibrary (mkSelector "apiMinorVersion") retCULong []

-- | The location of the iTunes music folder.
--
-- ObjC selector: @- mediaFolderLocation@
mediaFolderLocation :: IsITLibrary itLibrary => itLibrary -> IO RawId
mediaFolderLocation itLibrary  =
    fmap (RawId . castPtr) $ sendMsg itLibrary (mkSelector "mediaFolderLocation") (retPtr retVoid) []

-- | The location of the iTunes music folder. Replaced by mediaFolderLocation.
--
-- ObjC selector: @- musicFolderLocation@
musicFolderLocation :: IsITLibrary itLibrary => itLibrary -> IO (Id NSURL)
musicFolderLocation itLibrary  =
    sendMsg itLibrary (mkSelector "musicFolderLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether to show content rating labels.
--
-- ObjC selector: @- showContentRating@
showContentRating :: IsITLibrary itLibrary => itLibrary -> IO Bool
showContentRating itLibrary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibrary (mkSelector "showContentRating") retCULong []

-- | All media items in the library.
--
-- ObjC selector: @- allMediaItems@
allMediaItems :: IsITLibrary itLibrary => itLibrary -> IO (Id NSArray)
allMediaItems itLibrary  =
    sendMsg itLibrary (mkSelector "allMediaItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All playlists in the library.
--
-- ObjC selector: @- allPlaylists@
allPlaylists :: IsITLibrary itLibrary => itLibrary -> IO (Id NSArray)
allPlaylists itLibrary  =
    sendMsg itLibrary (mkSelector "allPlaylists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @libraryWithAPIVersion:error:@
libraryWithAPIVersion_errorSelector :: Selector
libraryWithAPIVersion_errorSelector = mkSelector "libraryWithAPIVersion:error:"

-- | @Selector@ for @libraryWithAPIVersion:options:error:@
libraryWithAPIVersion_options_errorSelector :: Selector
libraryWithAPIVersion_options_errorSelector = mkSelector "libraryWithAPIVersion:options:error:"

-- | @Selector@ for @initWithAPIVersion:error:@
initWithAPIVersion_errorSelector :: Selector
initWithAPIVersion_errorSelector = mkSelector "initWithAPIVersion:error:"

-- | @Selector@ for @initWithAPIVersion:options:error:@
initWithAPIVersion_options_errorSelector :: Selector
initWithAPIVersion_options_errorSelector = mkSelector "initWithAPIVersion:options:error:"

-- | @Selector@ for @artworkForMediaFile:@
artworkForMediaFileSelector :: Selector
artworkForMediaFileSelector = mkSelector "artworkForMediaFile:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @unloadData@
unloadDataSelector :: Selector
unloadDataSelector = mkSelector "unloadData"

-- | @Selector@ for @applicationVersion@
applicationVersionSelector :: Selector
applicationVersionSelector = mkSelector "applicationVersion"

-- | @Selector@ for @features@
featuresSelector :: Selector
featuresSelector = mkSelector "features"

-- | @Selector@ for @apiMajorVersion@
apiMajorVersionSelector :: Selector
apiMajorVersionSelector = mkSelector "apiMajorVersion"

-- | @Selector@ for @apiMinorVersion@
apiMinorVersionSelector :: Selector
apiMinorVersionSelector = mkSelector "apiMinorVersion"

-- | @Selector@ for @mediaFolderLocation@
mediaFolderLocationSelector :: Selector
mediaFolderLocationSelector = mkSelector "mediaFolderLocation"

-- | @Selector@ for @musicFolderLocation@
musicFolderLocationSelector :: Selector
musicFolderLocationSelector = mkSelector "musicFolderLocation"

-- | @Selector@ for @showContentRating@
showContentRatingSelector :: Selector
showContentRatingSelector = mkSelector "showContentRating"

-- | @Selector@ for @allMediaItems@
allMediaItemsSelector :: Selector
allMediaItemsSelector = mkSelector "allMediaItems"

-- | @Selector@ for @allPlaylists@
allPlaylistsSelector :: Selector
allPlaylistsSelector = mkSelector "allPlaylists"

