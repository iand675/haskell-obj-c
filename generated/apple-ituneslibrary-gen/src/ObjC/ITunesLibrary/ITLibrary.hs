{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allMediaItemsSelector
  , allPlaylistsSelector
  , apiMajorVersionSelector
  , apiMinorVersionSelector
  , applicationVersionSelector
  , artworkForMediaFileSelector
  , featuresSelector
  , initSelector
  , initWithAPIVersion_errorSelector
  , initWithAPIVersion_options_errorSelector
  , libraryWithAPIVersion_errorSelector
  , libraryWithAPIVersion_options_errorSelector
  , mediaFolderLocationSelector
  , musicFolderLocationSelector
  , reloadDataSelector
  , showContentRatingSelector
  , unloadDataSelector

  -- * Enum types
  , ITLibExportFeature(ITLibExportFeature)
  , pattern ITLibExportFeatureNone
  , ITLibInitOptions(ITLibInitOptions)
  , pattern ITLibInitOptionNone
  , pattern ITLibInitOptionLazyLoadData

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsITLibrary itLibrary => itLibrary -> IO (Id ITLibrary)
init_ itLibrary =
  sendOwnedMessage itLibrary initSelector

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
    sendClassMessage cls' libraryWithAPIVersion_errorSelector (toNSString requestedAPIVersion) (toNSError error_)

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
    sendClassMessage cls' libraryWithAPIVersion_options_errorSelector (toNSString requestedAPIVersion) options (toNSError error_)

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
initWithAPIVersion_error itLibrary requestedAPIVersion error_ =
  sendOwnedMessage itLibrary initWithAPIVersion_errorSelector (toNSString requestedAPIVersion) (toNSError error_)

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
initWithAPIVersion_options_error itLibrary requestedAPIVersion options error_ =
  sendOwnedMessage itLibrary initWithAPIVersion_options_errorSelector (toNSString requestedAPIVersion) options (toNSError error_)

-- | Retrieves the artwork from a media file.
--
-- @mediaFileURL@ — The URL of the media file whose artwork should be extracted.
--
-- Returns: A ITLibArtwork instance represeting the media file artwork, or nil if the artwork was not found or could not be extracted.
--
-- ObjC selector: @- artworkForMediaFile:@
artworkForMediaFile :: (IsITLibrary itLibrary, IsNSURL mediaFileURL) => itLibrary -> mediaFileURL -> IO (Id ITLibArtwork)
artworkForMediaFile itLibrary mediaFileURL =
  sendMessage itLibrary artworkForMediaFileSelector (toNSURL mediaFileURL)

-- | Refreshes the data used by the framework.
--
-- Returns: YES if the data was reloaded, false if an error occurred.
--
-- ObjC selector: @- reloadData@
reloadData :: IsITLibrary itLibrary => itLibrary -> IO Bool
reloadData itLibrary =
  sendMessage itLibrary reloadDataSelector

-- | Unloads the data used by the framework.
--
-- ObjC selector: @- unloadData@
unloadData :: IsITLibrary itLibrary => itLibrary -> IO ()
unloadData itLibrary =
  sendMessage itLibrary unloadDataSelector

-- | The version of iTunes being accessed.
--
-- ObjC selector: @- applicationVersion@
applicationVersion :: IsITLibrary itLibrary => itLibrary -> IO (Id NSString)
applicationVersion itLibrary =
  sendMessage itLibrary applicationVersionSelector

-- | A bitwise OR combination of the features of this library.
--
-- ObjC selector: @- features@
features :: IsITLibrary itLibrary => itLibrary -> IO ITLibExportFeature
features itLibrary =
  sendMessage itLibrary featuresSelector

-- | The major version number of this API.
--
-- ObjC selector: @- apiMajorVersion@
apiMajorVersion :: IsITLibrary itLibrary => itLibrary -> IO CULong
apiMajorVersion itLibrary =
  sendMessage itLibrary apiMajorVersionSelector

-- | The minor version number of this API.
--
-- ObjC selector: @- apiMinorVersion@
apiMinorVersion :: IsITLibrary itLibrary => itLibrary -> IO CULong
apiMinorVersion itLibrary =
  sendMessage itLibrary apiMinorVersionSelector

-- | The location of the iTunes music folder.
--
-- ObjC selector: @- mediaFolderLocation@
mediaFolderLocation :: IsITLibrary itLibrary => itLibrary -> IO RawId
mediaFolderLocation itLibrary =
  sendMessage itLibrary mediaFolderLocationSelector

-- | The location of the iTunes music folder. Replaced by mediaFolderLocation.
--
-- ObjC selector: @- musicFolderLocation@
musicFolderLocation :: IsITLibrary itLibrary => itLibrary -> IO (Id NSURL)
musicFolderLocation itLibrary =
  sendMessage itLibrary musicFolderLocationSelector

-- | Whether to show content rating labels.
--
-- ObjC selector: @- showContentRating@
showContentRating :: IsITLibrary itLibrary => itLibrary -> IO Bool
showContentRating itLibrary =
  sendMessage itLibrary showContentRatingSelector

-- | All media items in the library.
--
-- ObjC selector: @- allMediaItems@
allMediaItems :: IsITLibrary itLibrary => itLibrary -> IO (Id NSArray)
allMediaItems itLibrary =
  sendMessage itLibrary allMediaItemsSelector

-- | All playlists in the library.
--
-- ObjC selector: @- allPlaylists@
allPlaylists :: IsITLibrary itLibrary => itLibrary -> IO (Id NSArray)
allPlaylists itLibrary =
  sendMessage itLibrary allPlaylistsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ITLibrary)
initSelector = mkSelector "init"

-- | @Selector@ for @libraryWithAPIVersion:error:@
libraryWithAPIVersion_errorSelector :: Selector '[Id NSString, Id NSError] (Id ITLibrary)
libraryWithAPIVersion_errorSelector = mkSelector "libraryWithAPIVersion:error:"

-- | @Selector@ for @libraryWithAPIVersion:options:error:@
libraryWithAPIVersion_options_errorSelector :: Selector '[Id NSString, ITLibInitOptions, Id NSError] (Id ITLibrary)
libraryWithAPIVersion_options_errorSelector = mkSelector "libraryWithAPIVersion:options:error:"

-- | @Selector@ for @initWithAPIVersion:error:@
initWithAPIVersion_errorSelector :: Selector '[Id NSString, Id NSError] (Id ITLibrary)
initWithAPIVersion_errorSelector = mkSelector "initWithAPIVersion:error:"

-- | @Selector@ for @initWithAPIVersion:options:error:@
initWithAPIVersion_options_errorSelector :: Selector '[Id NSString, ITLibInitOptions, Id NSError] (Id ITLibrary)
initWithAPIVersion_options_errorSelector = mkSelector "initWithAPIVersion:options:error:"

-- | @Selector@ for @artworkForMediaFile:@
artworkForMediaFileSelector :: Selector '[Id NSURL] (Id ITLibArtwork)
artworkForMediaFileSelector = mkSelector "artworkForMediaFile:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] Bool
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @unloadData@
unloadDataSelector :: Selector '[] ()
unloadDataSelector = mkSelector "unloadData"

-- | @Selector@ for @applicationVersion@
applicationVersionSelector :: Selector '[] (Id NSString)
applicationVersionSelector = mkSelector "applicationVersion"

-- | @Selector@ for @features@
featuresSelector :: Selector '[] ITLibExportFeature
featuresSelector = mkSelector "features"

-- | @Selector@ for @apiMajorVersion@
apiMajorVersionSelector :: Selector '[] CULong
apiMajorVersionSelector = mkSelector "apiMajorVersion"

-- | @Selector@ for @apiMinorVersion@
apiMinorVersionSelector :: Selector '[] CULong
apiMinorVersionSelector = mkSelector "apiMinorVersion"

-- | @Selector@ for @mediaFolderLocation@
mediaFolderLocationSelector :: Selector '[] RawId
mediaFolderLocationSelector = mkSelector "mediaFolderLocation"

-- | @Selector@ for @musicFolderLocation@
musicFolderLocationSelector :: Selector '[] (Id NSURL)
musicFolderLocationSelector = mkSelector "musicFolderLocation"

-- | @Selector@ for @showContentRating@
showContentRatingSelector :: Selector '[] Bool
showContentRatingSelector = mkSelector "showContentRating"

-- | @Selector@ for @allMediaItems@
allMediaItemsSelector :: Selector '[] (Id NSArray)
allMediaItemsSelector = mkSelector "allMediaItems"

-- | @Selector@ for @allPlaylists@
allPlaylistsSelector :: Selector '[] (Id NSArray)
allPlaylistsSelector = mkSelector "allPlaylists"

