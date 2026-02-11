{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableMovie
--
-- AVMutableMovie adds to its immutable superclass, AVMovie, several categories of methods for editing QuickTime movie files, e.g. inserting and removing time ranges of media, adding and removing tracks, and modifying the metadata collections stored therein.
--
-- By default, after creating an AVMutableMovie the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil. If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- Generated bindings for @AVMutableMovie@.
module ObjC.AVFoundation.AVMutableMovie
  ( AVMutableMovie
  , IsAVMutableMovie(..)
  , movieWithURL_options_error
  , initWithURL_options_error
  , movieWithData_options_error
  , initWithData_options_error
  , movieWithSettingsFromMovie_options_error
  , initWithSettingsFromMovie_options_error
  , metadataForFormat
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys
  , chapterMetadataGroupsBestMatchingPreferredLanguages
  , mediaSelectionGroupForMediaCharacteristic
  , unusedTrackID
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , mutableTrackCompatibleWithTrack
  , addMutableTrackWithMediaType_copySettingsFromTrack_options
  , addMutableTracksCopyingSettingsFromTracks_options
  , removeTrack
  , preferredRate
  , setPreferredRate
  , preferredVolume
  , setPreferredVolume
  , timescale
  , setTimescale
  , tracks
  , metadata
  , setMetadata
  , modified
  , setModified
  , defaultMediaDataStorage
  , setDefaultMediaDataStorage
  , movieWithURL_options_errorSelector
  , initWithURL_options_errorSelector
  , movieWithData_options_errorSelector
  , initWithData_options_errorSelector
  , movieWithSettingsFromMovie_options_errorSelector
  , initWithSettingsFromMovie_options_errorSelector
  , metadataForFormatSelector
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector
  , chapterMetadataGroupsBestMatchingPreferredLanguagesSelector
  , mediaSelectionGroupForMediaCharacteristicSelector
  , unusedTrackIDSelector
  , trackWithTrackIDSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , tracksWithMediaTypeSelector
  , tracksWithMediaCharacteristicSelector
  , mutableTrackCompatibleWithTrackSelector
  , addMutableTrackWithMediaType_copySettingsFromTrack_optionsSelector
  , addMutableTracksCopyingSettingsFromTracks_optionsSelector
  , removeTrackSelector
  , preferredRateSelector
  , setPreferredRateSelector
  , preferredVolumeSelector
  , setPreferredVolumeSelector
  , timescaleSelector
  , setTimescaleSelector
  , tracksSelector
  , metadataSelector
  , setMetadataSelector
  , modifiedSelector
  , setModifiedSelector
  , defaultMediaDataStorageSelector
  , setDefaultMediaDataStorageSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | movieWithURL:options:error:
--
-- Creates an AVMutableMovie object from a movie header stored in a QuickTime movie file or ISO base media file.
--
-- @URL@ — An NSURL object that specifies a file containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties                     to indicate where the sample data should be written.
--
-- ObjC selector: @+ movieWithURL:options:error:@
movieWithURL_options_error :: (IsNSURL url, IsNSDictionary options, IsNSError outError) => url -> options -> outError -> IO (Id AVMutableMovie)
movieWithURL_options_error url options outError =
  do
    cls' <- getRequiredClass "AVMutableMovie"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "movieWithURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithURL:options:error:
--
-- Creates an AVMutableMovie object from a movie header stored in a QuickTime movie file or ISO base media file.
--
-- @URL@ — An NSURL object that specifies a file containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties                     to indicate where the sample data should be written.
--
-- ObjC selector: @- initWithURL:options:error:@
initWithURL_options_error :: (IsAVMutableMovie avMutableMovie, IsNSURL url, IsNSDictionary options, IsNSError outError) => avMutableMovie -> url -> options -> outError -> IO (Id AVMutableMovie)
initWithURL_options_error avMutableMovie  url options outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avMutableMovie (mkSelector "initWithURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | movieWithData:options:error:
--
-- Creates an AVMutableMovie object from a movie header stored in an NSData object.
--
-- @data@ — An NSData object containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- You can use this method to operate on movie headers that are not stored in files. In general you should avoid loading an entire movie file with its media data into an instance of NSData!
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil. If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @+ movieWithData:options:error:@
movieWithData_options_error :: (IsNSData data_, IsNSDictionary options, IsNSError outError) => data_ -> options -> outError -> IO (Id AVMutableMovie)
movieWithData_options_error data_ options outError =
  do
    cls' <- getRequiredClass "AVMutableMovie"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "movieWithData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithData:options:error:
--
-- Creates an AVMutableMovie object from a movie header stored in an NSData object.
--
-- @data@ — An NSData object containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- You can use this method to operate on movie headers that are not stored in files. In general you should avoid loading an entire movie file with its media data into an instance of NSData!
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMutableMovieTrack's mediaDataStorage property will be nil. If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @- initWithData:options:error:@
initWithData_options_error :: (IsAVMutableMovie avMutableMovie, IsNSData data_, IsNSDictionary options, IsNSError outError) => avMutableMovie -> data_ -> options -> outError -> IO (Id AVMutableMovie)
initWithData_options_error avMutableMovie  data_ options outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avMutableMovie (mkSelector "initWithData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | movieWithSettingsFromMovie:options:error:
--
-- Creates an AVMutableMovie object without tracks (and therefore without media).
--
-- @movie@ — If you wish to transfer settings from an existing movie (including movie userdata and metadata, preferred rate, preferred volume, etc.), pass a reference to an AVMovie object representing that movie. Otherwise pass nil. The userdata and metadata from the source movie may need to be converted if the format of that movie differs from fileType; you may wish to inspect the userdata or metadata of the receiver to ensure that important data was copied.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object. Pass nil for default initialization behavior.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @+ movieWithSettingsFromMovie:options:error:@
movieWithSettingsFromMovie_options_error :: (IsAVMovie movie, IsNSDictionary options, IsNSError outError) => movie -> options -> outError -> IO (Id AVMutableMovie)
movieWithSettingsFromMovie_options_error movie options outError =
  do
    cls' <- getRequiredClass "AVMutableMovie"
    withObjCPtr movie $ \raw_movie ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "movieWithSettingsFromMovie:options:error:") (retPtr retVoid) [argPtr (castPtr raw_movie :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithSettingsFromMovie:options:error:
--
-- Creates an AVMutableMovie object without tracks (and therefore without media).
--
-- @movie@ — If you wish to transfer settings from an existing movie (including movie userdata and metadata, preferred rate, preferred volume, etc.), pass a reference to an AVMovie object representing that movie. Otherwise pass nil. The userdata and metadata from the source movie may need to be converted if the format of that movie differs from fileType; you may wish to inspect the userdata or metadata of the receiver to ensure that important data was copied.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMutableMovie object. Pass nil for default initialization behavior.
--
-- @outError@ — If an error occurs creating a movie, describes the nature of the failure.
--
-- Returns: An AVMutableMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @- initWithSettingsFromMovie:options:error:@
initWithSettingsFromMovie_options_error :: (IsAVMutableMovie avMutableMovie, IsAVMovie movie, IsNSDictionary options, IsNSError outError) => avMutableMovie -> movie -> options -> outError -> IO (Id AVMutableMovie)
initWithSettingsFromMovie_options_error avMutableMovie  movie options outError =
withObjCPtr movie $ \raw_movie ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avMutableMovie (mkSelector "initWithSettingsFromMovie:options:error:") (retPtr retVoid) [argPtr (castPtr raw_movie :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVMutableMovie avMutableMovie, IsNSString format) => avMutableMovie -> format -> IO (Id NSArray)
metadataForFormat avMutableMovie  format =
withObjCPtr format $ \raw_format ->
    sendMsg avMutableMovie (mkSelector "metadataForFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys :: (IsAVMutableMovie avMutableMovie, IsNSLocale locale, IsNSArray commonKeys) => avMutableMovie -> locale -> commonKeys -> IO (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys avMutableMovie  locale commonKeys =
withObjCPtr locale $ \raw_locale ->
  withObjCPtr commonKeys $ \raw_commonKeys ->
      sendMsg avMutableMovie (mkSelector "chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr raw_commonKeys :: Ptr ())] >>= retainedObject . castPtr

-- | @- chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguages :: (IsAVMutableMovie avMutableMovie, IsNSArray preferredLanguages) => avMutableMovie -> preferredLanguages -> IO (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguages avMutableMovie  preferredLanguages =
withObjCPtr preferredLanguages $ \raw_preferredLanguages ->
    sendMsg avMutableMovie (mkSelector "chapterMetadataGroupsBestMatchingPreferredLanguages:") (retPtr retVoid) [argPtr (castPtr raw_preferredLanguages :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristic :: (IsAVMutableMovie avMutableMovie, IsNSString mediaCharacteristic) => avMutableMovie -> mediaCharacteristic -> IO (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristic avMutableMovie  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    sendMsg avMutableMovie (mkSelector "mediaSelectionGroupForMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | @- unusedTrackID@
unusedTrackID :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO CInt
unusedTrackID avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "unusedTrackID") retCInt []

-- | trackWithTrackID:
--
-- Provides an instance of AVMutableMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMutableMovieTrack.
--
-- Returns: An instance of AVMutableMovieTrack; may be nil if no track of the specified trackID is available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVMutableMovie avMutableMovie => avMutableMovie -> CInt -> IO (Id AVMutableMovieTrack)
trackWithTrackID avMutableMovie  trackID =
  sendMsg avMutableMovie (mkSelector "trackWithTrackID:") (retPtr retVoid) [argCInt (fromIntegral trackID)] >>= retainedObject . castPtr

-- | loadTrackWithTrackID:completionHandler:
--
-- Loads an instance of AVMutableMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMutableMovieTrack.
--
-- @completionHandler@ — A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVMutableMovie avMutableMovie => avMutableMovie -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avMutableMovie  trackID completionHandler =
  sendMsg avMutableMovie (mkSelector "loadTrackWithTrackID:completionHandler:") retVoid [argCInt (fromIntegral trackID), argPtr (castPtr completionHandler :: Ptr ())]

-- | tracksWithMediaType:
--
-- Provides an array of AVMutableMovieTracks of the asset that present media of the specified media type.
--
-- @mediaType@ — The media type according to which the receiver filters its AVMutableMovieTracks. (Media types are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMutableMovieTracks; may be empty if no tracks of the specified media type are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVMutableMovie avMutableMovie, IsNSString mediaType) => avMutableMovie -> mediaType -> IO (Id NSArray)
tracksWithMediaType avMutableMovie  mediaType =
withObjCPtr mediaType $ \raw_mediaType ->
    sendMsg avMutableMovie (mkSelector "tracksWithMediaType:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ())] >>= retainedObject . castPtr

-- | tracksWithMediaCharacteristic:
--
-- Provides an array of AVMutableMovieTracks of the asset that present media with the specified characteristic.
--
-- @mediaCharacteristic@ — The media characteristic according to which the receiver filters its AVMutableMovieTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMutableMovieTracks; may be empty if no tracks with the specified characteristic are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVMutableMovie avMutableMovie, IsNSString mediaCharacteristic) => avMutableMovie -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avMutableMovie  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    sendMsg avMutableMovie (mkSelector "tracksWithMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | mutableTrackCompatibleWithTrack:
--
-- Provides a reference to a track of a mutable movie into which any time range of an AVAssetTrack					can be inserted (via -[AVMutableMovieTrack insertTimeRange:ofTrack:atTime:copySampleData:error:]).
--
-- @track@ — A reference to the AVAssetTrack from which a time range may be inserted.
--
-- Returns: An AVMutableMovieTrack that can accommodate the insertion.					If no such track is available, the result is nil. A new track of the same media type					as the AVAssetTrack can be created via -addMutableTrackWithMediaType:copySettingsFromTrack:options:,					and this new track will be compatible.
--
-- For best performance, the number of tracks in a movie should be kept to a minimum, corresponding to the					number for which media data must be presented in parallel. If media data of the same type is to be presented					serially, even from multiple assets, a single track of that media type should be used. This method,					-mutableTrackCompatibleWithTrack:, can help the client to identify an existing target track for an insertion.
--
-- ObjC selector: @- mutableTrackCompatibleWithTrack:@
mutableTrackCompatibleWithTrack :: (IsAVMutableMovie avMutableMovie, IsAVAssetTrack track) => avMutableMovie -> track -> IO (Id AVMutableMovieTrack)
mutableTrackCompatibleWithTrack avMutableMovie  track =
withObjCPtr track $ \raw_track ->
    sendMsg avMutableMovie (mkSelector "mutableTrackCompatibleWithTrack:") (retPtr retVoid) [argPtr (castPtr raw_track :: Ptr ())] >>= retainedObject . castPtr

-- | addMutableTrackWithMediaType:copySettingsFromTrack:options:
--
-- Adds an empty track to the target movie.
--
-- @mediaType@ — The media type of the new track (e.g. AVMediaTypeVideo for a video track).
--
-- @track@ — If you wish to transfer settings from an existing track, including width, height, preferred volume, etc., pass a reference to an AVAssetTrack representing that track. Otherwise pass nil.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the new AVMutableMovieTrack object. Pass nil for default initialization behavior.
--
-- Returns: An AVMutableMovieTrack object
--
-- The trackID of the newly added track is a property of the returned instance of AVMutableMovieTrack.					This method throws an exception if media type is not equal to the track's media type, or if any option is invalid.					Note that metadata will not be automatically copied.
--
-- ObjC selector: @- addMutableTrackWithMediaType:copySettingsFromTrack:options:@
addMutableTrackWithMediaType_copySettingsFromTrack_options :: (IsAVMutableMovie avMutableMovie, IsNSString mediaType, IsAVAssetTrack track, IsNSDictionary options) => avMutableMovie -> mediaType -> track -> options -> IO (Id AVMutableMovieTrack)
addMutableTrackWithMediaType_copySettingsFromTrack_options avMutableMovie  mediaType track options =
withObjCPtr mediaType $ \raw_mediaType ->
  withObjCPtr track $ \raw_track ->
    withObjCPtr options $ \raw_options ->
        sendMsg avMutableMovie (mkSelector "addMutableTrackWithMediaType:copySettingsFromTrack:options:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ()), argPtr (castPtr raw_track :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | addMutableTracksCopyingSettingsFromTracks:options:
--
-- Adds one or more empty tracks to the target movie, copying track settings from the source tracks.
--
-- @existingTracks@ — An array of AVAssetTrack objects.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the new AVMutableMovieTrack objects. Pass nil for default initialization behavior.
--
-- Returns: An array of AVMutableMovieTrack objects; the index of a track in this array is the same as the index of its source track in the existingTracks array.
--
-- This method creates one or more empty tracks in the target movie and configures those tracks with settings (such as track userdata and metadata, width, height, and preferred volume) copied from the source tracks in the existingTracks array. Also, properties involving pairs of tracks (such as track references) are copied from the source tracks to the target tracks.					This method throws an exception if any option is invalid.
--
-- ObjC selector: @- addMutableTracksCopyingSettingsFromTracks:options:@
addMutableTracksCopyingSettingsFromTracks_options :: (IsAVMutableMovie avMutableMovie, IsNSArray existingTracks, IsNSDictionary options) => avMutableMovie -> existingTracks -> options -> IO (Id NSArray)
addMutableTracksCopyingSettingsFromTracks_options avMutableMovie  existingTracks options =
withObjCPtr existingTracks $ \raw_existingTracks ->
  withObjCPtr options $ \raw_options ->
      sendMsg avMutableMovie (mkSelector "addMutableTracksCopyingSettingsFromTracks:options:") (retPtr retVoid) [argPtr (castPtr raw_existingTracks :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | removeTrack:
--
-- Removes a track from the target movie.
--
-- @track@ — The track to be removed.
--
-- ObjC selector: @- removeTrack:@
removeTrack :: (IsAVMutableMovie avMutableMovie, IsAVMovieTrack track) => avMutableMovie -> track -> IO ()
removeTrack avMutableMovie  track =
withObjCPtr track $ \raw_track ->
    sendMsg avMutableMovie (mkSelector "removeTrack:") retVoid [argPtr (castPtr raw_track :: Ptr ())]

-- | preferredRate
--
-- The natural rate at which the movie is to be played; often but not always 1.0.
--
-- ObjC selector: @- preferredRate@
preferredRate :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO CFloat
preferredRate avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "preferredRate") retCFloat []

-- | preferredRate
--
-- The natural rate at which the movie is to be played; often but not always 1.0.
--
-- ObjC selector: @- setPreferredRate:@
setPreferredRate :: IsAVMutableMovie avMutableMovie => avMutableMovie -> CFloat -> IO ()
setPreferredRate avMutableMovie  value =
  sendMsg avMutableMovie (mkSelector "setPreferredRate:") retVoid [argCFloat (fromIntegral value)]

-- | preferredVolume
--
-- The preferred volume of the audible media data of the movie; often but not always 1.0.
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO CFloat
preferredVolume avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "preferredVolume") retCFloat []

-- | preferredVolume
--
-- The preferred volume of the audible media data of the movie; often but not always 1.0.
--
-- ObjC selector: @- setPreferredVolume:@
setPreferredVolume :: IsAVMutableMovie avMutableMovie => avMutableMovie -> CFloat -> IO ()
setPreferredVolume avMutableMovie  value =
  sendMsg avMutableMovie (mkSelector "setPreferredVolume:") retVoid [argCFloat (fromIntegral value)]

-- | timescale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the time scale of the movie.
--
-- The default movie time scale is 600. In certain cases, you may want to set this to a different value. For instance, a movie that					contains a single audio track should typically have the movie time scale set to the media time scale of that track.
--
-- This property should be set on a new empty movie before any edits are performed on the movie.
--
-- ObjC selector: @- timescale@
timescale :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO CInt
timescale avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "timescale") retCInt []

-- | timescale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the time scale of the movie.
--
-- The default movie time scale is 600. In certain cases, you may want to set this to a different value. For instance, a movie that					contains a single audio track should typically have the movie time scale set to the media time scale of that track.
--
-- This property should be set on a new empty movie before any edits are performed on the movie.
--
-- ObjC selector: @- setTimescale:@
setTimescale :: IsAVMutableMovie avMutableMovie => avMutableMovie -> CInt -> IO ()
setTimescale avMutableMovie  value =
  sendMsg avMutableMovie (mkSelector "setTimescale:") retVoid [argCInt (fromIntegral value)]

-- | tracks
--
-- The tracks in a mutable movie.
--
-- The value of this property is an array of tracks the mutable movie contains; the tracks are of type AVMutableMovieTrack.
--
-- ObjC selector: @- tracks@
tracks :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO (Id NSArray)
tracks avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata stored by the movie.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of metadata stored by the movie.
--
-- ObjC selector: @- metadata@
metadata :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO (Id NSArray)
metadata avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata stored by the movie.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of metadata stored by the movie.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVMutableMovie avMutableMovie, IsNSArray value) => avMutableMovie -> value -> IO ()
setMetadata avMutableMovie  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMovie (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | modified
--
-- Whether a movie has been modified.
--
-- The value of this property is a BOOL that indicates whether the AVMutableMovie object has been modified since it was created, was last written, or had its modified state cleared via a call to setModified:NO.
--
-- ObjC selector: @- modified@
modified :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO Bool
modified avMutableMovie  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovie (mkSelector "modified") retCULong []

-- | modified
--
-- Whether a movie has been modified.
--
-- The value of this property is a BOOL that indicates whether the AVMutableMovie object has been modified since it was created, was last written, or had its modified state cleared via a call to setModified:NO.
--
-- ObjC selector: @- setModified:@
setModified :: IsAVMutableMovie avMutableMovie => avMutableMovie -> Bool -> IO ()
setModified avMutableMovie  value =
  sendMsg avMutableMovie (mkSelector "setModified:") retVoid [argCULong (if value then 1 else 0)]

-- | defaultMediaDataStorage
--
-- The default storage container for media data added to a movie.
--
-- The value of this property is an AVMediaDataStorage object that indicates where sample data that is added to a movie should be written, for any track for whose mediaDataStorage property is nil.
--
-- ObjC selector: @- defaultMediaDataStorage@
defaultMediaDataStorage :: IsAVMutableMovie avMutableMovie => avMutableMovie -> IO (Id AVMediaDataStorage)
defaultMediaDataStorage avMutableMovie  =
  sendMsg avMutableMovie (mkSelector "defaultMediaDataStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultMediaDataStorage
--
-- The default storage container for media data added to a movie.
--
-- The value of this property is an AVMediaDataStorage object that indicates where sample data that is added to a movie should be written, for any track for whose mediaDataStorage property is nil.
--
-- ObjC selector: @- setDefaultMediaDataStorage:@
setDefaultMediaDataStorage :: (IsAVMutableMovie avMutableMovie, IsAVMediaDataStorage value) => avMutableMovie -> value -> IO ()
setDefaultMediaDataStorage avMutableMovie  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMovie (mkSelector "setDefaultMediaDataStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @movieWithURL:options:error:@
movieWithURL_options_errorSelector :: Selector
movieWithURL_options_errorSelector = mkSelector "movieWithURL:options:error:"

-- | @Selector@ for @initWithURL:options:error:@
initWithURL_options_errorSelector :: Selector
initWithURL_options_errorSelector = mkSelector "initWithURL:options:error:"

-- | @Selector@ for @movieWithData:options:error:@
movieWithData_options_errorSelector :: Selector
movieWithData_options_errorSelector = mkSelector "movieWithData:options:error:"

-- | @Selector@ for @initWithData:options:error:@
initWithData_options_errorSelector :: Selector
initWithData_options_errorSelector = mkSelector "initWithData:options:error:"

-- | @Selector@ for @movieWithSettingsFromMovie:options:error:@
movieWithSettingsFromMovie_options_errorSelector :: Selector
movieWithSettingsFromMovie_options_errorSelector = mkSelector "movieWithSettingsFromMovie:options:error:"

-- | @Selector@ for @initWithSettingsFromMovie:options:error:@
initWithSettingsFromMovie_options_errorSelector :: Selector
initWithSettingsFromMovie_options_errorSelector = mkSelector "initWithSettingsFromMovie:options:error:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector :: Selector
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector = mkSelector "chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:"

-- | @Selector@ for @chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector :: Selector
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector = mkSelector "chapterMetadataGroupsBestMatchingPreferredLanguages:"

-- | @Selector@ for @mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristicSelector :: Selector
mediaSelectionGroupForMediaCharacteristicSelector = mkSelector "mediaSelectionGroupForMediaCharacteristic:"

-- | @Selector@ for @unusedTrackID@
unusedTrackIDSelector :: Selector
unusedTrackIDSelector = mkSelector "unusedTrackID"

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector
trackWithTrackIDSelector = mkSelector "trackWithTrackID:"

-- | @Selector@ for @loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandlerSelector :: Selector
loadTrackWithTrackID_completionHandlerSelector = mkSelector "loadTrackWithTrackID:completionHandler:"

-- | @Selector@ for @tracksWithMediaType:@
tracksWithMediaTypeSelector :: Selector
tracksWithMediaTypeSelector = mkSelector "tracksWithMediaType:"

-- | @Selector@ for @tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristicSelector :: Selector
tracksWithMediaCharacteristicSelector = mkSelector "tracksWithMediaCharacteristic:"

-- | @Selector@ for @mutableTrackCompatibleWithTrack:@
mutableTrackCompatibleWithTrackSelector :: Selector
mutableTrackCompatibleWithTrackSelector = mkSelector "mutableTrackCompatibleWithTrack:"

-- | @Selector@ for @addMutableTrackWithMediaType:copySettingsFromTrack:options:@
addMutableTrackWithMediaType_copySettingsFromTrack_optionsSelector :: Selector
addMutableTrackWithMediaType_copySettingsFromTrack_optionsSelector = mkSelector "addMutableTrackWithMediaType:copySettingsFromTrack:options:"

-- | @Selector@ for @addMutableTracksCopyingSettingsFromTracks:options:@
addMutableTracksCopyingSettingsFromTracks_optionsSelector :: Selector
addMutableTracksCopyingSettingsFromTracks_optionsSelector = mkSelector "addMutableTracksCopyingSettingsFromTracks:options:"

-- | @Selector@ for @removeTrack:@
removeTrackSelector :: Selector
removeTrackSelector = mkSelector "removeTrack:"

-- | @Selector@ for @preferredRate@
preferredRateSelector :: Selector
preferredRateSelector = mkSelector "preferredRate"

-- | @Selector@ for @setPreferredRate:@
setPreferredRateSelector :: Selector
setPreferredRateSelector = mkSelector "setPreferredRate:"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @setPreferredVolume:@
setPreferredVolumeSelector :: Selector
setPreferredVolumeSelector = mkSelector "setPreferredVolume:"

-- | @Selector@ for @timescale@
timescaleSelector :: Selector
timescaleSelector = mkSelector "timescale"

-- | @Selector@ for @setTimescale:@
setTimescaleSelector :: Selector
setTimescaleSelector = mkSelector "setTimescale:"

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @modified@
modifiedSelector :: Selector
modifiedSelector = mkSelector "modified"

-- | @Selector@ for @setModified:@
setModifiedSelector :: Selector
setModifiedSelector = mkSelector "setModified:"

-- | @Selector@ for @defaultMediaDataStorage@
defaultMediaDataStorageSelector :: Selector
defaultMediaDataStorageSelector = mkSelector "defaultMediaDataStorage"

-- | @Selector@ for @setDefaultMediaDataStorage:@
setDefaultMediaDataStorageSelector :: Selector
setDefaultMediaDataStorageSelector = mkSelector "setDefaultMediaDataStorage:"

