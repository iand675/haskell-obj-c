{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMovie@.
module ObjC.AVFoundation.AVMovie
  ( AVMovie
  , IsAVMovie(..)
  , movieTypes
  , movieWithURL_options
  , initWithURL_options
  , movieWithData_options
  , initWithData_options
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , movieHeaderWithFileType_error
  , writeMovieHeaderToURL_fileType_options_error
  , isCompatibleWithFileType
  , url
  , data_
  , defaultMediaDataStorage
  , tracks
  , canContainMovieFragments
  , containsMovieFragments
  , movieTypesSelector
  , movieWithURL_optionsSelector
  , initWithURL_optionsSelector
  , movieWithData_optionsSelector
  , initWithData_optionsSelector
  , trackWithTrackIDSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , tracksWithMediaTypeSelector
  , tracksWithMediaCharacteristicSelector
  , movieHeaderWithFileType_errorSelector
  , writeMovieHeaderToURL_fileType_options_errorSelector
  , isCompatibleWithFileTypeSelector
  , urlSelector
  , dataSelector
  , defaultMediaDataStorageSelector
  , tracksSelector
  , canContainMovieFragmentsSelector
  , containsMovieFragmentsSelector

  -- * Enum types
  , AVMovieWritingOptions(AVMovieWritingOptions)
  , pattern AVMovieWritingAddMovieHeaderToDestination
  , pattern AVMovieWritingTruncateDestinationToMovieHeaderOnly

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | movieTypes
--
-- Provides the file types the AVMovie class understands.
--
-- Returns: An NSArray of UTIs identifying the file types the AVMovie class understands.
--
-- ObjC selector: @+ movieTypes@
movieTypes :: IO (Id NSArray)
movieTypes  =
  do
    cls' <- getRequiredClass "AVMovie"
    sendClassMsg cls' (mkSelector "movieTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | movieWithURL:options:
--
-- Creates an AVMovie object from a movie header stored in a QuickTime movie file or ISO base media file.
--
-- @URL@ — An NSURL object that specifies a file containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMovie object.
--
-- Returns: An AVMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties                     to indicate where the sample data should be written.
--
-- ObjC selector: @+ movieWithURL:options:@
movieWithURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id AVMovie)
movieWithURL_options url options =
  do
    cls' <- getRequiredClass "AVMovie"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "movieWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | initWithURL:options:
--
-- Creates an AVMovie object from a movie header stored in a QuickTime movie file or ISO base media file.
--
-- @URL@ — An NSURL object that specifies a file containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMovie object.
--
-- Returns: An AVMovie object
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from a file and then append sample buffers to any of its tracks, you must first set one of these properties                     to indicate where the sample data should be written.
--
-- ObjC selector: @- initWithURL:options:@
initWithURL_options :: (IsAVMovie avMovie, IsNSURL url, IsNSDictionary options) => avMovie -> url -> options -> IO (Id AVMovie)
initWithURL_options avMovie  url options =
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
        sendMsg avMovie (mkSelector "initWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | movieWithData:options:
--
-- Creates an AVMovie object from a movie header stored in an NSData object.
--
-- @data@ — An NSData object containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMovie object.
--
-- Returns: An AVMovie object
--
-- You can use this method to operate on movie headers that are not stored in files; this might include movie headers on the pasteboard (which do not contain media data). In general you should avoid loading an entire movie file with its media data into an instance of NSData! By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil.                    If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @+ movieWithData:options:@
movieWithData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id AVMovie)
movieWithData_options data_ options =
  do
    cls' <- getRequiredClass "AVMovie"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "movieWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | initWithData:options:
--
-- Creates an AVMovie object from a movie header stored in an NSData object.
--
-- @data@ — An NSData object containing a movie header.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMovie object.
--
-- Returns: An AVMovie object
--
-- You can use this method to operate on movie headers that are not stored in files. In general you should avoid loading an entire movie file with its media data into an instance of NSData!
--
-- By default, the defaultMediaDataStorage property will be nil and each associated AVMovieTrack's mediaDataStorage property will be nil. If you want to create an AVMutableMovie from an NSData object and then append sample buffers to any of its tracks, you must first set one of these properties to indicate where the sample data should be written.
--
-- ObjC selector: @- initWithData:options:@
initWithData_options :: (IsAVMovie avMovie, IsNSData data_, IsNSDictionary options) => avMovie -> data_ -> options -> IO (Id AVMovie)
initWithData_options avMovie  data_ options =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr options $ \raw_options ->
        sendMsg avMovie (mkSelector "initWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | trackWithTrackID:
--
-- Provides an instance of AVMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMovieTrack.
--
-- Returns: An instance of AVMovieTrack; may be nil if no track of the specified trackID is available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVMovie avMovie => avMovie -> CInt -> IO (Id AVMovieTrack)
trackWithTrackID avMovie  trackID =
    sendMsg avMovie (mkSelector "trackWithTrackID:") (retPtr retVoid) [argCInt trackID] >>= retainedObject . castPtr

-- | loadTrackWithTrackID:completionHandler:
--
-- Loads an instance of AVMovieTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVMovieTrack.
--
-- @completionHandler@ — A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVMovie avMovie => avMovie -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avMovie  trackID completionHandler =
    sendMsg avMovie (mkSelector "loadTrackWithTrackID:completionHandler:") retVoid [argCInt trackID, argPtr (castPtr completionHandler :: Ptr ())]

-- | tracksWithMediaType:
--
-- Provides an array of AVMovieTracks of the asset that present media of the specified media type.
--
-- @mediaType@ — The media type according to which the receiver filters its AVMovieTracks. (Media types are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMovieTracks; may be empty if no tracks of the specified media type are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVMovie avMovie, IsNSString mediaType) => avMovie -> mediaType -> IO (Id NSArray)
tracksWithMediaType avMovie  mediaType =
  withObjCPtr mediaType $ \raw_mediaType ->
      sendMsg avMovie (mkSelector "tracksWithMediaType:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ())] >>= retainedObject . castPtr

-- | tracksWithMediaCharacteristic:
--
-- Provides an array of AVMovieTracks of the asset that present media with the specified characteristic.
--
-- @mediaCharacteristic@ — The media characteristic according to which the receiver filters its AVMovieTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVMovieTracks; may be empty if no tracks with the specified characteristic are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVMovie avMovie, IsNSString mediaCharacteristic) => avMovie -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avMovie  mediaCharacteristic =
  withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
      sendMsg avMovie (mkSelector "tracksWithMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | movieHeaderWithFileType:error:
--
-- Creates an NSData object containing the movie header of the AVMovie object.
--
-- @fileType@ — A UTI indicating the specific file format of the movie header (e.g. AVFileTypeQuickTimeMovie for a QuickTime movie).
--
-- @outError@ — If an error occurs reading the movie header, describes the nature of the failure.
--
-- Returns: An NSData object.
--
-- The movie header will be a pure reference movie, with no base URL, suitable for use on the pasteboard.
--
-- ObjC selector: @- movieHeaderWithFileType:error:@
movieHeaderWithFileType_error :: (IsAVMovie avMovie, IsNSString fileType, IsNSError outError) => avMovie -> fileType -> outError -> IO (Id NSData)
movieHeaderWithFileType_error avMovie  fileType outError =
  withObjCPtr fileType $ \raw_fileType ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg avMovie (mkSelector "movieHeaderWithFileType:error:") (retPtr retVoid) [argPtr (castPtr raw_fileType :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | writeMovieHeaderToURL:fileType:options:error:
--
-- Writes the movie header to a destination URL.
--
-- @URL@ — An NSURL object indicating where to write the movie header.
--
-- @fileType@ — A UTI indicating the specific file format (e.g. AVFileTypeQuickTimeMovie for a QuickTime movie).
--
-- @options@ — An NSUInteger whose bits specify options for the writing of the movie header. See AVMovieWritingOptions above.
--
-- @outError@ — If an error occurs writing the movie header, describes the nature of the failure.
--
-- Note that modifications to instances of AVMutableMovie, to their constituent AVMutableMovieTracks, or to their collections of metadata are committed to storage when their movie headers are written.
--
-- ObjC selector: @- writeMovieHeaderToURL:fileType:options:error:@
writeMovieHeaderToURL_fileType_options_error :: (IsAVMovie avMovie, IsNSURL url, IsNSString fileType, IsNSError outError) => avMovie -> url -> fileType -> AVMovieWritingOptions -> outError -> IO Bool
writeMovieHeaderToURL_fileType_options_error avMovie  url fileType options outError =
  withObjCPtr url $ \raw_url ->
    withObjCPtr fileType $ \raw_fileType ->
      withObjCPtr outError $ \raw_outError ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMovie (mkSelector "writeMovieHeaderToURL:fileType:options:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_fileType :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())]

-- | isCompatibleWithFileType:
--
-- Indicates whether a movie header for the AVMovie object can be created for the specified file type.
--
-- @fileType@ — A UTI indicating a movie file format (e.g. AVFileTypeQuickTimeMovie for a QuickTime movie).
--
-- This method returns a BOOL that indicates whether a movie header of the specified type can be created for the receiver. For example, this method returns NO if the movie contains tracks whose media types or media subtypes are not allowed by the specified file type.
--
-- ObjC selector: @- isCompatibleWithFileType:@
isCompatibleWithFileType :: (IsAVMovie avMovie, IsNSString fileType) => avMovie -> fileType -> IO Bool
isCompatibleWithFileType avMovie  fileType =
  withObjCPtr fileType $ \raw_fileType ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMovie (mkSelector "isCompatibleWithFileType:") retCULong [argPtr (castPtr raw_fileType :: Ptr ())]

-- | URL
--
-- The URL with which the instance of AVMovie was initialized; may be nil.
--
-- ObjC selector: @- URL@
url :: IsAVMovie avMovie => avMovie -> IO (Id NSURL)
url avMovie  =
    sendMsg avMovie (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | data
--
-- The data block with which the instance of AVMovie was initialized; may be nil.
--
-- ObjC selector: @- data@
data_ :: IsAVMovie avMovie => avMovie -> IO (Id NSData)
data_ avMovie  =
    sendMsg avMovie (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultMediaDataStorage
--
-- The default storage container for media data added to a movie.
--
-- The value of this property is an AVMediaDataStorage object that indicates where sample data that is added to a movie should be written by default.
--
-- ObjC selector: @- defaultMediaDataStorage@
defaultMediaDataStorage :: IsAVMovie avMovie => avMovie -> IO (Id AVMediaDataStorage)
defaultMediaDataStorage avMovie  =
    sendMsg avMovie (mkSelector "defaultMediaDataStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tracks
--
-- The tracks in a movie.
--
-- The value of this property is an array of tracks the movie contains; the tracks are of type AVMovieTrack.
--
-- ObjC selector: @- tracks@
tracks :: IsAVMovie avMovie => avMovie -> IO (Id NSArray)
tracks avMovie  =
    sendMsg avMovie (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | canContainMovieFragments
--
-- Indicates whether the movie file is capable of being extended by fragments.
--
-- The value of this property is YES if an 'mvex' box is present in the 'moov' box. The 'mvex' box is necessary in order to signal the possible presence of later 'moof' boxes.
--
-- ObjC selector: @- canContainMovieFragments@
canContainMovieFragments :: IsAVMovie avMovie => avMovie -> IO Bool
canContainMovieFragments avMovie  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMovie (mkSelector "canContainMovieFragments") retCULong []

-- | containsMovieFragments
--
-- Indicates whether the movie file is extended by at least one movie fragment.
--
-- The value of this property is YES if canContainMovieFragments is YES and at least one 'moof' box is present after the 'moov' box.
--
-- ObjC selector: @- containsMovieFragments@
containsMovieFragments :: IsAVMovie avMovie => avMovie -> IO Bool
containsMovieFragments avMovie  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMovie (mkSelector "containsMovieFragments") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @movieTypes@
movieTypesSelector :: Selector
movieTypesSelector = mkSelector "movieTypes"

-- | @Selector@ for @movieWithURL:options:@
movieWithURL_optionsSelector :: Selector
movieWithURL_optionsSelector = mkSelector "movieWithURL:options:"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @movieWithData:options:@
movieWithData_optionsSelector :: Selector
movieWithData_optionsSelector = mkSelector "movieWithData:options:"

-- | @Selector@ for @initWithData:options:@
initWithData_optionsSelector :: Selector
initWithData_optionsSelector = mkSelector "initWithData:options:"

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

-- | @Selector@ for @movieHeaderWithFileType:error:@
movieHeaderWithFileType_errorSelector :: Selector
movieHeaderWithFileType_errorSelector = mkSelector "movieHeaderWithFileType:error:"

-- | @Selector@ for @writeMovieHeaderToURL:fileType:options:error:@
writeMovieHeaderToURL_fileType_options_errorSelector :: Selector
writeMovieHeaderToURL_fileType_options_errorSelector = mkSelector "writeMovieHeaderToURL:fileType:options:error:"

-- | @Selector@ for @isCompatibleWithFileType:@
isCompatibleWithFileTypeSelector :: Selector
isCompatibleWithFileTypeSelector = mkSelector "isCompatibleWithFileType:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @defaultMediaDataStorage@
defaultMediaDataStorageSelector :: Selector
defaultMediaDataStorageSelector = mkSelector "defaultMediaDataStorage"

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @canContainMovieFragments@
canContainMovieFragmentsSelector :: Selector
canContainMovieFragmentsSelector = mkSelector "canContainMovieFragments"

-- | @Selector@ for @containsMovieFragments@
containsMovieFragmentsSelector :: Selector
containsMovieFragmentsSelector = mkSelector "containsMovieFragments"

