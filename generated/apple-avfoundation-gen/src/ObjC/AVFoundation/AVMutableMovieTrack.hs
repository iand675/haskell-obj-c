{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableMovieTrack@.
module ObjC.AVFoundation.AVMutableMovieTrack
  ( AVMutableMovieTrack
  , IsAVMutableMovieTrack(..)
  , hasMediaCharacteristic
  , metadataForFormat
  , associatedTracksOfType
  , appendSampleBuffer_decodeTime_presentationTime_error
  , replaceFormatDescription_withFormatDescription
  , addTrackAssociationToTrack_type
  , removeTrackAssociationToTrack_type
  , mediaDataStorage
  , setMediaDataStorage
  , sampleReferenceBaseURL
  , setSampleReferenceBaseURL
  , enabled
  , setEnabled
  , alternateGroupID
  , setAlternateGroupID
  , modified
  , setModified
  , hasProtectedContent
  , timescale
  , setTimescale
  , metadata
  , setMetadata
  , preferredMediaChunkSize
  , setPreferredMediaChunkSize
  , preferredMediaChunkAlignment
  , setPreferredMediaChunkAlignment
  , preferredVolume
  , setPreferredVolume
  , layer
  , setLayer
  , languageCode
  , setLanguageCode
  , extendedLanguageTag
  , setExtendedLanguageTag
  , hasMediaCharacteristicSelector
  , metadataForFormatSelector
  , associatedTracksOfTypeSelector
  , appendSampleBuffer_decodeTime_presentationTime_errorSelector
  , replaceFormatDescription_withFormatDescriptionSelector
  , addTrackAssociationToTrack_typeSelector
  , removeTrackAssociationToTrack_typeSelector
  , mediaDataStorageSelector
  , setMediaDataStorageSelector
  , sampleReferenceBaseURLSelector
  , setSampleReferenceBaseURLSelector
  , enabledSelector
  , setEnabledSelector
  , alternateGroupIDSelector
  , setAlternateGroupIDSelector
  , modifiedSelector
  , setModifiedSelector
  , hasProtectedContentSelector
  , timescaleSelector
  , setTimescaleSelector
  , metadataSelector
  , setMetadataSelector
  , preferredMediaChunkSizeSelector
  , setPreferredMediaChunkSizeSelector
  , preferredMediaChunkAlignmentSelector
  , setPreferredMediaChunkAlignmentSelector
  , preferredVolumeSelector
  , setPreferredVolumeSelector
  , layerSelector
  , setLayerSelector
  , languageCodeSelector
  , setLanguageCodeSelector
  , extendedLanguageTagSelector
  , setExtendedLanguageTagSelector


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

-- | @- hasMediaCharacteristic:@
hasMediaCharacteristic :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSString mediaCharacteristic) => avMutableMovieTrack -> mediaCharacteristic -> IO Bool
hasMediaCharacteristic avMutableMovieTrack  mediaCharacteristic =
  withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovieTrack (mkSelector "hasMediaCharacteristic:") retCULong [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())]

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSString format) => avMutableMovieTrack -> format -> IO (Id NSArray)
metadataForFormat avMutableMovieTrack  format =
  withObjCPtr format $ \raw_format ->
      sendMsg avMutableMovieTrack (mkSelector "metadataForFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- associatedTracksOfType:@
associatedTracksOfType :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSString trackAssociationType) => avMutableMovieTrack -> trackAssociationType -> IO (Id NSArray)
associatedTracksOfType avMutableMovieTrack  trackAssociationType =
  withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
      sendMsg avMutableMovieTrack (mkSelector "associatedTracksOfType:") (retPtr retVoid) [argPtr (castPtr raw_trackAssociationType :: Ptr ())] >>= retainedObject . castPtr

-- | appendSampleBuffer:decodeTime:presentationTime:error:
--
-- Appends sample data to a media file and adds sample references for the added data to a track's media sample tables.
--
-- @sampleBuffer@ — The CMSampleBuffer to be appended; this may be obtained from an instance of AVAssetReader.
--
-- @outDecodeTime@ — A pointer to a CMTime structure to receive the decode time in the media of the first sample appended from the sample buffer. Pass NULL if you do not need this information.
--
-- @outPresentationTime@ — A pointer to a CMTime structure to receive the presentation time in the media of the first sample appended from the sample buffer. Pass NULL if you do not need this information.
--
-- @outError@ — If the appending fails, describes the nature of the failure. For example, if the device containing the track's media data storage is full, AVErrorDiskFull is returned.
--
-- Returns: A BOOL value indicating the success of the operation.
--
-- If the sample buffer carries sample data, the sample data is written to the container specified by the track property mediaDataStorage if non-nil,                    or else by the movie property defaultMediaDataStorage if non-nil, and sample references will be appended to the track's media.                    If both media data storage properties are nil, the method will fail and return NO.                    If the sample buffer carries sample references only, sample data will not be written and sample references to the samples in their                    original container will be appended to the track's media as necessary.
--
-- Note regarding sample timing: in a track's media, the first sample's decode timestamp must always be zero.                    For an audio track, each sample buffer's duration is used as the sample decode duration.                    For other track types, difference between a sample's decode timestamp and the following                     sample's decode timestamp is used as the first sample's decode duration, so as to preserve the relative timing.
--
-- Note that this method does not modify the track's sourceTimeMappings but only appends sample references and sample data to the track's media.                      To make the new samples appear in the track's timeline, invoke -insertMediaTimeRange:intoTimeRange:.                    You can retrieve the mediaPresentationTimeRange property before and after appending a sequence of samples,                    using CMTimeRangeGetEnd on each to calculate the media TimeRange for -insertMediaTimeRange:intoTimeRange:.
--
-- It's safe for multiple threads to call this method on different tracks at once.
--
-- This method throws an exception for any of the following reasons:                        - the sample buffer's media type does not match the track's media type                        - the sample buffer contains image buffers (must contain encoded video)                        - the sample buffer contains caption groups (must contain encoded media data)
--
-- ObjC selector: @- appendSampleBuffer:decodeTime:presentationTime:error:@
appendSampleBuffer_decodeTime_presentationTime_error :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSError outError) => avMutableMovieTrack -> Ptr () -> RawId -> RawId -> outError -> IO Bool
appendSampleBuffer_decodeTime_presentationTime_error avMutableMovieTrack  sampleBuffer outDecodeTime outPresentationTime outError =
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovieTrack (mkSelector "appendSampleBuffer:decodeTime:presentationTime:error:") retCULong [argPtr sampleBuffer, argPtr (castPtr (unRawId outDecodeTime) :: Ptr ()), argPtr (castPtr (unRawId outPresentationTime) :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | replaceFormatDescription:withFormatDescription:
--
-- Replaces one of the receiver's format descriptions with another format description
--
-- @formatDescription@ — A CMFormatDescription occurring in the array returned by the -formatDescriptions method.
--
-- @newFormatDescription@ — A CMFormatDescription to replace the specified format description.
--
-- You can use this method to make surgical changes to a track's format descriptions, such as adding format description extensions to a format description or changing the audio channel layout of an audio track. You should note that a format description can have extensions of type kCMFormatDescriptionExtension_VerbatimSampleDescription and kCMFormatDescriptionExtension_VerbatimISOSampleEntry; if you modify a copy of a format description, you should delete those extensions from the copy or your changes might be ignored.
--
-- This method throws an exception if the media type of the new format description does not match the media type of the receiver.
--
-- ObjC selector: @- replaceFormatDescription:withFormatDescription:@
replaceFormatDescription_withFormatDescription :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> RawId -> RawId -> IO ()
replaceFormatDescription_withFormatDescription avMutableMovieTrack  formatDescription newFormatDescription =
    sendMsg avMutableMovieTrack (mkSelector "replaceFormatDescription:withFormatDescription:") retVoid [argPtr (castPtr (unRawId formatDescription) :: Ptr ()), argPtr (castPtr (unRawId newFormatDescription) :: Ptr ())]

-- | addTrackAssociationToTrack:type:
--
-- Establishes a track association of a specific type between two tracks.
--
-- @movieTrack@ — An AVMovieTrack object that is to be associated with the receiver.
--
-- @trackAssociationType@ — The type of track association to add between the receiver and the specified movieTrack (for instance, AVTrackAssociationTypeChapterList).
--
-- This method throws an exception if the movie track belongs to a different movie.
--
-- ObjC selector: @- addTrackAssociationToTrack:type:@
addTrackAssociationToTrack_type :: (IsAVMutableMovieTrack avMutableMovieTrack, IsAVMovieTrack movieTrack, IsNSString trackAssociationType) => avMutableMovieTrack -> movieTrack -> trackAssociationType -> IO ()
addTrackAssociationToTrack_type avMutableMovieTrack  movieTrack trackAssociationType =
  withObjCPtr movieTrack $ \raw_movieTrack ->
    withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
        sendMsg avMutableMovieTrack (mkSelector "addTrackAssociationToTrack:type:") retVoid [argPtr (castPtr raw_movieTrack :: Ptr ()), argPtr (castPtr raw_trackAssociationType :: Ptr ())]

-- | removeTrackAssociationToTrack:type:
--
-- Removes a track association of a specific type between two tracks.
--
-- @movieTrack@ — An AVMovieTrack object that is associated with the receiver.
--
-- @trackAssociationType@ — The type of track association to remove between the receiver and the specified movieTrack (for instance, AVTrackAssociationTypeChapterList).
--
-- This method throws an exception if the movie track belongs to a different movie.
--
-- ObjC selector: @- removeTrackAssociationToTrack:type:@
removeTrackAssociationToTrack_type :: (IsAVMutableMovieTrack avMutableMovieTrack, IsAVMovieTrack movieTrack, IsNSString trackAssociationType) => avMutableMovieTrack -> movieTrack -> trackAssociationType -> IO ()
removeTrackAssociationToTrack_type avMutableMovieTrack  movieTrack trackAssociationType =
  withObjCPtr movieTrack $ \raw_movieTrack ->
    withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
        sendMsg avMutableMovieTrack (mkSelector "removeTrackAssociationToTrack:type:") retVoid [argPtr (castPtr raw_movieTrack :: Ptr ()), argPtr (castPtr raw_trackAssociationType :: Ptr ())]

-- | mediaDataStorage
--
-- The storage container for media data added to a track.
--
-- The value of this property is an AVMediaDataStorage object that indicates the location at which media data inserted or appended to the track will be written.
--
-- ObjC selector: @- mediaDataStorage@
mediaDataStorage :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO (Id AVMediaDataStorage)
mediaDataStorage avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "mediaDataStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaDataStorage
--
-- The storage container for media data added to a track.
--
-- The value of this property is an AVMediaDataStorage object that indicates the location at which media data inserted or appended to the track will be written.
--
-- ObjC selector: @- setMediaDataStorage:@
setMediaDataStorage :: (IsAVMutableMovieTrack avMutableMovieTrack, IsAVMediaDataStorage value) => avMutableMovieTrack -> value -> IO ()
setMediaDataStorage avMutableMovieTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableMovieTrack (mkSelector "setMediaDataStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sampleReferenceBaseURL
--
-- For file types that support writing sample references, such as QuickTime Movie files, specifies the base URL that sample references are relative to; may be nil.
--
-- If the value of this property can be resolved as an absolute URL, the sample locations written to the file when appending sample references to this track will be relative to this URL. The URL must point to a location contained by any common parent directory of the locations that will be referenced. For example, setting the sampleReferenceBaseURL property to "file:///Users/johnappleseed/Movies/" and appending sample buffers that refer to "file:///Users/johnappleseed/Movies/data/movie1.mov" will cause the sample reference "data/movie1.mov" to be written to the movie file.
--
-- If the value of the property cannot be resolved as an absolute URL or if it points to a location that is not contained by any common parent directory of the locations that will be referenced, the location will be written unmodified.
--
-- The default value is nil, which means that the location will be written unmodified.
--
-- ObjC selector: @- sampleReferenceBaseURL@
sampleReferenceBaseURL :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO (Id NSURL)
sampleReferenceBaseURL avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "sampleReferenceBaseURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleReferenceBaseURL
--
-- For file types that support writing sample references, such as QuickTime Movie files, specifies the base URL that sample references are relative to; may be nil.
--
-- If the value of this property can be resolved as an absolute URL, the sample locations written to the file when appending sample references to this track will be relative to this URL. The URL must point to a location contained by any common parent directory of the locations that will be referenced. For example, setting the sampleReferenceBaseURL property to "file:///Users/johnappleseed/Movies/" and appending sample buffers that refer to "file:///Users/johnappleseed/Movies/data/movie1.mov" will cause the sample reference "data/movie1.mov" to be written to the movie file.
--
-- If the value of the property cannot be resolved as an absolute URL or if it points to a location that is not contained by any common parent directory of the locations that will be referenced, the location will be written unmodified.
--
-- The default value is nil, which means that the location will be written unmodified.
--
-- ObjC selector: @- setSampleReferenceBaseURL:@
setSampleReferenceBaseURL :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSURL value) => avMutableMovieTrack -> value -> IO ()
setSampleReferenceBaseURL avMutableMovieTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableMovieTrack (mkSelector "setSampleReferenceBaseURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default for presentation.
--
-- ObjC selector: @- enabled@
enabled :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO Bool
enabled avMutableMovieTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovieTrack (mkSelector "enabled") retCULong []

-- | enabled
--
-- A BOOL value indicating whether the track is enabled by default for presentation.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> Bool -> IO ()
setEnabled avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | alternateGroupID
--
-- An integer indicating the track as a member of a particular alternate group.
--
-- ObjC selector: @- alternateGroupID@
alternateGroupID :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CLong
alternateGroupID avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "alternateGroupID") retCLong []

-- | alternateGroupID
--
-- An integer indicating the track as a member of a particular alternate group.
--
-- ObjC selector: @- setAlternateGroupID:@
setAlternateGroupID :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CLong -> IO ()
setAlternateGroupID avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setAlternateGroupID:") retVoid [argCLong value]

-- | modified
--
-- Whether a track has been modified.
--
-- The value of this property is a BOOL that indicates whether the AVMutableMovieTrack object has been modified since it was created, was last written, or had its modified state cleared via a call to setModified:NO.
--
-- ObjC selector: @- modified@
modified :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO Bool
modified avMutableMovieTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovieTrack (mkSelector "modified") retCULong []

-- | modified
--
-- Whether a track has been modified.
--
-- The value of this property is a BOOL that indicates whether the AVMutableMovieTrack object has been modified since it was created, was last written, or had its modified state cleared via a call to setModified:NO.
--
-- ObjC selector: @- setModified:@
setModified :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> Bool -> IO ()
setModified avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setModified:") retVoid [argCULong (if value then 1 else 0)]

-- | hasProtectedContent
--
-- Whether a track contains protected content.
--
-- The value of this property is a BOOL that indicates whether the track contains protected content.
--
-- ObjC selector: @- hasProtectedContent@
hasProtectedContent :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO Bool
hasProtectedContent avMutableMovieTrack  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMutableMovieTrack (mkSelector "hasProtectedContent") retCULong []

-- | timescale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the time scale of the track's media.
--
-- The default media time scale is 0.
--
-- This property should be set on a new empty track before any edits are performed on the track.
--
-- ObjC selector: @- timescale@
timescale :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CInt
timescale avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "timescale") retCInt []

-- | timescale
--
-- For file types that contain a 'moov' atom, such as QuickTime Movie files, specifies the time scale of the track's media.
--
-- The default media time scale is 0.
--
-- This property should be set on a new empty track before any edits are performed on the track.
--
-- ObjC selector: @- setTimescale:@
setTimescale :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CInt -> IO ()
setTimescale avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setTimescale:") retVoid [argCInt value]

-- | metadata
--
-- A collection of metadata stored by the track.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of metadata stored by the track.
--
-- ObjC selector: @- metadata@
metadata :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO (Id NSArray)
metadata avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- A collection of metadata stored by the track.
--
-- The value of this property is an array of AVMetadataItem objects representing the collection of metadata stored by the track.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSArray value) => avMutableMovieTrack -> value -> IO ()
setMetadata avMutableMovieTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableMovieTrack (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | preferredMediaChunkSize
--
-- For file types that support media chunk sizes, the maximum size (in bytes) to be used for each chunk of sample data written to the file.
--
-- The total size of the samples in a chunk will be no larger than this preferred chunk size, or the size of a single sample if the sample is larger than this preferred chunk size.
--
-- The default media chunk size is 1024 * 1024 bytes. It is an error to set a negative chunk size.
--
-- ObjC selector: @- preferredMediaChunkSize@
preferredMediaChunkSize :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CLong
preferredMediaChunkSize avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "preferredMediaChunkSize") retCLong []

-- | preferredMediaChunkSize
--
-- For file types that support media chunk sizes, the maximum size (in bytes) to be used for each chunk of sample data written to the file.
--
-- The total size of the samples in a chunk will be no larger than this preferred chunk size, or the size of a single sample if the sample is larger than this preferred chunk size.
--
-- The default media chunk size is 1024 * 1024 bytes. It is an error to set a negative chunk size.
--
-- ObjC selector: @- setPreferredMediaChunkSize:@
setPreferredMediaChunkSize :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CLong -> IO ()
setPreferredMediaChunkSize avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setPreferredMediaChunkSize:") retVoid [argCLong value]

-- | preferredMediaChunkAlignment
--
-- For file types that support media chunk alignment, the boundary for media chunk alignment (in bytes).
--
-- The default value is 0, which means that no padding should be used to achieve chunk alignment. It is an error to set a negative value for chunk alignment.
--
-- ObjC selector: @- preferredMediaChunkAlignment@
preferredMediaChunkAlignment :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CLong
preferredMediaChunkAlignment avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "preferredMediaChunkAlignment") retCLong []

-- | preferredMediaChunkAlignment
--
-- For file types that support media chunk alignment, the boundary for media chunk alignment (in bytes).
--
-- The default value is 0, which means that no padding should be used to achieve chunk alignment. It is an error to set a negative value for chunk alignment.
--
-- ObjC selector: @- setPreferredMediaChunkAlignment:@
setPreferredMediaChunkAlignment :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CLong -> IO ()
setPreferredMediaChunkAlignment avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setPreferredMediaChunkAlignment:") retVoid [argCLong value]

-- | preferredVolume
--
-- The preferred volume of the audible media data of the track; often but not always 1.0.
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CFloat
preferredVolume avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "preferredVolume") retCFloat []

-- | preferredVolume
--
-- The preferred volume of the audible media data of the track; often but not always 1.0.
--
-- ObjC selector: @- setPreferredVolume:@
setPreferredVolume :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CFloat -> IO ()
setPreferredVolume avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setPreferredVolume:") retVoid [argCFloat value]

-- | layer
--
-- The layer level of the visual media data of the track.
--
-- ObjC selector: @- layer@
layer :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO CLong
layer avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "layer") retCLong []

-- | layer
--
-- The layer level of the visual media data of the track.
--
-- ObjC selector: @- setLayer:@
setLayer :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> CLong -> IO ()
setLayer avMutableMovieTrack  value =
    sendMsg avMutableMovieTrack (mkSelector "setLayer:") retVoid [argCLong value]

-- | languageCode
--
-- The language associated with the track.
--
-- The value of this property is an ISO 639-2/T language code indicating the language associated with the track; may be nil if no language is indicated.
--
-- ObjC selector: @- languageCode@
languageCode :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO (Id NSString)
languageCode avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "languageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | languageCode
--
-- The language associated with the track.
--
-- The value of this property is an ISO 639-2/T language code indicating the language associated with the track; may be nil if no language is indicated.
--
-- ObjC selector: @- setLanguageCode:@
setLanguageCode :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSString value) => avMutableMovieTrack -> value -> IO ()
setLanguageCode avMutableMovieTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableMovieTrack (mkSelector "setLanguageCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | extendedLanguageTag
--
-- The language tag associated with the track.
--
-- The value of this property is an IETF BCP 47 (RFC 4646) language identifier indicating the language tag associated with the track; may be nil if no language tag is indicated.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsAVMutableMovieTrack avMutableMovieTrack => avMutableMovieTrack -> IO (Id NSString)
extendedLanguageTag avMutableMovieTrack  =
    sendMsg avMutableMovieTrack (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | extendedLanguageTag
--
-- The language tag associated with the track.
--
-- The value of this property is an IETF BCP 47 (RFC 4646) language identifier indicating the language tag associated with the track; may be nil if no language tag is indicated.
--
-- ObjC selector: @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsAVMutableMovieTrack avMutableMovieTrack, IsNSString value) => avMutableMovieTrack -> value -> IO ()
setExtendedLanguageTag avMutableMovieTrack  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avMutableMovieTrack (mkSelector "setExtendedLanguageTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasMediaCharacteristic:@
hasMediaCharacteristicSelector :: Selector
hasMediaCharacteristicSelector = mkSelector "hasMediaCharacteristic:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @associatedTracksOfType:@
associatedTracksOfTypeSelector :: Selector
associatedTracksOfTypeSelector = mkSelector "associatedTracksOfType:"

-- | @Selector@ for @appendSampleBuffer:decodeTime:presentationTime:error:@
appendSampleBuffer_decodeTime_presentationTime_errorSelector :: Selector
appendSampleBuffer_decodeTime_presentationTime_errorSelector = mkSelector "appendSampleBuffer:decodeTime:presentationTime:error:"

-- | @Selector@ for @replaceFormatDescription:withFormatDescription:@
replaceFormatDescription_withFormatDescriptionSelector :: Selector
replaceFormatDescription_withFormatDescriptionSelector = mkSelector "replaceFormatDescription:withFormatDescription:"

-- | @Selector@ for @addTrackAssociationToTrack:type:@
addTrackAssociationToTrack_typeSelector :: Selector
addTrackAssociationToTrack_typeSelector = mkSelector "addTrackAssociationToTrack:type:"

-- | @Selector@ for @removeTrackAssociationToTrack:type:@
removeTrackAssociationToTrack_typeSelector :: Selector
removeTrackAssociationToTrack_typeSelector = mkSelector "removeTrackAssociationToTrack:type:"

-- | @Selector@ for @mediaDataStorage@
mediaDataStorageSelector :: Selector
mediaDataStorageSelector = mkSelector "mediaDataStorage"

-- | @Selector@ for @setMediaDataStorage:@
setMediaDataStorageSelector :: Selector
setMediaDataStorageSelector = mkSelector "setMediaDataStorage:"

-- | @Selector@ for @sampleReferenceBaseURL@
sampleReferenceBaseURLSelector :: Selector
sampleReferenceBaseURLSelector = mkSelector "sampleReferenceBaseURL"

-- | @Selector@ for @setSampleReferenceBaseURL:@
setSampleReferenceBaseURLSelector :: Selector
setSampleReferenceBaseURLSelector = mkSelector "setSampleReferenceBaseURL:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @alternateGroupID@
alternateGroupIDSelector :: Selector
alternateGroupIDSelector = mkSelector "alternateGroupID"

-- | @Selector@ for @setAlternateGroupID:@
setAlternateGroupIDSelector :: Selector
setAlternateGroupIDSelector = mkSelector "setAlternateGroupID:"

-- | @Selector@ for @modified@
modifiedSelector :: Selector
modifiedSelector = mkSelector "modified"

-- | @Selector@ for @setModified:@
setModifiedSelector :: Selector
setModifiedSelector = mkSelector "setModified:"

-- | @Selector@ for @hasProtectedContent@
hasProtectedContentSelector :: Selector
hasProtectedContentSelector = mkSelector "hasProtectedContent"

-- | @Selector@ for @timescale@
timescaleSelector :: Selector
timescaleSelector = mkSelector "timescale"

-- | @Selector@ for @setTimescale:@
setTimescaleSelector :: Selector
setTimescaleSelector = mkSelector "setTimescale:"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @preferredMediaChunkSize@
preferredMediaChunkSizeSelector :: Selector
preferredMediaChunkSizeSelector = mkSelector "preferredMediaChunkSize"

-- | @Selector@ for @setPreferredMediaChunkSize:@
setPreferredMediaChunkSizeSelector :: Selector
setPreferredMediaChunkSizeSelector = mkSelector "setPreferredMediaChunkSize:"

-- | @Selector@ for @preferredMediaChunkAlignment@
preferredMediaChunkAlignmentSelector :: Selector
preferredMediaChunkAlignmentSelector = mkSelector "preferredMediaChunkAlignment"

-- | @Selector@ for @setPreferredMediaChunkAlignment:@
setPreferredMediaChunkAlignmentSelector :: Selector
setPreferredMediaChunkAlignmentSelector = mkSelector "setPreferredMediaChunkAlignment:"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @setPreferredVolume:@
setPreferredVolumeSelector :: Selector
setPreferredVolumeSelector = mkSelector "setPreferredVolume:"

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector
setLayerSelector = mkSelector "setLayer:"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @setLanguageCode:@
setLanguageCodeSelector :: Selector
setLanguageCodeSelector = mkSelector "setLanguageCode:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

