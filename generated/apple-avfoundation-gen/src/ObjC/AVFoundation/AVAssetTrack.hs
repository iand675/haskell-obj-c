{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAssetTrack object provides provides the track-level inspection interface for all assets.
--
-- AVAssetTrack adopts the AVAsynchronousKeyValueLoading protocol. Methods in the protocol should be used to access a track's properties without blocking the current thread. To cancel load requests for all keys of AVAssetTrack one must message the parent AVAsset object (for example, [track.asset cancelLoading]).
--
-- For clients who want to examine a subset of the metadata or other parts of the track, asynchronous methods like -loadMetadataForFormat:completionHandler: can be used to load this information without blocking. When using these asynchronous methods, it is not necessary to load the associated property beforehand. Swift clients can also use the load(:) method to load properties in a type safe manner.
--
-- Generated bindings for @AVAssetTrack@.
module ObjC.AVFoundation.AVAssetTrack
  ( AVAssetTrack
  , IsAVAssetTrack(..)
  , init_
  , new
  , makeSampleCursorAtFirstSampleInDecodeOrder
  , makeSampleCursorAtLastSampleInDecodeOrder
  , associatedTracksOfType
  , metadataForFormat
  , hasMediaCharacteristic
  , asset
  , trackID
  , canProvideSampleCursors
  , availableTrackAssociationTypes
  , commonMetadata
  , metadata
  , availableMetadataFormats
  , segments
  , nominalFrameRate
  , requiresFrameReordering
  , preferredVolume
  , hasAudioSampleDependencies
  , languageCode
  , extendedLanguageTag
  , naturalTimeScale
  , estimatedDataRate
  , mediaType
  , formatDescriptions
  , playable
  , decodable
  , enabled
  , selfContained
  , totalSampleDataLength
  , assetSelector
  , associatedTracksOfTypeSelector
  , availableMetadataFormatsSelector
  , availableTrackAssociationTypesSelector
  , canProvideSampleCursorsSelector
  , commonMetadataSelector
  , decodableSelector
  , enabledSelector
  , estimatedDataRateSelector
  , extendedLanguageTagSelector
  , formatDescriptionsSelector
  , hasAudioSampleDependenciesSelector
  , hasMediaCharacteristicSelector
  , initSelector
  , languageCodeSelector
  , makeSampleCursorAtFirstSampleInDecodeOrderSelector
  , makeSampleCursorAtLastSampleInDecodeOrderSelector
  , mediaTypeSelector
  , metadataForFormatSelector
  , metadataSelector
  , naturalTimeScaleSelector
  , newSelector
  , nominalFrameRateSelector
  , playableSelector
  , preferredVolumeSelector
  , requiresFrameReorderingSelector
  , segmentsSelector
  , selfContainedSelector
  , totalSampleDataLengthSelector
  , trackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id AVAssetTrack)
init_ avAssetTrack =
  sendOwnedMessage avAssetTrack initSelector

-- | @+ new@
new :: IO (Id AVAssetTrack)
new  =
  do
    cls' <- getRequiredClass "AVAssetTrack"
    sendOwnedClassMessage cls' newSelector

-- | Creates an instance of AVSampleCursor and positions it at the receiver's first media sample in decode order.
--
-- This method will return nil if there are no samples in the track.
--
-- - Returns: An instance of AVSampleCursor.
--
-- ObjC selector: @- makeSampleCursorAtFirstSampleInDecodeOrder@
makeSampleCursorAtFirstSampleInDecodeOrder :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id AVSampleCursor)
makeSampleCursorAtFirstSampleInDecodeOrder avAssetTrack =
  sendMessage avAssetTrack makeSampleCursorAtFirstSampleInDecodeOrderSelector

-- | Creates an instance of AVSampleCursor and positions it at the receiver's last media sample in decode order.
--
-- This method will return nil if there are no samples in the track.
--
-- - Returns: An instance of AVSampleCursor.
--
-- ObjC selector: @- makeSampleCursorAtLastSampleInDecodeOrder@
makeSampleCursorAtLastSampleInDecodeOrder :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id AVSampleCursor)
makeSampleCursorAtLastSampleInDecodeOrder avAssetTrack =
  sendMessage avAssetTrack makeSampleCursorAtLastSampleInDecodeOrderSelector

-- | Provides an NSArray of AVAssetTracks, one for each track associated with the receiver with the specified type of track association.
--
-- Becomes callable without blocking when the key "availableTrackAssociationTypes" has been loaded.
--
-- - Parameter trackAssociationType: The type of track association for which associated tracks are requested.
--
-- - Returns: An NSArray containing AVAssetTracks; may be empty if there is no associated tracks of the specified type.
--
-- ObjC selector: @- associatedTracksOfType:@
associatedTracksOfType :: (IsAVAssetTrack avAssetTrack, IsNSString trackAssociationType) => avAssetTrack -> trackAssociationType -> IO (Id NSArray)
associatedTracksOfType avAssetTrack trackAssociationType =
  sendMessage avAssetTrack associatedTracksOfTypeSelector (toNSString trackAssociationType)

-- | Provides an NSArray of AVMetadataItems, one for each metadata item in the container of the specified format.
--
-- Becomes callable without blocking when the key "availableMetadataFormats" has been loaded
--
-- - Parameter format: The metadata format for which items are requested.
--
-- - Returns: An NSArray containing AVMetadataItems.
--
-- ObjC selector: @- metadataForFormat:@
metadataForFormat :: (IsAVAssetTrack avAssetTrack, IsNSString format) => avAssetTrack -> format -> IO (Id NSArray)
metadataForFormat avAssetTrack format =
  sendMessage avAssetTrack metadataForFormatSelector (toNSString format)

-- | Reports whether the track references media with the specified media characteristic.
--
-- - Parameter mediaCharacteristic: The media characteristic of interest, e.g. AVMediaCharacteristicVisual, AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, etc., as defined above.
--
-- - Returns: YES if the track references media with the specified characteristic, otherwise NO.
--
-- ObjC selector: @- hasMediaCharacteristic:@
hasMediaCharacteristic :: (IsAVAssetTrack avAssetTrack, IsNSString mediaCharacteristic) => avAssetTrack -> mediaCharacteristic -> IO Bool
hasMediaCharacteristic avAssetTrack mediaCharacteristic =
  sendMessage avAssetTrack hasMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | Provides a reference to the AVAsset of which the AVAssetTrack is a part
--
-- ObjC selector: @- asset@
asset :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id AVAsset)
asset avAssetTrack =
  sendMessage avAssetTrack assetSelector

-- | Indicates the persistent unique identifier for this track of the asset
--
-- ObjC selector: @- trackID@
trackID :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CInt
trackID avAssetTrack =
  sendMessage avAssetTrack trackIDSelector

-- | Indicates whether the receiver can provide instances of AVSampleCursor for traversing its media samples and discovering information about them.
--
-- ObjC selector: @- canProvideSampleCursors@
canProvideSampleCursors :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
canProvideSampleCursors avAssetTrack =
  sendMessage avAssetTrack canProvideSampleCursorsSelector

-- | Provides an NSArray of NSStrings, each representing a type of track association that the receiver has with one or more of the other tracks of the asset (e.g. AVTrackAssociationTypeChapterList, AVTrackAssociationTypeTimecode, etc.). Track association types are defined immediately above.
--
-- ObjC selector: @- availableTrackAssociationTypes@
availableTrackAssociationTypes :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
availableTrackAssociationTypes avAssetTrack =
  sendMessage avAssetTrack availableTrackAssociationTypesSelector

-- | Provides access to an array of AVMetadataItems for each common metadata key for which a value is available
--
-- ObjC selector: @- commonMetadata@
commonMetadata :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
commonMetadata avAssetTrack =
  sendMessage avAssetTrack commonMetadataSelector

-- | Provides access to an array of AVMetadataItems for all metadata identifiers for which a value is available; items can be filtered according to language via +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:] and according to identifier via +[AVMetadataItem metadataItemsFromArray:filteredByIdentifier:].
--
-- ObjC selector: @- metadata@
metadata :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
metadata avAssetTrack =
  sendMessage avAssetTrack metadataSelector

-- | Provides an NSArray of NSStrings, each representing a format of metadata that's available for the track (e.g. QuickTime userdata, etc.) Metadata formats are defined in AVMetadataItem.h.
--
-- ObjC selector: @- availableMetadataFormats@
availableMetadataFormats :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
availableMetadataFormats avAssetTrack =
  sendMessage avAssetTrack availableMetadataFormatsSelector

-- | Provides an array of AVAssetTrackSegments with time mappings from the timeline of the track's media samples to the timeline of the track. Empty edits, i.e. timeRanges for which no media data is available to be presented, have a value of AVAssetTrackSegment.empty equal to YES.
--
-- ObjC selector: @- segments@
segments :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
segments avAssetTrack =
  sendMessage avAssetTrack segmentsSelector

-- | For tracks that carry a full frame per media sample, indicates the frame rate of the track in units of frames per second.
--
-- For field-based video tracks that carry one field per media sample, the value of this property is the field rate, not the frame rate.
--
-- ObjC selector: @- nominalFrameRate@
nominalFrameRate :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CFloat
nominalFrameRate avAssetTrack =
  sendMessage avAssetTrack nominalFrameRateSelector

-- | Indicates whether samples in the track may have different values for their presentation and decode timestamps.
--
-- ObjC selector: @- requiresFrameReordering@
requiresFrameReordering :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
requiresFrameReordering avAssetTrack =
  sendMessage avAssetTrack requiresFrameReorderingSelector

-- | Indicates the volume specified in the track's storage container as the preferred volume of the audible media data
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CFloat
preferredVolume avAssetTrack =
  sendMessage avAssetTrack preferredVolumeSelector

-- | Indicates whether this audio track has dependencies (e.g. kAudioFormatMPEGD_USAC)
--
-- ObjC selector: @- hasAudioSampleDependencies@
hasAudioSampleDependencies :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
hasAudioSampleDependencies avAssetTrack =
  sendMessage avAssetTrack hasAudioSampleDependenciesSelector

-- | Indicates the language associated with the track, as an ISO 639-2/T language code; may be nil if no language is indicated
--
-- ObjC selector: @- languageCode@
languageCode :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSString)
languageCode avAssetTrack =
  sendMessage avAssetTrack languageCodeSelector

-- | Indicates the language tag associated with the track, as an IETF BCP 47 (RFC 4646) language identifier; may be nil if no language tag is indicated
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSString)
extendedLanguageTag avAssetTrack =
  sendMessage avAssetTrack extendedLanguageTagSelector

-- | Indicates a timescale in which time values for the track can be operated upon without extraneous numerical conversion
--
-- ObjC selector: @- naturalTimeScale@
naturalTimeScale :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CInt
naturalTimeScale avAssetTrack =
  sendMessage avAssetTrack naturalTimeScaleSelector

-- | Indicates the estimated data rate of the media data referenced by the track, in units of bits per second
--
-- ObjC selector: @- estimatedDataRate@
estimatedDataRate :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CFloat
estimatedDataRate avAssetTrack =
  sendMessage avAssetTrack estimatedDataRateSelector

-- | Indicates the media type for this track, e.g. AVMediaTypeVideo, AVMediaTypeAudio, etc., as defined in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSString)
mediaType avAssetTrack =
  sendMessage avAssetTrack mediaTypeSelector

-- | Provides an array of CMFormatDescriptions each of which indicates the format of media samples referenced by the track; a track that presents uniform media, e.g. encoded according to the same encoding settings, will provide an array with a count of 1.
--
-- ObjC selector: @- formatDescriptions@
formatDescriptions :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO (Id NSArray)
formatDescriptions avAssetTrack =
  sendMessage avAssetTrack formatDescriptionsSelector

-- | Indicates whether the receiver is playable in the current environment; if YES, an AVPlayerItemTrack of an AVPlayerItem initialized with the receiver's asset can be enabled for playback.
--
-- ObjC selector: @- playable@
playable :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
playable avAssetTrack =
  sendMessage avAssetTrack playableSelector

-- | Indicates whether the receiver is decodable in the current environment; if YES, the track can be decoded even though decoding may be too slow for real time playback.
--
-- ObjC selector: @- decodable@
decodable :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
decodable avAssetTrack =
  sendMessage avAssetTrack decodableSelector

-- | Indicates whether the track is enabled according to state stored in its container or construct; note that its presentation state can be changed from this default via AVPlayerItemTrack
--
-- ObjC selector: @- enabled@
enabled :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
enabled avAssetTrack =
  sendMessage avAssetTrack enabledSelector

-- | Indicates whether the track references sample data only within its storage container
--
-- ObjC selector: @- selfContained@
selfContained :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO Bool
selfContained avAssetTrack =
  sendMessage avAssetTrack selfContainedSelector

-- | Indicates the total number of bytes of sample data required by the track
--
-- ObjC selector: @- totalSampleDataLength@
totalSampleDataLength :: IsAVAssetTrack avAssetTrack => avAssetTrack -> IO CLong
totalSampleDataLength avAssetTrack =
  sendMessage avAssetTrack totalSampleDataLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetTrack)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetTrack)
newSelector = mkSelector "new"

-- | @Selector@ for @makeSampleCursorAtFirstSampleInDecodeOrder@
makeSampleCursorAtFirstSampleInDecodeOrderSelector :: Selector '[] (Id AVSampleCursor)
makeSampleCursorAtFirstSampleInDecodeOrderSelector = mkSelector "makeSampleCursorAtFirstSampleInDecodeOrder"

-- | @Selector@ for @makeSampleCursorAtLastSampleInDecodeOrder@
makeSampleCursorAtLastSampleInDecodeOrderSelector :: Selector '[] (Id AVSampleCursor)
makeSampleCursorAtLastSampleInDecodeOrderSelector = mkSelector "makeSampleCursorAtLastSampleInDecodeOrder"

-- | @Selector@ for @associatedTracksOfType:@
associatedTracksOfTypeSelector :: Selector '[Id NSString] (Id NSArray)
associatedTracksOfTypeSelector = mkSelector "associatedTracksOfType:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector '[Id NSString] (Id NSArray)
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @hasMediaCharacteristic:@
hasMediaCharacteristicSelector :: Selector '[Id NSString] Bool
hasMediaCharacteristicSelector = mkSelector "hasMediaCharacteristic:"

-- | @Selector@ for @asset@
assetSelector :: Selector '[] (Id AVAsset)
assetSelector = mkSelector "asset"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @canProvideSampleCursors@
canProvideSampleCursorsSelector :: Selector '[] Bool
canProvideSampleCursorsSelector = mkSelector "canProvideSampleCursors"

-- | @Selector@ for @availableTrackAssociationTypes@
availableTrackAssociationTypesSelector :: Selector '[] (Id NSArray)
availableTrackAssociationTypesSelector = mkSelector "availableTrackAssociationTypes"

-- | @Selector@ for @commonMetadata@
commonMetadataSelector :: Selector '[] (Id NSArray)
commonMetadataSelector = mkSelector "commonMetadata"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSArray)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @availableMetadataFormats@
availableMetadataFormatsSelector :: Selector '[] (Id NSArray)
availableMetadataFormatsSelector = mkSelector "availableMetadataFormats"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @nominalFrameRate@
nominalFrameRateSelector :: Selector '[] CFloat
nominalFrameRateSelector = mkSelector "nominalFrameRate"

-- | @Selector@ for @requiresFrameReordering@
requiresFrameReorderingSelector :: Selector '[] Bool
requiresFrameReorderingSelector = mkSelector "requiresFrameReordering"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector '[] CFloat
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @hasAudioSampleDependencies@
hasAudioSampleDependenciesSelector :: Selector '[] Bool
hasAudioSampleDependenciesSelector = mkSelector "hasAudioSampleDependencies"

-- | @Selector@ for @languageCode@
languageCodeSelector :: Selector '[] (Id NSString)
languageCodeSelector = mkSelector "languageCode"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector '[] (Id NSString)
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @naturalTimeScale@
naturalTimeScaleSelector :: Selector '[] CInt
naturalTimeScaleSelector = mkSelector "naturalTimeScale"

-- | @Selector@ for @estimatedDataRate@
estimatedDataRateSelector :: Selector '[] CFloat
estimatedDataRateSelector = mkSelector "estimatedDataRate"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @formatDescriptions@
formatDescriptionsSelector :: Selector '[] (Id NSArray)
formatDescriptionsSelector = mkSelector "formatDescriptions"

-- | @Selector@ for @playable@
playableSelector :: Selector '[] Bool
playableSelector = mkSelector "playable"

-- | @Selector@ for @decodable@
decodableSelector :: Selector '[] Bool
decodableSelector = mkSelector "decodable"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @selfContained@
selfContainedSelector :: Selector '[] Bool
selfContainedSelector = mkSelector "selfContained"

-- | @Selector@ for @totalSampleDataLength@
totalSampleDataLengthSelector :: Selector '[] CLong
totalSampleDataLengthSelector = mkSelector "totalSampleDataLength"

