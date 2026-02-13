{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAsset is an abstract class that defines AVFoundation's model for timed audiovisual media.
--
-- Each asset contains a collection of tracks that are intended to be presented or processed together, each of a uniform media type, including but not limited to audio, video, text, closed captions, and subtitles.
--
-- AVAssets are often instantiated via its concrete subclass AVURLAsset with NSURLs that refer to audiovisual media resources, such as streams (including HTTP live streams), QuickTime movie files, MP3 files, and files of other types.
--
-- They can also be instantiated using other concrete subclasses that extend the basic model for audiovisual media in useful ways, as AVComposition does for temporal editing.
--
-- Properties of assets as a whole are defined by AVAsset. Additionally, references to instances of AVAssetTracks representing tracks of the collection can be obtained, so that each of these can be examined independently.
--
-- Because of the nature of timed audiovisual media, upon successful initialization of an AVAsset some or all of the values for its keys may not be immediately available. The value of any key can be requested at any time, and AVAsset will always return its value synchronously, although it may have to block the calling thread in order to do so.
--
-- In order to avoid blocking, clients can register their interest in particular keys and to become notified when their values become available. For further details, see AVAsynchronousKeyValueLoading.h. For clients who want to examine a subset of the tracks, metadata, and other parts of the asset, asynchronous methods like -loadTracksWithMediaType:completionHandler: can be used to load this information without blocking. When using these asynchronous methods, it is not necessary to load the associated property beforehand. Swift clients can also use the load(:) method to load properties in a type safe manner.
--
-- On platforms other than macOS, it is particularly important to avoid blocking. To preserve responsiveness, a synchronous request that blocks for too long (eg, a property request on an asset on a slow HTTP server) may lead to media services being reset.
--
-- To play an instance of AVAsset, initialize an instance of AVPlayerItem with it, use the AVPlayerItem to set up its presentation state (such as whether only a limited timeRange of the asset should be played, etc.), and provide the AVPlayerItem to an AVPlayer according to whether the items is to be played by itself or together with a collection of other items. Full details available in AVPlayerItem.h and AVPlayer.h.
--
-- AVAssets can also be inserted into AVMutableCompositions in order to assemble audiovisual constructs from one or more source assets.
--
-- Generated bindings for @AVAsset@.
module ObjC.AVFoundation.AVAsset
  ( AVAsset
  , IsAVAsset(..)
  , assetWithURL
  , unusedTrackID
  , findUnusedTrackIDWithCompletionHandler
  , mediaSelectionGroupForMediaCharacteristic
  , loadMediaSelectionGroupForMediaCharacteristic_completionHandler
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys
  , chapterMetadataGroupsBestMatchingPreferredLanguages
  , metadataForFormat
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , cancelLoading
  , preferredRate
  , preferredVolume
  , preferredDisplayCriteria
  , playable
  , exportable
  , readable
  , composable
  , compatibleWithSavedPhotosAlbum
  , compatibleWithAirPlayVideo
  , canContainFragments
  , containsFragments
  , hasProtectedContent
  , availableMediaCharacteristicsWithMediaSelectionOptions
  , preferredMediaSelection
  , allMediaSelections
  , availableChapterLocales
  , creationDate
  , lyrics
  , commonMetadata
  , metadata
  , availableMetadataFormats
  , tracks
  , trackGroups
  , referenceRestrictions
  , providesPreciseDurationAndTiming
  , allMediaSelectionsSelector
  , assetWithURLSelector
  , availableChapterLocalesSelector
  , availableMediaCharacteristicsWithMediaSelectionOptionsSelector
  , availableMetadataFormatsSelector
  , canContainFragmentsSelector
  , cancelLoadingSelector
  , chapterMetadataGroupsBestMatchingPreferredLanguagesSelector
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector
  , commonMetadataSelector
  , compatibleWithAirPlayVideoSelector
  , compatibleWithSavedPhotosAlbumSelector
  , composableSelector
  , containsFragmentsSelector
  , creationDateSelector
  , exportableSelector
  , findUnusedTrackIDWithCompletionHandlerSelector
  , hasProtectedContentSelector
  , loadMediaSelectionGroupForMediaCharacteristic_completionHandlerSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , lyricsSelector
  , mediaSelectionGroupForMediaCharacteristicSelector
  , metadataForFormatSelector
  , metadataSelector
  , playableSelector
  , preferredDisplayCriteriaSelector
  , preferredMediaSelectionSelector
  , preferredRateSelector
  , preferredVolumeSelector
  , providesPreciseDurationAndTimingSelector
  , readableSelector
  , referenceRestrictionsSelector
  , trackGroupsSelector
  , trackWithTrackIDSelector
  , tracksSelector
  , tracksWithMediaCharacteristicSelector
  , tracksWithMediaTypeSelector
  , unusedTrackIDSelector

  -- * Enum types
  , AVAssetReferenceRestrictions(AVAssetReferenceRestrictions)
  , pattern AVAssetReferenceRestrictionForbidNone
  , pattern AVAssetReferenceRestrictionForbidRemoteReferenceToLocal
  , pattern AVAssetReferenceRestrictionForbidLocalReferenceToRemote
  , pattern AVAssetReferenceRestrictionForbidCrossSiteReference
  , pattern AVAssetReferenceRestrictionForbidLocalReferenceToLocal
  , pattern AVAssetReferenceRestrictionForbidAll
  , pattern AVAssetReferenceRestrictionDefaultPolicy

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns an instance of AVAsset for inspection of a media resource.
--
-- Returns a newly allocated instance of a subclass of AVAsset initialized with the specified URL.
--
-- - Parameter URL: An instance of NSURL that references a media resource.
--
-- - Returns: An instance of AVAsset.
--
-- ObjC selector: @+ assetWithURL:@
assetWithURL :: IsNSURL url => url -> IO (Id AVAsset)
assetWithURL url =
  do
    cls' <- getRequiredClass "AVAsset"
    sendClassMessage cls' assetWithURLSelector (toNSURL url)

-- | @- unusedTrackID@
unusedTrackID :: IsAVAsset avAsset => avAsset -> IO CInt
unusedTrackID avAsset =
  sendMessage avAsset unusedTrackIDSelector

-- | Loads a track ID that will not collide with any existing track
--
-- - Parameter completionHandler: A block that is invoked when loading is complete, vending the track ID or an error.
--
-- ObjC selector: @- findUnusedTrackIDWithCompletionHandler:@
findUnusedTrackIDWithCompletionHandler :: IsAVAsset avAsset => avAsset -> Ptr () -> IO ()
findUnusedTrackIDWithCompletionHandler avAsset completionHandler =
  sendMessage avAsset findUnusedTrackIDWithCompletionHandlerSelector completionHandler

-- | Provides an instance of AVMediaSelectionGroup that contains one or more options with the specified media characteristic.
--
-- Becomes callable without blocking when the key "availableMediaCharacteristicsWithMediaSelectionOptions" has been loaded.
--
-- If the asset has no AVMediaSelectionGroup containing options with the specified media characteristic, the return value will be nil.
--
-- Filtering of the options in the returned AVMediaSelectionGroup according to playability, locale, and additional media characteristics can be accomplished using the category AVMediaSelectionOptionFiltering defined on AVMediaSelectionGroup.
--
-- - Parameter mediaCharacteristic: A media characteristic for which you wish to obtain the available media selection options. AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, and AVMediaCharacteristicVisual are currently supported. Pass AVMediaCharacteristicAudible to obtain the group of available options for audio media in various languages and for various purposes, such as descriptive audio. Pass AVMediaCharacteristicLegible to obtain the group of available options for subtitles in various languages and for various purposes. Pass AVMediaCharacteristicVisual to obtain the group of available options for video media.
--
-- - Returns: An instance of AVMediaSelectionGroup. May be nil.
--
-- ObjC selector: @- mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristic :: (IsAVAsset avAsset, IsNSString mediaCharacteristic) => avAsset -> mediaCharacteristic -> IO (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristic avAsset mediaCharacteristic =
  sendMessage avAsset mediaSelectionGroupForMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | Loads an instance of AVMediaSelectionGroup that contains one or more options with the specified media characteristic.
--
-- If the asset has no AVMediaSelectionGroup containing options with the specified media characteristic, the return value will be nil.
--
-- Filtering of the options in the returned AVMediaSelectionGroup according to playability, locale, and additional media characteristics can be accomplished using the category AVMediaSelectionOptionFiltering defined on AVMediaSelectionGroup.
--
-- - Parameter mediaCharacteristic: A media characteristic for which you wish to obtain the available media selection options. AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, and AVMediaCharacteristicVisual are currently supported. Pass AVMediaCharacteristicAudible to obtain the group of available options for audio media in various languages and for various purposes, such as descriptive audio. Pass AVMediaCharacteristicLegible to obtain the group of available options for subtitles in various languages and for various purposes Pass AVMediaCharacteristicVisual to obtain the group of available options for video media. - Parameter completionHandler: A block that is invoked when loading is complete, vending an instance of AVMediaSelectionGroup (which may be nil) or an error.
--
-- ObjC selector: @- loadMediaSelectionGroupForMediaCharacteristic:completionHandler:@
loadMediaSelectionGroupForMediaCharacteristic_completionHandler :: (IsAVAsset avAsset, IsNSString mediaCharacteristic) => avAsset -> mediaCharacteristic -> Ptr () -> IO ()
loadMediaSelectionGroupForMediaCharacteristic_completionHandler avAsset mediaCharacteristic completionHandler =
  sendMessage avAsset loadMediaSelectionGroupForMediaCharacteristic_completionHandlerSelector (toNSString mediaCharacteristic) completionHandler

-- | Provides an array of chapters.
--
-- This method returns an array of AVTimedMetadataGroup objects. Each object in the array always contains an AVMetadataItem representing the chapter title; the timeRange property of the AVTimedMetadataGroup object is equal to the time range of the chapter title item.
--
-- An AVMetadataItem with the specified common key will be added to an existing AVTimedMetadataGroup object if the time range (timestamp and duration) of the metadata item and the metadata group overlaps. The locale of items not carrying chapter titles need not match the specified locale parameter.
--
-- Further filtering of the metadata items in AVTimedMetadataGroups according to language can be accomplished using +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:]; filtering of the metadata items according to locale can be accomplished using +[AVMetadataItem metadataItemsFromArray:withLocale:].
--
-- - Parameter locale: Locale of the metadata items carrying chapter titles to be returned (supports the IETF BCP 47 specification). - Parameter commonKeys: Array of common keys of AVMetadataItem to be included; can be nil. AVMetadataCommonKeyArtwork is the only supported key for now.
--
-- - Returns: An NSArray of AVTimedMetadataGroup.
--
-- ObjC selector: @- chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys :: (IsAVAsset avAsset, IsNSLocale locale, IsNSArray commonKeys) => avAsset -> locale -> commonKeys -> IO (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys avAsset locale commonKeys =
  sendMessage avAsset chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector (toNSLocale locale) (toNSArray commonKeys)

-- | Tests, in order of preference, for a match between language identifiers in the specified array of preferred languages and the available chapter locales, and returns the array of chapters corresponding to the first match that's found.
--
-- Safe to call without blocking when the AVAsset key availableChapterLocales has status AVKeyValueStatusLoaded.
--
-- Returns an array of AVTimedMetadataGroup objects. Each object in the array always contains an AVMetadataItem representing the chapter title; the timeRange property of the AVTimedMetadataGroup object is equal to the time range of the chapter title item.
--
-- All of the available chapter metadata is included in the metadata groups, including items with the common key AVMetadataCommonKeyArtwork, if such items are present. Items not carrying chapter titles will be added to an existing AVTimedMetadataGroup object if the time range (timestamp and duration) of the metadata item and that of the metadata group overlaps. The locale of such items need not match the locale of the chapter titles.
--
-- Further filtering of the metadata items in AVTimedMetadataGroups according to language can be accomplished using +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:]; filtering of the metadata items according to locale can be accomplished using +[AVMetadataItem metadataItemsFromArray:withLocale:].
--
-- - Parameter preferredLanguages: An array of language identifiers in order of preference, each of which is an IETF BCP 47 (RFC 4646) language identifier. If your goal is to provide the best match for the end user's preferred languages without consideration of your app's available localizations, pass [NSLocale preferredLanguages] as the value of preferredLanguages. However, if you want to filter the available choices in order to obtain the best match among the localizations that are available for your app, pass [NSBundle preferredLocalizationsFromArray:[[NSBundle mainBundle] localizations] forPreferences:[NSLocale preferredLanguages]] instead. The latter choice is normally more appropriate for strings intended for display as part of the app's UI.
--
-- - Returns: An NSArray of AVTimedMetadataGroup.
--
-- ObjC selector: @- chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguages :: (IsAVAsset avAsset, IsNSArray preferredLanguages) => avAsset -> preferredLanguages -> IO (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguages avAsset preferredLanguages =
  sendMessage avAsset chapterMetadataGroupsBestMatchingPreferredLanguagesSelector (toNSArray preferredLanguages)

-- | Provides an NSArray of AVMetadataItems, one for each metadata item in the container of the specified format; can subsequently be filtered according to language via +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:], according to locale via +[AVMetadataItem metadataItemsFromArray:withLocale:], or according to key via +[AVMetadataItem metadataItemsFromArray:withKey:keySpace:].
--
-- Becomes callable without blocking when the key "availableMetadataFormats" has been loaded
--
-- - Parameter format: The metadata format for which items are requested.
--
-- - Returns: An NSArray containing AVMetadataItems; may be empty if there is no metadata of the specified format.
--
-- ObjC selector: @- metadataForFormat:@
metadataForFormat :: (IsAVAsset avAsset, IsNSString format) => avAsset -> format -> IO (Id NSArray)
metadataForFormat avAsset format =
  sendMessage avAsset metadataForFormatSelector (toNSString format)

-- | Provides an instance of AVAssetTrack that represents the track of the specified trackID.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter trackID: The trackID of the requested AVAssetTrack.
--
-- - Returns: An instance of AVAssetTrack; may be nil if no track of the specified trackID is available.
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVAsset avAsset => avAsset -> CInt -> IO (Id AVAssetTrack)
trackWithTrackID avAsset trackID =
  sendMessage avAsset trackWithTrackIDSelector trackID

-- | Loads an instance of AVAssetTrack that represents the track of the specified trackID.
--
-- - Parameter trackID: The trackID of the requested AVAssetTrack. - Parameter completionHandler: A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVAsset avAsset => avAsset -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avAsset trackID completionHandler =
  sendMessage avAsset loadTrackWithTrackID_completionHandlerSelector trackID completionHandler

-- | Provides an array of AVAssetTracks of the asset that present media of the specified media type.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter mediaType: The media type according to which AVAsset filters its AVAssetTracks. (Media types are defined in AVMediaFormat.h.)
--
-- - Returns: An NSArray of AVAssetTracks; may be empty if no tracks of the specified media type are available.
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVAsset avAsset, IsNSString mediaType) => avAsset -> mediaType -> IO (Id NSArray)
tracksWithMediaType avAsset mediaType =
  sendMessage avAsset tracksWithMediaTypeSelector (toNSString mediaType)

-- | Provides an array of AVAssetTracks of the asset that present media with the specified characteristic.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- - Parameter mediaCharacteristic: The media characteristic according to which AVAsset filters its AVAssetTracks. (Media characteristics are defined in AVMediaFormat.h.)
--
-- - Returns: An NSArray of AVAssetTracks; may be empty if no tracks with the specified characteristic are available.
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVAsset avAsset, IsNSString mediaCharacteristic) => avAsset -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avAsset mediaCharacteristic =
  sendMessage avAsset tracksWithMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | Cancels the loading of all values for all observers.
--
-- Deallocation or finalization of an instance of AVAsset will implicitly cancel loading if any loading requests are still outstanding.
--
-- ObjC selector: @- cancelLoading@
cancelLoading :: IsAVAsset avAsset => avAsset -> IO ()
cancelLoading avAsset =
  sendMessage avAsset cancelLoadingSelector

-- | Indicates the natural rate at which the asset is to be played; often but not always 1.0
--
-- ObjC selector: @- preferredRate@
preferredRate :: IsAVAsset avAsset => avAsset -> IO CFloat
preferredRate avAsset =
  sendMessage avAsset preferredRateSelector

-- | Indicates the preferred volume at which the audible media of an asset is to be played; often but not always 1.0
--
-- ObjC selector: @- preferredVolume@
preferredVolume :: IsAVAsset avAsset => avAsset -> IO CFloat
preferredVolume avAsset =
  sendMessage avAsset preferredVolumeSelector

-- | Guides to a display mode that is optimal for playing this particular asset.
--
-- ObjC selector: @- preferredDisplayCriteria@
preferredDisplayCriteria :: IsAVAsset avAsset => avAsset -> IO RawId
preferredDisplayCriteria avAsset =
  sendMessage avAsset preferredDisplayCriteriaSelector

-- | Indicates whether an AVPlayer can play the contents of the asset in a manner that meets user expectations.
--
-- A client can attempt playback when playable is NO, this however may lead to a substandard playback experience.
--
-- ObjC selector: @- playable@
playable :: IsAVAsset avAsset => avAsset -> IO Bool
playable avAsset =
  sendMessage avAsset playableSelector

-- | Indicates whether an AVAssetExportSession can be used with the receiver for export
--
-- ObjC selector: @- exportable@
exportable :: IsAVAsset avAsset => avAsset -> IO Bool
exportable avAsset =
  sendMessage avAsset exportableSelector

-- | Indicates whether an AVAssetReader can be used with the receiver for extracting media data
--
-- ObjC selector: @- readable@
readable :: IsAVAsset avAsset => avAsset -> IO Bool
readable avAsset =
  sendMessage avAsset readableSelector

-- | Indicates whether the receiver can be used to build an AVMutableComposition
--
-- ObjC selector: @- composable@
composable :: IsAVAsset avAsset => avAsset -> IO Bool
composable avAsset =
  sendMessage avAsset composableSelector

-- | Indicates whether the receiver can be written to the saved photos album
--
-- ObjC selector: @- compatibleWithSavedPhotosAlbum@
compatibleWithSavedPhotosAlbum :: IsAVAsset avAsset => avAsset -> IO Bool
compatibleWithSavedPhotosAlbum avAsset =
  sendMessage avAsset compatibleWithSavedPhotosAlbumSelector

-- | Indicates whether the asset is compatible with AirPlay Video.
--
-- YES if an AVPlayerItem initialized with the receiver can be played by an external device via AirPlay Video.
--
-- ObjC selector: @- compatibleWithAirPlayVideo@
compatibleWithAirPlayVideo :: IsAVAsset avAsset => avAsset -> IO Bool
compatibleWithAirPlayVideo avAsset =
  sendMessage avAsset compatibleWithAirPlayVideoSelector

-- | Indicates whether the asset is capable of being extended by fragments.
--
-- For QuickTime movie files and MPEG-4 files, the value of canContainFragments is YES if an 'mvex' box is present in the 'moov' box. For those types, the 'mvex' box signals the possible presence of later 'moof' boxes.
--
-- ObjC selector: @- canContainFragments@
canContainFragments :: IsAVAsset avAsset => avAsset -> IO Bool
canContainFragments avAsset =
  sendMessage avAsset canContainFragmentsSelector

-- | Indicates whether the asset is extended by at least one fragment.
--
-- For QuickTime movie files and MPEG-4 files, the value of this property is YES if canContainFragments is YES and at least one 'moof' box is present after the 'moov' box.
--
-- ObjC selector: @- containsFragments@
containsFragments :: IsAVAsset avAsset => avAsset -> IO Bool
containsFragments avAsset =
  sendMessage avAsset containsFragmentsSelector

-- | Indicates whether or not the asset has protected content.
--
-- Assets containing protected content may not be playable without successful authorization, even if the value of the "playable" property is YES. See the properties in the AVAssetUsability category for details on how such an asset may be used. On macOS, clients can use the interfaces in AVPlayerItemProtectedContentAdditions.h to request authorization to play the asset.
--
-- ObjC selector: @- hasProtectedContent@
hasProtectedContent :: IsAVAsset avAsset => avAsset -> IO Bool
hasProtectedContent avAsset =
  sendMessage avAsset hasProtectedContentSelector

-- | Provides an NSArray of NSStrings, each NSString indicating a media characteristic for which a media selection option is available.
--
-- ObjC selector: @- availableMediaCharacteristicsWithMediaSelectionOptions@
availableMediaCharacteristicsWithMediaSelectionOptions :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
availableMediaCharacteristicsWithMediaSelectionOptions avAsset =
  sendMessage avAsset availableMediaCharacteristicsWithMediaSelectionOptionsSelector

-- | Provides an instance of AVMediaSelection with default selections for each of the receiver's media selection groups.
--
-- ObjC selector: @- preferredMediaSelection@
preferredMediaSelection :: IsAVAsset avAsset => avAsset -> IO (Id AVMediaSelection)
preferredMediaSelection avAsset =
  sendMessage avAsset preferredMediaSelectionSelector

-- | Provides an array of all permutations of AVMediaSelection for this asset.
--
-- ObjC selector: @- allMediaSelections@
allMediaSelections :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
allMediaSelections avAsset =
  sendMessage avAsset allMediaSelectionsSelector

-- | array of NSLocale
--
-- ObjC selector: @- availableChapterLocales@
availableChapterLocales :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
availableChapterLocales avAsset =
  sendMessage avAsset availableChapterLocalesSelector

-- | Indicates the creation date of the asset as an AVMetadataItem. May be nil. If a creation date has been stored by the asset in a form that can be converted to an NSDate, the dateValue property of the AVMetadataItem will provide an instance of NSDate. Otherwise the creation date is available only as a string value, via -[AVMetadataItem stringValue].
--
-- ObjC selector: @- creationDate@
creationDate :: IsAVAsset avAsset => avAsset -> IO (Id AVMetadataItem)
creationDate avAsset =
  sendMessage avAsset creationDateSelector

-- | Provides access to the lyrics of the asset suitable for the current locale.
--
-- ObjC selector: @- lyrics@
lyrics :: IsAVAsset avAsset => avAsset -> IO (Id NSString)
lyrics avAsset =
  sendMessage avAsset lyricsSelector

-- | Provides access to an array of AVMetadataItems for each common metadata key for which a value is available; items can be filtered according to language via +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:] and according to identifier via +[AVMetadataItem metadataItemsFromArray:filteredByIdentifier:].
--
-- ObjC selector: @- commonMetadata@
commonMetadata :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
commonMetadata avAsset =
  sendMessage avAsset commonMetadataSelector

-- | Provides access to an array of AVMetadataItems for all metadata identifiers for which a value is available; items can be filtered according to language via +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:] and according to identifier via +[AVMetadataItem metadataItemsFromArray:filteredByIdentifier:].
--
-- ObjC selector: @- metadata@
metadata :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
metadata avAsset =
  sendMessage avAsset metadataSelector

-- | Provides an NSArray of NSStrings, each representing a metadata format that's available to the asset (e.g. ID3, iTunes metadata, etc.). Metadata formats are defined in AVMetadataFormat.h.
--
-- ObjC selector: @- availableMetadataFormats@
availableMetadataFormats :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
availableMetadataFormats avAsset =
  sendMessage avAsset availableMetadataFormatsSelector

-- | Provides the array of AVAssetTracks contained by the asset
--
-- ObjC selector: @- tracks@
tracks :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
tracks avAsset =
  sendMessage avAsset tracksSelector

-- | All track groups in the receiver.
--
-- The value of this property is an NSArray of AVAssetTrackGroups, each representing a different grouping of tracks in the receiver.
--
-- ObjC selector: @- trackGroups@
trackGroups :: IsAVAsset avAsset => avAsset -> IO (Id NSArray)
trackGroups avAsset =
  sendMessage avAsset trackGroupsSelector

-- | Indicates the reference restrictions being used by the receiver.
--
-- For AVURLAsset, this property reflects the value passed in for AVURLAssetReferenceRestrictionsKey, if any. See AVURLAssetReferenceRestrictionsKey below for a full discussion of reference restrictions. The default value for this property is AVAssetReferenceRestrictionForbidLocalReferenceToRemote.
--
-- ObjC selector: @- referenceRestrictions@
referenceRestrictions :: IsAVAsset avAsset => avAsset -> IO AVAssetReferenceRestrictions
referenceRestrictions avAsset =
  sendMessage avAsset referenceRestrictionsSelector

-- | Indicates that the asset provides precise timing. See "duration" above and AVURLAssetPreferPreciseDurationAndTimingKey below.
--
-- ObjC selector: @- providesPreciseDurationAndTiming@
providesPreciseDurationAndTiming :: IsAVAsset avAsset => avAsset -> IO Bool
providesPreciseDurationAndTiming avAsset =
  sendMessage avAsset providesPreciseDurationAndTimingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @assetWithURL:@
assetWithURLSelector :: Selector '[Id NSURL] (Id AVAsset)
assetWithURLSelector = mkSelector "assetWithURL:"

-- | @Selector@ for @unusedTrackID@
unusedTrackIDSelector :: Selector '[] CInt
unusedTrackIDSelector = mkSelector "unusedTrackID"

-- | @Selector@ for @findUnusedTrackIDWithCompletionHandler:@
findUnusedTrackIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
findUnusedTrackIDWithCompletionHandlerSelector = mkSelector "findUnusedTrackIDWithCompletionHandler:"

-- | @Selector@ for @mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristicSelector :: Selector '[Id NSString] (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristicSelector = mkSelector "mediaSelectionGroupForMediaCharacteristic:"

-- | @Selector@ for @loadMediaSelectionGroupForMediaCharacteristic:completionHandler:@
loadMediaSelectionGroupForMediaCharacteristic_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
loadMediaSelectionGroupForMediaCharacteristic_completionHandlerSelector = mkSelector "loadMediaSelectionGroupForMediaCharacteristic:completionHandler:"

-- | @Selector@ for @chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector :: Selector '[Id NSLocale, Id NSArray] (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector = mkSelector "chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:"

-- | @Selector@ for @chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector :: Selector '[Id NSArray] (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector = mkSelector "chapterMetadataGroupsBestMatchingPreferredLanguages:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector '[Id NSString] (Id NSArray)
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector '[CInt] (Id AVAssetTrack)
trackWithTrackIDSelector = mkSelector "trackWithTrackID:"

-- | @Selector@ for @loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandlerSelector :: Selector '[CInt, Ptr ()] ()
loadTrackWithTrackID_completionHandlerSelector = mkSelector "loadTrackWithTrackID:completionHandler:"

-- | @Selector@ for @tracksWithMediaType:@
tracksWithMediaTypeSelector :: Selector '[Id NSString] (Id NSArray)
tracksWithMediaTypeSelector = mkSelector "tracksWithMediaType:"

-- | @Selector@ for @tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristicSelector :: Selector '[Id NSString] (Id NSArray)
tracksWithMediaCharacteristicSelector = mkSelector "tracksWithMediaCharacteristic:"

-- | @Selector@ for @cancelLoading@
cancelLoadingSelector :: Selector '[] ()
cancelLoadingSelector = mkSelector "cancelLoading"

-- | @Selector@ for @preferredRate@
preferredRateSelector :: Selector '[] CFloat
preferredRateSelector = mkSelector "preferredRate"

-- | @Selector@ for @preferredVolume@
preferredVolumeSelector :: Selector '[] CFloat
preferredVolumeSelector = mkSelector "preferredVolume"

-- | @Selector@ for @preferredDisplayCriteria@
preferredDisplayCriteriaSelector :: Selector '[] RawId
preferredDisplayCriteriaSelector = mkSelector "preferredDisplayCriteria"

-- | @Selector@ for @playable@
playableSelector :: Selector '[] Bool
playableSelector = mkSelector "playable"

-- | @Selector@ for @exportable@
exportableSelector :: Selector '[] Bool
exportableSelector = mkSelector "exportable"

-- | @Selector@ for @readable@
readableSelector :: Selector '[] Bool
readableSelector = mkSelector "readable"

-- | @Selector@ for @composable@
composableSelector :: Selector '[] Bool
composableSelector = mkSelector "composable"

-- | @Selector@ for @compatibleWithSavedPhotosAlbum@
compatibleWithSavedPhotosAlbumSelector :: Selector '[] Bool
compatibleWithSavedPhotosAlbumSelector = mkSelector "compatibleWithSavedPhotosAlbum"

-- | @Selector@ for @compatibleWithAirPlayVideo@
compatibleWithAirPlayVideoSelector :: Selector '[] Bool
compatibleWithAirPlayVideoSelector = mkSelector "compatibleWithAirPlayVideo"

-- | @Selector@ for @canContainFragments@
canContainFragmentsSelector :: Selector '[] Bool
canContainFragmentsSelector = mkSelector "canContainFragments"

-- | @Selector@ for @containsFragments@
containsFragmentsSelector :: Selector '[] Bool
containsFragmentsSelector = mkSelector "containsFragments"

-- | @Selector@ for @hasProtectedContent@
hasProtectedContentSelector :: Selector '[] Bool
hasProtectedContentSelector = mkSelector "hasProtectedContent"

-- | @Selector@ for @availableMediaCharacteristicsWithMediaSelectionOptions@
availableMediaCharacteristicsWithMediaSelectionOptionsSelector :: Selector '[] (Id NSArray)
availableMediaCharacteristicsWithMediaSelectionOptionsSelector = mkSelector "availableMediaCharacteristicsWithMediaSelectionOptions"

-- | @Selector@ for @preferredMediaSelection@
preferredMediaSelectionSelector :: Selector '[] (Id AVMediaSelection)
preferredMediaSelectionSelector = mkSelector "preferredMediaSelection"

-- | @Selector@ for @allMediaSelections@
allMediaSelectionsSelector :: Selector '[] (Id NSArray)
allMediaSelectionsSelector = mkSelector "allMediaSelections"

-- | @Selector@ for @availableChapterLocales@
availableChapterLocalesSelector :: Selector '[] (Id NSArray)
availableChapterLocalesSelector = mkSelector "availableChapterLocales"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id AVMetadataItem)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @lyrics@
lyricsSelector :: Selector '[] (Id NSString)
lyricsSelector = mkSelector "lyrics"

-- | @Selector@ for @commonMetadata@
commonMetadataSelector :: Selector '[] (Id NSArray)
commonMetadataSelector = mkSelector "commonMetadata"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSArray)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @availableMetadataFormats@
availableMetadataFormatsSelector :: Selector '[] (Id NSArray)
availableMetadataFormatsSelector = mkSelector "availableMetadataFormats"

-- | @Selector@ for @tracks@
tracksSelector :: Selector '[] (Id NSArray)
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @trackGroups@
trackGroupsSelector :: Selector '[] (Id NSArray)
trackGroupsSelector = mkSelector "trackGroups"

-- | @Selector@ for @referenceRestrictions@
referenceRestrictionsSelector :: Selector '[] AVAssetReferenceRestrictions
referenceRestrictionsSelector = mkSelector "referenceRestrictions"

-- | @Selector@ for @providesPreciseDurationAndTiming@
providesPreciseDurationAndTimingSelector :: Selector '[] Bool
providesPreciseDurationAndTimingSelector = mkSelector "providesPreciseDurationAndTiming"

