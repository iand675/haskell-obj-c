{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVComposition@.
module ObjC.AVFoundation.AVComposition
  ( AVComposition
  , IsAVComposition(..)
  , metadataForFormat
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys
  , chapterMetadataGroupsBestMatchingPreferredLanguages
  , mediaSelectionGroupForMediaCharacteristic
  , unusedTrackID
  , trackWithTrackID
  , loadTrackWithTrackID_completionHandler
  , tracksWithMediaType
  , tracksWithMediaCharacteristic
  , tracks
  , urlAssetInitializationOptions
  , chapterMetadataGroupsBestMatchingPreferredLanguagesSelector
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , mediaSelectionGroupForMediaCharacteristicSelector
  , metadataForFormatSelector
  , trackWithTrackIDSelector
  , tracksSelector
  , tracksWithMediaCharacteristicSelector
  , tracksWithMediaTypeSelector
  , unusedTrackIDSelector
  , urlAssetInitializationOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVComposition avComposition, IsNSString format) => avComposition -> format -> IO (Id NSArray)
metadataForFormat avComposition format =
  sendMessage avComposition metadataForFormatSelector (toNSString format)

-- | @- chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys :: (IsAVComposition avComposition, IsNSLocale locale, IsNSArray commonKeys) => avComposition -> locale -> commonKeys -> IO (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys avComposition locale commonKeys =
  sendMessage avComposition chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector (toNSLocale locale) (toNSArray commonKeys)

-- | @- chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguages :: (IsAVComposition avComposition, IsNSArray preferredLanguages) => avComposition -> preferredLanguages -> IO (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguages avComposition preferredLanguages =
  sendMessage avComposition chapterMetadataGroupsBestMatchingPreferredLanguagesSelector (toNSArray preferredLanguages)

-- | @- mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristic :: (IsAVComposition avComposition, IsNSString mediaCharacteristic) => avComposition -> mediaCharacteristic -> IO (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristic avComposition mediaCharacteristic =
  sendMessage avComposition mediaSelectionGroupForMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | @- unusedTrackID@
unusedTrackID :: IsAVComposition avComposition => avComposition -> IO CInt
unusedTrackID avComposition =
  sendMessage avComposition unusedTrackIDSelector

-- | trackWithTrackID:
--
-- Provides an instance of AVCompositionTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVCompositionTrack.
--
-- Returns: An instance of AVCompositionTrack; may be nil if no track of the specified trackID is available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- trackWithTrackID:@
trackWithTrackID :: IsAVComposition avComposition => avComposition -> CInt -> IO (Id AVCompositionTrack)
trackWithTrackID avComposition trackID =
  sendMessage avComposition trackWithTrackIDSelector trackID

-- | loadTrackWithTrackID:completionHandler:
--
-- Loads an instance of AVCompositionTrack that represents the track of the specified trackID.
--
-- @trackID@ — The trackID of the requested AVCompositionTrack.
--
-- @completionHandler@ — A block that is called when the loading is finished, with either the loaded track (which may be nil if no track of the specified trackID is available) or an error.
--
-- ObjC selector: @- loadTrackWithTrackID:completionHandler:@
loadTrackWithTrackID_completionHandler :: IsAVComposition avComposition => avComposition -> CInt -> Ptr () -> IO ()
loadTrackWithTrackID_completionHandler avComposition trackID completionHandler =
  sendMessage avComposition loadTrackWithTrackID_completionHandlerSelector trackID completionHandler

-- | tracksWithMediaType:
--
-- Provides an array of AVCompositionTracks of the asset that present media of the specified media type.
--
-- @mediaType@ — The media type according to which the receiver filters its AVCompositionTracks. (Media types are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVCompositionTracks; may be empty if no tracks of the specified media type are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaType:@
tracksWithMediaType :: (IsAVComposition avComposition, IsNSString mediaType) => avComposition -> mediaType -> IO (Id NSArray)
tracksWithMediaType avComposition mediaType =
  sendMessage avComposition tracksWithMediaTypeSelector (toNSString mediaType)

-- | tracksWithMediaCharacteristic:
--
-- Provides an array of AVCompositionTracks of the asset that present media with the specified characteristic.
--
-- @mediaCharacteristic@ — The media characteristic according to which the receiver filters its AVCompositionTracks. (Media characteristics are defined in AVMediaFormat.h)
--
-- Returns: An NSArray of AVCompositionTracks; may be empty if no tracks with the specified characteristic are available.
--
-- Becomes callable without blocking when the key "tracks" has been loaded
--
-- ObjC selector: @- tracksWithMediaCharacteristic:@
tracksWithMediaCharacteristic :: (IsAVComposition avComposition, IsNSString mediaCharacteristic) => avComposition -> mediaCharacteristic -> IO (Id NSArray)
tracksWithMediaCharacteristic avComposition mediaCharacteristic =
  sendMessage avComposition tracksWithMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | tracks
--
-- Provides the array of AVCompositionTracks contained by the composition.
--
-- ObjC selector: @- tracks@
tracks :: IsAVComposition avComposition => avComposition -> IO (Id NSArray)
tracks avComposition =
  sendMessage avComposition tracksSelector

-- | URLAssetInitializationOptions
--
-- Specifies the initialization options for the creation of AVURLAssets by the receiver, e.g. AVURLAssetPreferPreciseDurationAndTimingKey. The default behavior for creation of AVURLAssets by an AVComposition is equivalent to the behavior of +[AVURLAsset URLAssetWithURL:options:] when specifying no initialization options.
--
-- AVCompositions create AVURLAssets internally for URLs specified by AVCompositionTrackSegments of AVCompositionTracks, as needed, whenever AVCompositionTrackSegments were originally added to a track via -[AVMutableCompositionTrack setSegments:] rather than by inserting timeranges of already existing AVAssets or AVAssetTracks.      The value of URLAssetInitializationOptions can be specified at the time an AVMutableComposition is created via +compositionWithURLAssetInitializationOptions:.
--
-- ObjC selector: @- URLAssetInitializationOptions@
urlAssetInitializationOptions :: IsAVComposition avComposition => avComposition -> IO (Id NSDictionary)
urlAssetInitializationOptions avComposition =
  sendMessage avComposition urlAssetInitializationOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector '[Id NSString] (Id NSArray)
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector :: Selector '[Id NSLocale, Id NSArray] (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector = mkSelector "chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:"

-- | @Selector@ for @chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector :: Selector '[Id NSArray] (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguagesSelector = mkSelector "chapterMetadataGroupsBestMatchingPreferredLanguages:"

-- | @Selector@ for @mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristicSelector :: Selector '[Id NSString] (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristicSelector = mkSelector "mediaSelectionGroupForMediaCharacteristic:"

-- | @Selector@ for @unusedTrackID@
unusedTrackIDSelector :: Selector '[] CInt
unusedTrackIDSelector = mkSelector "unusedTrackID"

-- | @Selector@ for @trackWithTrackID:@
trackWithTrackIDSelector :: Selector '[CInt] (Id AVCompositionTrack)
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

-- | @Selector@ for @tracks@
tracksSelector :: Selector '[] (Id NSArray)
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @URLAssetInitializationOptions@
urlAssetInitializationOptionsSelector :: Selector '[] (Id NSDictionary)
urlAssetInitializationOptionsSelector = mkSelector "URLAssetInitializationOptions"

