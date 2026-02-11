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
  , metadataForFormatSelector
  , chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeysSelector
  , chapterMetadataGroupsBestMatchingPreferredLanguagesSelector
  , mediaSelectionGroupForMediaCharacteristicSelector
  , unusedTrackIDSelector
  , trackWithTrackIDSelector
  , loadTrackWithTrackID_completionHandlerSelector
  , tracksWithMediaTypeSelector
  , tracksWithMediaCharacteristicSelector
  , tracksSelector
  , urlAssetInitializationOptionsSelector


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

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVComposition avComposition, IsNSString format) => avComposition -> format -> IO (Id NSArray)
metadataForFormat avComposition  format =
withObjCPtr format $ \raw_format ->
    sendMsg avComposition (mkSelector "metadataForFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:@
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys :: (IsAVComposition avComposition, IsNSLocale locale, IsNSArray commonKeys) => avComposition -> locale -> commonKeys -> IO (Id NSArray)
chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys avComposition  locale commonKeys =
withObjCPtr locale $ \raw_locale ->
  withObjCPtr commonKeys $ \raw_commonKeys ->
      sendMsg avComposition (mkSelector "chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr raw_commonKeys :: Ptr ())] >>= retainedObject . castPtr

-- | @- chapterMetadataGroupsBestMatchingPreferredLanguages:@
chapterMetadataGroupsBestMatchingPreferredLanguages :: (IsAVComposition avComposition, IsNSArray preferredLanguages) => avComposition -> preferredLanguages -> IO (Id NSArray)
chapterMetadataGroupsBestMatchingPreferredLanguages avComposition  preferredLanguages =
withObjCPtr preferredLanguages $ \raw_preferredLanguages ->
    sendMsg avComposition (mkSelector "chapterMetadataGroupsBestMatchingPreferredLanguages:") (retPtr retVoid) [argPtr (castPtr raw_preferredLanguages :: Ptr ())] >>= retainedObject . castPtr

-- | @- mediaSelectionGroupForMediaCharacteristic:@
mediaSelectionGroupForMediaCharacteristic :: (IsAVComposition avComposition, IsNSString mediaCharacteristic) => avComposition -> mediaCharacteristic -> IO (Id AVMediaSelectionGroup)
mediaSelectionGroupForMediaCharacteristic avComposition  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    sendMsg avComposition (mkSelector "mediaSelectionGroupForMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | @- unusedTrackID@
unusedTrackID :: IsAVComposition avComposition => avComposition -> IO CInt
unusedTrackID avComposition  =
  sendMsg avComposition (mkSelector "unusedTrackID") retCInt []

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
trackWithTrackID avComposition  trackID =
  sendMsg avComposition (mkSelector "trackWithTrackID:") (retPtr retVoid) [argCInt (fromIntegral trackID)] >>= retainedObject . castPtr

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
loadTrackWithTrackID_completionHandler avComposition  trackID completionHandler =
  sendMsg avComposition (mkSelector "loadTrackWithTrackID:completionHandler:") retVoid [argCInt (fromIntegral trackID), argPtr (castPtr completionHandler :: Ptr ())]

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
tracksWithMediaType avComposition  mediaType =
withObjCPtr mediaType $ \raw_mediaType ->
    sendMsg avComposition (mkSelector "tracksWithMediaType:") (retPtr retVoid) [argPtr (castPtr raw_mediaType :: Ptr ())] >>= retainedObject . castPtr

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
tracksWithMediaCharacteristic avComposition  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    sendMsg avComposition (mkSelector "tracksWithMediaCharacteristic:") (retPtr retVoid) [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())] >>= retainedObject . castPtr

-- | tracks
--
-- Provides the array of AVCompositionTracks contained by the composition.
--
-- ObjC selector: @- tracks@
tracks :: IsAVComposition avComposition => avComposition -> IO (Id NSArray)
tracks avComposition  =
  sendMsg avComposition (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URLAssetInitializationOptions
--
-- Specifies the initialization options for the creation of AVURLAssets by the receiver, e.g. AVURLAssetPreferPreciseDurationAndTimingKey. The default behavior for creation of AVURLAssets by an AVComposition is equivalent to the behavior of +[AVURLAsset URLAssetWithURL:options:] when specifying no initialization options.
--
-- AVCompositions create AVURLAssets internally for URLs specified by AVCompositionTrackSegments of AVCompositionTracks, as needed, whenever AVCompositionTrackSegments were originally added to a track via -[AVMutableCompositionTrack setSegments:] rather than by inserting timeranges of already existing AVAssets or AVAssetTracks.      The value of URLAssetInitializationOptions can be specified at the time an AVMutableComposition is created via +compositionWithURLAssetInitializationOptions:.
--
-- ObjC selector: @- URLAssetInitializationOptions@
urlAssetInitializationOptions :: IsAVComposition avComposition => avComposition -> IO (Id NSDictionary)
urlAssetInitializationOptions avComposition  =
  sendMsg avComposition (mkSelector "URLAssetInitializationOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @URLAssetInitializationOptions@
urlAssetInitializationOptionsSelector :: Selector
urlAssetInitializationOptionsSelector = mkSelector "URLAssetInitializationOptions"

