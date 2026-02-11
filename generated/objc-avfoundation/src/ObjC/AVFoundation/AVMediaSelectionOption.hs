{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMediaSelectionOption represents a specific option for the presentation of media within a group of options.
--
-- Generated bindings for @AVMediaSelectionOption@.
module ObjC.AVFoundation.AVMediaSelectionOption
  ( AVMediaSelectionOption
  , IsAVMediaSelectionOption(..)
  , hasMediaCharacteristic
  , metadataForFormat
  , associatedMediaSelectionOptionInMediaSelectionGroup
  , propertyList
  , displayNameWithLocale
  , mediaType
  , mediaSubTypes
  , playable
  , extendedLanguageTag
  , locale
  , commonMetadata
  , availableMetadataFormats
  , displayName
  , hasMediaCharacteristicSelector
  , metadataForFormatSelector
  , associatedMediaSelectionOptionInMediaSelectionGroupSelector
  , propertyListSelector
  , displayNameWithLocaleSelector
  , mediaTypeSelector
  , mediaSubTypesSelector
  , playableSelector
  , extendedLanguageTagSelector
  , localeSelector
  , commonMetadataSelector
  , availableMetadataFormatsSelector
  , displayNameSelector


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
import ObjC.Foundation.Internal.Classes

-- | Reports whether the media selection option includes media with the specified media characteristic.
--
-- - Parameter mediaCharacteristic: The media characteristic of interest, e.g. AVMediaCharacteristicVisual, AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, etc.
--
-- - Returns: YES if the media selection option includes media with the specified characteristic, otherwise NO.
--
-- ObjC selector: @- hasMediaCharacteristic:@
hasMediaCharacteristic :: (IsAVMediaSelectionOption avMediaSelectionOption, IsNSString mediaCharacteristic) => avMediaSelectionOption -> mediaCharacteristic -> IO Bool
hasMediaCharacteristic avMediaSelectionOption  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMediaSelectionOption (mkSelector "hasMediaCharacteristic:") retCULong [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())]

-- | Provides an NSArray of AVMetadataItems, one for each metadata item in the container of the specified format.
--
-- - Parameter format: The metadata format for which items are requested.
--
-- - Returns: An NSArray containing AVMetadataItems.
--
-- ObjC selector: @- metadataForFormat:@
metadataForFormat :: (IsAVMediaSelectionOption avMediaSelectionOption, IsNSString format) => avMediaSelectionOption -> format -> IO (Id NSArray)
metadataForFormat avMediaSelectionOption  format =
withObjCPtr format $ \raw_format ->
    sendMsg avMediaSelectionOption (mkSelector "metadataForFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | If a media selection option in another group is associated with the specified option, returns a reference to the associated option.
--
-- Audible media selection options often have associated legible media selection options; in particular, audible options are typically associated with forced-only subtitle options with the same locale. See AVMediaCharacteristicContainsOnlyForcedSubtitles in AVMediaFormat.h for a discussion of forced-only subtitles.
--
-- - Parameter mediaSelectionGroup: A media selection group in which an associated option is to be sought.
--
-- - Returns: An instance of AVMediaSelectionOption.
--
-- ObjC selector: @- associatedMediaSelectionOptionInMediaSelectionGroup:@
associatedMediaSelectionOptionInMediaSelectionGroup :: (IsAVMediaSelectionOption avMediaSelectionOption, IsAVMediaSelectionGroup mediaSelectionGroup) => avMediaSelectionOption -> mediaSelectionGroup -> IO (Id AVMediaSelectionOption)
associatedMediaSelectionOptionInMediaSelectionGroup avMediaSelectionOption  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avMediaSelectionOption (mkSelector "associatedMediaSelectionOptionInMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a serializable property list that can be used to obtain an instance of AVMediaSelectionOption representing the same option as the receiver via -[AVMediaSelectionGroup mediaSelectionOptionWithPropertyList:].
--
-- - Returns: A serializable property list that's sufficient to identify the option within its group. For serialization utilities, see NSPropertyList.h.
--
-- ObjC selector: @- propertyList@
propertyList :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO RawId
propertyList avMediaSelectionOption  =
  fmap (RawId . castPtr) $ sendMsg avMediaSelectionOption (mkSelector "propertyList") (retPtr retVoid) []

-- | Provides an NSString suitable for display.
--
-- May use this option's common metadata, media characteristics and locale properties in addition to the provided locale to formulate an NSString intended for display. Will only consider common metadata with the specified locale.
--
-- - Parameter locale: Localize manufactured portions of the string using the specificed locale.
--
-- ObjC selector: @- displayNameWithLocale:@
displayNameWithLocale :: (IsAVMediaSelectionOption avMediaSelectionOption, IsNSLocale locale) => avMediaSelectionOption -> locale -> IO (Id NSString)
displayNameWithLocale avMediaSelectionOption  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg avMediaSelectionOption (mkSelector "displayNameWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | The media type of the media data, e.g. AVMediaTypeAudio, AVMediaTypeSubtitle, etc.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSString)
mediaType avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The mediaSubTypes of the media data associated with the option.
--
-- An NSArray of NSNumbers carrying four character codes (of type FourCharCode) as defined in CoreAudioTypes.h for audio media and in CMFormatDescription.h for video media. Also see CMFormatDescriptionGetMediaSubType in CMFormatDescription.h for more information about media subtypes.
--
-- Note that if no information is available about the encoding of the media presented when a media option is selected, the value of mediaSubTypes will be an empty array. This can occur, for example, with streaming media. In these cases the value of mediaSubTypes should simply not be used as a criteria for selection.
--
-- ObjC selector: @- mediaSubTypes@
mediaSubTypes :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSArray)
mediaSubTypes avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "mediaSubTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether a media selection option is playable.
--
-- If the media data associated with the option cannot be decoded or otherwise rendered, playable is NO.
--
-- ObjC selector: @- playable@
playable :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO Bool
playable avMediaSelectionOption  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMediaSelectionOption (mkSelector "playable") retCULong []

-- | Indicates the RFC 4646 language tag associated with the option. May be nil.
--
-- ObjC selector: @- extendedLanguageTag@
extendedLanguageTag :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSString)
extendedLanguageTag avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the locale for which the media option was authored.
--
-- Use -[NSLocale objectForKey:NSLocaleLanguageCode] to obtain the language code of the locale. See NSLocale.h for additional information.
--
-- ObjC selector: @- locale@
locale :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSLocale)
locale avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of AVMetadataItems for each common metadata key for which a value is available.
--
-- The array of AVMetadataItems can be filtered according to language via +[AVMetadataItem metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:], according to locale via +[AVMetadataItem metadataItemsFromArray:withLocale:], or according to key via +[AVMetadataItem metadataItemsFromArray:withKey:keySpace:]. Example: to obtain the name (or title) of a media selection option in any of the user's preferred languages. ```objc NSString *title = nil; NSArray *titles = [AVMetadataItem metadataItemsFromArray:[mediaSelectionOption commonMetadata] withKey:AVMetadataCommonKeyTitle keySpace:AVMetadataKeySpaceCommon]; if ([titles count] > 0) { 	// Try to get a title that matches one of the user's preferred languages. 	NSArray *titlesForPreferredLanguages = [AVMetadataItem metadataItemsFromArray:titles filteredAndSortedAccordingToPreferredLanguages:[NSLocale preferredLanguages]]; 	if ([titlesForPreferredLanguages count] > 0) 	{ 		title = [[titlesForPreferredLanguages objectAtIndex:0] stringValue]; 	}
--
-- // No matches in any of the preferred languages. Just use the primary title metadata we find. 	if (title == nil) 	{ 		title = [[titles objectAtIndex:0] stringValue]; 	} } ```
--
-- ObjC selector: @- commonMetadata@
commonMetadata :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSArray)
commonMetadata avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "commonMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an NSArray of NSStrings, each representing a metadata format that contains metadata associated with the option (e.g. ID3, iTunes metadata, etc.).
--
-- Metadata formats are defined in AVMetadataFormat.h.
--
-- ObjC selector: @- availableMetadataFormats@
availableMetadataFormats :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSArray)
availableMetadataFormats avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "availableMetadataFormats") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an NSString suitable for display using the current system locale.
--
-- May use this option's common metadata, media characteristics and locale properties in addition to the current system locale to formulate an NSString intended for display. In the event that common metadata is not available in the specified locale, displayName will fall back to considering locales with the multilingual ("mul") then undetermined ("und") locale identifiers. For a display name strictly with the specified locale use displayNameWithLocale: instead.
--
-- ObjC selector: @- displayName@
displayName :: IsAVMediaSelectionOption avMediaSelectionOption => avMediaSelectionOption -> IO (Id NSString)
displayName avMediaSelectionOption  =
  sendMsg avMediaSelectionOption (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasMediaCharacteristic:@
hasMediaCharacteristicSelector :: Selector
hasMediaCharacteristicSelector = mkSelector "hasMediaCharacteristic:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @associatedMediaSelectionOptionInMediaSelectionGroup:@
associatedMediaSelectionOptionInMediaSelectionGroupSelector :: Selector
associatedMediaSelectionOptionInMediaSelectionGroupSelector = mkSelector "associatedMediaSelectionOptionInMediaSelectionGroup:"

-- | @Selector@ for @propertyList@
propertyListSelector :: Selector
propertyListSelector = mkSelector "propertyList"

-- | @Selector@ for @displayNameWithLocale:@
displayNameWithLocaleSelector :: Selector
displayNameWithLocaleSelector = mkSelector "displayNameWithLocale:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaSubTypes@
mediaSubTypesSelector :: Selector
mediaSubTypesSelector = mkSelector "mediaSubTypes"

-- | @Selector@ for @playable@
playableSelector :: Selector
playableSelector = mkSelector "playable"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @commonMetadata@
commonMetadataSelector :: Selector
commonMetadataSelector = mkSelector "commonMetadata"

-- | @Selector@ for @availableMetadataFormats@
availableMetadataFormatsSelector :: Selector
availableMetadataFormatsSelector = mkSelector "availableMetadataFormats"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

