{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMetadataItem@.
module ObjC.AVFoundation.AVMetadataItem
  ( AVMetadataItem
  , IsAVMetadataItem(..)
  , metadataItemsFromArray_withLocale
  , metadataItemsFromArray_withKey_keySpace
  , metadataItemWithPropertiesOfMetadataItem_valueLoadingHandler
  , identifierForKey_keySpace
  , keySpaceForIdentifier
  , keyForIdentifier
  , metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguages
  , metadataItemsFromArray_filteredByIdentifier
  , metadataItemsFromArray_filteredByMetadataItemFilter
  , statusOfValueForKey_error
  , loadValuesAsynchronouslyForKeys_completionHandler
  , identifier
  , extendedLanguageTag
  , locale
  , dataType
  , value
  , extraAttributes
  , key
  , commonKey
  , keySpace
  , stringValue
  , numberValue
  , dateValue
  , dataValue
  , startDate
  , commonKeySelector
  , dataTypeSelector
  , dataValueSelector
  , dateValueSelector
  , extendedLanguageTagSelector
  , extraAttributesSelector
  , identifierForKey_keySpaceSelector
  , identifierSelector
  , keyForIdentifierSelector
  , keySelector
  , keySpaceForIdentifierSelector
  , keySpaceSelector
  , loadValuesAsynchronouslyForKeys_completionHandlerSelector
  , localeSelector
  , metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector
  , metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector
  , metadataItemsFromArray_filteredByIdentifierSelector
  , metadataItemsFromArray_filteredByMetadataItemFilterSelector
  , metadataItemsFromArray_withKey_keySpaceSelector
  , metadataItemsFromArray_withLocaleSelector
  , numberValueSelector
  , startDateSelector
  , statusOfValueForKey_errorSelector
  , stringValueSelector
  , valueSelector

  -- * Enum types
  , AVKeyValueStatus(AVKeyValueStatus)
  , pattern AVKeyValueStatusUnknown
  , pattern AVKeyValueStatusLoading
  , pattern AVKeyValueStatusLoaded
  , pattern AVKeyValueStatusFailed
  , pattern AVKeyValueStatusCancelled

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

-- | metadataItemsFromArray:withLocale:
--
-- Instead, use metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:.
--
-- ObjC selector: @+ metadataItemsFromArray:withLocale:@
metadataItemsFromArray_withLocale :: (IsNSArray metadataItems, IsNSLocale locale) => metadataItems -> locale -> IO (Id NSArray)
metadataItemsFromArray_withLocale metadataItems locale =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemsFromArray_withLocaleSelector (toNSArray metadataItems) (toNSLocale locale)

-- | metadataItemsFromArray:withKey:keySpace:
--
-- Instead, use metadataItemsFromArray:filteredByIdentifier:.
--
-- ObjC selector: @+ metadataItemsFromArray:withKey:keySpace:@
metadataItemsFromArray_withKey_keySpace :: (IsNSArray metadataItems, IsNSString keySpace) => metadataItems -> RawId -> keySpace -> IO (Id NSArray)
metadataItemsFromArray_withKey_keySpace metadataItems key keySpace =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemsFromArray_withKey_keySpaceSelector (toNSArray metadataItems) key (toNSString keySpace)

-- | metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:
--
-- Creates an instance of AVMutableMetadataItem with a value that you do not wish to load unless required, e.g. a large image value that needn't be loaded into memory until another module wants to display it.
--
-- @metadataItem@ — An instance of AVMetadataItem with the identifier, extendedLanguageTag, and other property values that you want the newly created instance of AVMetadataItem to share. The value of metadataItem is ignored.
--
-- @handler@ — A block that loads the value of the metadata item.
--
-- Returns: An instance of AVMetadataItem.
--
-- This method is intended for the creation of metadata items for optional display purposes, when there is no immediate need to load specific metadata values. For example, see the interface for navigation markers as consumed by AVPlayerViewController. It's not intended for the creation of metadata items with values that are required immediately, such as metadata items that are provided for impending serialization operations (e.g. via -[AVAssetExportSession setMetadata:] and other similar methods defined on AVAssetWriter and AVAssetWriterInput). 		When -loadValuesAsynchronouslyForKeys:completionHandler: is invoked on an AVMetadataItem created via +metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler: and "value" is among the keys for which loading is requested, the block you provide as the value loading handler will be executed on an arbitrary dispatch queue, off the main thread. The handler can perform I/O and other necessary operations to obtain the value. If loading of the value succeeds, provide the value by invoking -[AVMetadataItemValueRequest respondWithValue:]. If loading of the value fails, provide an instance of NSError that describes the failure by invoking -[AVMetadataItemValueRequest respondWithError:].
--
-- ObjC selector: @+ metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:@
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandler :: IsAVMetadataItem metadataItem => metadataItem -> Ptr () -> IO (Id AVMetadataItem)
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandler metadataItem handler =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector (toAVMetadataItem metadataItem) handler

-- | identifierForKey:keySpace:
--
-- Provides the metadata identifier that's equivalent to a key and keySpace.
--
-- @key@ — The metadata key.
--
-- @keySpace@ — The metadata keySpace.
--
-- Returns: A metadata identifier equivalent to the given key and keySpace, or nil if no identifier can be constructed from the given key and keySpace.
--
-- Metadata keys that are not instances of NSString, NSNumber, or NSData cannot be converted to metadata identifiers; they also cannot be written to media resources via AVAssetExportSession or AVAssetWriter.  Metadata item keySpaces must be a string of one to four printable ASCII characters.
--
-- For custom identifiers, the keySpace AVMetadataKeySpaceQuickTimeMetadata is recommended.  This keySpace defines its key values to be expressed as reverse-DNS strings, which allows third parties to define their own keys in a well established way that avoids collisions.
--
-- ObjC selector: @+ identifierForKey:keySpace:@
identifierForKey_keySpace :: IsNSString keySpace => RawId -> keySpace -> IO (Id NSString)
identifierForKey_keySpace key keySpace =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' identifierForKey_keySpaceSelector key (toNSString keySpace)

-- | @+ keySpaceForIdentifier:@
keySpaceForIdentifier :: IsNSString identifier => identifier -> IO (Id NSString)
keySpaceForIdentifier identifier =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' keySpaceForIdentifierSelector (toNSString identifier)

-- | @+ keyForIdentifier:@
keyForIdentifier :: IsNSString identifier => identifier -> IO RawId
keyForIdentifier identifier =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' keyForIdentifierSelector (toNSString identifier)

-- | metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:
--
-- Filters an array of AVMetadataItems according to whether their locales match any language identifier in the specified array of preferred languages. The returned array is sorted according to the order of preference of the language each matches.
--
-- @metadataItems@ — An array of AVMetadataItems to be filtered and sorted.
--
-- @preferredLanguages@ — An array of language identifiers in order of preference, each of which is an IETF BCP 47 (RFC 4646) language identifier. If your goal is to provide the best match for the end user's preferred languages without consideration of your app's available localizations, pass [NSLocale preferredLanguages] as the value of preferredLanguages. However, if you want to filter the available choices in order to obtain the best match among the localizations that are available for your app, pass [NSBundle preferredLocalizationsFromArray:[[NSBundle mainBundle] localizations] forPreferences:[NSLocale preferredLanguages]] instead. The latter choice is normally more appropriate for strings intended for display as part of the app's UI.
--
-- Returns: An instance of NSArray containing metadata items of the specified NSArray that match a preferred language, sorted according to the order of preference of the language each matches.
--
-- ObjC selector: @+ metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:@
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguages :: (IsNSArray metadataItems, IsNSArray preferredLanguages) => metadataItems -> preferredLanguages -> IO (Id NSArray)
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguages metadataItems preferredLanguages =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector (toNSArray metadataItems) (toNSArray preferredLanguages)

-- | metadataItemsFromArray:filteredByIdentifier:
--
-- Filters an array of AVMetadataItems according to identifier.
--
-- @metadataItems@ — An array of AVMetadataItems to be filtered by identifier.
--
-- @identifier@ — The identifier that must be matched for a metadata item to be copied to the output array. Items are considered a match not only when their identifiers are equal to the specified identifier, and also when their identifiers conform to the specified identifier.
--
-- Returns: An instance of NSArray containing the metadata items of the target NSArray that match the specified identifier.
--
-- ObjC selector: @+ metadataItemsFromArray:filteredByIdentifier:@
metadataItemsFromArray_filteredByIdentifier :: (IsNSArray metadataItems, IsNSString identifier) => metadataItems -> identifier -> IO (Id NSArray)
metadataItemsFromArray_filteredByIdentifier metadataItems identifier =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemsFromArray_filteredByIdentifierSelector (toNSArray metadataItems) (toNSString identifier)

-- | metadataItemsFromArray:filteredByMetadataItemFilter:
--
-- Filters an array of AVMetadataItems using the supplied AVMetadataItemFilter.
--
-- @metadataItems@ — An array of AVMetadataItems to be filtered.
--
-- @metadataItemFilter@ — The AVMetadataItemFilter object for filtering the metadataItems.
--
-- Returns: An instance of NSArray containing the metadata items of the target NSArray that have not been removed by metadataItemFilter.
--
-- ObjC selector: @+ metadataItemsFromArray:filteredByMetadataItemFilter:@
metadataItemsFromArray_filteredByMetadataItemFilter :: (IsNSArray metadataItems, IsAVMetadataItemFilter metadataItemFilter) => metadataItems -> metadataItemFilter -> IO (Id NSArray)
metadataItemsFromArray_filteredByMetadataItemFilter metadataItems metadataItemFilter =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    sendClassMessage cls' metadataItemsFromArray_filteredByMetadataItemFilterSelector (toNSArray metadataItems) (toAVMetadataItemFilter metadataItemFilter)

-- | @- statusOfValueForKey:error:@
statusOfValueForKey_error :: (IsAVMetadataItem avMetadataItem, IsNSString key, IsNSError outError) => avMetadataItem -> key -> outError -> IO AVKeyValueStatus
statusOfValueForKey_error avMetadataItem key outError =
  sendMessage avMetadataItem statusOfValueForKey_errorSelector (toNSString key) (toNSError outError)

-- | @- loadValuesAsynchronouslyForKeys:completionHandler:@
loadValuesAsynchronouslyForKeys_completionHandler :: (IsAVMetadataItem avMetadataItem, IsNSArray keys) => avMetadataItem -> keys -> Ptr () -> IO ()
loadValuesAsynchronouslyForKeys_completionHandler avMetadataItem keys handler =
  sendMessage avMetadataItem loadValuesAsynchronouslyForKeys_completionHandlerSelector (toNSArray keys) handler

-- | @- identifier@
identifier :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
identifier avMetadataItem =
  sendMessage avMetadataItem identifierSelector

-- | @- extendedLanguageTag@
extendedLanguageTag :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
extendedLanguageTag avMetadataItem =
  sendMessage avMetadataItem extendedLanguageTagSelector

-- | @- locale@
locale :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSLocale)
locale avMetadataItem =
  sendMessage avMetadataItem localeSelector

-- | @- dataType@
dataType :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
dataType avMetadataItem =
  sendMessage avMetadataItem dataTypeSelector

-- | @- value@
value :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO RawId
value avMetadataItem =
  sendMessage avMetadataItem valueSelector

-- | @- extraAttributes@
extraAttributes :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDictionary)
extraAttributes avMetadataItem =
  sendMessage avMetadataItem extraAttributesSelector

-- | @- key@
key :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO RawId
key avMetadataItem =
  sendMessage avMetadataItem keySelector

-- | @- commonKey@
commonKey :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
commonKey avMetadataItem =
  sendMessage avMetadataItem commonKeySelector

-- | @- keySpace@
keySpace :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
keySpace avMetadataItem =
  sendMessage avMetadataItem keySpaceSelector

-- | @- stringValue@
stringValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
stringValue avMetadataItem =
  sendMessage avMetadataItem stringValueSelector

-- | @- numberValue@
numberValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSNumber)
numberValue avMetadataItem =
  sendMessage avMetadataItem numberValueSelector

-- | @- dateValue@
dateValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDate)
dateValue avMetadataItem =
  sendMessage avMetadataItem dateValueSelector

-- | @- dataValue@
dataValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSData)
dataValue avMetadataItem =
  sendMessage avMetadataItem dataValueSelector

-- | @- startDate@
startDate :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDate)
startDate avMetadataItem =
  sendMessage avMetadataItem startDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItemsFromArray:withLocale:@
metadataItemsFromArray_withLocaleSelector :: Selector '[Id NSArray, Id NSLocale] (Id NSArray)
metadataItemsFromArray_withLocaleSelector = mkSelector "metadataItemsFromArray:withLocale:"

-- | @Selector@ for @metadataItemsFromArray:withKey:keySpace:@
metadataItemsFromArray_withKey_keySpaceSelector :: Selector '[Id NSArray, RawId, Id NSString] (Id NSArray)
metadataItemsFromArray_withKey_keySpaceSelector = mkSelector "metadataItemsFromArray:withKey:keySpace:"

-- | @Selector@ for @metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:@
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector :: Selector '[Id AVMetadataItem, Ptr ()] (Id AVMetadataItem)
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector = mkSelector "metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:"

-- | @Selector@ for @identifierForKey:keySpace:@
identifierForKey_keySpaceSelector :: Selector '[RawId, Id NSString] (Id NSString)
identifierForKey_keySpaceSelector = mkSelector "identifierForKey:keySpace:"

-- | @Selector@ for @keySpaceForIdentifier:@
keySpaceForIdentifierSelector :: Selector '[Id NSString] (Id NSString)
keySpaceForIdentifierSelector = mkSelector "keySpaceForIdentifier:"

-- | @Selector@ for @keyForIdentifier:@
keyForIdentifierSelector :: Selector '[Id NSString] RawId
keyForIdentifierSelector = mkSelector "keyForIdentifier:"

-- | @Selector@ for @metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:@
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector :: Selector '[Id NSArray, Id NSArray] (Id NSArray)
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector = mkSelector "metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:"

-- | @Selector@ for @metadataItemsFromArray:filteredByIdentifier:@
metadataItemsFromArray_filteredByIdentifierSelector :: Selector '[Id NSArray, Id NSString] (Id NSArray)
metadataItemsFromArray_filteredByIdentifierSelector = mkSelector "metadataItemsFromArray:filteredByIdentifier:"

-- | @Selector@ for @metadataItemsFromArray:filteredByMetadataItemFilter:@
metadataItemsFromArray_filteredByMetadataItemFilterSelector :: Selector '[Id NSArray, Id AVMetadataItemFilter] (Id NSArray)
metadataItemsFromArray_filteredByMetadataItemFilterSelector = mkSelector "metadataItemsFromArray:filteredByMetadataItemFilter:"

-- | @Selector@ for @statusOfValueForKey:error:@
statusOfValueForKey_errorSelector :: Selector '[Id NSString, Id NSError] AVKeyValueStatus
statusOfValueForKey_errorSelector = mkSelector "statusOfValueForKey:error:"

-- | @Selector@ for @loadValuesAsynchronouslyForKeys:completionHandler:@
loadValuesAsynchronouslyForKeys_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
loadValuesAsynchronouslyForKeys_completionHandlerSelector = mkSelector "loadValuesAsynchronouslyForKeys:completionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector '[] (Id NSString)
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] (Id NSString)
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @extraAttributes@
extraAttributesSelector :: Selector '[] (Id NSDictionary)
extraAttributesSelector = mkSelector "extraAttributes"

-- | @Selector@ for @key@
keySelector :: Selector '[] RawId
keySelector = mkSelector "key"

-- | @Selector@ for @commonKey@
commonKeySelector :: Selector '[] (Id NSString)
commonKeySelector = mkSelector "commonKey"

-- | @Selector@ for @keySpace@
keySpaceSelector :: Selector '[] (Id NSString)
keySpaceSelector = mkSelector "keySpace"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @numberValue@
numberValueSelector :: Selector '[] (Id NSNumber)
numberValueSelector = mkSelector "numberValue"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector '[] (Id NSDate)
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @dataValue@
dataValueSelector :: Selector '[] (Id NSData)
dataValueSelector = mkSelector "dataValue"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

