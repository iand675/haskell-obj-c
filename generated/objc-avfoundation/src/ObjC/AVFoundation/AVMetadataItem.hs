{-# LANGUAGE PatternSynonyms #-}
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
  , extraAttributes
  , commonKey
  , keySpace
  , stringValue
  , numberValue
  , dateValue
  , dataValue
  , startDate
  , metadataItemsFromArray_withLocaleSelector
  , metadataItemsFromArray_withKey_keySpaceSelector
  , metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector
  , identifierForKey_keySpaceSelector
  , keySpaceForIdentifierSelector
  , keyForIdentifierSelector
  , metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector
  , metadataItemsFromArray_filteredByIdentifierSelector
  , metadataItemsFromArray_filteredByMetadataItemFilterSelector
  , statusOfValueForKey_errorSelector
  , loadValuesAsynchronouslyForKeys_completionHandlerSelector
  , identifierSelector
  , extendedLanguageTagSelector
  , localeSelector
  , dataTypeSelector
  , extraAttributesSelector
  , commonKeySelector
  , keySpaceSelector
  , stringValueSelector
  , numberValueSelector
  , dateValueSelector
  , dataValueSelector
  , startDateSelector

  -- * Enum types
  , AVKeyValueStatus(AVKeyValueStatus)
  , pattern AVKeyValueStatusUnknown
  , pattern AVKeyValueStatusLoading
  , pattern AVKeyValueStatusLoaded
  , pattern AVKeyValueStatusFailed
  , pattern AVKeyValueStatusCancelled

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

-- | metadataItemsFromArray:withLocale:
--
-- Instead, use metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:.
--
-- ObjC selector: @+ metadataItemsFromArray:withLocale:@
metadataItemsFromArray_withLocale :: (IsNSArray metadataItems, IsNSLocale locale) => metadataItems -> locale -> IO (Id NSArray)
metadataItemsFromArray_withLocale metadataItems locale =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    withObjCPtr metadataItems $ \raw_metadataItems ->
      withObjCPtr locale $ \raw_locale ->
        sendClassMsg cls' (mkSelector "metadataItemsFromArray:withLocale:") (retPtr retVoid) [argPtr (castPtr raw_metadataItems :: Ptr ()), argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | metadataItemsFromArray:withKey:keySpace:
--
-- Instead, use metadataItemsFromArray:filteredByIdentifier:.
--
-- ObjC selector: @+ metadataItemsFromArray:withKey:keySpace:@
metadataItemsFromArray_withKey_keySpace :: (IsNSArray metadataItems, IsNSString keySpace) => metadataItems -> RawId -> keySpace -> IO (Id NSArray)
metadataItemsFromArray_withKey_keySpace metadataItems key keySpace =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    withObjCPtr metadataItems $ \raw_metadataItems ->
      withObjCPtr keySpace $ \raw_keySpace ->
        sendClassMsg cls' (mkSelector "metadataItemsFromArray:withKey:keySpace:") (retPtr retVoid) [argPtr (castPtr raw_metadataItems :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ()), argPtr (castPtr raw_keySpace :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr metadataItem $ \raw_metadataItem ->
      sendClassMsg cls' (mkSelector "metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:") (retPtr retVoid) [argPtr (castPtr raw_metadataItem :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr keySpace $ \raw_keySpace ->
      sendClassMsg cls' (mkSelector "identifierForKey:keySpace:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ()), argPtr (castPtr raw_keySpace :: Ptr ())] >>= retainedObject . castPtr

-- | @+ keySpaceForIdentifier:@
keySpaceForIdentifier :: IsNSString identifier => identifier -> IO (Id NSString)
keySpaceForIdentifier identifier =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "keySpaceForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ keyForIdentifier:@
keyForIdentifier :: IsNSString identifier => identifier -> IO RawId
keyForIdentifier identifier =
  do
    cls' <- getRequiredClass "AVMetadataItem"
    withObjCPtr identifier $ \raw_identifier ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "keyForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())]

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
    withObjCPtr metadataItems $ \raw_metadataItems ->
      withObjCPtr preferredLanguages $ \raw_preferredLanguages ->
        sendClassMsg cls' (mkSelector "metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:") (retPtr retVoid) [argPtr (castPtr raw_metadataItems :: Ptr ()), argPtr (castPtr raw_preferredLanguages :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr metadataItems $ \raw_metadataItems ->
      withObjCPtr identifier $ \raw_identifier ->
        sendClassMsg cls' (mkSelector "metadataItemsFromArray:filteredByIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_metadataItems :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr metadataItems $ \raw_metadataItems ->
      withObjCPtr metadataItemFilter $ \raw_metadataItemFilter ->
        sendClassMsg cls' (mkSelector "metadataItemsFromArray:filteredByMetadataItemFilter:") (retPtr retVoid) [argPtr (castPtr raw_metadataItems :: Ptr ()), argPtr (castPtr raw_metadataItemFilter :: Ptr ())] >>= retainedObject . castPtr

-- | @- statusOfValueForKey:error:@
statusOfValueForKey_error :: (IsAVMetadataItem avMetadataItem, IsNSString key, IsNSError outError) => avMetadataItem -> key -> outError -> IO AVKeyValueStatus
statusOfValueForKey_error avMetadataItem  key outError =
withObjCPtr key $ \raw_key ->
  withObjCPtr outError $ \raw_outError ->
      fmap (coerce :: CLong -> AVKeyValueStatus) $ sendMsg avMetadataItem (mkSelector "statusOfValueForKey:error:") retCLong [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- loadValuesAsynchronouslyForKeys:completionHandler:@
loadValuesAsynchronouslyForKeys_completionHandler :: (IsAVMetadataItem avMetadataItem, IsNSArray keys) => avMetadataItem -> keys -> Ptr () -> IO ()
loadValuesAsynchronouslyForKeys_completionHandler avMetadataItem  keys handler =
withObjCPtr keys $ \raw_keys ->
    sendMsg avMetadataItem (mkSelector "loadValuesAsynchronouslyForKeys:completionHandler:") retVoid [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- identifier@
identifier :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
identifier avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- extendedLanguageTag@
extendedLanguageTag :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
extendedLanguageTag avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locale@
locale :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSLocale)
locale avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataType@
dataType :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
dataType avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "dataType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- extraAttributes@
extraAttributes :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDictionary)
extraAttributes avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "extraAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- commonKey@
commonKey :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
commonKey avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "commonKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- keySpace@
keySpace :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
keySpace avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "keySpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringValue@
stringValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSString)
stringValue avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberValue@
numberValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSNumber)
numberValue avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "numberValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateValue@
dateValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDate)
dateValue avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "dateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataValue@
dataValue :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSData)
dataValue avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "dataValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startDate@
startDate :: IsAVMetadataItem avMetadataItem => avMetadataItem -> IO (Id NSDate)
startDate avMetadataItem  =
  sendMsg avMetadataItem (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItemsFromArray:withLocale:@
metadataItemsFromArray_withLocaleSelector :: Selector
metadataItemsFromArray_withLocaleSelector = mkSelector "metadataItemsFromArray:withLocale:"

-- | @Selector@ for @metadataItemsFromArray:withKey:keySpace:@
metadataItemsFromArray_withKey_keySpaceSelector :: Selector
metadataItemsFromArray_withKey_keySpaceSelector = mkSelector "metadataItemsFromArray:withKey:keySpace:"

-- | @Selector@ for @metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:@
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector :: Selector
metadataItemWithPropertiesOfMetadataItem_valueLoadingHandlerSelector = mkSelector "metadataItemWithPropertiesOfMetadataItem:valueLoadingHandler:"

-- | @Selector@ for @identifierForKey:keySpace:@
identifierForKey_keySpaceSelector :: Selector
identifierForKey_keySpaceSelector = mkSelector "identifierForKey:keySpace:"

-- | @Selector@ for @keySpaceForIdentifier:@
keySpaceForIdentifierSelector :: Selector
keySpaceForIdentifierSelector = mkSelector "keySpaceForIdentifier:"

-- | @Selector@ for @keyForIdentifier:@
keyForIdentifierSelector :: Selector
keyForIdentifierSelector = mkSelector "keyForIdentifier:"

-- | @Selector@ for @metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:@
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector :: Selector
metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguagesSelector = mkSelector "metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:"

-- | @Selector@ for @metadataItemsFromArray:filteredByIdentifier:@
metadataItemsFromArray_filteredByIdentifierSelector :: Selector
metadataItemsFromArray_filteredByIdentifierSelector = mkSelector "metadataItemsFromArray:filteredByIdentifier:"

-- | @Selector@ for @metadataItemsFromArray:filteredByMetadataItemFilter:@
metadataItemsFromArray_filteredByMetadataItemFilterSelector :: Selector
metadataItemsFromArray_filteredByMetadataItemFilterSelector = mkSelector "metadataItemsFromArray:filteredByMetadataItemFilter:"

-- | @Selector@ for @statusOfValueForKey:error:@
statusOfValueForKey_errorSelector :: Selector
statusOfValueForKey_errorSelector = mkSelector "statusOfValueForKey:error:"

-- | @Selector@ for @loadValuesAsynchronouslyForKeys:completionHandler:@
loadValuesAsynchronouslyForKeys_completionHandlerSelector :: Selector
loadValuesAsynchronouslyForKeys_completionHandlerSelector = mkSelector "loadValuesAsynchronouslyForKeys:completionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @extraAttributes@
extraAttributesSelector :: Selector
extraAttributesSelector = mkSelector "extraAttributes"

-- | @Selector@ for @commonKey@
commonKeySelector :: Selector
commonKeySelector = mkSelector "commonKey"

-- | @Selector@ for @keySpace@
keySpaceSelector :: Selector
keySpaceSelector = mkSelector "keySpace"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @numberValue@
numberValueSelector :: Selector
numberValueSelector = mkSelector "numberValue"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @dataValue@
dataValueSelector :: Selector
dataValueSelector = mkSelector "dataValue"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

