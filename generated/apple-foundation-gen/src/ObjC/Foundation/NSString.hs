{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.Foundation.NSString
  ( NSString
  , IsNSString(..)
  , characterAtIndex
  , init_
  , initWithCoder
  , linguisticTagsInRange_scheme_options_orthography_tokenRanges
  , enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlock
  , stringByAddingPercentEncodingWithAllowedCharacters
  , stringByAddingPercentEscapesUsingEncoding
  , stringByReplacingPercentEscapesUsingEncoding
  , pathWithComponents
  , stringByAppendingPathComponent
  , stringByAppendingPathExtension
  , stringsByAppendingPaths
  , completePathIntoString_caseSensitive_matchesIntoArray_filterTypes
  , getFileSystemRepresentation_maxLength
  , variantFittingPresentationWidth
  , cString
  , lossyCString
  , cStringLength
  , getCString
  , getCString_maxLength
  , getCString_maxLength_range_remainingRange
  , writeToFile_atomically
  , writeToURL_atomically
  , initWithContentsOfFile
  , initWithContentsOfURL
  , stringWithContentsOfFile
  , stringWithContentsOfURL
  , initWithCStringNoCopy_length_freeWhenDone
  , initWithCString_length
  , initWithCString
  , stringWithCString_length
  , stringWithCString
  , getCharacters
  , propertyList
  , propertyListFromStringsFileFormat
  , stringEncodingForData_encodingOptions_convertedString_usedLossyConversion
  , substringFromIndex
  , substringToIndex
  , substringWithRange
  , getCharacters_range
  , compare_
  , compare_options
  , compare_options_range
  , compare_options_range_locale
  , caseInsensitiveCompare
  , localizedCompare
  , localizedCaseInsensitiveCompare
  , localizedStandardCompare
  , isEqualToString
  , hasPrefix
  , hasSuffix
  , commonPrefixWithString_options
  , containsString
  , localizedCaseInsensitiveContainsString
  , localizedStandardContainsString
  , localizedStandardRangeOfString
  , rangeOfString
  , rangeOfString_options
  , rangeOfString_options_range
  , rangeOfString_options_range_locale
  , rangeOfCharacterFromSet
  , rangeOfCharacterFromSet_options
  , rangeOfCharacterFromSet_options_range
  , rangeOfComposedCharacterSequenceAtIndex
  , rangeOfComposedCharacterSequencesForRange
  , stringByAppendingString
  , stringByAppendingFormat
  , uppercaseStringWithLocale
  , lowercaseStringWithLocale
  , capitalizedStringWithLocale
  , getLineStart_end_contentsEnd_forRange
  , lineRangeForRange
  , getParagraphStart_end_contentsEnd_forRange
  , paragraphRangeForRange
  , enumerateSubstringsInRange_options_usingBlock
  , enumerateLinesUsingBlock
  , dataUsingEncoding_allowLossyConversion
  , dataUsingEncoding
  , canBeConvertedToEncoding
  , cStringUsingEncoding
  , getCString_maxLength_encoding
  , getBytes_maxLength_usedLength_encoding_options_range_remainingRange
  , maximumLengthOfBytesUsingEncoding
  , lengthOfBytesUsingEncoding
  , localizedNameOfStringEncoding
  , componentsSeparatedByString
  , componentsSeparatedByCharactersInSet
  , stringByTrimmingCharactersInSet
  , stringByPaddingToLength_withString_startingAtIndex
  , stringByFoldingWithOptions_locale
  , stringByReplacingOccurrencesOfString_withString_options_range
  , stringByReplacingOccurrencesOfString_withString
  , stringByReplacingCharactersInRange_withString
  , stringByApplyingTransform_reverse
  , writeToURL_atomically_encoding_error
  , writeToFile_atomically_encoding_error
  , initWithCharactersNoCopy_length_freeWhenDone
  , initWithCharactersNoCopy_length_deallocator
  , initWithCharacters_length
  , initWithUTF8String
  , initWithString
  , initWithFormat
  , initWithFormat_arguments
  , initWithFormat_locale
  , initWithFormat_locale_arguments
  , initWithValidatedFormat_validFormatSpecifiers_error
  , initWithValidatedFormat_validFormatSpecifiers_locale_error
  , initWithValidatedFormat_validFormatSpecifiers_arguments_error
  , initWithValidatedFormat_validFormatSpecifiers_locale_arguments_error
  , initWithData_encoding
  , initWithBytes_length_encoding
  , initWithBytesNoCopy_length_encoding_freeWhenDone
  , initWithBytesNoCopy_length_encoding_deallocator
  , string
  , stringWithString
  , stringWithCharacters_length
  , stringWithUTF8String
  , stringWithFormat
  , localizedStringWithFormat
  , stringWithValidatedFormat_validFormatSpecifiers_error
  , localizedStringWithValidatedFormat_validFormatSpecifiers_error
  , initWithCString_encoding
  , stringWithCString_encoding
  , initWithContentsOfURL_encoding_error
  , initWithContentsOfFile_encoding_error
  , stringWithContentsOfURL_encoding_error
  , stringWithContentsOfFile_encoding_error
  , initWithContentsOfURL_usedEncoding_error
  , initWithContentsOfFile_usedEncoding_error
  , stringWithContentsOfURL_usedEncoding_error
  , stringWithContentsOfFile_usedEncoding_error
  , length_
  , stringByRemovingPercentEncoding
  , pathComponents
  , absolutePath
  , lastPathComponent
  , stringByDeletingLastPathComponent
  , pathExtension
  , stringByDeletingPathExtension
  , stringByAbbreviatingWithTildeInPath
  , stringByExpandingTildeInPath
  , stringByStandardizingPath
  , stringByResolvingSymlinksInPath
  , fileSystemRepresentation
  , doubleValue
  , floatValue
  , intValue
  , integerValue
  , longLongValue
  , boolValue
  , uppercaseString
  , lowercaseString
  , capitalizedString
  , localizedUppercaseString
  , localizedLowercaseString
  , localizedCapitalizedString
  , utF8String
  , fastestEncoding
  , smallestEncoding
  , availableStringEncodings
  , defaultCStringEncoding
  , decomposedStringWithCanonicalMapping
  , precomposedStringWithCanonicalMapping
  , decomposedStringWithCompatibilityMapping
  , precomposedStringWithCompatibilityMapping
  , description
  , hash
  , absolutePathSelector
  , availableStringEncodingsSelector
  , boolValueSelector
  , cStringLengthSelector
  , cStringSelector
  , cStringUsingEncodingSelector
  , canBeConvertedToEncodingSelector
  , capitalizedStringSelector
  , capitalizedStringWithLocaleSelector
  , caseInsensitiveCompareSelector
  , characterAtIndexSelector
  , commonPrefixWithString_optionsSelector
  , compareSelector
  , compare_optionsSelector
  , compare_options_rangeSelector
  , compare_options_range_localeSelector
  , completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector
  , componentsSeparatedByCharactersInSetSelector
  , componentsSeparatedByStringSelector
  , containsStringSelector
  , dataUsingEncodingSelector
  , dataUsingEncoding_allowLossyConversionSelector
  , decomposedStringWithCanonicalMappingSelector
  , decomposedStringWithCompatibilityMappingSelector
  , defaultCStringEncodingSelector
  , descriptionSelector
  , doubleValueSelector
  , enumerateLinesUsingBlockSelector
  , enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector
  , enumerateSubstringsInRange_options_usingBlockSelector
  , fastestEncodingSelector
  , fileSystemRepresentationSelector
  , floatValueSelector
  , getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector
  , getCStringSelector
  , getCString_maxLengthSelector
  , getCString_maxLength_encodingSelector
  , getCString_maxLength_range_remainingRangeSelector
  , getCharactersSelector
  , getCharacters_rangeSelector
  , getFileSystemRepresentation_maxLengthSelector
  , getLineStart_end_contentsEnd_forRangeSelector
  , getParagraphStart_end_contentsEnd_forRangeSelector
  , hasPrefixSelector
  , hasSuffixSelector
  , hashSelector
  , initSelector
  , initWithBytesNoCopy_length_encoding_deallocatorSelector
  , initWithBytesNoCopy_length_encoding_freeWhenDoneSelector
  , initWithBytes_length_encodingSelector
  , initWithCStringNoCopy_length_freeWhenDoneSelector
  , initWithCStringSelector
  , initWithCString_encodingSelector
  , initWithCString_lengthSelector
  , initWithCharactersNoCopy_length_deallocatorSelector
  , initWithCharactersNoCopy_length_freeWhenDoneSelector
  , initWithCharacters_lengthSelector
  , initWithCoderSelector
  , initWithContentsOfFileSelector
  , initWithContentsOfFile_encoding_errorSelector
  , initWithContentsOfFile_usedEncoding_errorSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_encoding_errorSelector
  , initWithContentsOfURL_usedEncoding_errorSelector
  , initWithData_encodingSelector
  , initWithFormatSelector
  , initWithFormat_argumentsSelector
  , initWithFormat_localeSelector
  , initWithFormat_locale_argumentsSelector
  , initWithStringSelector
  , initWithUTF8StringSelector
  , initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector
  , intValueSelector
  , integerValueSelector
  , isEqualToStringSelector
  , lastPathComponentSelector
  , lengthOfBytesUsingEncodingSelector
  , lengthSelector
  , lineRangeForRangeSelector
  , linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector
  , localizedCapitalizedStringSelector
  , localizedCaseInsensitiveCompareSelector
  , localizedCaseInsensitiveContainsStringSelector
  , localizedCompareSelector
  , localizedLowercaseStringSelector
  , localizedNameOfStringEncodingSelector
  , localizedStandardCompareSelector
  , localizedStandardContainsStringSelector
  , localizedStandardRangeOfStringSelector
  , localizedStringWithFormatSelector
  , localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector
  , localizedUppercaseStringSelector
  , longLongValueSelector
  , lossyCStringSelector
  , lowercaseStringSelector
  , lowercaseStringWithLocaleSelector
  , maximumLengthOfBytesUsingEncodingSelector
  , paragraphRangeForRangeSelector
  , pathComponentsSelector
  , pathExtensionSelector
  , pathWithComponentsSelector
  , precomposedStringWithCanonicalMappingSelector
  , precomposedStringWithCompatibilityMappingSelector
  , propertyListFromStringsFileFormatSelector
  , propertyListSelector
  , rangeOfCharacterFromSetSelector
  , rangeOfCharacterFromSet_optionsSelector
  , rangeOfCharacterFromSet_options_rangeSelector
  , rangeOfComposedCharacterSequenceAtIndexSelector
  , rangeOfComposedCharacterSequencesForRangeSelector
  , rangeOfStringSelector
  , rangeOfString_optionsSelector
  , rangeOfString_options_rangeSelector
  , rangeOfString_options_range_localeSelector
  , smallestEncodingSelector
  , stringByAbbreviatingWithTildeInPathSelector
  , stringByAddingPercentEncodingWithAllowedCharactersSelector
  , stringByAddingPercentEscapesUsingEncodingSelector
  , stringByAppendingFormatSelector
  , stringByAppendingPathComponentSelector
  , stringByAppendingPathExtensionSelector
  , stringByAppendingStringSelector
  , stringByApplyingTransform_reverseSelector
  , stringByDeletingLastPathComponentSelector
  , stringByDeletingPathExtensionSelector
  , stringByExpandingTildeInPathSelector
  , stringByFoldingWithOptions_localeSelector
  , stringByPaddingToLength_withString_startingAtIndexSelector
  , stringByRemovingPercentEncodingSelector
  , stringByReplacingCharactersInRange_withStringSelector
  , stringByReplacingOccurrencesOfString_withStringSelector
  , stringByReplacingOccurrencesOfString_withString_options_rangeSelector
  , stringByReplacingPercentEscapesUsingEncodingSelector
  , stringByResolvingSymlinksInPathSelector
  , stringByStandardizingPathSelector
  , stringByTrimmingCharactersInSetSelector
  , stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector
  , stringSelector
  , stringWithCStringSelector
  , stringWithCString_encodingSelector
  , stringWithCString_lengthSelector
  , stringWithCharacters_lengthSelector
  , stringWithContentsOfFileSelector
  , stringWithContentsOfFile_encoding_errorSelector
  , stringWithContentsOfFile_usedEncoding_errorSelector
  , stringWithContentsOfURLSelector
  , stringWithContentsOfURL_encoding_errorSelector
  , stringWithContentsOfURL_usedEncoding_errorSelector
  , stringWithFormatSelector
  , stringWithStringSelector
  , stringWithUTF8StringSelector
  , stringWithValidatedFormat_validFormatSpecifiers_errorSelector
  , stringsByAppendingPathsSelector
  , substringFromIndexSelector
  , substringToIndexSelector
  , substringWithRangeSelector
  , uppercaseStringSelector
  , uppercaseStringWithLocaleSelector
  , utF8StringSelector
  , variantFittingPresentationWidthSelector
  , writeToFile_atomicallySelector
  , writeToFile_atomically_encoding_errorSelector
  , writeToURL_atomicallySelector
  , writeToURL_atomically_encoding_errorSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending
  , NSLinguisticTaggerOptions(NSLinguisticTaggerOptions)
  , pattern NSLinguisticTaggerOmitWords
  , pattern NSLinguisticTaggerOmitPunctuation
  , pattern NSLinguisticTaggerOmitWhitespace
  , pattern NSLinguisticTaggerOmitOther
  , pattern NSLinguisticTaggerJoinNames
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch
  , NSStringEncodingConversionOptions(NSStringEncodingConversionOptions)
  , pattern NSStringEncodingConversionAllowLossy
  , pattern NSStringEncodingConversionExternalRepresentation
  , NSStringEnumerationOptions(NSStringEnumerationOptions)
  , pattern NSStringEnumerationByLines
  , pattern NSStringEnumerationByParagraphs
  , pattern NSStringEnumerationByComposedCharacterSequences
  , pattern NSStringEnumerationByWords
  , pattern NSStringEnumerationBySentences
  , pattern NSStringEnumerationByCaretPositions
  , pattern NSStringEnumerationByDeletionClusters
  , pattern NSStringEnumerationReverse
  , pattern NSStringEnumerationSubstringNotRequired
  , pattern NSStringEnumerationLocalized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @- characterAtIndex:@
characterAtIndex :: IsNSString nsString => nsString -> CULong -> IO CUShort
characterAtIndex nsString index =
  sendMessage nsString characterAtIndexSelector index

-- | @- init@
init_ :: IsNSString nsString => nsString -> IO (Id NSString)
init_ nsString =
  sendOwnedMessage nsString initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSString nsString, IsNSCoder coder) => nsString -> coder -> IO (Id NSString)
initWithCoder nsString coder =
  sendOwnedMessage nsString initWithCoderSelector (toNSCoder coder)

-- | @- linguisticTagsInRange:scheme:options:orthography:tokenRanges:@
linguisticTagsInRange_scheme_options_orthography_tokenRanges :: (IsNSString nsString, IsNSString scheme, IsNSOrthography orthography, IsNSArray tokenRanges) => nsString -> NSRange -> scheme -> NSLinguisticTaggerOptions -> orthography -> tokenRanges -> IO (Id NSArray)
linguisticTagsInRange_scheme_options_orthography_tokenRanges nsString range scheme options orthography tokenRanges =
  sendMessage nsString linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector range (toNSString scheme) options (toNSOrthography orthography) (toNSArray tokenRanges)

-- | @- enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:@
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlock :: (IsNSString nsString, IsNSString scheme, IsNSOrthography orthography) => nsString -> NSRange -> scheme -> NSLinguisticTaggerOptions -> orthography -> Ptr () -> IO ()
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlock nsString range scheme options orthography block =
  sendMessage nsString enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector range (toNSString scheme) options (toNSOrthography orthography) block

-- | @- stringByAddingPercentEncodingWithAllowedCharacters:@
stringByAddingPercentEncodingWithAllowedCharacters :: (IsNSString nsString, IsNSCharacterSet allowedCharacters) => nsString -> allowedCharacters -> IO (Id NSString)
stringByAddingPercentEncodingWithAllowedCharacters nsString allowedCharacters =
  sendMessage nsString stringByAddingPercentEncodingWithAllowedCharactersSelector (toNSCharacterSet allowedCharacters)

-- | @- stringByAddingPercentEscapesUsingEncoding:@
stringByAddingPercentEscapesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
stringByAddingPercentEscapesUsingEncoding nsString enc =
  sendMessage nsString stringByAddingPercentEscapesUsingEncodingSelector enc

-- | @- stringByReplacingPercentEscapesUsingEncoding:@
stringByReplacingPercentEscapesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
stringByReplacingPercentEscapesUsingEncoding nsString enc =
  sendMessage nsString stringByReplacingPercentEscapesUsingEncodingSelector enc

-- | @+ pathWithComponents:@
pathWithComponents :: IsNSArray components => components -> IO (Id NSString)
pathWithComponents components =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' pathWithComponentsSelector (toNSArray components)

-- | @- stringByAppendingPathComponent:@
stringByAppendingPathComponent :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO (Id NSString)
stringByAppendingPathComponent nsString str =
  sendMessage nsString stringByAppendingPathComponentSelector (toNSString str)

-- | @- stringByAppendingPathExtension:@
stringByAppendingPathExtension :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO (Id NSString)
stringByAppendingPathExtension nsString str =
  sendMessage nsString stringByAppendingPathExtensionSelector (toNSString str)

-- | @- stringsByAppendingPaths:@
stringsByAppendingPaths :: (IsNSString nsString, IsNSArray paths) => nsString -> paths -> IO (Id NSArray)
stringsByAppendingPaths nsString paths =
  sendMessage nsString stringsByAppendingPathsSelector (toNSArray paths)

-- | @- completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:@
completePathIntoString_caseSensitive_matchesIntoArray_filterTypes :: (IsNSString nsString, IsNSString outputName, IsNSArray outputArray, IsNSArray filterTypes) => nsString -> outputName -> Bool -> outputArray -> filterTypes -> IO CULong
completePathIntoString_caseSensitive_matchesIntoArray_filterTypes nsString outputName flag outputArray filterTypes =
  sendMessage nsString completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector (toNSString outputName) flag (toNSArray outputArray) (toNSArray filterTypes)

-- | @- getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLength :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> IO Bool
getFileSystemRepresentation_maxLength nsString cname max_ =
  sendMessage nsString getFileSystemRepresentation_maxLengthSelector cname max_

-- | @- variantFittingPresentationWidth:@
variantFittingPresentationWidth :: IsNSString nsString => nsString -> CLong -> IO (Id NSString)
variantFittingPresentationWidth nsString width =
  sendMessage nsString variantFittingPresentationWidthSelector width

-- | @- cString@
cString :: IsNSString nsString => nsString -> IO (Const (Ptr CChar))
cString nsString =
  sendMessage nsString cStringSelector

-- | @- lossyCString@
lossyCString :: IsNSString nsString => nsString -> IO (Const (Ptr CChar))
lossyCString nsString =
  sendMessage nsString lossyCStringSelector

-- | @- cStringLength@
cStringLength :: IsNSString nsString => nsString -> IO CULong
cStringLength nsString =
  sendMessage nsString cStringLengthSelector

-- | @- getCString:@
getCString :: IsNSString nsString => nsString -> Ptr CChar -> IO ()
getCString nsString bytes =
  sendMessage nsString getCStringSelector bytes

-- | @- getCString:maxLength:@
getCString_maxLength :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> IO ()
getCString_maxLength nsString bytes maxLength =
  sendMessage nsString getCString_maxLengthSelector bytes maxLength

-- | @- getCString:maxLength:range:remainingRange:@
getCString_maxLength_range_remainingRange :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> NSRange -> Ptr NSRange -> IO ()
getCString_maxLength_range_remainingRange nsString bytes maxLength aRange leftoverRange =
  sendMessage nsString getCString_maxLength_range_remainingRangeSelector bytes maxLength aRange leftoverRange

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSString nsString, IsNSString path) => nsString -> path -> Bool -> IO Bool
writeToFile_atomically nsString path useAuxiliaryFile =
  sendMessage nsString writeToFile_atomicallySelector (toNSString path) useAuxiliaryFile

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSString nsString, IsNSURL url) => nsString -> url -> Bool -> IO Bool
writeToURL_atomically nsString url atomically =
  sendMessage nsString writeToURL_atomicallySelector (toNSURL url) atomically

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSString nsString, IsNSString path) => nsString -> path -> IO RawId
initWithContentsOfFile nsString path =
  sendOwnedMessage nsString initWithContentsOfFileSelector (toNSString path)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSString nsString, IsNSURL url) => nsString -> url -> IO RawId
initWithContentsOfURL nsString url =
  sendOwnedMessage nsString initWithContentsOfURLSelector (toNSURL url)

-- | @+ stringWithContentsOfFile:@
stringWithContentsOfFile :: IsNSString path => path -> IO RawId
stringWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfFileSelector (toNSString path)

-- | @+ stringWithContentsOfURL:@
stringWithContentsOfURL :: IsNSURL url => url -> IO RawId
stringWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfURLSelector (toNSURL url)

-- | @- initWithCStringNoCopy:length:freeWhenDone:@
initWithCStringNoCopy_length_freeWhenDone :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> Bool -> IO RawId
initWithCStringNoCopy_length_freeWhenDone nsString bytes length_ freeBuffer =
  sendOwnedMessage nsString initWithCStringNoCopy_length_freeWhenDoneSelector bytes length_ freeBuffer

-- | @- initWithCString:length:@
initWithCString_length :: IsNSString nsString => nsString -> Const (Ptr CChar) -> CULong -> IO RawId
initWithCString_length nsString bytes length_ =
  sendOwnedMessage nsString initWithCString_lengthSelector bytes length_

-- | @- initWithCString:@
initWithCString :: IsNSString nsString => nsString -> Const (Ptr CChar) -> IO RawId
initWithCString nsString bytes =
  sendOwnedMessage nsString initWithCStringSelector bytes

-- | @+ stringWithCString:length:@
stringWithCString_length :: Const (Ptr CChar) -> CULong -> IO RawId
stringWithCString_length bytes length_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithCString_lengthSelector bytes length_

-- | @+ stringWithCString:@
stringWithCString :: Const (Ptr CChar) -> IO RawId
stringWithCString bytes =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithCStringSelector bytes

-- | @- getCharacters:@
getCharacters :: IsNSString nsString => nsString -> Ptr CUShort -> IO ()
getCharacters nsString buffer =
  sendMessage nsString getCharactersSelector buffer

-- | @- propertyList@
propertyList :: IsNSString nsString => nsString -> IO RawId
propertyList nsString =
  sendMessage nsString propertyListSelector

-- | @- propertyListFromStringsFileFormat@
propertyListFromStringsFileFormat :: IsNSString nsString => nsString -> IO (Id NSDictionary)
propertyListFromStringsFileFormat nsString =
  sendMessage nsString propertyListFromStringsFileFormatSelector

-- | @+ stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:@
stringEncodingForData_encodingOptions_convertedString_usedLossyConversion :: (IsNSData data_, IsNSDictionary opts, IsNSString string) => data_ -> opts -> string -> Ptr Bool -> IO CULong
stringEncodingForData_encodingOptions_convertedString_usedLossyConversion data_ opts string usedLossyConversion =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector (toNSData data_) (toNSDictionary opts) (toNSString string) usedLossyConversion

-- | @- substringFromIndex:@
substringFromIndex :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
substringFromIndex nsString from =
  sendMessage nsString substringFromIndexSelector from

-- | @- substringToIndex:@
substringToIndex :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
substringToIndex nsString to =
  sendMessage nsString substringToIndexSelector to

-- | @- substringWithRange:@
substringWithRange :: IsNSString nsString => nsString -> NSRange -> IO (Id NSString)
substringWithRange nsString range =
  sendMessage nsString substringWithRangeSelector range

-- | @- getCharacters:range:@
getCharacters_range :: IsNSString nsString => nsString -> Ptr CUShort -> NSRange -> IO ()
getCharacters_range nsString buffer range =
  sendMessage nsString getCharacters_rangeSelector buffer range

-- | @- compare:@
compare_ :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
compare_ nsString string =
  sendMessage nsString compareSelector (toNSString string)

-- | @- compare:options:@
compare_options :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> IO NSComparisonResult
compare_options nsString string mask =
  sendMessage nsString compare_optionsSelector (toNSString string) mask

-- | @- compare:options:range:@
compare_options_range :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> NSRange -> IO NSComparisonResult
compare_options_range nsString string mask rangeOfReceiverToCompare =
  sendMessage nsString compare_options_rangeSelector (toNSString string) mask rangeOfReceiverToCompare

-- | @- compare:options:range:locale:@
compare_options_range_locale :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> NSRange -> RawId -> IO NSComparisonResult
compare_options_range_locale nsString string mask rangeOfReceiverToCompare locale =
  sendMessage nsString compare_options_range_localeSelector (toNSString string) mask rangeOfReceiverToCompare locale

-- | @- caseInsensitiveCompare:@
caseInsensitiveCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
caseInsensitiveCompare nsString string =
  sendMessage nsString caseInsensitiveCompareSelector (toNSString string)

-- | @- localizedCompare:@
localizedCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedCompare nsString string =
  sendMessage nsString localizedCompareSelector (toNSString string)

-- | @- localizedCaseInsensitiveCompare:@
localizedCaseInsensitiveCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedCaseInsensitiveCompare nsString string =
  sendMessage nsString localizedCaseInsensitiveCompareSelector (toNSString string)

-- | @- localizedStandardCompare:@
localizedStandardCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedStandardCompare nsString string =
  sendMessage nsString localizedStandardCompareSelector (toNSString string)

-- | @- isEqualToString:@
isEqualToString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO Bool
isEqualToString nsString aString =
  sendMessage nsString isEqualToStringSelector (toNSString aString)

-- | @- hasPrefix:@
hasPrefix :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
hasPrefix nsString str =
  sendMessage nsString hasPrefixSelector (toNSString str)

-- | @- hasSuffix:@
hasSuffix :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
hasSuffix nsString str =
  sendMessage nsString hasSuffixSelector (toNSString str)

-- | @- commonPrefixWithString:options:@
commonPrefixWithString_options :: (IsNSString nsString, IsNSString str) => nsString -> str -> NSStringCompareOptions -> IO (Id NSString)
commonPrefixWithString_options nsString str mask =
  sendMessage nsString commonPrefixWithString_optionsSelector (toNSString str) mask

-- | @- containsString:@
containsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
containsString nsString str =
  sendMessage nsString containsStringSelector (toNSString str)

-- | @- localizedCaseInsensitiveContainsString:@
localizedCaseInsensitiveContainsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
localizedCaseInsensitiveContainsString nsString str =
  sendMessage nsString localizedCaseInsensitiveContainsStringSelector (toNSString str)

-- | @- localizedStandardContainsString:@
localizedStandardContainsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
localizedStandardContainsString nsString str =
  sendMessage nsString localizedStandardContainsStringSelector (toNSString str)

-- | @- localizedStandardRangeOfString:@
localizedStandardRangeOfString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO NSRange
localizedStandardRangeOfString nsString str =
  sendMessage nsString localizedStandardRangeOfStringSelector (toNSString str)

-- | @- rangeOfString:@
rangeOfString :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> IO NSRange
rangeOfString nsString searchString =
  sendMessage nsString rangeOfStringSelector (toNSString searchString)

-- | @- rangeOfString:options:@
rangeOfString_options :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> NSStringCompareOptions -> IO NSRange
rangeOfString_options nsString searchString mask =
  sendMessage nsString rangeOfString_optionsSelector (toNSString searchString) mask

-- | @- rangeOfString:options:range:@
rangeOfString_options_range :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> NSStringCompareOptions -> NSRange -> IO NSRange
rangeOfString_options_range nsString searchString mask rangeOfReceiverToSearch =
  sendMessage nsString rangeOfString_options_rangeSelector (toNSString searchString) mask rangeOfReceiverToSearch

-- | @- rangeOfString:options:range:locale:@
rangeOfString_options_range_locale :: (IsNSString nsString, IsNSString searchString, IsNSLocale locale) => nsString -> searchString -> NSStringCompareOptions -> NSRange -> locale -> IO NSRange
rangeOfString_options_range_locale nsString searchString mask rangeOfReceiverToSearch locale =
  sendMessage nsString rangeOfString_options_range_localeSelector (toNSString searchString) mask rangeOfReceiverToSearch (toNSLocale locale)

-- | @- rangeOfCharacterFromSet:@
rangeOfCharacterFromSet :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> IO NSRange
rangeOfCharacterFromSet nsString searchSet =
  sendMessage nsString rangeOfCharacterFromSetSelector (toNSCharacterSet searchSet)

-- | @- rangeOfCharacterFromSet:options:@
rangeOfCharacterFromSet_options :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> NSStringCompareOptions -> IO NSRange
rangeOfCharacterFromSet_options nsString searchSet mask =
  sendMessage nsString rangeOfCharacterFromSet_optionsSelector (toNSCharacterSet searchSet) mask

-- | @- rangeOfCharacterFromSet:options:range:@
rangeOfCharacterFromSet_options_range :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> NSStringCompareOptions -> NSRange -> IO NSRange
rangeOfCharacterFromSet_options_range nsString searchSet mask rangeOfReceiverToSearch =
  sendMessage nsString rangeOfCharacterFromSet_options_rangeSelector (toNSCharacterSet searchSet) mask rangeOfReceiverToSearch

-- | @- rangeOfComposedCharacterSequenceAtIndex:@
rangeOfComposedCharacterSequenceAtIndex :: IsNSString nsString => nsString -> CULong -> IO NSRange
rangeOfComposedCharacterSequenceAtIndex nsString index =
  sendMessage nsString rangeOfComposedCharacterSequenceAtIndexSelector index

-- | @- rangeOfComposedCharacterSequencesForRange:@
rangeOfComposedCharacterSequencesForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
rangeOfComposedCharacterSequencesForRange nsString range =
  sendMessage nsString rangeOfComposedCharacterSequencesForRangeSelector range

-- | @- stringByAppendingString:@
stringByAppendingString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO (Id NSString)
stringByAppendingString nsString aString =
  sendMessage nsString stringByAppendingStringSelector (toNSString aString)

-- | @- stringByAppendingFormat:@
stringByAppendingFormat :: (IsNSString nsString, IsNSString format) => nsString -> format -> IO (Id NSString)
stringByAppendingFormat nsString format =
  sendMessage nsString stringByAppendingFormatSelector (toNSString format)

-- | @- uppercaseStringWithLocale:@
uppercaseStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
uppercaseStringWithLocale nsString locale =
  sendMessage nsString uppercaseStringWithLocaleSelector (toNSLocale locale)

-- | @- lowercaseStringWithLocale:@
lowercaseStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
lowercaseStringWithLocale nsString locale =
  sendMessage nsString lowercaseStringWithLocaleSelector (toNSLocale locale)

-- | @- capitalizedStringWithLocale:@
capitalizedStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
capitalizedStringWithLocale nsString locale =
  sendMessage nsString capitalizedStringWithLocaleSelector (toNSLocale locale)

-- | @- getLineStart:end:contentsEnd:forRange:@
getLineStart_end_contentsEnd_forRange :: IsNSString nsString => nsString -> Ptr CULong -> Ptr CULong -> Ptr CULong -> NSRange -> IO ()
getLineStart_end_contentsEnd_forRange nsString startPtr lineEndPtr contentsEndPtr range =
  sendMessage nsString getLineStart_end_contentsEnd_forRangeSelector startPtr lineEndPtr contentsEndPtr range

-- | @- lineRangeForRange:@
lineRangeForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
lineRangeForRange nsString range =
  sendMessage nsString lineRangeForRangeSelector range

-- | @- getParagraphStart:end:contentsEnd:forRange:@
getParagraphStart_end_contentsEnd_forRange :: IsNSString nsString => nsString -> Ptr CULong -> Ptr CULong -> Ptr CULong -> NSRange -> IO ()
getParagraphStart_end_contentsEnd_forRange nsString startPtr parEndPtr contentsEndPtr range =
  sendMessage nsString getParagraphStart_end_contentsEnd_forRangeSelector startPtr parEndPtr contentsEndPtr range

-- | @- paragraphRangeForRange:@
paragraphRangeForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
paragraphRangeForRange nsString range =
  sendMessage nsString paragraphRangeForRangeSelector range

-- | @- enumerateSubstringsInRange:options:usingBlock:@
enumerateSubstringsInRange_options_usingBlock :: IsNSString nsString => nsString -> NSRange -> NSStringEnumerationOptions -> Ptr () -> IO ()
enumerateSubstringsInRange_options_usingBlock nsString range opts block =
  sendMessage nsString enumerateSubstringsInRange_options_usingBlockSelector range opts block

-- | @- enumerateLinesUsingBlock:@
enumerateLinesUsingBlock :: IsNSString nsString => nsString -> Ptr () -> IO ()
enumerateLinesUsingBlock nsString block =
  sendMessage nsString enumerateLinesUsingBlockSelector block

-- | @- dataUsingEncoding:allowLossyConversion:@
dataUsingEncoding_allowLossyConversion :: IsNSString nsString => nsString -> CULong -> Bool -> IO (Id NSData)
dataUsingEncoding_allowLossyConversion nsString encoding lossy =
  sendMessage nsString dataUsingEncoding_allowLossyConversionSelector encoding lossy

-- | @- dataUsingEncoding:@
dataUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSData)
dataUsingEncoding nsString encoding =
  sendMessage nsString dataUsingEncodingSelector encoding

-- | @- canBeConvertedToEncoding:@
canBeConvertedToEncoding :: IsNSString nsString => nsString -> CULong -> IO Bool
canBeConvertedToEncoding nsString encoding =
  sendMessage nsString canBeConvertedToEncodingSelector encoding

-- | @- cStringUsingEncoding:@
cStringUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Const (Ptr CChar))
cStringUsingEncoding nsString encoding =
  sendMessage nsString cStringUsingEncodingSelector encoding

-- | @- getCString:maxLength:encoding:@
getCString_maxLength_encoding :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> CULong -> IO Bool
getCString_maxLength_encoding nsString buffer maxBufferCount encoding =
  sendMessage nsString getCString_maxLength_encodingSelector buffer maxBufferCount encoding

-- | @- getBytes:maxLength:usedLength:encoding:options:range:remainingRange:@
getBytes_maxLength_usedLength_encoding_options_range_remainingRange :: IsNSString nsString => nsString -> Ptr () -> CULong -> Ptr CULong -> CULong -> NSStringEncodingConversionOptions -> NSRange -> Ptr NSRange -> IO Bool
getBytes_maxLength_usedLength_encoding_options_range_remainingRange nsString buffer maxBufferCount usedBufferCount encoding options range leftover =
  sendMessage nsString getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector buffer maxBufferCount usedBufferCount encoding options range leftover

-- | @- maximumLengthOfBytesUsingEncoding:@
maximumLengthOfBytesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO CULong
maximumLengthOfBytesUsingEncoding nsString enc =
  sendMessage nsString maximumLengthOfBytesUsingEncodingSelector enc

-- | @- lengthOfBytesUsingEncoding:@
lengthOfBytesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO CULong
lengthOfBytesUsingEncoding nsString enc =
  sendMessage nsString lengthOfBytesUsingEncodingSelector enc

-- | @+ localizedNameOfStringEncoding:@
localizedNameOfStringEncoding :: CULong -> IO (Id NSString)
localizedNameOfStringEncoding encoding =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' localizedNameOfStringEncodingSelector encoding

-- | @- componentsSeparatedByString:@
componentsSeparatedByString :: (IsNSString nsString, IsNSString separator) => nsString -> separator -> IO (Id NSArray)
componentsSeparatedByString nsString separator =
  sendMessage nsString componentsSeparatedByStringSelector (toNSString separator)

-- | @- componentsSeparatedByCharactersInSet:@
componentsSeparatedByCharactersInSet :: (IsNSString nsString, IsNSCharacterSet separator) => nsString -> separator -> IO (Id NSArray)
componentsSeparatedByCharactersInSet nsString separator =
  sendMessage nsString componentsSeparatedByCharactersInSetSelector (toNSCharacterSet separator)

-- | @- stringByTrimmingCharactersInSet:@
stringByTrimmingCharactersInSet :: (IsNSString nsString, IsNSCharacterSet set) => nsString -> set -> IO (Id NSString)
stringByTrimmingCharactersInSet nsString set =
  sendMessage nsString stringByTrimmingCharactersInSetSelector (toNSCharacterSet set)

-- | @- stringByPaddingToLength:withString:startingAtIndex:@
stringByPaddingToLength_withString_startingAtIndex :: (IsNSString nsString, IsNSString padString) => nsString -> CULong -> padString -> CULong -> IO (Id NSString)
stringByPaddingToLength_withString_startingAtIndex nsString newLength padString padIndex =
  sendMessage nsString stringByPaddingToLength_withString_startingAtIndexSelector newLength (toNSString padString) padIndex

-- | @- stringByFoldingWithOptions:locale:@
stringByFoldingWithOptions_locale :: (IsNSString nsString, IsNSLocale locale) => nsString -> NSStringCompareOptions -> locale -> IO (Id NSString)
stringByFoldingWithOptions_locale nsString options locale =
  sendMessage nsString stringByFoldingWithOptions_localeSelector options (toNSLocale locale)

-- | @- stringByReplacingOccurrencesOfString:withString:options:range:@
stringByReplacingOccurrencesOfString_withString_options_range :: (IsNSString nsString, IsNSString target, IsNSString replacement) => nsString -> target -> replacement -> NSStringCompareOptions -> NSRange -> IO (Id NSString)
stringByReplacingOccurrencesOfString_withString_options_range nsString target replacement options searchRange =
  sendMessage nsString stringByReplacingOccurrencesOfString_withString_options_rangeSelector (toNSString target) (toNSString replacement) options searchRange

-- | @- stringByReplacingOccurrencesOfString:withString:@
stringByReplacingOccurrencesOfString_withString :: (IsNSString nsString, IsNSString target, IsNSString replacement) => nsString -> target -> replacement -> IO (Id NSString)
stringByReplacingOccurrencesOfString_withString nsString target replacement =
  sendMessage nsString stringByReplacingOccurrencesOfString_withStringSelector (toNSString target) (toNSString replacement)

-- | @- stringByReplacingCharactersInRange:withString:@
stringByReplacingCharactersInRange_withString :: (IsNSString nsString, IsNSString replacement) => nsString -> NSRange -> replacement -> IO (Id NSString)
stringByReplacingCharactersInRange_withString nsString range replacement =
  sendMessage nsString stringByReplacingCharactersInRange_withStringSelector range (toNSString replacement)

-- | @- stringByApplyingTransform:reverse:@
stringByApplyingTransform_reverse :: (IsNSString nsString, IsNSString transform) => nsString -> transform -> Bool -> IO (Id NSString)
stringByApplyingTransform_reverse nsString transform reverse_ =
  sendMessage nsString stringByApplyingTransform_reverseSelector (toNSString transform) reverse_

-- | @- writeToURL:atomically:encoding:error:@
writeToURL_atomically_encoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> Bool -> CULong -> error_ -> IO Bool
writeToURL_atomically_encoding_error nsString url useAuxiliaryFile enc error_ =
  sendMessage nsString writeToURL_atomically_encoding_errorSelector (toNSURL url) useAuxiliaryFile enc (toNSError error_)

-- | @- writeToFile:atomically:encoding:error:@
writeToFile_atomically_encoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> Bool -> CULong -> error_ -> IO Bool
writeToFile_atomically_encoding_error nsString path useAuxiliaryFile enc error_ =
  sendMessage nsString writeToFile_atomically_encoding_errorSelector (toNSString path) useAuxiliaryFile enc (toNSError error_)

-- | @- initWithCharactersNoCopy:length:freeWhenDone:@
initWithCharactersNoCopy_length_freeWhenDone :: IsNSString nsString => nsString -> Ptr CUShort -> CULong -> Bool -> IO (Id NSString)
initWithCharactersNoCopy_length_freeWhenDone nsString characters length_ freeBuffer =
  sendOwnedMessage nsString initWithCharactersNoCopy_length_freeWhenDoneSelector characters length_ freeBuffer

-- | @- initWithCharactersNoCopy:length:deallocator:@
initWithCharactersNoCopy_length_deallocator :: IsNSString nsString => nsString -> Ptr CUShort -> CULong -> Ptr () -> IO (Id NSString)
initWithCharactersNoCopy_length_deallocator nsString chars len deallocator =
  sendOwnedMessage nsString initWithCharactersNoCopy_length_deallocatorSelector chars len deallocator

-- | @- initWithCharacters:length:@
initWithCharacters_length :: IsNSString nsString => nsString -> Const (Ptr CUShort) -> CULong -> IO (Id NSString)
initWithCharacters_length nsString characters length_ =
  sendOwnedMessage nsString initWithCharacters_lengthSelector characters length_

-- | @- initWithUTF8String:@
initWithUTF8String :: IsNSString nsString => nsString -> Const (Ptr CChar) -> IO (Id NSString)
initWithUTF8String nsString nullTerminatedCString =
  sendOwnedMessage nsString initWithUTF8StringSelector nullTerminatedCString

-- | @- initWithString:@
initWithString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO (Id NSString)
initWithString nsString aString =
  sendOwnedMessage nsString initWithStringSelector (toNSString aString)

-- | @- initWithFormat:@
initWithFormat :: (IsNSString nsString, IsNSString format) => nsString -> format -> IO (Id NSString)
initWithFormat nsString format =
  sendOwnedMessage nsString initWithFormatSelector (toNSString format)

-- | @- initWithFormat:arguments:@
initWithFormat_arguments :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> IO (Id NSString)
initWithFormat_arguments nsString format argList =
  sendOwnedMessage nsString initWithFormat_argumentsSelector (toNSString format) argList

-- | @- initWithFormat:locale:@
initWithFormat_locale :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> IO (Id NSString)
initWithFormat_locale nsString format locale =
  sendOwnedMessage nsString initWithFormat_localeSelector (toNSString format) locale

-- | @- initWithFormat:locale:arguments:@
initWithFormat_locale_arguments :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> RawId -> IO (Id NSString)
initWithFormat_locale_arguments nsString format locale argList =
  sendOwnedMessage nsString initWithFormat_locale_argumentsSelector (toNSString format) locale argList

-- | @- initWithValidatedFormat:validFormatSpecifiers:error:@
initWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_error nsString format validFormatSpecifiers error_ =
  sendOwnedMessage nsString initWithValidatedFormat_validFormatSpecifiers_errorSelector (toNSString format) (toNSString validFormatSpecifiers) (toNSError error_)

-- | @- initWithValidatedFormat:validFormatSpecifiers:locale:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_error nsString format validFormatSpecifiers locale error_ =
  sendOwnedMessage nsString initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector (toNSString format) (toNSString validFormatSpecifiers) locale (toNSError error_)

-- | @- initWithValidatedFormat:validFormatSpecifiers:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_arguments_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_arguments_error nsString format validFormatSpecifiers argList error_ =
  sendOwnedMessage nsString initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector (toNSString format) (toNSString validFormatSpecifiers) argList (toNSError error_)

-- | @- initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_error nsString format validFormatSpecifiers locale argList error_ =
  sendOwnedMessage nsString initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector (toNSString format) (toNSString validFormatSpecifiers) locale argList (toNSError error_)

-- | @- initWithData:encoding:@
initWithData_encoding :: (IsNSString nsString, IsNSData data_) => nsString -> data_ -> CULong -> IO (Id NSString)
initWithData_encoding nsString data_ encoding =
  sendOwnedMessage nsString initWithData_encodingSelector (toNSData data_) encoding

-- | @- initWithBytes:length:encoding:@
initWithBytes_length_encoding :: IsNSString nsString => nsString -> Const (Ptr ()) -> CULong -> CULong -> IO (Id NSString)
initWithBytes_length_encoding nsString bytes len encoding =
  sendOwnedMessage nsString initWithBytes_length_encodingSelector bytes len encoding

-- | @- initWithBytesNoCopy:length:encoding:freeWhenDone:@
initWithBytesNoCopy_length_encoding_freeWhenDone :: IsNSString nsString => nsString -> Ptr () -> CULong -> CULong -> Bool -> IO (Id NSString)
initWithBytesNoCopy_length_encoding_freeWhenDone nsString bytes len encoding freeBuffer =
  sendOwnedMessage nsString initWithBytesNoCopy_length_encoding_freeWhenDoneSelector bytes len encoding freeBuffer

-- | @- initWithBytesNoCopy:length:encoding:deallocator:@
initWithBytesNoCopy_length_encoding_deallocator :: IsNSString nsString => nsString -> Ptr () -> CULong -> CULong -> Ptr () -> IO (Id NSString)
initWithBytesNoCopy_length_encoding_deallocator nsString bytes len encoding deallocator =
  sendOwnedMessage nsString initWithBytesNoCopy_length_encoding_deallocatorSelector bytes len encoding deallocator

-- | @+ string@
string :: IO (Id NSString)
string  =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringSelector

-- | @+ stringWithString:@
stringWithString :: IsNSString string => string -> IO (Id NSString)
stringWithString string =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithStringSelector (toNSString string)

-- | @+ stringWithCharacters:length:@
stringWithCharacters_length :: Const (Ptr CUShort) -> CULong -> IO (Id NSString)
stringWithCharacters_length characters length_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithCharacters_lengthSelector characters length_

-- | @+ stringWithUTF8String:@
stringWithUTF8String :: Const (Ptr CChar) -> IO (Id NSString)
stringWithUTF8String nullTerminatedCString =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithUTF8StringSelector nullTerminatedCString

-- | @+ stringWithFormat:@
stringWithFormat :: IsNSString format => format -> IO (Id NSString)
stringWithFormat format =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithFormatSelector (toNSString format)

-- | @+ localizedStringWithFormat:@
localizedStringWithFormat :: IsNSString format => format -> IO (Id NSString)
localizedStringWithFormat format =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' localizedStringWithFormatSelector (toNSString format)

-- | @+ stringWithValidatedFormat:validFormatSpecifiers:error:@
stringWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
stringWithValidatedFormat_validFormatSpecifiers_error format validFormatSpecifiers error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithValidatedFormat_validFormatSpecifiers_errorSelector (toNSString format) (toNSString validFormatSpecifiers) (toNSError error_)

-- | @+ localizedStringWithValidatedFormat:validFormatSpecifiers:error:@
localizedStringWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
localizedStringWithValidatedFormat_validFormatSpecifiers_error format validFormatSpecifiers error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector (toNSString format) (toNSString validFormatSpecifiers) (toNSError error_)

-- | @- initWithCString:encoding:@
initWithCString_encoding :: IsNSString nsString => nsString -> Const (Ptr CChar) -> CULong -> IO (Id NSString)
initWithCString_encoding nsString nullTerminatedCString encoding =
  sendOwnedMessage nsString initWithCString_encodingSelector nullTerminatedCString encoding

-- | @+ stringWithCString:encoding:@
stringWithCString_encoding :: Const (Ptr CChar) -> CULong -> IO (Id NSString)
stringWithCString_encoding cString enc =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithCString_encodingSelector cString enc

-- | @- initWithContentsOfURL:encoding:error:@
initWithContentsOfURL_encoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> CULong -> error_ -> IO (Id NSString)
initWithContentsOfURL_encoding_error nsString url enc error_ =
  sendOwnedMessage nsString initWithContentsOfURL_encoding_errorSelector (toNSURL url) enc (toNSError error_)

-- | @- initWithContentsOfFile:encoding:error:@
initWithContentsOfFile_encoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> CULong -> error_ -> IO (Id NSString)
initWithContentsOfFile_encoding_error nsString path enc error_ =
  sendOwnedMessage nsString initWithContentsOfFile_encoding_errorSelector (toNSString path) enc (toNSError error_)

-- | @+ stringWithContentsOfURL:encoding:error:@
stringWithContentsOfURL_encoding_error :: (IsNSURL url, IsNSError error_) => url -> CULong -> error_ -> IO (Id NSString)
stringWithContentsOfURL_encoding_error url enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfURL_encoding_errorSelector (toNSURL url) enc (toNSError error_)

-- | @+ stringWithContentsOfFile:encoding:error:@
stringWithContentsOfFile_encoding_error :: (IsNSString path, IsNSError error_) => path -> CULong -> error_ -> IO (Id NSString)
stringWithContentsOfFile_encoding_error path enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfFile_encoding_errorSelector (toNSString path) enc (toNSError error_)

-- | @- initWithContentsOfURL:usedEncoding:error:@
initWithContentsOfURL_usedEncoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> Ptr CULong -> error_ -> IO (Id NSString)
initWithContentsOfURL_usedEncoding_error nsString url enc error_ =
  sendOwnedMessage nsString initWithContentsOfURL_usedEncoding_errorSelector (toNSURL url) enc (toNSError error_)

-- | @- initWithContentsOfFile:usedEncoding:error:@
initWithContentsOfFile_usedEncoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> Ptr CULong -> error_ -> IO (Id NSString)
initWithContentsOfFile_usedEncoding_error nsString path enc error_ =
  sendOwnedMessage nsString initWithContentsOfFile_usedEncoding_errorSelector (toNSString path) enc (toNSError error_)

-- | @+ stringWithContentsOfURL:usedEncoding:error:@
stringWithContentsOfURL_usedEncoding_error :: (IsNSURL url, IsNSError error_) => url -> Ptr CULong -> error_ -> IO (Id NSString)
stringWithContentsOfURL_usedEncoding_error url enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfURL_usedEncoding_errorSelector (toNSURL url) enc (toNSError error_)

-- | @+ stringWithContentsOfFile:usedEncoding:error:@
stringWithContentsOfFile_usedEncoding_error :: (IsNSString path, IsNSError error_) => path -> Ptr CULong -> error_ -> IO (Id NSString)
stringWithContentsOfFile_usedEncoding_error path enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' stringWithContentsOfFile_usedEncoding_errorSelector (toNSString path) enc (toNSError error_)

-- | @- length@
length_ :: IsNSString nsString => nsString -> IO CULong
length_ nsString =
  sendMessage nsString lengthSelector

-- | @- stringByRemovingPercentEncoding@
stringByRemovingPercentEncoding :: IsNSString nsString => nsString -> IO (Id NSString)
stringByRemovingPercentEncoding nsString =
  sendMessage nsString stringByRemovingPercentEncodingSelector

-- | @- pathComponents@
pathComponents :: IsNSString nsString => nsString -> IO (Id NSArray)
pathComponents nsString =
  sendMessage nsString pathComponentsSelector

-- | @- absolutePath@
absolutePath :: IsNSString nsString => nsString -> IO Bool
absolutePath nsString =
  sendMessage nsString absolutePathSelector

-- | @- lastPathComponent@
lastPathComponent :: IsNSString nsString => nsString -> IO (Id NSString)
lastPathComponent nsString =
  sendMessage nsString lastPathComponentSelector

-- | @- stringByDeletingLastPathComponent@
stringByDeletingLastPathComponent :: IsNSString nsString => nsString -> IO (Id NSString)
stringByDeletingLastPathComponent nsString =
  sendMessage nsString stringByDeletingLastPathComponentSelector

-- | @- pathExtension@
pathExtension :: IsNSString nsString => nsString -> IO (Id NSString)
pathExtension nsString =
  sendMessage nsString pathExtensionSelector

-- | @- stringByDeletingPathExtension@
stringByDeletingPathExtension :: IsNSString nsString => nsString -> IO (Id NSString)
stringByDeletingPathExtension nsString =
  sendMessage nsString stringByDeletingPathExtensionSelector

-- | @- stringByAbbreviatingWithTildeInPath@
stringByAbbreviatingWithTildeInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByAbbreviatingWithTildeInPath nsString =
  sendMessage nsString stringByAbbreviatingWithTildeInPathSelector

-- | @- stringByExpandingTildeInPath@
stringByExpandingTildeInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByExpandingTildeInPath nsString =
  sendMessage nsString stringByExpandingTildeInPathSelector

-- | @- stringByStandardizingPath@
stringByStandardizingPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByStandardizingPath nsString =
  sendMessage nsString stringByStandardizingPathSelector

-- | @- stringByResolvingSymlinksInPath@
stringByResolvingSymlinksInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByResolvingSymlinksInPath nsString =
  sendMessage nsString stringByResolvingSymlinksInPathSelector

-- | @- fileSystemRepresentation@
fileSystemRepresentation :: IsNSString nsString => nsString -> IO (Ptr CChar)
fileSystemRepresentation nsString =
  sendMessage nsString fileSystemRepresentationSelector

-- | @- doubleValue@
doubleValue :: IsNSString nsString => nsString -> IO CDouble
doubleValue nsString =
  sendMessage nsString doubleValueSelector

-- | @- floatValue@
floatValue :: IsNSString nsString => nsString -> IO CFloat
floatValue nsString =
  sendMessage nsString floatValueSelector

-- | @- intValue@
intValue :: IsNSString nsString => nsString -> IO CInt
intValue nsString =
  sendMessage nsString intValueSelector

-- | @- integerValue@
integerValue :: IsNSString nsString => nsString -> IO CLong
integerValue nsString =
  sendMessage nsString integerValueSelector

-- | @- longLongValue@
longLongValue :: IsNSString nsString => nsString -> IO CLong
longLongValue nsString =
  sendMessage nsString longLongValueSelector

-- | @- boolValue@
boolValue :: IsNSString nsString => nsString -> IO Bool
boolValue nsString =
  sendMessage nsString boolValueSelector

-- | @- uppercaseString@
uppercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
uppercaseString nsString =
  sendMessage nsString uppercaseStringSelector

-- | @- lowercaseString@
lowercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
lowercaseString nsString =
  sendMessage nsString lowercaseStringSelector

-- | @- capitalizedString@
capitalizedString :: IsNSString nsString => nsString -> IO (Id NSString)
capitalizedString nsString =
  sendMessage nsString capitalizedStringSelector

-- | @- localizedUppercaseString@
localizedUppercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedUppercaseString nsString =
  sendMessage nsString localizedUppercaseStringSelector

-- | @- localizedLowercaseString@
localizedLowercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedLowercaseString nsString =
  sendMessage nsString localizedLowercaseStringSelector

-- | @- localizedCapitalizedString@
localizedCapitalizedString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedCapitalizedString nsString =
  sendMessage nsString localizedCapitalizedStringSelector

-- | @- UTF8String@
utF8String :: IsNSString nsString => nsString -> IO (Ptr CChar)
utF8String nsString =
  sendMessage nsString utF8StringSelector

-- | @- fastestEncoding@
fastestEncoding :: IsNSString nsString => nsString -> IO CULong
fastestEncoding nsString =
  sendMessage nsString fastestEncodingSelector

-- | @- smallestEncoding@
smallestEncoding :: IsNSString nsString => nsString -> IO CULong
smallestEncoding nsString =
  sendMessage nsString smallestEncodingSelector

-- | @+ availableStringEncodings@
availableStringEncodings :: IO (Const (Ptr CULong))
availableStringEncodings  =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' availableStringEncodingsSelector

-- | @+ defaultCStringEncoding@
defaultCStringEncoding :: IO CULong
defaultCStringEncoding  =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' defaultCStringEncodingSelector

-- | @- decomposedStringWithCanonicalMapping@
decomposedStringWithCanonicalMapping :: IsNSString nsString => nsString -> IO (Id NSString)
decomposedStringWithCanonicalMapping nsString =
  sendMessage nsString decomposedStringWithCanonicalMappingSelector

-- | @- precomposedStringWithCanonicalMapping@
precomposedStringWithCanonicalMapping :: IsNSString nsString => nsString -> IO (Id NSString)
precomposedStringWithCanonicalMapping nsString =
  sendMessage nsString precomposedStringWithCanonicalMappingSelector

-- | @- decomposedStringWithCompatibilityMapping@
decomposedStringWithCompatibilityMapping :: IsNSString nsString => nsString -> IO (Id NSString)
decomposedStringWithCompatibilityMapping nsString =
  sendMessage nsString decomposedStringWithCompatibilityMappingSelector

-- | @- precomposedStringWithCompatibilityMapping@
precomposedStringWithCompatibilityMapping :: IsNSString nsString => nsString -> IO (Id NSString)
precomposedStringWithCompatibilityMapping nsString =
  sendMessage nsString precomposedStringWithCompatibilityMappingSelector

-- | @- description@
description :: IsNSString nsString => nsString -> IO (Id NSString)
description nsString =
  sendMessage nsString descriptionSelector

-- | @- hash@
hash :: IsNSString nsString => nsString -> IO CULong
hash nsString =
  sendMessage nsString hashSelector


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @characterAtIndex:@
characterAtIndexSelector :: Selector '[CULong] CUShort
characterAtIndexSelector = mkSelector "characterAtIndex:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSString)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSString)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @linguisticTagsInRange:scheme:options:orthography:tokenRanges:@
linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector :: Selector '[NSRange, Id NSString, NSLinguisticTaggerOptions, Id NSOrthography, Id NSArray] (Id NSArray)
linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector = mkSelector "linguisticTagsInRange:scheme:options:orthography:tokenRanges:"

-- | @Selector@ for @enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:@
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector :: Selector '[NSRange, Id NSString, NSLinguisticTaggerOptions, Id NSOrthography, Ptr ()] ()
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector = mkSelector "enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:"

-- | @Selector@ for @stringByAddingPercentEncodingWithAllowedCharacters:@
stringByAddingPercentEncodingWithAllowedCharactersSelector :: Selector '[Id NSCharacterSet] (Id NSString)
stringByAddingPercentEncodingWithAllowedCharactersSelector = mkSelector "stringByAddingPercentEncodingWithAllowedCharacters:"

-- | @Selector@ for @stringByAddingPercentEscapesUsingEncoding:@
stringByAddingPercentEscapesUsingEncodingSelector :: Selector '[CULong] (Id NSString)
stringByAddingPercentEscapesUsingEncodingSelector = mkSelector "stringByAddingPercentEscapesUsingEncoding:"

-- | @Selector@ for @stringByReplacingPercentEscapesUsingEncoding:@
stringByReplacingPercentEscapesUsingEncodingSelector :: Selector '[CULong] (Id NSString)
stringByReplacingPercentEscapesUsingEncodingSelector = mkSelector "stringByReplacingPercentEscapesUsingEncoding:"

-- | @Selector@ for @pathWithComponents:@
pathWithComponentsSelector :: Selector '[Id NSArray] (Id NSString)
pathWithComponentsSelector = mkSelector "pathWithComponents:"

-- | @Selector@ for @stringByAppendingPathComponent:@
stringByAppendingPathComponentSelector :: Selector '[Id NSString] (Id NSString)
stringByAppendingPathComponentSelector = mkSelector "stringByAppendingPathComponent:"

-- | @Selector@ for @stringByAppendingPathExtension:@
stringByAppendingPathExtensionSelector :: Selector '[Id NSString] (Id NSString)
stringByAppendingPathExtensionSelector = mkSelector "stringByAppendingPathExtension:"

-- | @Selector@ for @stringsByAppendingPaths:@
stringsByAppendingPathsSelector :: Selector '[Id NSArray] (Id NSArray)
stringsByAppendingPathsSelector = mkSelector "stringsByAppendingPaths:"

-- | @Selector@ for @completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:@
completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector :: Selector '[Id NSString, Bool, Id NSArray, Id NSArray] CULong
completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector = mkSelector "completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:"

-- | @Selector@ for @getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLengthSelector :: Selector '[Ptr CChar, CULong] Bool
getFileSystemRepresentation_maxLengthSelector = mkSelector "getFileSystemRepresentation:maxLength:"

-- | @Selector@ for @variantFittingPresentationWidth:@
variantFittingPresentationWidthSelector :: Selector '[CLong] (Id NSString)
variantFittingPresentationWidthSelector = mkSelector "variantFittingPresentationWidth:"

-- | @Selector@ for @cString@
cStringSelector :: Selector '[] (Const (Ptr CChar))
cStringSelector = mkSelector "cString"

-- | @Selector@ for @lossyCString@
lossyCStringSelector :: Selector '[] (Const (Ptr CChar))
lossyCStringSelector = mkSelector "lossyCString"

-- | @Selector@ for @cStringLength@
cStringLengthSelector :: Selector '[] CULong
cStringLengthSelector = mkSelector "cStringLength"

-- | @Selector@ for @getCString:@
getCStringSelector :: Selector '[Ptr CChar] ()
getCStringSelector = mkSelector "getCString:"

-- | @Selector@ for @getCString:maxLength:@
getCString_maxLengthSelector :: Selector '[Ptr CChar, CULong] ()
getCString_maxLengthSelector = mkSelector "getCString:maxLength:"

-- | @Selector@ for @getCString:maxLength:range:remainingRange:@
getCString_maxLength_range_remainingRangeSelector :: Selector '[Ptr CChar, CULong, NSRange, Ptr NSRange] ()
getCString_maxLength_range_remainingRangeSelector = mkSelector "getCString:maxLength:range:remainingRange:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector '[Id NSString, Bool] Bool
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector '[Id NSURL, Bool] Bool
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector '[Id NSString] RawId
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] RawId
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @stringWithContentsOfFile:@
stringWithContentsOfFileSelector :: Selector '[Id NSString] RawId
stringWithContentsOfFileSelector = mkSelector "stringWithContentsOfFile:"

-- | @Selector@ for @stringWithContentsOfURL:@
stringWithContentsOfURLSelector :: Selector '[Id NSURL] RawId
stringWithContentsOfURLSelector = mkSelector "stringWithContentsOfURL:"

-- | @Selector@ for @initWithCStringNoCopy:length:freeWhenDone:@
initWithCStringNoCopy_length_freeWhenDoneSelector :: Selector '[Ptr CChar, CULong, Bool] RawId
initWithCStringNoCopy_length_freeWhenDoneSelector = mkSelector "initWithCStringNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithCString:length:@
initWithCString_lengthSelector :: Selector '[Const (Ptr CChar), CULong] RawId
initWithCString_lengthSelector = mkSelector "initWithCString:length:"

-- | @Selector@ for @initWithCString:@
initWithCStringSelector :: Selector '[Const (Ptr CChar)] RawId
initWithCStringSelector = mkSelector "initWithCString:"

-- | @Selector@ for @stringWithCString:length:@
stringWithCString_lengthSelector :: Selector '[Const (Ptr CChar), CULong] RawId
stringWithCString_lengthSelector = mkSelector "stringWithCString:length:"

-- | @Selector@ for @stringWithCString:@
stringWithCStringSelector :: Selector '[Const (Ptr CChar)] RawId
stringWithCStringSelector = mkSelector "stringWithCString:"

-- | @Selector@ for @getCharacters:@
getCharactersSelector :: Selector '[Ptr CUShort] ()
getCharactersSelector = mkSelector "getCharacters:"

-- | @Selector@ for @propertyList@
propertyListSelector :: Selector '[] RawId
propertyListSelector = mkSelector "propertyList"

-- | @Selector@ for @propertyListFromStringsFileFormat@
propertyListFromStringsFileFormatSelector :: Selector '[] (Id NSDictionary)
propertyListFromStringsFileFormatSelector = mkSelector "propertyListFromStringsFileFormat"

-- | @Selector@ for @stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:@
stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector :: Selector '[Id NSData, Id NSDictionary, Id NSString, Ptr Bool] CULong
stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector = mkSelector "stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:"

-- | @Selector@ for @substringFromIndex:@
substringFromIndexSelector :: Selector '[CULong] (Id NSString)
substringFromIndexSelector = mkSelector "substringFromIndex:"

-- | @Selector@ for @substringToIndex:@
substringToIndexSelector :: Selector '[CULong] (Id NSString)
substringToIndexSelector = mkSelector "substringToIndex:"

-- | @Selector@ for @substringWithRange:@
substringWithRangeSelector :: Selector '[NSRange] (Id NSString)
substringWithRangeSelector = mkSelector "substringWithRange:"

-- | @Selector@ for @getCharacters:range:@
getCharacters_rangeSelector :: Selector '[Ptr CUShort, NSRange] ()
getCharacters_rangeSelector = mkSelector "getCharacters:range:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSString] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @compare:options:@
compare_optionsSelector :: Selector '[Id NSString, NSStringCompareOptions] NSComparisonResult
compare_optionsSelector = mkSelector "compare:options:"

-- | @Selector@ for @compare:options:range:@
compare_options_rangeSelector :: Selector '[Id NSString, NSStringCompareOptions, NSRange] NSComparisonResult
compare_options_rangeSelector = mkSelector "compare:options:range:"

-- | @Selector@ for @compare:options:range:locale:@
compare_options_range_localeSelector :: Selector '[Id NSString, NSStringCompareOptions, NSRange, RawId] NSComparisonResult
compare_options_range_localeSelector = mkSelector "compare:options:range:locale:"

-- | @Selector@ for @caseInsensitiveCompare:@
caseInsensitiveCompareSelector :: Selector '[Id NSString] NSComparisonResult
caseInsensitiveCompareSelector = mkSelector "caseInsensitiveCompare:"

-- | @Selector@ for @localizedCompare:@
localizedCompareSelector :: Selector '[Id NSString] NSComparisonResult
localizedCompareSelector = mkSelector "localizedCompare:"

-- | @Selector@ for @localizedCaseInsensitiveCompare:@
localizedCaseInsensitiveCompareSelector :: Selector '[Id NSString] NSComparisonResult
localizedCaseInsensitiveCompareSelector = mkSelector "localizedCaseInsensitiveCompare:"

-- | @Selector@ for @localizedStandardCompare:@
localizedStandardCompareSelector :: Selector '[Id NSString] NSComparisonResult
localizedStandardCompareSelector = mkSelector "localizedStandardCompare:"

-- | @Selector@ for @isEqualToString:@
isEqualToStringSelector :: Selector '[Id NSString] Bool
isEqualToStringSelector = mkSelector "isEqualToString:"

-- | @Selector@ for @hasPrefix:@
hasPrefixSelector :: Selector '[Id NSString] Bool
hasPrefixSelector = mkSelector "hasPrefix:"

-- | @Selector@ for @hasSuffix:@
hasSuffixSelector :: Selector '[Id NSString] Bool
hasSuffixSelector = mkSelector "hasSuffix:"

-- | @Selector@ for @commonPrefixWithString:options:@
commonPrefixWithString_optionsSelector :: Selector '[Id NSString, NSStringCompareOptions] (Id NSString)
commonPrefixWithString_optionsSelector = mkSelector "commonPrefixWithString:options:"

-- | @Selector@ for @containsString:@
containsStringSelector :: Selector '[Id NSString] Bool
containsStringSelector = mkSelector "containsString:"

-- | @Selector@ for @localizedCaseInsensitiveContainsString:@
localizedCaseInsensitiveContainsStringSelector :: Selector '[Id NSString] Bool
localizedCaseInsensitiveContainsStringSelector = mkSelector "localizedCaseInsensitiveContainsString:"

-- | @Selector@ for @localizedStandardContainsString:@
localizedStandardContainsStringSelector :: Selector '[Id NSString] Bool
localizedStandardContainsStringSelector = mkSelector "localizedStandardContainsString:"

-- | @Selector@ for @localizedStandardRangeOfString:@
localizedStandardRangeOfStringSelector :: Selector '[Id NSString] NSRange
localizedStandardRangeOfStringSelector = mkSelector "localizedStandardRangeOfString:"

-- | @Selector@ for @rangeOfString:@
rangeOfStringSelector :: Selector '[Id NSString] NSRange
rangeOfStringSelector = mkSelector "rangeOfString:"

-- | @Selector@ for @rangeOfString:options:@
rangeOfString_optionsSelector :: Selector '[Id NSString, NSStringCompareOptions] NSRange
rangeOfString_optionsSelector = mkSelector "rangeOfString:options:"

-- | @Selector@ for @rangeOfString:options:range:@
rangeOfString_options_rangeSelector :: Selector '[Id NSString, NSStringCompareOptions, NSRange] NSRange
rangeOfString_options_rangeSelector = mkSelector "rangeOfString:options:range:"

-- | @Selector@ for @rangeOfString:options:range:locale:@
rangeOfString_options_range_localeSelector :: Selector '[Id NSString, NSStringCompareOptions, NSRange, Id NSLocale] NSRange
rangeOfString_options_range_localeSelector = mkSelector "rangeOfString:options:range:locale:"

-- | @Selector@ for @rangeOfCharacterFromSet:@
rangeOfCharacterFromSetSelector :: Selector '[Id NSCharacterSet] NSRange
rangeOfCharacterFromSetSelector = mkSelector "rangeOfCharacterFromSet:"

-- | @Selector@ for @rangeOfCharacterFromSet:options:@
rangeOfCharacterFromSet_optionsSelector :: Selector '[Id NSCharacterSet, NSStringCompareOptions] NSRange
rangeOfCharacterFromSet_optionsSelector = mkSelector "rangeOfCharacterFromSet:options:"

-- | @Selector@ for @rangeOfCharacterFromSet:options:range:@
rangeOfCharacterFromSet_options_rangeSelector :: Selector '[Id NSCharacterSet, NSStringCompareOptions, NSRange] NSRange
rangeOfCharacterFromSet_options_rangeSelector = mkSelector "rangeOfCharacterFromSet:options:range:"

-- | @Selector@ for @rangeOfComposedCharacterSequenceAtIndex:@
rangeOfComposedCharacterSequenceAtIndexSelector :: Selector '[CULong] NSRange
rangeOfComposedCharacterSequenceAtIndexSelector = mkSelector "rangeOfComposedCharacterSequenceAtIndex:"

-- | @Selector@ for @rangeOfComposedCharacterSequencesForRange:@
rangeOfComposedCharacterSequencesForRangeSelector :: Selector '[NSRange] NSRange
rangeOfComposedCharacterSequencesForRangeSelector = mkSelector "rangeOfComposedCharacterSequencesForRange:"

-- | @Selector@ for @stringByAppendingString:@
stringByAppendingStringSelector :: Selector '[Id NSString] (Id NSString)
stringByAppendingStringSelector = mkSelector "stringByAppendingString:"

-- | @Selector@ for @stringByAppendingFormat:@
stringByAppendingFormatSelector :: Selector '[Id NSString] (Id NSString)
stringByAppendingFormatSelector = mkSelector "stringByAppendingFormat:"

-- | @Selector@ for @uppercaseStringWithLocale:@
uppercaseStringWithLocaleSelector :: Selector '[Id NSLocale] (Id NSString)
uppercaseStringWithLocaleSelector = mkSelector "uppercaseStringWithLocale:"

-- | @Selector@ for @lowercaseStringWithLocale:@
lowercaseStringWithLocaleSelector :: Selector '[Id NSLocale] (Id NSString)
lowercaseStringWithLocaleSelector = mkSelector "lowercaseStringWithLocale:"

-- | @Selector@ for @capitalizedStringWithLocale:@
capitalizedStringWithLocaleSelector :: Selector '[Id NSLocale] (Id NSString)
capitalizedStringWithLocaleSelector = mkSelector "capitalizedStringWithLocale:"

-- | @Selector@ for @getLineStart:end:contentsEnd:forRange:@
getLineStart_end_contentsEnd_forRangeSelector :: Selector '[Ptr CULong, Ptr CULong, Ptr CULong, NSRange] ()
getLineStart_end_contentsEnd_forRangeSelector = mkSelector "getLineStart:end:contentsEnd:forRange:"

-- | @Selector@ for @lineRangeForRange:@
lineRangeForRangeSelector :: Selector '[NSRange] NSRange
lineRangeForRangeSelector = mkSelector "lineRangeForRange:"

-- | @Selector@ for @getParagraphStart:end:contentsEnd:forRange:@
getParagraphStart_end_contentsEnd_forRangeSelector :: Selector '[Ptr CULong, Ptr CULong, Ptr CULong, NSRange] ()
getParagraphStart_end_contentsEnd_forRangeSelector = mkSelector "getParagraphStart:end:contentsEnd:forRange:"

-- | @Selector@ for @paragraphRangeForRange:@
paragraphRangeForRangeSelector :: Selector '[NSRange] NSRange
paragraphRangeForRangeSelector = mkSelector "paragraphRangeForRange:"

-- | @Selector@ for @enumerateSubstringsInRange:options:usingBlock:@
enumerateSubstringsInRange_options_usingBlockSelector :: Selector '[NSRange, NSStringEnumerationOptions, Ptr ()] ()
enumerateSubstringsInRange_options_usingBlockSelector = mkSelector "enumerateSubstringsInRange:options:usingBlock:"

-- | @Selector@ for @enumerateLinesUsingBlock:@
enumerateLinesUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateLinesUsingBlockSelector = mkSelector "enumerateLinesUsingBlock:"

-- | @Selector@ for @dataUsingEncoding:allowLossyConversion:@
dataUsingEncoding_allowLossyConversionSelector :: Selector '[CULong, Bool] (Id NSData)
dataUsingEncoding_allowLossyConversionSelector = mkSelector "dataUsingEncoding:allowLossyConversion:"

-- | @Selector@ for @dataUsingEncoding:@
dataUsingEncodingSelector :: Selector '[CULong] (Id NSData)
dataUsingEncodingSelector = mkSelector "dataUsingEncoding:"

-- | @Selector@ for @canBeConvertedToEncoding:@
canBeConvertedToEncodingSelector :: Selector '[CULong] Bool
canBeConvertedToEncodingSelector = mkSelector "canBeConvertedToEncoding:"

-- | @Selector@ for @cStringUsingEncoding:@
cStringUsingEncodingSelector :: Selector '[CULong] (Const (Ptr CChar))
cStringUsingEncodingSelector = mkSelector "cStringUsingEncoding:"

-- | @Selector@ for @getCString:maxLength:encoding:@
getCString_maxLength_encodingSelector :: Selector '[Ptr CChar, CULong, CULong] Bool
getCString_maxLength_encodingSelector = mkSelector "getCString:maxLength:encoding:"

-- | @Selector@ for @getBytes:maxLength:usedLength:encoding:options:range:remainingRange:@
getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector :: Selector '[Ptr (), CULong, Ptr CULong, CULong, NSStringEncodingConversionOptions, NSRange, Ptr NSRange] Bool
getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector = mkSelector "getBytes:maxLength:usedLength:encoding:options:range:remainingRange:"

-- | @Selector@ for @maximumLengthOfBytesUsingEncoding:@
maximumLengthOfBytesUsingEncodingSelector :: Selector '[CULong] CULong
maximumLengthOfBytesUsingEncodingSelector = mkSelector "maximumLengthOfBytesUsingEncoding:"

-- | @Selector@ for @lengthOfBytesUsingEncoding:@
lengthOfBytesUsingEncodingSelector :: Selector '[CULong] CULong
lengthOfBytesUsingEncodingSelector = mkSelector "lengthOfBytesUsingEncoding:"

-- | @Selector@ for @localizedNameOfStringEncoding:@
localizedNameOfStringEncodingSelector :: Selector '[CULong] (Id NSString)
localizedNameOfStringEncodingSelector = mkSelector "localizedNameOfStringEncoding:"

-- | @Selector@ for @componentsSeparatedByString:@
componentsSeparatedByStringSelector :: Selector '[Id NSString] (Id NSArray)
componentsSeparatedByStringSelector = mkSelector "componentsSeparatedByString:"

-- | @Selector@ for @componentsSeparatedByCharactersInSet:@
componentsSeparatedByCharactersInSetSelector :: Selector '[Id NSCharacterSet] (Id NSArray)
componentsSeparatedByCharactersInSetSelector = mkSelector "componentsSeparatedByCharactersInSet:"

-- | @Selector@ for @stringByTrimmingCharactersInSet:@
stringByTrimmingCharactersInSetSelector :: Selector '[Id NSCharacterSet] (Id NSString)
stringByTrimmingCharactersInSetSelector = mkSelector "stringByTrimmingCharactersInSet:"

-- | @Selector@ for @stringByPaddingToLength:withString:startingAtIndex:@
stringByPaddingToLength_withString_startingAtIndexSelector :: Selector '[CULong, Id NSString, CULong] (Id NSString)
stringByPaddingToLength_withString_startingAtIndexSelector = mkSelector "stringByPaddingToLength:withString:startingAtIndex:"

-- | @Selector@ for @stringByFoldingWithOptions:locale:@
stringByFoldingWithOptions_localeSelector :: Selector '[NSStringCompareOptions, Id NSLocale] (Id NSString)
stringByFoldingWithOptions_localeSelector = mkSelector "stringByFoldingWithOptions:locale:"

-- | @Selector@ for @stringByReplacingOccurrencesOfString:withString:options:range:@
stringByReplacingOccurrencesOfString_withString_options_rangeSelector :: Selector '[Id NSString, Id NSString, NSStringCompareOptions, NSRange] (Id NSString)
stringByReplacingOccurrencesOfString_withString_options_rangeSelector = mkSelector "stringByReplacingOccurrencesOfString:withString:options:range:"

-- | @Selector@ for @stringByReplacingOccurrencesOfString:withString:@
stringByReplacingOccurrencesOfString_withStringSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
stringByReplacingOccurrencesOfString_withStringSelector = mkSelector "stringByReplacingOccurrencesOfString:withString:"

-- | @Selector@ for @stringByReplacingCharactersInRange:withString:@
stringByReplacingCharactersInRange_withStringSelector :: Selector '[NSRange, Id NSString] (Id NSString)
stringByReplacingCharactersInRange_withStringSelector = mkSelector "stringByReplacingCharactersInRange:withString:"

-- | @Selector@ for @stringByApplyingTransform:reverse:@
stringByApplyingTransform_reverseSelector :: Selector '[Id NSString, Bool] (Id NSString)
stringByApplyingTransform_reverseSelector = mkSelector "stringByApplyingTransform:reverse:"

-- | @Selector@ for @writeToURL:atomically:encoding:error:@
writeToURL_atomically_encoding_errorSelector :: Selector '[Id NSURL, Bool, CULong, Id NSError] Bool
writeToURL_atomically_encoding_errorSelector = mkSelector "writeToURL:atomically:encoding:error:"

-- | @Selector@ for @writeToFile:atomically:encoding:error:@
writeToFile_atomically_encoding_errorSelector :: Selector '[Id NSString, Bool, CULong, Id NSError] Bool
writeToFile_atomically_encoding_errorSelector = mkSelector "writeToFile:atomically:encoding:error:"

-- | @Selector@ for @initWithCharactersNoCopy:length:freeWhenDone:@
initWithCharactersNoCopy_length_freeWhenDoneSelector :: Selector '[Ptr CUShort, CULong, Bool] (Id NSString)
initWithCharactersNoCopy_length_freeWhenDoneSelector = mkSelector "initWithCharactersNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithCharactersNoCopy:length:deallocator:@
initWithCharactersNoCopy_length_deallocatorSelector :: Selector '[Ptr CUShort, CULong, Ptr ()] (Id NSString)
initWithCharactersNoCopy_length_deallocatorSelector = mkSelector "initWithCharactersNoCopy:length:deallocator:"

-- | @Selector@ for @initWithCharacters:length:@
initWithCharacters_lengthSelector :: Selector '[Const (Ptr CUShort), CULong] (Id NSString)
initWithCharacters_lengthSelector = mkSelector "initWithCharacters:length:"

-- | @Selector@ for @initWithUTF8String:@
initWithUTF8StringSelector :: Selector '[Const (Ptr CChar)] (Id NSString)
initWithUTF8StringSelector = mkSelector "initWithUTF8String:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSString)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithFormat:@
initWithFormatSelector :: Selector '[Id NSString] (Id NSString)
initWithFormatSelector = mkSelector "initWithFormat:"

-- | @Selector@ for @initWithFormat:arguments:@
initWithFormat_argumentsSelector :: Selector '[Id NSString, RawId] (Id NSString)
initWithFormat_argumentsSelector = mkSelector "initWithFormat:arguments:"

-- | @Selector@ for @initWithFormat:locale:@
initWithFormat_localeSelector :: Selector '[Id NSString, RawId] (Id NSString)
initWithFormat_localeSelector = mkSelector "initWithFormat:locale:"

-- | @Selector@ for @initWithFormat:locale:arguments:@
initWithFormat_locale_argumentsSelector :: Selector '[Id NSString, RawId, RawId] (Id NSString)
initWithFormat_locale_argumentsSelector = mkSelector "initWithFormat:locale:arguments:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:error:@
initWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:locale:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector :: Selector '[Id NSString, Id NSString, RawId, Id NSError] (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector :: Selector '[Id NSString, Id NSString, RawId, Id NSError] (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:arguments:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector :: Selector '[Id NSString, Id NSString, RawId, RawId, Id NSError] (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:"

-- | @Selector@ for @initWithData:encoding:@
initWithData_encodingSelector :: Selector '[Id NSData, CULong] (Id NSString)
initWithData_encodingSelector = mkSelector "initWithData:encoding:"

-- | @Selector@ for @initWithBytes:length:encoding:@
initWithBytes_length_encodingSelector :: Selector '[Const (Ptr ()), CULong, CULong] (Id NSString)
initWithBytes_length_encodingSelector = mkSelector "initWithBytes:length:encoding:"

-- | @Selector@ for @initWithBytesNoCopy:length:encoding:freeWhenDone:@
initWithBytesNoCopy_length_encoding_freeWhenDoneSelector :: Selector '[Ptr (), CULong, CULong, Bool] (Id NSString)
initWithBytesNoCopy_length_encoding_freeWhenDoneSelector = mkSelector "initWithBytesNoCopy:length:encoding:freeWhenDone:"

-- | @Selector@ for @initWithBytesNoCopy:length:encoding:deallocator:@
initWithBytesNoCopy_length_encoding_deallocatorSelector :: Selector '[Ptr (), CULong, CULong, Ptr ()] (Id NSString)
initWithBytesNoCopy_length_encoding_deallocatorSelector = mkSelector "initWithBytesNoCopy:length:encoding:deallocator:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @stringWithString:@
stringWithStringSelector :: Selector '[Id NSString] (Id NSString)
stringWithStringSelector = mkSelector "stringWithString:"

-- | @Selector@ for @stringWithCharacters:length:@
stringWithCharacters_lengthSelector :: Selector '[Const (Ptr CUShort), CULong] (Id NSString)
stringWithCharacters_lengthSelector = mkSelector "stringWithCharacters:length:"

-- | @Selector@ for @stringWithUTF8String:@
stringWithUTF8StringSelector :: Selector '[Const (Ptr CChar)] (Id NSString)
stringWithUTF8StringSelector = mkSelector "stringWithUTF8String:"

-- | @Selector@ for @stringWithFormat:@
stringWithFormatSelector :: Selector '[Id NSString] (Id NSString)
stringWithFormatSelector = mkSelector "stringWithFormat:"

-- | @Selector@ for @localizedStringWithFormat:@
localizedStringWithFormatSelector :: Selector '[Id NSString] (Id NSString)
localizedStringWithFormatSelector = mkSelector "localizedStringWithFormat:"

-- | @Selector@ for @stringWithValidatedFormat:validFormatSpecifiers:error:@
stringWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] (Id NSString)
stringWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "stringWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @localizedStringWithValidatedFormat:validFormatSpecifiers:error:@
localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector '[Id NSString, Id NSString, Id NSError] (Id NSString)
localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "localizedStringWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @initWithCString:encoding:@
initWithCString_encodingSelector :: Selector '[Const (Ptr CChar), CULong] (Id NSString)
initWithCString_encodingSelector = mkSelector "initWithCString:encoding:"

-- | @Selector@ for @stringWithCString:encoding:@
stringWithCString_encodingSelector :: Selector '[Const (Ptr CChar), CULong] (Id NSString)
stringWithCString_encodingSelector = mkSelector "stringWithCString:encoding:"

-- | @Selector@ for @initWithContentsOfURL:encoding:error:@
initWithContentsOfURL_encoding_errorSelector :: Selector '[Id NSURL, CULong, Id NSError] (Id NSString)
initWithContentsOfURL_encoding_errorSelector = mkSelector "initWithContentsOfURL:encoding:error:"

-- | @Selector@ for @initWithContentsOfFile:encoding:error:@
initWithContentsOfFile_encoding_errorSelector :: Selector '[Id NSString, CULong, Id NSError] (Id NSString)
initWithContentsOfFile_encoding_errorSelector = mkSelector "initWithContentsOfFile:encoding:error:"

-- | @Selector@ for @stringWithContentsOfURL:encoding:error:@
stringWithContentsOfURL_encoding_errorSelector :: Selector '[Id NSURL, CULong, Id NSError] (Id NSString)
stringWithContentsOfURL_encoding_errorSelector = mkSelector "stringWithContentsOfURL:encoding:error:"

-- | @Selector@ for @stringWithContentsOfFile:encoding:error:@
stringWithContentsOfFile_encoding_errorSelector :: Selector '[Id NSString, CULong, Id NSError] (Id NSString)
stringWithContentsOfFile_encoding_errorSelector = mkSelector "stringWithContentsOfFile:encoding:error:"

-- | @Selector@ for @initWithContentsOfURL:usedEncoding:error:@
initWithContentsOfURL_usedEncoding_errorSelector :: Selector '[Id NSURL, Ptr CULong, Id NSError] (Id NSString)
initWithContentsOfURL_usedEncoding_errorSelector = mkSelector "initWithContentsOfURL:usedEncoding:error:"

-- | @Selector@ for @initWithContentsOfFile:usedEncoding:error:@
initWithContentsOfFile_usedEncoding_errorSelector :: Selector '[Id NSString, Ptr CULong, Id NSError] (Id NSString)
initWithContentsOfFile_usedEncoding_errorSelector = mkSelector "initWithContentsOfFile:usedEncoding:error:"

-- | @Selector@ for @stringWithContentsOfURL:usedEncoding:error:@
stringWithContentsOfURL_usedEncoding_errorSelector :: Selector '[Id NSURL, Ptr CULong, Id NSError] (Id NSString)
stringWithContentsOfURL_usedEncoding_errorSelector = mkSelector "stringWithContentsOfURL:usedEncoding:error:"

-- | @Selector@ for @stringWithContentsOfFile:usedEncoding:error:@
stringWithContentsOfFile_usedEncoding_errorSelector :: Selector '[Id NSString, Ptr CULong, Id NSError] (Id NSString)
stringWithContentsOfFile_usedEncoding_errorSelector = mkSelector "stringWithContentsOfFile:usedEncoding:error:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

-- | @Selector@ for @stringByRemovingPercentEncoding@
stringByRemovingPercentEncodingSelector :: Selector '[] (Id NSString)
stringByRemovingPercentEncodingSelector = mkSelector "stringByRemovingPercentEncoding"

-- | @Selector@ for @pathComponents@
pathComponentsSelector :: Selector '[] (Id NSArray)
pathComponentsSelector = mkSelector "pathComponents"

-- | @Selector@ for @absolutePath@
absolutePathSelector :: Selector '[] Bool
absolutePathSelector = mkSelector "absolutePath"

-- | @Selector@ for @lastPathComponent@
lastPathComponentSelector :: Selector '[] (Id NSString)
lastPathComponentSelector = mkSelector "lastPathComponent"

-- | @Selector@ for @stringByDeletingLastPathComponent@
stringByDeletingLastPathComponentSelector :: Selector '[] (Id NSString)
stringByDeletingLastPathComponentSelector = mkSelector "stringByDeletingLastPathComponent"

-- | @Selector@ for @pathExtension@
pathExtensionSelector :: Selector '[] (Id NSString)
pathExtensionSelector = mkSelector "pathExtension"

-- | @Selector@ for @stringByDeletingPathExtension@
stringByDeletingPathExtensionSelector :: Selector '[] (Id NSString)
stringByDeletingPathExtensionSelector = mkSelector "stringByDeletingPathExtension"

-- | @Selector@ for @stringByAbbreviatingWithTildeInPath@
stringByAbbreviatingWithTildeInPathSelector :: Selector '[] (Id NSString)
stringByAbbreviatingWithTildeInPathSelector = mkSelector "stringByAbbreviatingWithTildeInPath"

-- | @Selector@ for @stringByExpandingTildeInPath@
stringByExpandingTildeInPathSelector :: Selector '[] (Id NSString)
stringByExpandingTildeInPathSelector = mkSelector "stringByExpandingTildeInPath"

-- | @Selector@ for @stringByStandardizingPath@
stringByStandardizingPathSelector :: Selector '[] (Id NSString)
stringByStandardizingPathSelector = mkSelector "stringByStandardizingPath"

-- | @Selector@ for @stringByResolvingSymlinksInPath@
stringByResolvingSymlinksInPathSelector :: Selector '[] (Id NSString)
stringByResolvingSymlinksInPathSelector = mkSelector "stringByResolvingSymlinksInPath"

-- | @Selector@ for @fileSystemRepresentation@
fileSystemRepresentationSelector :: Selector '[] (Ptr CChar)
fileSystemRepresentationSelector = mkSelector "fileSystemRepresentation"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @intValue@
intValueSelector :: Selector '[] CInt
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector '[] CLong
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @longLongValue@
longLongValueSelector :: Selector '[] CLong
longLongValueSelector = mkSelector "longLongValue"

-- | @Selector@ for @boolValue@
boolValueSelector :: Selector '[] Bool
boolValueSelector = mkSelector "boolValue"

-- | @Selector@ for @uppercaseString@
uppercaseStringSelector :: Selector '[] (Id NSString)
uppercaseStringSelector = mkSelector "uppercaseString"

-- | @Selector@ for @lowercaseString@
lowercaseStringSelector :: Selector '[] (Id NSString)
lowercaseStringSelector = mkSelector "lowercaseString"

-- | @Selector@ for @capitalizedString@
capitalizedStringSelector :: Selector '[] (Id NSString)
capitalizedStringSelector = mkSelector "capitalizedString"

-- | @Selector@ for @localizedUppercaseString@
localizedUppercaseStringSelector :: Selector '[] (Id NSString)
localizedUppercaseStringSelector = mkSelector "localizedUppercaseString"

-- | @Selector@ for @localizedLowercaseString@
localizedLowercaseStringSelector :: Selector '[] (Id NSString)
localizedLowercaseStringSelector = mkSelector "localizedLowercaseString"

-- | @Selector@ for @localizedCapitalizedString@
localizedCapitalizedStringSelector :: Selector '[] (Id NSString)
localizedCapitalizedStringSelector = mkSelector "localizedCapitalizedString"

-- | @Selector@ for @UTF8String@
utF8StringSelector :: Selector '[] (Ptr CChar)
utF8StringSelector = mkSelector "UTF8String"

-- | @Selector@ for @fastestEncoding@
fastestEncodingSelector :: Selector '[] CULong
fastestEncodingSelector = mkSelector "fastestEncoding"

-- | @Selector@ for @smallestEncoding@
smallestEncodingSelector :: Selector '[] CULong
smallestEncodingSelector = mkSelector "smallestEncoding"

-- | @Selector@ for @availableStringEncodings@
availableStringEncodingsSelector :: Selector '[] (Const (Ptr CULong))
availableStringEncodingsSelector = mkSelector "availableStringEncodings"

-- | @Selector@ for @defaultCStringEncoding@
defaultCStringEncodingSelector :: Selector '[] CULong
defaultCStringEncodingSelector = mkSelector "defaultCStringEncoding"

-- | @Selector@ for @decomposedStringWithCanonicalMapping@
decomposedStringWithCanonicalMappingSelector :: Selector '[] (Id NSString)
decomposedStringWithCanonicalMappingSelector = mkSelector "decomposedStringWithCanonicalMapping"

-- | @Selector@ for @precomposedStringWithCanonicalMapping@
precomposedStringWithCanonicalMappingSelector :: Selector '[] (Id NSString)
precomposedStringWithCanonicalMappingSelector = mkSelector "precomposedStringWithCanonicalMapping"

-- | @Selector@ for @decomposedStringWithCompatibilityMapping@
decomposedStringWithCompatibilityMappingSelector :: Selector '[] (Id NSString)
decomposedStringWithCompatibilityMappingSelector = mkSelector "decomposedStringWithCompatibilityMapping"

-- | @Selector@ for @precomposedStringWithCompatibilityMapping@
precomposedStringWithCompatibilityMappingSelector :: Selector '[] (Id NSString)
precomposedStringWithCompatibilityMappingSelector = mkSelector "precomposedStringWithCompatibilityMapping"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @hash@
hashSelector :: Selector '[] CULong
hashSelector = mkSelector "hash"

