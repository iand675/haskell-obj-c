{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , fastestEncoding
  , smallestEncoding
  , defaultCStringEncoding
  , decomposedStringWithCanonicalMapping
  , precomposedStringWithCanonicalMapping
  , decomposedStringWithCompatibilityMapping
  , precomposedStringWithCompatibilityMapping
  , description
  , hash
  , characterAtIndexSelector
  , initSelector
  , initWithCoderSelector
  , linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector
  , enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector
  , stringByAddingPercentEncodingWithAllowedCharactersSelector
  , stringByAddingPercentEscapesUsingEncodingSelector
  , stringByReplacingPercentEscapesUsingEncodingSelector
  , pathWithComponentsSelector
  , stringByAppendingPathComponentSelector
  , stringByAppendingPathExtensionSelector
  , stringsByAppendingPathsSelector
  , completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector
  , getFileSystemRepresentation_maxLengthSelector
  , variantFittingPresentationWidthSelector
  , cStringSelector
  , lossyCStringSelector
  , cStringLengthSelector
  , getCStringSelector
  , getCString_maxLengthSelector
  , getCString_maxLength_range_remainingRangeSelector
  , writeToFile_atomicallySelector
  , writeToURL_atomicallySelector
  , initWithContentsOfFileSelector
  , initWithContentsOfURLSelector
  , stringWithContentsOfFileSelector
  , stringWithContentsOfURLSelector
  , initWithCStringNoCopy_length_freeWhenDoneSelector
  , initWithCString_lengthSelector
  , initWithCStringSelector
  , stringWithCString_lengthSelector
  , stringWithCStringSelector
  , getCharactersSelector
  , propertyListSelector
  , propertyListFromStringsFileFormatSelector
  , stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector
  , substringFromIndexSelector
  , substringToIndexSelector
  , substringWithRangeSelector
  , getCharacters_rangeSelector
  , compareSelector
  , compare_optionsSelector
  , compare_options_rangeSelector
  , compare_options_range_localeSelector
  , caseInsensitiveCompareSelector
  , localizedCompareSelector
  , localizedCaseInsensitiveCompareSelector
  , localizedStandardCompareSelector
  , isEqualToStringSelector
  , hasPrefixSelector
  , hasSuffixSelector
  , commonPrefixWithString_optionsSelector
  , containsStringSelector
  , localizedCaseInsensitiveContainsStringSelector
  , localizedStandardContainsStringSelector
  , localizedStandardRangeOfStringSelector
  , rangeOfStringSelector
  , rangeOfString_optionsSelector
  , rangeOfString_options_rangeSelector
  , rangeOfString_options_range_localeSelector
  , rangeOfCharacterFromSetSelector
  , rangeOfCharacterFromSet_optionsSelector
  , rangeOfCharacterFromSet_options_rangeSelector
  , rangeOfComposedCharacterSequenceAtIndexSelector
  , rangeOfComposedCharacterSequencesForRangeSelector
  , stringByAppendingStringSelector
  , stringByAppendingFormatSelector
  , uppercaseStringWithLocaleSelector
  , lowercaseStringWithLocaleSelector
  , capitalizedStringWithLocaleSelector
  , getLineStart_end_contentsEnd_forRangeSelector
  , lineRangeForRangeSelector
  , getParagraphStart_end_contentsEnd_forRangeSelector
  , paragraphRangeForRangeSelector
  , enumerateSubstringsInRange_options_usingBlockSelector
  , enumerateLinesUsingBlockSelector
  , dataUsingEncoding_allowLossyConversionSelector
  , dataUsingEncodingSelector
  , canBeConvertedToEncodingSelector
  , cStringUsingEncodingSelector
  , getCString_maxLength_encodingSelector
  , getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector
  , maximumLengthOfBytesUsingEncodingSelector
  , lengthOfBytesUsingEncodingSelector
  , localizedNameOfStringEncodingSelector
  , componentsSeparatedByStringSelector
  , componentsSeparatedByCharactersInSetSelector
  , stringByTrimmingCharactersInSetSelector
  , stringByPaddingToLength_withString_startingAtIndexSelector
  , stringByFoldingWithOptions_localeSelector
  , stringByReplacingOccurrencesOfString_withString_options_rangeSelector
  , stringByReplacingOccurrencesOfString_withStringSelector
  , stringByReplacingCharactersInRange_withStringSelector
  , stringByApplyingTransform_reverseSelector
  , writeToURL_atomically_encoding_errorSelector
  , writeToFile_atomically_encoding_errorSelector
  , initWithCharactersNoCopy_length_freeWhenDoneSelector
  , initWithCharactersNoCopy_length_deallocatorSelector
  , initWithCharacters_lengthSelector
  , initWithUTF8StringSelector
  , initWithStringSelector
  , initWithFormatSelector
  , initWithFormat_argumentsSelector
  , initWithFormat_localeSelector
  , initWithFormat_locale_argumentsSelector
  , initWithValidatedFormat_validFormatSpecifiers_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector
  , initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector
  , initWithData_encodingSelector
  , initWithBytes_length_encodingSelector
  , initWithBytesNoCopy_length_encoding_freeWhenDoneSelector
  , initWithBytesNoCopy_length_encoding_deallocatorSelector
  , stringSelector
  , stringWithStringSelector
  , stringWithCharacters_lengthSelector
  , stringWithUTF8StringSelector
  , stringWithFormatSelector
  , localizedStringWithFormatSelector
  , stringWithValidatedFormat_validFormatSpecifiers_errorSelector
  , localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector
  , initWithCString_encodingSelector
  , stringWithCString_encodingSelector
  , initWithContentsOfURL_encoding_errorSelector
  , initWithContentsOfFile_encoding_errorSelector
  , stringWithContentsOfURL_encoding_errorSelector
  , stringWithContentsOfFile_encoding_errorSelector
  , initWithContentsOfURL_usedEncoding_errorSelector
  , initWithContentsOfFile_usedEncoding_errorSelector
  , stringWithContentsOfURL_usedEncoding_errorSelector
  , stringWithContentsOfFile_usedEncoding_errorSelector
  , lengthSelector
  , stringByRemovingPercentEncodingSelector
  , pathComponentsSelector
  , absolutePathSelector
  , lastPathComponentSelector
  , stringByDeletingLastPathComponentSelector
  , pathExtensionSelector
  , stringByDeletingPathExtensionSelector
  , stringByAbbreviatingWithTildeInPathSelector
  , stringByExpandingTildeInPathSelector
  , stringByStandardizingPathSelector
  , stringByResolvingSymlinksInPathSelector
  , doubleValueSelector
  , floatValueSelector
  , intValueSelector
  , integerValueSelector
  , longLongValueSelector
  , boolValueSelector
  , uppercaseStringSelector
  , lowercaseStringSelector
  , capitalizedStringSelector
  , localizedUppercaseStringSelector
  , localizedLowercaseStringSelector
  , localizedCapitalizedStringSelector
  , fastestEncodingSelector
  , smallestEncodingSelector
  , defaultCStringEncodingSelector
  , decomposedStringWithCanonicalMappingSelector
  , precomposedStringWithCanonicalMappingSelector
  , decomposedStringWithCompatibilityMappingSelector
  , precomposedStringWithCompatibilityMappingSelector
  , descriptionSelector
  , hashSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.CoreFoundation.Internal.Enums
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @- characterAtIndex:@
characterAtIndex :: IsNSString nsString => nsString -> CULong -> IO CUShort
characterAtIndex nsString  index =
  fmap fromIntegral $ sendMsg nsString (mkSelector "characterAtIndex:") retCUInt [argCULong (fromIntegral index)]

-- | @- init@
init_ :: IsNSString nsString => nsString -> IO (Id NSString)
init_ nsString  =
  sendMsg nsString (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSString nsString, IsNSCoder coder) => nsString -> coder -> IO (Id NSString)
initWithCoder nsString  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsString (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- linguisticTagsInRange:scheme:options:orthography:tokenRanges:@
linguisticTagsInRange_scheme_options_orthography_tokenRanges :: (IsNSString nsString, IsNSString scheme, IsNSOrthography orthography, IsNSArray tokenRanges) => nsString -> NSRange -> scheme -> NSLinguisticTaggerOptions -> orthography -> tokenRanges -> IO (Id NSArray)
linguisticTagsInRange_scheme_options_orthography_tokenRanges nsString  range scheme options orthography tokenRanges =
withObjCPtr scheme $ \raw_scheme ->
  withObjCPtr orthography $ \raw_orthography ->
    withObjCPtr tokenRanges $ \raw_tokenRanges ->
        sendMsg nsString (mkSelector "linguisticTagsInRange:scheme:options:orthography:tokenRanges:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_orthography :: Ptr ()), argPtr (castPtr raw_tokenRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:@
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlock :: (IsNSString nsString, IsNSString scheme, IsNSOrthography orthography) => nsString -> NSRange -> scheme -> NSLinguisticTaggerOptions -> orthography -> Ptr () -> IO ()
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlock nsString  range scheme options orthography block =
withObjCPtr scheme $ \raw_scheme ->
  withObjCPtr orthography $ \raw_orthography ->
      sendMsg nsString (mkSelector "enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:") retVoid [argNSRange range, argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_orthography :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- stringByAddingPercentEncodingWithAllowedCharacters:@
stringByAddingPercentEncodingWithAllowedCharacters :: (IsNSString nsString, IsNSCharacterSet allowedCharacters) => nsString -> allowedCharacters -> IO (Id NSString)
stringByAddingPercentEncodingWithAllowedCharacters nsString  allowedCharacters =
withObjCPtr allowedCharacters $ \raw_allowedCharacters ->
    sendMsg nsString (mkSelector "stringByAddingPercentEncodingWithAllowedCharacters:") (retPtr retVoid) [argPtr (castPtr raw_allowedCharacters :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByAddingPercentEscapesUsingEncoding:@
stringByAddingPercentEscapesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
stringByAddingPercentEscapesUsingEncoding nsString  enc =
  sendMsg nsString (mkSelector "stringByAddingPercentEscapesUsingEncoding:") (retPtr retVoid) [argCULong (fromIntegral enc)] >>= retainedObject . castPtr

-- | @- stringByReplacingPercentEscapesUsingEncoding:@
stringByReplacingPercentEscapesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
stringByReplacingPercentEscapesUsingEncoding nsString  enc =
  sendMsg nsString (mkSelector "stringByReplacingPercentEscapesUsingEncoding:") (retPtr retVoid) [argCULong (fromIntegral enc)] >>= retainedObject . castPtr

-- | @+ pathWithComponents:@
pathWithComponents :: IsNSArray components => components -> IO (Id NSString)
pathWithComponents components =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "pathWithComponents:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByAppendingPathComponent:@
stringByAppendingPathComponent :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO (Id NSString)
stringByAppendingPathComponent nsString  str =
withObjCPtr str $ \raw_str ->
    sendMsg nsString (mkSelector "stringByAppendingPathComponent:") (retPtr retVoid) [argPtr (castPtr raw_str :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByAppendingPathExtension:@
stringByAppendingPathExtension :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO (Id NSString)
stringByAppendingPathExtension nsString  str =
withObjCPtr str $ \raw_str ->
    sendMsg nsString (mkSelector "stringByAppendingPathExtension:") (retPtr retVoid) [argPtr (castPtr raw_str :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringsByAppendingPaths:@
stringsByAppendingPaths :: (IsNSString nsString, IsNSArray paths) => nsString -> paths -> IO (Id NSArray)
stringsByAppendingPaths nsString  paths =
withObjCPtr paths $ \raw_paths ->
    sendMsg nsString (mkSelector "stringsByAppendingPaths:") (retPtr retVoid) [argPtr (castPtr raw_paths :: Ptr ())] >>= retainedObject . castPtr

-- | @- completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:@
completePathIntoString_caseSensitive_matchesIntoArray_filterTypes :: (IsNSString nsString, IsNSString outputName, IsNSArray outputArray, IsNSArray filterTypes) => nsString -> outputName -> Bool -> outputArray -> filterTypes -> IO CULong
completePathIntoString_caseSensitive_matchesIntoArray_filterTypes nsString  outputName flag outputArray filterTypes =
withObjCPtr outputName $ \raw_outputName ->
  withObjCPtr outputArray $ \raw_outputArray ->
    withObjCPtr filterTypes $ \raw_filterTypes ->
        sendMsg nsString (mkSelector "completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:") retCULong [argPtr (castPtr raw_outputName :: Ptr ()), argCULong (if flag then 1 else 0), argPtr (castPtr raw_outputArray :: Ptr ()), argPtr (castPtr raw_filterTypes :: Ptr ())]

-- | @- getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLength :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> IO Bool
getFileSystemRepresentation_maxLength nsString  cname max_ =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "getFileSystemRepresentation:maxLength:") retCULong [argPtr cname, argCULong (fromIntegral max_)]

-- | @- variantFittingPresentationWidth:@
variantFittingPresentationWidth :: IsNSString nsString => nsString -> CLong -> IO (Id NSString)
variantFittingPresentationWidth nsString  width =
  sendMsg nsString (mkSelector "variantFittingPresentationWidth:") (retPtr retVoid) [argCLong (fromIntegral width)] >>= retainedObject . castPtr

-- | @- cString@
cString :: IsNSString nsString => nsString -> IO (Const (Ptr CChar))
cString nsString  =
  fmap Const $ fmap castPtr $ sendMsg nsString (mkSelector "cString") (retPtr retVoid) []

-- | @- lossyCString@
lossyCString :: IsNSString nsString => nsString -> IO (Const (Ptr CChar))
lossyCString nsString  =
  fmap Const $ fmap castPtr $ sendMsg nsString (mkSelector "lossyCString") (retPtr retVoid) []

-- | @- cStringLength@
cStringLength :: IsNSString nsString => nsString -> IO CULong
cStringLength nsString  =
  sendMsg nsString (mkSelector "cStringLength") retCULong []

-- | @- getCString:@
getCString :: IsNSString nsString => nsString -> Ptr CChar -> IO ()
getCString nsString  bytes =
  sendMsg nsString (mkSelector "getCString:") retVoid [argPtr bytes]

-- | @- getCString:maxLength:@
getCString_maxLength :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> IO ()
getCString_maxLength nsString  bytes maxLength =
  sendMsg nsString (mkSelector "getCString:maxLength:") retVoid [argPtr bytes, argCULong (fromIntegral maxLength)]

-- | @- getCString:maxLength:range:remainingRange:@
getCString_maxLength_range_remainingRange :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> NSRange -> Ptr NSRange -> IO ()
getCString_maxLength_range_remainingRange nsString  bytes maxLength aRange leftoverRange =
  sendMsg nsString (mkSelector "getCString:maxLength:range:remainingRange:") retVoid [argPtr bytes, argCULong (fromIntegral maxLength), argNSRange aRange, argPtr leftoverRange]

-- | @- writeToFile:atomically:@
writeToFile_atomically :: (IsNSString nsString, IsNSString path) => nsString -> path -> Bool -> IO Bool
writeToFile_atomically nsString  path useAuxiliaryFile =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "writeToFile:atomically:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0)]

-- | @- writeToURL:atomically:@
writeToURL_atomically :: (IsNSString nsString, IsNSURL url) => nsString -> url -> Bool -> IO Bool
writeToURL_atomically nsString  url atomically =
withObjCPtr url $ \raw_url ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "writeToURL:atomically:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if atomically then 1 else 0)]

-- | @- initWithContentsOfFile:@
initWithContentsOfFile :: (IsNSString nsString, IsNSString path) => nsString -> path -> IO RawId
initWithContentsOfFile nsString  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "initWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSString nsString, IsNSURL url) => nsString -> url -> IO RawId
initWithContentsOfURL nsString  url =
withObjCPtr url $ \raw_url ->
    fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())]

-- | @+ stringWithContentsOfFile:@
stringWithContentsOfFile :: IsNSString path => path -> IO RawId
stringWithContentsOfFile path =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "stringWithContentsOfFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @+ stringWithContentsOfURL:@
stringWithContentsOfURL :: IsNSURL url => url -> IO RawId
stringWithContentsOfURL url =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr url $ \raw_url ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "stringWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())]

-- | @- initWithCStringNoCopy:length:freeWhenDone:@
initWithCStringNoCopy_length_freeWhenDone :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> Bool -> IO RawId
initWithCStringNoCopy_length_freeWhenDone nsString  bytes length_ freeBuffer =
  fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "initWithCStringNoCopy:length:freeWhenDone:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral length_), argCULong (if freeBuffer then 1 else 0)]

-- | @- initWithCString:length:@
initWithCString_length :: IsNSString nsString => nsString -> Const (Ptr CChar) -> CULong -> IO RawId
initWithCString_length nsString  bytes length_ =
  fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "initWithCString:length:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral length_)]

-- | @- initWithCString:@
initWithCString :: IsNSString nsString => nsString -> Const (Ptr CChar) -> IO RawId
initWithCString nsString  bytes =
  fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "initWithCString:") (retPtr retVoid) [argPtr (unConst bytes)]

-- | @+ stringWithCString:length:@
stringWithCString_length :: Const (Ptr CChar) -> CULong -> IO RawId
stringWithCString_length bytes length_ =
  do
    cls' <- getRequiredClass "NSString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "stringWithCString:length:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral length_)]

-- | @+ stringWithCString:@
stringWithCString :: Const (Ptr CChar) -> IO RawId
stringWithCString bytes =
  do
    cls' <- getRequiredClass "NSString"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "stringWithCString:") (retPtr retVoid) [argPtr (unConst bytes)]

-- | @- getCharacters:@
getCharacters :: IsNSString nsString => nsString -> Ptr CUShort -> IO ()
getCharacters nsString  buffer =
  sendMsg nsString (mkSelector "getCharacters:") retVoid [argPtr buffer]

-- | @- propertyList@
propertyList :: IsNSString nsString => nsString -> IO RawId
propertyList nsString  =
  fmap (RawId . castPtr) $ sendMsg nsString (mkSelector "propertyList") (retPtr retVoid) []

-- | @- propertyListFromStringsFileFormat@
propertyListFromStringsFileFormat :: IsNSString nsString => nsString -> IO (Id NSDictionary)
propertyListFromStringsFileFormat nsString  =
  sendMsg nsString (mkSelector "propertyListFromStringsFileFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:@
stringEncodingForData_encodingOptions_convertedString_usedLossyConversion :: (IsNSData data_, IsNSDictionary opts, IsNSString string) => data_ -> opts -> string -> Ptr Bool -> IO CULong
stringEncodingForData_encodingOptions_convertedString_usedLossyConversion data_ opts string usedLossyConversion =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr opts $ \raw_opts ->
        withObjCPtr string $ \raw_string ->
          sendClassMsg cls' (mkSelector "stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_opts :: Ptr ()), argPtr (castPtr raw_string :: Ptr ()), argPtr usedLossyConversion]

-- | @- substringFromIndex:@
substringFromIndex :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
substringFromIndex nsString  from =
  sendMsg nsString (mkSelector "substringFromIndex:") (retPtr retVoid) [argCULong (fromIntegral from)] >>= retainedObject . castPtr

-- | @- substringToIndex:@
substringToIndex :: IsNSString nsString => nsString -> CULong -> IO (Id NSString)
substringToIndex nsString  to =
  sendMsg nsString (mkSelector "substringToIndex:") (retPtr retVoid) [argCULong (fromIntegral to)] >>= retainedObject . castPtr

-- | @- substringWithRange:@
substringWithRange :: IsNSString nsString => nsString -> NSRange -> IO (Id NSString)
substringWithRange nsString  range =
  sendMsg nsString (mkSelector "substringWithRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- getCharacters:range:@
getCharacters_range :: IsNSString nsString => nsString -> Ptr CUShort -> NSRange -> IO ()
getCharacters_range nsString  buffer range =
  sendMsg nsString (mkSelector "getCharacters:range:") retVoid [argPtr buffer, argNSRange range]

-- | @- compare:@
compare_ :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
compare_ nsString  string =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "compare:") retCLong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- compare:options:@
compare_options :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> IO NSComparisonResult
compare_options nsString  string mask =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "compare:options:") retCLong [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce mask)]

-- | @- compare:options:range:@
compare_options_range :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> NSRange -> IO NSComparisonResult
compare_options_range nsString  string mask rangeOfReceiverToCompare =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "compare:options:range:") retCLong [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce mask), argNSRange rangeOfReceiverToCompare]

-- | @- compare:options:range:locale:@
compare_options_range_locale :: (IsNSString nsString, IsNSString string) => nsString -> string -> NSStringCompareOptions -> NSRange -> RawId -> IO NSComparisonResult
compare_options_range_locale nsString  string mask rangeOfReceiverToCompare locale =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "compare:options:range:locale:") retCLong [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce mask), argNSRange rangeOfReceiverToCompare, argPtr (castPtr (unRawId locale) :: Ptr ())]

-- | @- caseInsensitiveCompare:@
caseInsensitiveCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
caseInsensitiveCompare nsString  string =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "caseInsensitiveCompare:") retCLong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- localizedCompare:@
localizedCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedCompare nsString  string =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "localizedCompare:") retCLong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- localizedCaseInsensitiveCompare:@
localizedCaseInsensitiveCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedCaseInsensitiveCompare nsString  string =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "localizedCaseInsensitiveCompare:") retCLong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- localizedStandardCompare:@
localizedStandardCompare :: (IsNSString nsString, IsNSString string) => nsString -> string -> IO NSComparisonResult
localizedStandardCompare nsString  string =
withObjCPtr string $ \raw_string ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsString (mkSelector "localizedStandardCompare:") retCLong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- isEqualToString:@
isEqualToString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO Bool
isEqualToString nsString  aString =
withObjCPtr aString $ \raw_aString ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "isEqualToString:") retCULong [argPtr (castPtr raw_aString :: Ptr ())]

-- | @- hasPrefix:@
hasPrefix :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
hasPrefix nsString  str =
withObjCPtr str $ \raw_str ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "hasPrefix:") retCULong [argPtr (castPtr raw_str :: Ptr ())]

-- | @- hasSuffix:@
hasSuffix :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
hasSuffix nsString  str =
withObjCPtr str $ \raw_str ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "hasSuffix:") retCULong [argPtr (castPtr raw_str :: Ptr ())]

-- | @- commonPrefixWithString:options:@
commonPrefixWithString_options :: (IsNSString nsString, IsNSString str) => nsString -> str -> NSStringCompareOptions -> IO (Id NSString)
commonPrefixWithString_options nsString  str mask =
withObjCPtr str $ \raw_str ->
    sendMsg nsString (mkSelector "commonPrefixWithString:options:") (retPtr retVoid) [argPtr (castPtr raw_str :: Ptr ()), argCULong (coerce mask)] >>= retainedObject . castPtr

-- | @- containsString:@
containsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
containsString nsString  str =
withObjCPtr str $ \raw_str ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "containsString:") retCULong [argPtr (castPtr raw_str :: Ptr ())]

-- | @- localizedCaseInsensitiveContainsString:@
localizedCaseInsensitiveContainsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
localizedCaseInsensitiveContainsString nsString  str =
withObjCPtr str $ \raw_str ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "localizedCaseInsensitiveContainsString:") retCULong [argPtr (castPtr raw_str :: Ptr ())]

-- | @- localizedStandardContainsString:@
localizedStandardContainsString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO Bool
localizedStandardContainsString nsString  str =
withObjCPtr str $ \raw_str ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "localizedStandardContainsString:") retCULong [argPtr (castPtr raw_str :: Ptr ())]

-- | @- localizedStandardRangeOfString:@
localizedStandardRangeOfString :: (IsNSString nsString, IsNSString str) => nsString -> str -> IO NSRange
localizedStandardRangeOfString nsString  str =
withObjCPtr str $ \raw_str ->
    sendMsgStret nsString (mkSelector "localizedStandardRangeOfString:") retNSRange [argPtr (castPtr raw_str :: Ptr ())]

-- | @- rangeOfString:@
rangeOfString :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> IO NSRange
rangeOfString nsString  searchString =
withObjCPtr searchString $ \raw_searchString ->
    sendMsgStret nsString (mkSelector "rangeOfString:") retNSRange [argPtr (castPtr raw_searchString :: Ptr ())]

-- | @- rangeOfString:options:@
rangeOfString_options :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> NSStringCompareOptions -> IO NSRange
rangeOfString_options nsString  searchString mask =
withObjCPtr searchString $ \raw_searchString ->
    sendMsgStret nsString (mkSelector "rangeOfString:options:") retNSRange [argPtr (castPtr raw_searchString :: Ptr ()), argCULong (coerce mask)]

-- | @- rangeOfString:options:range:@
rangeOfString_options_range :: (IsNSString nsString, IsNSString searchString) => nsString -> searchString -> NSStringCompareOptions -> NSRange -> IO NSRange
rangeOfString_options_range nsString  searchString mask rangeOfReceiverToSearch =
withObjCPtr searchString $ \raw_searchString ->
    sendMsgStret nsString (mkSelector "rangeOfString:options:range:") retNSRange [argPtr (castPtr raw_searchString :: Ptr ()), argCULong (coerce mask), argNSRange rangeOfReceiverToSearch]

-- | @- rangeOfString:options:range:locale:@
rangeOfString_options_range_locale :: (IsNSString nsString, IsNSString searchString, IsNSLocale locale) => nsString -> searchString -> NSStringCompareOptions -> NSRange -> locale -> IO NSRange
rangeOfString_options_range_locale nsString  searchString mask rangeOfReceiverToSearch locale =
withObjCPtr searchString $ \raw_searchString ->
  withObjCPtr locale $ \raw_locale ->
      sendMsgStret nsString (mkSelector "rangeOfString:options:range:locale:") retNSRange [argPtr (castPtr raw_searchString :: Ptr ()), argCULong (coerce mask), argNSRange rangeOfReceiverToSearch, argPtr (castPtr raw_locale :: Ptr ())]

-- | @- rangeOfCharacterFromSet:@
rangeOfCharacterFromSet :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> IO NSRange
rangeOfCharacterFromSet nsString  searchSet =
withObjCPtr searchSet $ \raw_searchSet ->
    sendMsgStret nsString (mkSelector "rangeOfCharacterFromSet:") retNSRange [argPtr (castPtr raw_searchSet :: Ptr ())]

-- | @- rangeOfCharacterFromSet:options:@
rangeOfCharacterFromSet_options :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> NSStringCompareOptions -> IO NSRange
rangeOfCharacterFromSet_options nsString  searchSet mask =
withObjCPtr searchSet $ \raw_searchSet ->
    sendMsgStret nsString (mkSelector "rangeOfCharacterFromSet:options:") retNSRange [argPtr (castPtr raw_searchSet :: Ptr ()), argCULong (coerce mask)]

-- | @- rangeOfCharacterFromSet:options:range:@
rangeOfCharacterFromSet_options_range :: (IsNSString nsString, IsNSCharacterSet searchSet) => nsString -> searchSet -> NSStringCompareOptions -> NSRange -> IO NSRange
rangeOfCharacterFromSet_options_range nsString  searchSet mask rangeOfReceiverToSearch =
withObjCPtr searchSet $ \raw_searchSet ->
    sendMsgStret nsString (mkSelector "rangeOfCharacterFromSet:options:range:") retNSRange [argPtr (castPtr raw_searchSet :: Ptr ()), argCULong (coerce mask), argNSRange rangeOfReceiverToSearch]

-- | @- rangeOfComposedCharacterSequenceAtIndex:@
rangeOfComposedCharacterSequenceAtIndex :: IsNSString nsString => nsString -> CULong -> IO NSRange
rangeOfComposedCharacterSequenceAtIndex nsString  index =
  sendMsgStret nsString (mkSelector "rangeOfComposedCharacterSequenceAtIndex:") retNSRange [argCULong (fromIntegral index)]

-- | @- rangeOfComposedCharacterSequencesForRange:@
rangeOfComposedCharacterSequencesForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
rangeOfComposedCharacterSequencesForRange nsString  range =
  sendMsgStret nsString (mkSelector "rangeOfComposedCharacterSequencesForRange:") retNSRange [argNSRange range]

-- | @- stringByAppendingString:@
stringByAppendingString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO (Id NSString)
stringByAppendingString nsString  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsString (mkSelector "stringByAppendingString:") (retPtr retVoid) [argPtr (castPtr raw_aString :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByAppendingFormat:@
stringByAppendingFormat :: (IsNSString nsString, IsNSString format) => nsString -> format -> IO (Id NSString)
stringByAppendingFormat nsString  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsString (mkSelector "stringByAppendingFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- uppercaseStringWithLocale:@
uppercaseStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
uppercaseStringWithLocale nsString  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsString (mkSelector "uppercaseStringWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- lowercaseStringWithLocale:@
lowercaseStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
lowercaseStringWithLocale nsString  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsString (mkSelector "lowercaseStringWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- capitalizedStringWithLocale:@
capitalizedStringWithLocale :: (IsNSString nsString, IsNSLocale locale) => nsString -> locale -> IO (Id NSString)
capitalizedStringWithLocale nsString  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsString (mkSelector "capitalizedStringWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- getLineStart:end:contentsEnd:forRange:@
getLineStart_end_contentsEnd_forRange :: IsNSString nsString => nsString -> Ptr CULong -> Ptr CULong -> Ptr CULong -> NSRange -> IO ()
getLineStart_end_contentsEnd_forRange nsString  startPtr lineEndPtr contentsEndPtr range =
  sendMsg nsString (mkSelector "getLineStart:end:contentsEnd:forRange:") retVoid [argPtr startPtr, argPtr lineEndPtr, argPtr contentsEndPtr, argNSRange range]

-- | @- lineRangeForRange:@
lineRangeForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
lineRangeForRange nsString  range =
  sendMsgStret nsString (mkSelector "lineRangeForRange:") retNSRange [argNSRange range]

-- | @- getParagraphStart:end:contentsEnd:forRange:@
getParagraphStart_end_contentsEnd_forRange :: IsNSString nsString => nsString -> Ptr CULong -> Ptr CULong -> Ptr CULong -> NSRange -> IO ()
getParagraphStart_end_contentsEnd_forRange nsString  startPtr parEndPtr contentsEndPtr range =
  sendMsg nsString (mkSelector "getParagraphStart:end:contentsEnd:forRange:") retVoid [argPtr startPtr, argPtr parEndPtr, argPtr contentsEndPtr, argNSRange range]

-- | @- paragraphRangeForRange:@
paragraphRangeForRange :: IsNSString nsString => nsString -> NSRange -> IO NSRange
paragraphRangeForRange nsString  range =
  sendMsgStret nsString (mkSelector "paragraphRangeForRange:") retNSRange [argNSRange range]

-- | @- enumerateSubstringsInRange:options:usingBlock:@
enumerateSubstringsInRange_options_usingBlock :: IsNSString nsString => nsString -> NSRange -> NSStringEnumerationOptions -> Ptr () -> IO ()
enumerateSubstringsInRange_options_usingBlock nsString  range opts block =
  sendMsg nsString (mkSelector "enumerateSubstringsInRange:options:usingBlock:") retVoid [argNSRange range, argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateLinesUsingBlock:@
enumerateLinesUsingBlock :: IsNSString nsString => nsString -> Ptr () -> IO ()
enumerateLinesUsingBlock nsString  block =
  sendMsg nsString (mkSelector "enumerateLinesUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- dataUsingEncoding:allowLossyConversion:@
dataUsingEncoding_allowLossyConversion :: IsNSString nsString => nsString -> CULong -> Bool -> IO (Id NSData)
dataUsingEncoding_allowLossyConversion nsString  encoding lossy =
  sendMsg nsString (mkSelector "dataUsingEncoding:allowLossyConversion:") (retPtr retVoid) [argCULong (fromIntegral encoding), argCULong (if lossy then 1 else 0)] >>= retainedObject . castPtr

-- | @- dataUsingEncoding:@
dataUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Id NSData)
dataUsingEncoding nsString  encoding =
  sendMsg nsString (mkSelector "dataUsingEncoding:") (retPtr retVoid) [argCULong (fromIntegral encoding)] >>= retainedObject . castPtr

-- | @- canBeConvertedToEncoding:@
canBeConvertedToEncoding :: IsNSString nsString => nsString -> CULong -> IO Bool
canBeConvertedToEncoding nsString  encoding =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "canBeConvertedToEncoding:") retCULong [argCULong (fromIntegral encoding)]

-- | @- cStringUsingEncoding:@
cStringUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO (Const (Ptr CChar))
cStringUsingEncoding nsString  encoding =
  fmap Const $ fmap castPtr $ sendMsg nsString (mkSelector "cStringUsingEncoding:") (retPtr retVoid) [argCULong (fromIntegral encoding)]

-- | @- getCString:maxLength:encoding:@
getCString_maxLength_encoding :: IsNSString nsString => nsString -> Ptr CChar -> CULong -> CULong -> IO Bool
getCString_maxLength_encoding nsString  buffer maxBufferCount encoding =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "getCString:maxLength:encoding:") retCULong [argPtr buffer, argCULong (fromIntegral maxBufferCount), argCULong (fromIntegral encoding)]

-- | @- getBytes:maxLength:usedLength:encoding:options:range:remainingRange:@
getBytes_maxLength_usedLength_encoding_options_range_remainingRange :: IsNSString nsString => nsString -> Ptr () -> CULong -> Ptr CULong -> CULong -> NSStringEncodingConversionOptions -> NSRange -> Ptr NSRange -> IO Bool
getBytes_maxLength_usedLength_encoding_options_range_remainingRange nsString  buffer maxBufferCount usedBufferCount encoding options range leftover =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "getBytes:maxLength:usedLength:encoding:options:range:remainingRange:") retCULong [argPtr buffer, argCULong (fromIntegral maxBufferCount), argPtr usedBufferCount, argCULong (fromIntegral encoding), argCULong (coerce options), argNSRange range, argPtr leftover]

-- | @- maximumLengthOfBytesUsingEncoding:@
maximumLengthOfBytesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO CULong
maximumLengthOfBytesUsingEncoding nsString  enc =
  sendMsg nsString (mkSelector "maximumLengthOfBytesUsingEncoding:") retCULong [argCULong (fromIntegral enc)]

-- | @- lengthOfBytesUsingEncoding:@
lengthOfBytesUsingEncoding :: IsNSString nsString => nsString -> CULong -> IO CULong
lengthOfBytesUsingEncoding nsString  enc =
  sendMsg nsString (mkSelector "lengthOfBytesUsingEncoding:") retCULong [argCULong (fromIntegral enc)]

-- | @+ localizedNameOfStringEncoding:@
localizedNameOfStringEncoding :: CULong -> IO (Id NSString)
localizedNameOfStringEncoding encoding =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "localizedNameOfStringEncoding:") (retPtr retVoid) [argCULong (fromIntegral encoding)] >>= retainedObject . castPtr

-- | @- componentsSeparatedByString:@
componentsSeparatedByString :: (IsNSString nsString, IsNSString separator) => nsString -> separator -> IO (Id NSArray)
componentsSeparatedByString nsString  separator =
withObjCPtr separator $ \raw_separator ->
    sendMsg nsString (mkSelector "componentsSeparatedByString:") (retPtr retVoid) [argPtr (castPtr raw_separator :: Ptr ())] >>= retainedObject . castPtr

-- | @- componentsSeparatedByCharactersInSet:@
componentsSeparatedByCharactersInSet :: (IsNSString nsString, IsNSCharacterSet separator) => nsString -> separator -> IO (Id NSArray)
componentsSeparatedByCharactersInSet nsString  separator =
withObjCPtr separator $ \raw_separator ->
    sendMsg nsString (mkSelector "componentsSeparatedByCharactersInSet:") (retPtr retVoid) [argPtr (castPtr raw_separator :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByTrimmingCharactersInSet:@
stringByTrimmingCharactersInSet :: (IsNSString nsString, IsNSCharacterSet set) => nsString -> set -> IO (Id NSString)
stringByTrimmingCharactersInSet nsString  set =
withObjCPtr set $ \raw_set ->
    sendMsg nsString (mkSelector "stringByTrimmingCharactersInSet:") (retPtr retVoid) [argPtr (castPtr raw_set :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByPaddingToLength:withString:startingAtIndex:@
stringByPaddingToLength_withString_startingAtIndex :: (IsNSString nsString, IsNSString padString) => nsString -> CULong -> padString -> CULong -> IO (Id NSString)
stringByPaddingToLength_withString_startingAtIndex nsString  newLength padString padIndex =
withObjCPtr padString $ \raw_padString ->
    sendMsg nsString (mkSelector "stringByPaddingToLength:withString:startingAtIndex:") (retPtr retVoid) [argCULong (fromIntegral newLength), argPtr (castPtr raw_padString :: Ptr ()), argCULong (fromIntegral padIndex)] >>= retainedObject . castPtr

-- | @- stringByFoldingWithOptions:locale:@
stringByFoldingWithOptions_locale :: (IsNSString nsString, IsNSLocale locale) => nsString -> NSStringCompareOptions -> locale -> IO (Id NSString)
stringByFoldingWithOptions_locale nsString  options locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg nsString (mkSelector "stringByFoldingWithOptions:locale:") (retPtr retVoid) [argCULong (coerce options), argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByReplacingOccurrencesOfString:withString:options:range:@
stringByReplacingOccurrencesOfString_withString_options_range :: (IsNSString nsString, IsNSString target, IsNSString replacement) => nsString -> target -> replacement -> NSStringCompareOptions -> NSRange -> IO (Id NSString)
stringByReplacingOccurrencesOfString_withString_options_range nsString  target replacement options searchRange =
withObjCPtr target $ \raw_target ->
  withObjCPtr replacement $ \raw_replacement ->
      sendMsg nsString (mkSelector "stringByReplacingOccurrencesOfString:withString:options:range:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_replacement :: Ptr ()), argCULong (coerce options), argNSRange searchRange] >>= retainedObject . castPtr

-- | @- stringByReplacingOccurrencesOfString:withString:@
stringByReplacingOccurrencesOfString_withString :: (IsNSString nsString, IsNSString target, IsNSString replacement) => nsString -> target -> replacement -> IO (Id NSString)
stringByReplacingOccurrencesOfString_withString nsString  target replacement =
withObjCPtr target $ \raw_target ->
  withObjCPtr replacement $ \raw_replacement ->
      sendMsg nsString (mkSelector "stringByReplacingOccurrencesOfString:withString:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_replacement :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByReplacingCharactersInRange:withString:@
stringByReplacingCharactersInRange_withString :: (IsNSString nsString, IsNSString replacement) => nsString -> NSRange -> replacement -> IO (Id NSString)
stringByReplacingCharactersInRange_withString nsString  range replacement =
withObjCPtr replacement $ \raw_replacement ->
    sendMsg nsString (mkSelector "stringByReplacingCharactersInRange:withString:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_replacement :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByApplyingTransform:reverse:@
stringByApplyingTransform_reverse :: (IsNSString nsString, IsNSString transform) => nsString -> transform -> Bool -> IO (Id NSString)
stringByApplyingTransform_reverse nsString  transform reverse_ =
withObjCPtr transform $ \raw_transform ->
    sendMsg nsString (mkSelector "stringByApplyingTransform:reverse:") (retPtr retVoid) [argPtr (castPtr raw_transform :: Ptr ()), argCULong (if reverse_ then 1 else 0)] >>= retainedObject . castPtr

-- | @- writeToURL:atomically:encoding:error:@
writeToURL_atomically_encoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> Bool -> CULong -> error_ -> IO Bool
writeToURL_atomically_encoding_error nsString  url useAuxiliaryFile enc error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "writeToURL:atomically:encoding:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- writeToFile:atomically:encoding:error:@
writeToFile_atomically_encoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> Bool -> CULong -> error_ -> IO Bool
writeToFile_atomically_encoding_error nsString  path useAuxiliaryFile enc error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "writeToFile:atomically:encoding:error:") retCULong [argPtr (castPtr raw_path :: Ptr ()), argCULong (if useAuxiliaryFile then 1 else 0), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- initWithCharactersNoCopy:length:freeWhenDone:@
initWithCharactersNoCopy_length_freeWhenDone :: IsNSString nsString => nsString -> Ptr CUShort -> CULong -> Bool -> IO (Id NSString)
initWithCharactersNoCopy_length_freeWhenDone nsString  characters length_ freeBuffer =
  sendMsg nsString (mkSelector "initWithCharactersNoCopy:length:freeWhenDone:") (retPtr retVoid) [argPtr characters, argCULong (fromIntegral length_), argCULong (if freeBuffer then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithCharactersNoCopy:length:deallocator:@
initWithCharactersNoCopy_length_deallocator :: IsNSString nsString => nsString -> Ptr CUShort -> CULong -> Ptr () -> IO (Id NSString)
initWithCharactersNoCopy_length_deallocator nsString  chars len deallocator =
  sendMsg nsString (mkSelector "initWithCharactersNoCopy:length:deallocator:") (retPtr retVoid) [argPtr chars, argCULong (fromIntegral len), argPtr (castPtr deallocator :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCharacters:length:@
initWithCharacters_length :: IsNSString nsString => nsString -> Const (Ptr CUShort) -> CULong -> IO (Id NSString)
initWithCharacters_length nsString  characters length_ =
  sendMsg nsString (mkSelector "initWithCharacters:length:") (retPtr retVoid) [argPtr (unConst characters), argCULong (fromIntegral length_)] >>= ownedObject . castPtr

-- | @- initWithUTF8String:@
initWithUTF8String :: IsNSString nsString => nsString -> Const (Ptr CChar) -> IO (Id NSString)
initWithUTF8String nsString  nullTerminatedCString =
  sendMsg nsString (mkSelector "initWithUTF8String:") (retPtr retVoid) [argPtr (unConst nullTerminatedCString)] >>= ownedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSString nsString, IsNSString aString) => nsString -> aString -> IO (Id NSString)
initWithString nsString  aString =
withObjCPtr aString $ \raw_aString ->
    sendMsg nsString (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_aString :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFormat:@
initWithFormat :: (IsNSString nsString, IsNSString format) => nsString -> format -> IO (Id NSString)
initWithFormat nsString  format =
withObjCPtr format $ \raw_format ->
    sendMsg nsString (mkSelector "initWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFormat:arguments:@
initWithFormat_arguments :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> IO (Id NSString)
initWithFormat_arguments nsString  format argList =
withObjCPtr format $ \raw_format ->
    sendMsg nsString (mkSelector "initWithFormat:arguments:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFormat:locale:@
initWithFormat_locale :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> IO (Id NSString)
initWithFormat_locale nsString  format locale =
withObjCPtr format $ \raw_format ->
    sendMsg nsString (mkSelector "initWithFormat:locale:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFormat:locale:arguments:@
initWithFormat_locale_arguments :: (IsNSString nsString, IsNSString format) => nsString -> format -> RawId -> RawId -> IO (Id NSString)
initWithFormat_locale_arguments nsString  format locale argList =
withObjCPtr format $ \raw_format ->
    sendMsg nsString (mkSelector "initWithFormat:locale:arguments:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValidatedFormat:validFormatSpecifiers:error:@
initWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_error nsString  format validFormatSpecifiers error_ =
withObjCPtr format $ \raw_format ->
  withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsString (mkSelector "initWithValidatedFormat:validFormatSpecifiers:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValidatedFormat:validFormatSpecifiers:locale:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_error nsString  format validFormatSpecifiers locale error_ =
withObjCPtr format $ \raw_format ->
  withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsString (mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValidatedFormat:validFormatSpecifiers:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_arguments_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_arguments_error nsString  format validFormatSpecifiers argList error_ =
withObjCPtr format $ \raw_format ->
  withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsString (mkSelector "initWithValidatedFormat:validFormatSpecifiers:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_error :: (IsNSString nsString, IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => nsString -> format -> validFormatSpecifiers -> RawId -> RawId -> error_ -> IO (Id NSString)
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_error nsString  format validFormatSpecifiers locale argList error_ =
withObjCPtr format $ \raw_format ->
  withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsString (mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr (unRawId locale) :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:encoding:@
initWithData_encoding :: (IsNSString nsString, IsNSData data_) => nsString -> data_ -> CULong -> IO (Id NSString)
initWithData_encoding nsString  data_ encoding =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsString (mkSelector "initWithData:encoding:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (fromIntegral encoding)] >>= ownedObject . castPtr

-- | @- initWithBytes:length:encoding:@
initWithBytes_length_encoding :: IsNSString nsString => nsString -> Const (Ptr ()) -> CULong -> CULong -> IO (Id NSString)
initWithBytes_length_encoding nsString  bytes len encoding =
  sendMsg nsString (mkSelector "initWithBytes:length:encoding:") (retPtr retVoid) [argPtr (unConst bytes), argCULong (fromIntegral len), argCULong (fromIntegral encoding)] >>= ownedObject . castPtr

-- | @- initWithBytesNoCopy:length:encoding:freeWhenDone:@
initWithBytesNoCopy_length_encoding_freeWhenDone :: IsNSString nsString => nsString -> Ptr () -> CULong -> CULong -> Bool -> IO (Id NSString)
initWithBytesNoCopy_length_encoding_freeWhenDone nsString  bytes len encoding freeBuffer =
  sendMsg nsString (mkSelector "initWithBytesNoCopy:length:encoding:freeWhenDone:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral len), argCULong (fromIntegral encoding), argCULong (if freeBuffer then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithBytesNoCopy:length:encoding:deallocator:@
initWithBytesNoCopy_length_encoding_deallocator :: IsNSString nsString => nsString -> Ptr () -> CULong -> CULong -> Ptr () -> IO (Id NSString)
initWithBytesNoCopy_length_encoding_deallocator nsString  bytes len encoding deallocator =
  sendMsg nsString (mkSelector "initWithBytesNoCopy:length:encoding:deallocator:") (retPtr retVoid) [argPtr bytes, argCULong (fromIntegral len), argCULong (fromIntegral encoding), argPtr (castPtr deallocator :: Ptr ())] >>= ownedObject . castPtr

-- | @+ string@
string :: IO (Id NSString)
string  =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ stringWithString:@
stringWithString :: IsNSString string => string -> IO (Id NSString)
stringWithString string =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "stringWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ stringWithCharacters:length:@
stringWithCharacters_length :: Const (Ptr CUShort) -> CULong -> IO (Id NSString)
stringWithCharacters_length characters length_ =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "stringWithCharacters:length:") (retPtr retVoid) [argPtr (unConst characters), argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | @+ stringWithUTF8String:@
stringWithUTF8String :: Const (Ptr CChar) -> IO (Id NSString)
stringWithUTF8String nullTerminatedCString =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "stringWithUTF8String:") (retPtr retVoid) [argPtr (unConst nullTerminatedCString)] >>= retainedObject . castPtr

-- | @+ stringWithFormat:@
stringWithFormat :: IsNSString format => format -> IO (Id NSString)
stringWithFormat format =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      sendClassMsg cls' (mkSelector "stringWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localizedStringWithFormat:@
localizedStringWithFormat :: IsNSString format => format -> IO (Id NSString)
localizedStringWithFormat format =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      sendClassMsg cls' (mkSelector "localizedStringWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @+ stringWithValidatedFormat:validFormatSpecifiers:error:@
stringWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
stringWithValidatedFormat_validFormatSpecifiers_error format validFormatSpecifiers error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "stringWithValidatedFormat:validFormatSpecifiers:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ localizedStringWithValidatedFormat:validFormatSpecifiers:error:@
localizedStringWithValidatedFormat_validFormatSpecifiers_error :: (IsNSString format, IsNSString validFormatSpecifiers, IsNSError error_) => format -> validFormatSpecifiers -> error_ -> IO (Id NSString)
localizedStringWithValidatedFormat_validFormatSpecifiers_error format validFormatSpecifiers error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr validFormatSpecifiers $ \raw_validFormatSpecifiers ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "localizedStringWithValidatedFormat:validFormatSpecifiers:error:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_validFormatSpecifiers :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithCString:encoding:@
initWithCString_encoding :: IsNSString nsString => nsString -> Const (Ptr CChar) -> CULong -> IO (Id NSString)
initWithCString_encoding nsString  nullTerminatedCString encoding =
  sendMsg nsString (mkSelector "initWithCString:encoding:") (retPtr retVoid) [argPtr (unConst nullTerminatedCString), argCULong (fromIntegral encoding)] >>= ownedObject . castPtr

-- | @+ stringWithCString:encoding:@
stringWithCString_encoding :: Const (Ptr CChar) -> CULong -> IO (Id NSString)
stringWithCString_encoding cString enc =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "stringWithCString:encoding:") (retPtr retVoid) [argPtr (unConst cString), argCULong (fromIntegral enc)] >>= retainedObject . castPtr

-- | @- initWithContentsOfURL:encoding:error:@
initWithContentsOfURL_encoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> CULong -> error_ -> IO (Id NSString)
initWithContentsOfURL_encoding_error nsString  url enc error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsString (mkSelector "initWithContentsOfURL:encoding:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:encoding:error:@
initWithContentsOfFile_encoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> CULong -> error_ -> IO (Id NSString)
initWithContentsOfFile_encoding_error nsString  path enc error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsString (mkSelector "initWithContentsOfFile:encoding:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ stringWithContentsOfURL:encoding:error:@
stringWithContentsOfURL_encoding_error :: (IsNSURL url, IsNSError error_) => url -> CULong -> error_ -> IO (Id NSString)
stringWithContentsOfURL_encoding_error url enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "stringWithContentsOfURL:encoding:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ stringWithContentsOfFile:encoding:error:@
stringWithContentsOfFile_encoding_error :: (IsNSString path, IsNSError error_) => path -> CULong -> error_ -> IO (Id NSString)
stringWithContentsOfFile_encoding_error path enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr path $ \raw_path ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "stringWithContentsOfFile:encoding:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (fromIntegral enc), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfURL:usedEncoding:error:@
initWithContentsOfURL_usedEncoding_error :: (IsNSString nsString, IsNSURL url, IsNSError error_) => nsString -> url -> Ptr CULong -> error_ -> IO (Id NSString)
initWithContentsOfURL_usedEncoding_error nsString  url enc error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsString (mkSelector "initWithContentsOfURL:usedEncoding:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr enc, argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:usedEncoding:error:@
initWithContentsOfFile_usedEncoding_error :: (IsNSString nsString, IsNSString path, IsNSError error_) => nsString -> path -> Ptr CULong -> error_ -> IO (Id NSString)
initWithContentsOfFile_usedEncoding_error nsString  path enc error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsString (mkSelector "initWithContentsOfFile:usedEncoding:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr enc, argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ stringWithContentsOfURL:usedEncoding:error:@
stringWithContentsOfURL_usedEncoding_error :: (IsNSURL url, IsNSError error_) => url -> Ptr CULong -> error_ -> IO (Id NSString)
stringWithContentsOfURL_usedEncoding_error url enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "stringWithContentsOfURL:usedEncoding:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr enc, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ stringWithContentsOfFile:usedEncoding:error:@
stringWithContentsOfFile_usedEncoding_error :: (IsNSString path, IsNSError error_) => path -> Ptr CULong -> error_ -> IO (Id NSString)
stringWithContentsOfFile_usedEncoding_error path enc error_ =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr path $ \raw_path ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "stringWithContentsOfFile:usedEncoding:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr enc, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsNSString nsString => nsString -> IO CULong
length_ nsString  =
  sendMsg nsString (mkSelector "length") retCULong []

-- | @- stringByRemovingPercentEncoding@
stringByRemovingPercentEncoding :: IsNSString nsString => nsString -> IO (Id NSString)
stringByRemovingPercentEncoding nsString  =
  sendMsg nsString (mkSelector "stringByRemovingPercentEncoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathComponents@
pathComponents :: IsNSString nsString => nsString -> IO (Id NSArray)
pathComponents nsString  =
  sendMsg nsString (mkSelector "pathComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absolutePath@
absolutePath :: IsNSString nsString => nsString -> IO Bool
absolutePath nsString  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "absolutePath") retCULong []

-- | @- lastPathComponent@
lastPathComponent :: IsNSString nsString => nsString -> IO (Id NSString)
lastPathComponent nsString  =
  sendMsg nsString (mkSelector "lastPathComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByDeletingLastPathComponent@
stringByDeletingLastPathComponent :: IsNSString nsString => nsString -> IO (Id NSString)
stringByDeletingLastPathComponent nsString  =
  sendMsg nsString (mkSelector "stringByDeletingLastPathComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathExtension@
pathExtension :: IsNSString nsString => nsString -> IO (Id NSString)
pathExtension nsString  =
  sendMsg nsString (mkSelector "pathExtension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByDeletingPathExtension@
stringByDeletingPathExtension :: IsNSString nsString => nsString -> IO (Id NSString)
stringByDeletingPathExtension nsString  =
  sendMsg nsString (mkSelector "stringByDeletingPathExtension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByAbbreviatingWithTildeInPath@
stringByAbbreviatingWithTildeInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByAbbreviatingWithTildeInPath nsString  =
  sendMsg nsString (mkSelector "stringByAbbreviatingWithTildeInPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByExpandingTildeInPath@
stringByExpandingTildeInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByExpandingTildeInPath nsString  =
  sendMsg nsString (mkSelector "stringByExpandingTildeInPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByStandardizingPath@
stringByStandardizingPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByStandardizingPath nsString  =
  sendMsg nsString (mkSelector "stringByStandardizingPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stringByResolvingSymlinksInPath@
stringByResolvingSymlinksInPath :: IsNSString nsString => nsString -> IO (Id NSString)
stringByResolvingSymlinksInPath nsString  =
  sendMsg nsString (mkSelector "stringByResolvingSymlinksInPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- doubleValue@
doubleValue :: IsNSString nsString => nsString -> IO CDouble
doubleValue nsString  =
  sendMsg nsString (mkSelector "doubleValue") retCDouble []

-- | @- floatValue@
floatValue :: IsNSString nsString => nsString -> IO CFloat
floatValue nsString  =
  sendMsg nsString (mkSelector "floatValue") retCFloat []

-- | @- intValue@
intValue :: IsNSString nsString => nsString -> IO CInt
intValue nsString  =
  sendMsg nsString (mkSelector "intValue") retCInt []

-- | @- integerValue@
integerValue :: IsNSString nsString => nsString -> IO CLong
integerValue nsString  =
  sendMsg nsString (mkSelector "integerValue") retCLong []

-- | @- longLongValue@
longLongValue :: IsNSString nsString => nsString -> IO CLong
longLongValue nsString  =
  sendMsg nsString (mkSelector "longLongValue") retCLong []

-- | @- boolValue@
boolValue :: IsNSString nsString => nsString -> IO Bool
boolValue nsString  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsString (mkSelector "boolValue") retCULong []

-- | @- uppercaseString@
uppercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
uppercaseString nsString  =
  sendMsg nsString (mkSelector "uppercaseString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lowercaseString@
lowercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
lowercaseString nsString  =
  sendMsg nsString (mkSelector "lowercaseString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- capitalizedString@
capitalizedString :: IsNSString nsString => nsString -> IO (Id NSString)
capitalizedString nsString  =
  sendMsg nsString (mkSelector "capitalizedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedUppercaseString@
localizedUppercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedUppercaseString nsString  =
  sendMsg nsString (mkSelector "localizedUppercaseString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedLowercaseString@
localizedLowercaseString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedLowercaseString nsString  =
  sendMsg nsString (mkSelector "localizedLowercaseString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedCapitalizedString@
localizedCapitalizedString :: IsNSString nsString => nsString -> IO (Id NSString)
localizedCapitalizedString nsString  =
  sendMsg nsString (mkSelector "localizedCapitalizedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fastestEncoding@
fastestEncoding :: IsNSString nsString => nsString -> IO CULong
fastestEncoding nsString  =
  sendMsg nsString (mkSelector "fastestEncoding") retCULong []

-- | @- smallestEncoding@
smallestEncoding :: IsNSString nsString => nsString -> IO CULong
smallestEncoding nsString  =
  sendMsg nsString (mkSelector "smallestEncoding") retCULong []

-- | @+ defaultCStringEncoding@
defaultCStringEncoding :: IO CULong
defaultCStringEncoding  =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMsg cls' (mkSelector "defaultCStringEncoding") retCULong []

-- | @- decomposedStringWithCanonicalMapping@
decomposedStringWithCanonicalMapping :: IsNSString nsString => nsString -> IO (Id NSString)
decomposedStringWithCanonicalMapping nsString  =
  sendMsg nsString (mkSelector "decomposedStringWithCanonicalMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- precomposedStringWithCanonicalMapping@
precomposedStringWithCanonicalMapping :: IsNSString nsString => nsString -> IO (Id NSString)
precomposedStringWithCanonicalMapping nsString  =
  sendMsg nsString (mkSelector "precomposedStringWithCanonicalMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- decomposedStringWithCompatibilityMapping@
decomposedStringWithCompatibilityMapping :: IsNSString nsString => nsString -> IO (Id NSString)
decomposedStringWithCompatibilityMapping nsString  =
  sendMsg nsString (mkSelector "decomposedStringWithCompatibilityMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- precomposedStringWithCompatibilityMapping@
precomposedStringWithCompatibilityMapping :: IsNSString nsString => nsString -> IO (Id NSString)
precomposedStringWithCompatibilityMapping nsString  =
  sendMsg nsString (mkSelector "precomposedStringWithCompatibilityMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- description@
description :: IsNSString nsString => nsString -> IO (Id NSString)
description nsString  =
  sendMsg nsString (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hash@
hash :: IsNSString nsString => nsString -> IO CULong
hash nsString  =
  sendMsg nsString (mkSelector "hash") retCULong []


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
characterAtIndexSelector :: Selector
characterAtIndexSelector = mkSelector "characterAtIndex:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @linguisticTagsInRange:scheme:options:orthography:tokenRanges:@
linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector :: Selector
linguisticTagsInRange_scheme_options_orthography_tokenRangesSelector = mkSelector "linguisticTagsInRange:scheme:options:orthography:tokenRanges:"

-- | @Selector@ for @enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:@
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector :: Selector
enumerateLinguisticTagsInRange_scheme_options_orthography_usingBlockSelector = mkSelector "enumerateLinguisticTagsInRange:scheme:options:orthography:usingBlock:"

-- | @Selector@ for @stringByAddingPercentEncodingWithAllowedCharacters:@
stringByAddingPercentEncodingWithAllowedCharactersSelector :: Selector
stringByAddingPercentEncodingWithAllowedCharactersSelector = mkSelector "stringByAddingPercentEncodingWithAllowedCharacters:"

-- | @Selector@ for @stringByAddingPercentEscapesUsingEncoding:@
stringByAddingPercentEscapesUsingEncodingSelector :: Selector
stringByAddingPercentEscapesUsingEncodingSelector = mkSelector "stringByAddingPercentEscapesUsingEncoding:"

-- | @Selector@ for @stringByReplacingPercentEscapesUsingEncoding:@
stringByReplacingPercentEscapesUsingEncodingSelector :: Selector
stringByReplacingPercentEscapesUsingEncodingSelector = mkSelector "stringByReplacingPercentEscapesUsingEncoding:"

-- | @Selector@ for @pathWithComponents:@
pathWithComponentsSelector :: Selector
pathWithComponentsSelector = mkSelector "pathWithComponents:"

-- | @Selector@ for @stringByAppendingPathComponent:@
stringByAppendingPathComponentSelector :: Selector
stringByAppendingPathComponentSelector = mkSelector "stringByAppendingPathComponent:"

-- | @Selector@ for @stringByAppendingPathExtension:@
stringByAppendingPathExtensionSelector :: Selector
stringByAppendingPathExtensionSelector = mkSelector "stringByAppendingPathExtension:"

-- | @Selector@ for @stringsByAppendingPaths:@
stringsByAppendingPathsSelector :: Selector
stringsByAppendingPathsSelector = mkSelector "stringsByAppendingPaths:"

-- | @Selector@ for @completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:@
completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector :: Selector
completePathIntoString_caseSensitive_matchesIntoArray_filterTypesSelector = mkSelector "completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:"

-- | @Selector@ for @getFileSystemRepresentation:maxLength:@
getFileSystemRepresentation_maxLengthSelector :: Selector
getFileSystemRepresentation_maxLengthSelector = mkSelector "getFileSystemRepresentation:maxLength:"

-- | @Selector@ for @variantFittingPresentationWidth:@
variantFittingPresentationWidthSelector :: Selector
variantFittingPresentationWidthSelector = mkSelector "variantFittingPresentationWidth:"

-- | @Selector@ for @cString@
cStringSelector :: Selector
cStringSelector = mkSelector "cString"

-- | @Selector@ for @lossyCString@
lossyCStringSelector :: Selector
lossyCStringSelector = mkSelector "lossyCString"

-- | @Selector@ for @cStringLength@
cStringLengthSelector :: Selector
cStringLengthSelector = mkSelector "cStringLength"

-- | @Selector@ for @getCString:@
getCStringSelector :: Selector
getCStringSelector = mkSelector "getCString:"

-- | @Selector@ for @getCString:maxLength:@
getCString_maxLengthSelector :: Selector
getCString_maxLengthSelector = mkSelector "getCString:maxLength:"

-- | @Selector@ for @getCString:maxLength:range:remainingRange:@
getCString_maxLength_range_remainingRangeSelector :: Selector
getCString_maxLength_range_remainingRangeSelector = mkSelector "getCString:maxLength:range:remainingRange:"

-- | @Selector@ for @writeToFile:atomically:@
writeToFile_atomicallySelector :: Selector
writeToFile_atomicallySelector = mkSelector "writeToFile:atomically:"

-- | @Selector@ for @writeToURL:atomically:@
writeToURL_atomicallySelector :: Selector
writeToURL_atomicallySelector = mkSelector "writeToURL:atomically:"

-- | @Selector@ for @initWithContentsOfFile:@
initWithContentsOfFileSelector :: Selector
initWithContentsOfFileSelector = mkSelector "initWithContentsOfFile:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @stringWithContentsOfFile:@
stringWithContentsOfFileSelector :: Selector
stringWithContentsOfFileSelector = mkSelector "stringWithContentsOfFile:"

-- | @Selector@ for @stringWithContentsOfURL:@
stringWithContentsOfURLSelector :: Selector
stringWithContentsOfURLSelector = mkSelector "stringWithContentsOfURL:"

-- | @Selector@ for @initWithCStringNoCopy:length:freeWhenDone:@
initWithCStringNoCopy_length_freeWhenDoneSelector :: Selector
initWithCStringNoCopy_length_freeWhenDoneSelector = mkSelector "initWithCStringNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithCString:length:@
initWithCString_lengthSelector :: Selector
initWithCString_lengthSelector = mkSelector "initWithCString:length:"

-- | @Selector@ for @initWithCString:@
initWithCStringSelector :: Selector
initWithCStringSelector = mkSelector "initWithCString:"

-- | @Selector@ for @stringWithCString:length:@
stringWithCString_lengthSelector :: Selector
stringWithCString_lengthSelector = mkSelector "stringWithCString:length:"

-- | @Selector@ for @stringWithCString:@
stringWithCStringSelector :: Selector
stringWithCStringSelector = mkSelector "stringWithCString:"

-- | @Selector@ for @getCharacters:@
getCharactersSelector :: Selector
getCharactersSelector = mkSelector "getCharacters:"

-- | @Selector@ for @propertyList@
propertyListSelector :: Selector
propertyListSelector = mkSelector "propertyList"

-- | @Selector@ for @propertyListFromStringsFileFormat@
propertyListFromStringsFileFormatSelector :: Selector
propertyListFromStringsFileFormatSelector = mkSelector "propertyListFromStringsFileFormat"

-- | @Selector@ for @stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:@
stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector :: Selector
stringEncodingForData_encodingOptions_convertedString_usedLossyConversionSelector = mkSelector "stringEncodingForData:encodingOptions:convertedString:usedLossyConversion:"

-- | @Selector@ for @substringFromIndex:@
substringFromIndexSelector :: Selector
substringFromIndexSelector = mkSelector "substringFromIndex:"

-- | @Selector@ for @substringToIndex:@
substringToIndexSelector :: Selector
substringToIndexSelector = mkSelector "substringToIndex:"

-- | @Selector@ for @substringWithRange:@
substringWithRangeSelector :: Selector
substringWithRangeSelector = mkSelector "substringWithRange:"

-- | @Selector@ for @getCharacters:range:@
getCharacters_rangeSelector :: Selector
getCharacters_rangeSelector = mkSelector "getCharacters:range:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @compare:options:@
compare_optionsSelector :: Selector
compare_optionsSelector = mkSelector "compare:options:"

-- | @Selector@ for @compare:options:range:@
compare_options_rangeSelector :: Selector
compare_options_rangeSelector = mkSelector "compare:options:range:"

-- | @Selector@ for @compare:options:range:locale:@
compare_options_range_localeSelector :: Selector
compare_options_range_localeSelector = mkSelector "compare:options:range:locale:"

-- | @Selector@ for @caseInsensitiveCompare:@
caseInsensitiveCompareSelector :: Selector
caseInsensitiveCompareSelector = mkSelector "caseInsensitiveCompare:"

-- | @Selector@ for @localizedCompare:@
localizedCompareSelector :: Selector
localizedCompareSelector = mkSelector "localizedCompare:"

-- | @Selector@ for @localizedCaseInsensitiveCompare:@
localizedCaseInsensitiveCompareSelector :: Selector
localizedCaseInsensitiveCompareSelector = mkSelector "localizedCaseInsensitiveCompare:"

-- | @Selector@ for @localizedStandardCompare:@
localizedStandardCompareSelector :: Selector
localizedStandardCompareSelector = mkSelector "localizedStandardCompare:"

-- | @Selector@ for @isEqualToString:@
isEqualToStringSelector :: Selector
isEqualToStringSelector = mkSelector "isEqualToString:"

-- | @Selector@ for @hasPrefix:@
hasPrefixSelector :: Selector
hasPrefixSelector = mkSelector "hasPrefix:"

-- | @Selector@ for @hasSuffix:@
hasSuffixSelector :: Selector
hasSuffixSelector = mkSelector "hasSuffix:"

-- | @Selector@ for @commonPrefixWithString:options:@
commonPrefixWithString_optionsSelector :: Selector
commonPrefixWithString_optionsSelector = mkSelector "commonPrefixWithString:options:"

-- | @Selector@ for @containsString:@
containsStringSelector :: Selector
containsStringSelector = mkSelector "containsString:"

-- | @Selector@ for @localizedCaseInsensitiveContainsString:@
localizedCaseInsensitiveContainsStringSelector :: Selector
localizedCaseInsensitiveContainsStringSelector = mkSelector "localizedCaseInsensitiveContainsString:"

-- | @Selector@ for @localizedStandardContainsString:@
localizedStandardContainsStringSelector :: Selector
localizedStandardContainsStringSelector = mkSelector "localizedStandardContainsString:"

-- | @Selector@ for @localizedStandardRangeOfString:@
localizedStandardRangeOfStringSelector :: Selector
localizedStandardRangeOfStringSelector = mkSelector "localizedStandardRangeOfString:"

-- | @Selector@ for @rangeOfString:@
rangeOfStringSelector :: Selector
rangeOfStringSelector = mkSelector "rangeOfString:"

-- | @Selector@ for @rangeOfString:options:@
rangeOfString_optionsSelector :: Selector
rangeOfString_optionsSelector = mkSelector "rangeOfString:options:"

-- | @Selector@ for @rangeOfString:options:range:@
rangeOfString_options_rangeSelector :: Selector
rangeOfString_options_rangeSelector = mkSelector "rangeOfString:options:range:"

-- | @Selector@ for @rangeOfString:options:range:locale:@
rangeOfString_options_range_localeSelector :: Selector
rangeOfString_options_range_localeSelector = mkSelector "rangeOfString:options:range:locale:"

-- | @Selector@ for @rangeOfCharacterFromSet:@
rangeOfCharacterFromSetSelector :: Selector
rangeOfCharacterFromSetSelector = mkSelector "rangeOfCharacterFromSet:"

-- | @Selector@ for @rangeOfCharacterFromSet:options:@
rangeOfCharacterFromSet_optionsSelector :: Selector
rangeOfCharacterFromSet_optionsSelector = mkSelector "rangeOfCharacterFromSet:options:"

-- | @Selector@ for @rangeOfCharacterFromSet:options:range:@
rangeOfCharacterFromSet_options_rangeSelector :: Selector
rangeOfCharacterFromSet_options_rangeSelector = mkSelector "rangeOfCharacterFromSet:options:range:"

-- | @Selector@ for @rangeOfComposedCharacterSequenceAtIndex:@
rangeOfComposedCharacterSequenceAtIndexSelector :: Selector
rangeOfComposedCharacterSequenceAtIndexSelector = mkSelector "rangeOfComposedCharacterSequenceAtIndex:"

-- | @Selector@ for @rangeOfComposedCharacterSequencesForRange:@
rangeOfComposedCharacterSequencesForRangeSelector :: Selector
rangeOfComposedCharacterSequencesForRangeSelector = mkSelector "rangeOfComposedCharacterSequencesForRange:"

-- | @Selector@ for @stringByAppendingString:@
stringByAppendingStringSelector :: Selector
stringByAppendingStringSelector = mkSelector "stringByAppendingString:"

-- | @Selector@ for @stringByAppendingFormat:@
stringByAppendingFormatSelector :: Selector
stringByAppendingFormatSelector = mkSelector "stringByAppendingFormat:"

-- | @Selector@ for @uppercaseStringWithLocale:@
uppercaseStringWithLocaleSelector :: Selector
uppercaseStringWithLocaleSelector = mkSelector "uppercaseStringWithLocale:"

-- | @Selector@ for @lowercaseStringWithLocale:@
lowercaseStringWithLocaleSelector :: Selector
lowercaseStringWithLocaleSelector = mkSelector "lowercaseStringWithLocale:"

-- | @Selector@ for @capitalizedStringWithLocale:@
capitalizedStringWithLocaleSelector :: Selector
capitalizedStringWithLocaleSelector = mkSelector "capitalizedStringWithLocale:"

-- | @Selector@ for @getLineStart:end:contentsEnd:forRange:@
getLineStart_end_contentsEnd_forRangeSelector :: Selector
getLineStart_end_contentsEnd_forRangeSelector = mkSelector "getLineStart:end:contentsEnd:forRange:"

-- | @Selector@ for @lineRangeForRange:@
lineRangeForRangeSelector :: Selector
lineRangeForRangeSelector = mkSelector "lineRangeForRange:"

-- | @Selector@ for @getParagraphStart:end:contentsEnd:forRange:@
getParagraphStart_end_contentsEnd_forRangeSelector :: Selector
getParagraphStart_end_contentsEnd_forRangeSelector = mkSelector "getParagraphStart:end:contentsEnd:forRange:"

-- | @Selector@ for @paragraphRangeForRange:@
paragraphRangeForRangeSelector :: Selector
paragraphRangeForRangeSelector = mkSelector "paragraphRangeForRange:"

-- | @Selector@ for @enumerateSubstringsInRange:options:usingBlock:@
enumerateSubstringsInRange_options_usingBlockSelector :: Selector
enumerateSubstringsInRange_options_usingBlockSelector = mkSelector "enumerateSubstringsInRange:options:usingBlock:"

-- | @Selector@ for @enumerateLinesUsingBlock:@
enumerateLinesUsingBlockSelector :: Selector
enumerateLinesUsingBlockSelector = mkSelector "enumerateLinesUsingBlock:"

-- | @Selector@ for @dataUsingEncoding:allowLossyConversion:@
dataUsingEncoding_allowLossyConversionSelector :: Selector
dataUsingEncoding_allowLossyConversionSelector = mkSelector "dataUsingEncoding:allowLossyConversion:"

-- | @Selector@ for @dataUsingEncoding:@
dataUsingEncodingSelector :: Selector
dataUsingEncodingSelector = mkSelector "dataUsingEncoding:"

-- | @Selector@ for @canBeConvertedToEncoding:@
canBeConvertedToEncodingSelector :: Selector
canBeConvertedToEncodingSelector = mkSelector "canBeConvertedToEncoding:"

-- | @Selector@ for @cStringUsingEncoding:@
cStringUsingEncodingSelector :: Selector
cStringUsingEncodingSelector = mkSelector "cStringUsingEncoding:"

-- | @Selector@ for @getCString:maxLength:encoding:@
getCString_maxLength_encodingSelector :: Selector
getCString_maxLength_encodingSelector = mkSelector "getCString:maxLength:encoding:"

-- | @Selector@ for @getBytes:maxLength:usedLength:encoding:options:range:remainingRange:@
getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector :: Selector
getBytes_maxLength_usedLength_encoding_options_range_remainingRangeSelector = mkSelector "getBytes:maxLength:usedLength:encoding:options:range:remainingRange:"

-- | @Selector@ for @maximumLengthOfBytesUsingEncoding:@
maximumLengthOfBytesUsingEncodingSelector :: Selector
maximumLengthOfBytesUsingEncodingSelector = mkSelector "maximumLengthOfBytesUsingEncoding:"

-- | @Selector@ for @lengthOfBytesUsingEncoding:@
lengthOfBytesUsingEncodingSelector :: Selector
lengthOfBytesUsingEncodingSelector = mkSelector "lengthOfBytesUsingEncoding:"

-- | @Selector@ for @localizedNameOfStringEncoding:@
localizedNameOfStringEncodingSelector :: Selector
localizedNameOfStringEncodingSelector = mkSelector "localizedNameOfStringEncoding:"

-- | @Selector@ for @componentsSeparatedByString:@
componentsSeparatedByStringSelector :: Selector
componentsSeparatedByStringSelector = mkSelector "componentsSeparatedByString:"

-- | @Selector@ for @componentsSeparatedByCharactersInSet:@
componentsSeparatedByCharactersInSetSelector :: Selector
componentsSeparatedByCharactersInSetSelector = mkSelector "componentsSeparatedByCharactersInSet:"

-- | @Selector@ for @stringByTrimmingCharactersInSet:@
stringByTrimmingCharactersInSetSelector :: Selector
stringByTrimmingCharactersInSetSelector = mkSelector "stringByTrimmingCharactersInSet:"

-- | @Selector@ for @stringByPaddingToLength:withString:startingAtIndex:@
stringByPaddingToLength_withString_startingAtIndexSelector :: Selector
stringByPaddingToLength_withString_startingAtIndexSelector = mkSelector "stringByPaddingToLength:withString:startingAtIndex:"

-- | @Selector@ for @stringByFoldingWithOptions:locale:@
stringByFoldingWithOptions_localeSelector :: Selector
stringByFoldingWithOptions_localeSelector = mkSelector "stringByFoldingWithOptions:locale:"

-- | @Selector@ for @stringByReplacingOccurrencesOfString:withString:options:range:@
stringByReplacingOccurrencesOfString_withString_options_rangeSelector :: Selector
stringByReplacingOccurrencesOfString_withString_options_rangeSelector = mkSelector "stringByReplacingOccurrencesOfString:withString:options:range:"

-- | @Selector@ for @stringByReplacingOccurrencesOfString:withString:@
stringByReplacingOccurrencesOfString_withStringSelector :: Selector
stringByReplacingOccurrencesOfString_withStringSelector = mkSelector "stringByReplacingOccurrencesOfString:withString:"

-- | @Selector@ for @stringByReplacingCharactersInRange:withString:@
stringByReplacingCharactersInRange_withStringSelector :: Selector
stringByReplacingCharactersInRange_withStringSelector = mkSelector "stringByReplacingCharactersInRange:withString:"

-- | @Selector@ for @stringByApplyingTransform:reverse:@
stringByApplyingTransform_reverseSelector :: Selector
stringByApplyingTransform_reverseSelector = mkSelector "stringByApplyingTransform:reverse:"

-- | @Selector@ for @writeToURL:atomically:encoding:error:@
writeToURL_atomically_encoding_errorSelector :: Selector
writeToURL_atomically_encoding_errorSelector = mkSelector "writeToURL:atomically:encoding:error:"

-- | @Selector@ for @writeToFile:atomically:encoding:error:@
writeToFile_atomically_encoding_errorSelector :: Selector
writeToFile_atomically_encoding_errorSelector = mkSelector "writeToFile:atomically:encoding:error:"

-- | @Selector@ for @initWithCharactersNoCopy:length:freeWhenDone:@
initWithCharactersNoCopy_length_freeWhenDoneSelector :: Selector
initWithCharactersNoCopy_length_freeWhenDoneSelector = mkSelector "initWithCharactersNoCopy:length:freeWhenDone:"

-- | @Selector@ for @initWithCharactersNoCopy:length:deallocator:@
initWithCharactersNoCopy_length_deallocatorSelector :: Selector
initWithCharactersNoCopy_length_deallocatorSelector = mkSelector "initWithCharactersNoCopy:length:deallocator:"

-- | @Selector@ for @initWithCharacters:length:@
initWithCharacters_lengthSelector :: Selector
initWithCharacters_lengthSelector = mkSelector "initWithCharacters:length:"

-- | @Selector@ for @initWithUTF8String:@
initWithUTF8StringSelector :: Selector
initWithUTF8StringSelector = mkSelector "initWithUTF8String:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithFormat:@
initWithFormatSelector :: Selector
initWithFormatSelector = mkSelector "initWithFormat:"

-- | @Selector@ for @initWithFormat:arguments:@
initWithFormat_argumentsSelector :: Selector
initWithFormat_argumentsSelector = mkSelector "initWithFormat:arguments:"

-- | @Selector@ for @initWithFormat:locale:@
initWithFormat_localeSelector :: Selector
initWithFormat_localeSelector = mkSelector "initWithFormat:locale:"

-- | @Selector@ for @initWithFormat:locale:arguments:@
initWithFormat_locale_argumentsSelector :: Selector
initWithFormat_locale_argumentsSelector = mkSelector "initWithFormat:locale:arguments:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:error:@
initWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector
initWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:locale:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector :: Selector
initWithValidatedFormat_validFormatSpecifiers_locale_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector :: Selector
initWithValidatedFormat_validFormatSpecifiers_arguments_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:arguments:error:"

-- | @Selector@ for @initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:@
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector :: Selector
initWithValidatedFormat_validFormatSpecifiers_locale_arguments_errorSelector = mkSelector "initWithValidatedFormat:validFormatSpecifiers:locale:arguments:error:"

-- | @Selector@ for @initWithData:encoding:@
initWithData_encodingSelector :: Selector
initWithData_encodingSelector = mkSelector "initWithData:encoding:"

-- | @Selector@ for @initWithBytes:length:encoding:@
initWithBytes_length_encodingSelector :: Selector
initWithBytes_length_encodingSelector = mkSelector "initWithBytes:length:encoding:"

-- | @Selector@ for @initWithBytesNoCopy:length:encoding:freeWhenDone:@
initWithBytesNoCopy_length_encoding_freeWhenDoneSelector :: Selector
initWithBytesNoCopy_length_encoding_freeWhenDoneSelector = mkSelector "initWithBytesNoCopy:length:encoding:freeWhenDone:"

-- | @Selector@ for @initWithBytesNoCopy:length:encoding:deallocator:@
initWithBytesNoCopy_length_encoding_deallocatorSelector :: Selector
initWithBytesNoCopy_length_encoding_deallocatorSelector = mkSelector "initWithBytesNoCopy:length:encoding:deallocator:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @stringWithString:@
stringWithStringSelector :: Selector
stringWithStringSelector = mkSelector "stringWithString:"

-- | @Selector@ for @stringWithCharacters:length:@
stringWithCharacters_lengthSelector :: Selector
stringWithCharacters_lengthSelector = mkSelector "stringWithCharacters:length:"

-- | @Selector@ for @stringWithUTF8String:@
stringWithUTF8StringSelector :: Selector
stringWithUTF8StringSelector = mkSelector "stringWithUTF8String:"

-- | @Selector@ for @stringWithFormat:@
stringWithFormatSelector :: Selector
stringWithFormatSelector = mkSelector "stringWithFormat:"

-- | @Selector@ for @localizedStringWithFormat:@
localizedStringWithFormatSelector :: Selector
localizedStringWithFormatSelector = mkSelector "localizedStringWithFormat:"

-- | @Selector@ for @stringWithValidatedFormat:validFormatSpecifiers:error:@
stringWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector
stringWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "stringWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @localizedStringWithValidatedFormat:validFormatSpecifiers:error:@
localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector :: Selector
localizedStringWithValidatedFormat_validFormatSpecifiers_errorSelector = mkSelector "localizedStringWithValidatedFormat:validFormatSpecifiers:error:"

-- | @Selector@ for @initWithCString:encoding:@
initWithCString_encodingSelector :: Selector
initWithCString_encodingSelector = mkSelector "initWithCString:encoding:"

-- | @Selector@ for @stringWithCString:encoding:@
stringWithCString_encodingSelector :: Selector
stringWithCString_encodingSelector = mkSelector "stringWithCString:encoding:"

-- | @Selector@ for @initWithContentsOfURL:encoding:error:@
initWithContentsOfURL_encoding_errorSelector :: Selector
initWithContentsOfURL_encoding_errorSelector = mkSelector "initWithContentsOfURL:encoding:error:"

-- | @Selector@ for @initWithContentsOfFile:encoding:error:@
initWithContentsOfFile_encoding_errorSelector :: Selector
initWithContentsOfFile_encoding_errorSelector = mkSelector "initWithContentsOfFile:encoding:error:"

-- | @Selector@ for @stringWithContentsOfURL:encoding:error:@
stringWithContentsOfURL_encoding_errorSelector :: Selector
stringWithContentsOfURL_encoding_errorSelector = mkSelector "stringWithContentsOfURL:encoding:error:"

-- | @Selector@ for @stringWithContentsOfFile:encoding:error:@
stringWithContentsOfFile_encoding_errorSelector :: Selector
stringWithContentsOfFile_encoding_errorSelector = mkSelector "stringWithContentsOfFile:encoding:error:"

-- | @Selector@ for @initWithContentsOfURL:usedEncoding:error:@
initWithContentsOfURL_usedEncoding_errorSelector :: Selector
initWithContentsOfURL_usedEncoding_errorSelector = mkSelector "initWithContentsOfURL:usedEncoding:error:"

-- | @Selector@ for @initWithContentsOfFile:usedEncoding:error:@
initWithContentsOfFile_usedEncoding_errorSelector :: Selector
initWithContentsOfFile_usedEncoding_errorSelector = mkSelector "initWithContentsOfFile:usedEncoding:error:"

-- | @Selector@ for @stringWithContentsOfURL:usedEncoding:error:@
stringWithContentsOfURL_usedEncoding_errorSelector :: Selector
stringWithContentsOfURL_usedEncoding_errorSelector = mkSelector "stringWithContentsOfURL:usedEncoding:error:"

-- | @Selector@ for @stringWithContentsOfFile:usedEncoding:error:@
stringWithContentsOfFile_usedEncoding_errorSelector :: Selector
stringWithContentsOfFile_usedEncoding_errorSelector = mkSelector "stringWithContentsOfFile:usedEncoding:error:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @stringByRemovingPercentEncoding@
stringByRemovingPercentEncodingSelector :: Selector
stringByRemovingPercentEncodingSelector = mkSelector "stringByRemovingPercentEncoding"

-- | @Selector@ for @pathComponents@
pathComponentsSelector :: Selector
pathComponentsSelector = mkSelector "pathComponents"

-- | @Selector@ for @absolutePath@
absolutePathSelector :: Selector
absolutePathSelector = mkSelector "absolutePath"

-- | @Selector@ for @lastPathComponent@
lastPathComponentSelector :: Selector
lastPathComponentSelector = mkSelector "lastPathComponent"

-- | @Selector@ for @stringByDeletingLastPathComponent@
stringByDeletingLastPathComponentSelector :: Selector
stringByDeletingLastPathComponentSelector = mkSelector "stringByDeletingLastPathComponent"

-- | @Selector@ for @pathExtension@
pathExtensionSelector :: Selector
pathExtensionSelector = mkSelector "pathExtension"

-- | @Selector@ for @stringByDeletingPathExtension@
stringByDeletingPathExtensionSelector :: Selector
stringByDeletingPathExtensionSelector = mkSelector "stringByDeletingPathExtension"

-- | @Selector@ for @stringByAbbreviatingWithTildeInPath@
stringByAbbreviatingWithTildeInPathSelector :: Selector
stringByAbbreviatingWithTildeInPathSelector = mkSelector "stringByAbbreviatingWithTildeInPath"

-- | @Selector@ for @stringByExpandingTildeInPath@
stringByExpandingTildeInPathSelector :: Selector
stringByExpandingTildeInPathSelector = mkSelector "stringByExpandingTildeInPath"

-- | @Selector@ for @stringByStandardizingPath@
stringByStandardizingPathSelector :: Selector
stringByStandardizingPathSelector = mkSelector "stringByStandardizingPath"

-- | @Selector@ for @stringByResolvingSymlinksInPath@
stringByResolvingSymlinksInPathSelector :: Selector
stringByResolvingSymlinksInPathSelector = mkSelector "stringByResolvingSymlinksInPath"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @intValue@
intValueSelector :: Selector
intValueSelector = mkSelector "intValue"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @longLongValue@
longLongValueSelector :: Selector
longLongValueSelector = mkSelector "longLongValue"

-- | @Selector@ for @boolValue@
boolValueSelector :: Selector
boolValueSelector = mkSelector "boolValue"

-- | @Selector@ for @uppercaseString@
uppercaseStringSelector :: Selector
uppercaseStringSelector = mkSelector "uppercaseString"

-- | @Selector@ for @lowercaseString@
lowercaseStringSelector :: Selector
lowercaseStringSelector = mkSelector "lowercaseString"

-- | @Selector@ for @capitalizedString@
capitalizedStringSelector :: Selector
capitalizedStringSelector = mkSelector "capitalizedString"

-- | @Selector@ for @localizedUppercaseString@
localizedUppercaseStringSelector :: Selector
localizedUppercaseStringSelector = mkSelector "localizedUppercaseString"

-- | @Selector@ for @localizedLowercaseString@
localizedLowercaseStringSelector :: Selector
localizedLowercaseStringSelector = mkSelector "localizedLowercaseString"

-- | @Selector@ for @localizedCapitalizedString@
localizedCapitalizedStringSelector :: Selector
localizedCapitalizedStringSelector = mkSelector "localizedCapitalizedString"

-- | @Selector@ for @fastestEncoding@
fastestEncodingSelector :: Selector
fastestEncodingSelector = mkSelector "fastestEncoding"

-- | @Selector@ for @smallestEncoding@
smallestEncodingSelector :: Selector
smallestEncodingSelector = mkSelector "smallestEncoding"

-- | @Selector@ for @defaultCStringEncoding@
defaultCStringEncodingSelector :: Selector
defaultCStringEncodingSelector = mkSelector "defaultCStringEncoding"

-- | @Selector@ for @decomposedStringWithCanonicalMapping@
decomposedStringWithCanonicalMappingSelector :: Selector
decomposedStringWithCanonicalMappingSelector = mkSelector "decomposedStringWithCanonicalMapping"

-- | @Selector@ for @precomposedStringWithCanonicalMapping@
precomposedStringWithCanonicalMappingSelector :: Selector
precomposedStringWithCanonicalMappingSelector = mkSelector "precomposedStringWithCanonicalMapping"

-- | @Selector@ for @decomposedStringWithCompatibilityMapping@
decomposedStringWithCompatibilityMappingSelector :: Selector
decomposedStringWithCompatibilityMappingSelector = mkSelector "decomposedStringWithCompatibilityMapping"

-- | @Selector@ for @precomposedStringWithCompatibilityMapping@
precomposedStringWithCompatibilityMappingSelector :: Selector
precomposedStringWithCompatibilityMappingSelector = mkSelector "precomposedStringWithCompatibilityMapping"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @hash@
hashSelector :: Selector
hashSelector = mkSelector "hash"

