{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedString@.
module ObjC.Foundation.NSAttributedString
  ( NSAttributedString
  , IsNSAttributedString(..)
  , attributesAtIndex_effectiveRange
  , attributedStringByInflectingString
  , initWithFormat_options_locale
  , initWithFormat_options_locale_arguments
  , localizedAttributedStringWithFormat
  , localizedAttributedStringWithFormat_options
  , initWithFormat_options_locale_context
  , initWithFormat_options_locale_context_arguments
  , localizedAttributedStringWithFormat_context
  , localizedAttributedStringWithFormat_options_context
  , initWithContentsOfMarkdownFileAtURL_options_baseURL_error
  , initWithMarkdown_options_baseURL_error
  , initWithMarkdownString_options_baseURL_error
  , attribute_atIndex_effectiveRange
  , attributedSubstringFromRange
  , attributesAtIndex_longestEffectiveRange_inRange
  , attribute_atIndex_longestEffectiveRange_inRange
  , isEqualToAttributedString
  , initWithString
  , initWithString_attributes
  , initWithAttributedString
  , enumerateAttribute_inRange_options_usingBlock
  , string
  , length_
  , attribute_atIndex_effectiveRangeSelector
  , attribute_atIndex_longestEffectiveRange_inRangeSelector
  , attributedStringByInflectingStringSelector
  , attributedSubstringFromRangeSelector
  , attributesAtIndex_effectiveRangeSelector
  , attributesAtIndex_longestEffectiveRange_inRangeSelector
  , enumerateAttribute_inRange_options_usingBlockSelector
  , initWithAttributedStringSelector
  , initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector
  , initWithFormat_options_localeSelector
  , initWithFormat_options_locale_argumentsSelector
  , initWithFormat_options_locale_contextSelector
  , initWithFormat_options_locale_context_argumentsSelector
  , initWithMarkdownString_options_baseURL_errorSelector
  , initWithMarkdown_options_baseURL_errorSelector
  , initWithStringSelector
  , initWithString_attributesSelector
  , isEqualToAttributedStringSelector
  , lengthSelector
  , localizedAttributedStringWithFormatSelector
  , localizedAttributedStringWithFormat_contextSelector
  , localizedAttributedStringWithFormat_optionsSelector
  , localizedAttributedStringWithFormat_options_contextSelector
  , stringSelector

  -- * Enum types
  , NSAttributedStringEnumerationOptions(NSAttributedStringEnumerationOptions)
  , pattern NSAttributedStringEnumerationReverse
  , pattern NSAttributedStringEnumerationLongestEffectiveRangeNotRequired
  , NSAttributedStringFormattingOptions(NSAttributedStringFormattingOptions)
  , pattern NSAttributedStringFormattingInsertArgumentAttributesWithoutMerging
  , pattern NSAttributedStringFormattingApplyReplacementIndexAttribute

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

-- | @- attributesAtIndex:effectiveRange:@
attributesAtIndex_effectiveRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
attributesAtIndex_effectiveRange nsAttributedString location range =
  sendMessage nsAttributedString attributesAtIndex_effectiveRangeSelector location range

-- | If the string has portions tagged with NSInflectionRuleAttributeName that have no format specifiers, create a new string with those portions inflected by following the rule in the attribute.
--
-- ObjC selector: @- attributedStringByInflectingString@
attributedStringByInflectingString :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO (Id NSAttributedString)
attributedStringByInflectingString nsAttributedString =
  sendMessage nsAttributedString attributedStringByInflectingStringSelector

-- | Formats the string using the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:@
initWithFormat_options_locale :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> IO (Id NSAttributedString)
initWithFormat_options_locale nsAttributedString format options locale =
  sendOwnedMessage nsAttributedString initWithFormat_options_localeSelector (toNSAttributedString format) options (toNSLocale locale)

-- | Formats the string using the arguments list and the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:arguments:@
initWithFormat_options_locale_arguments :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> RawId -> IO (Id NSAttributedString)
initWithFormat_options_locale_arguments nsAttributedString format options locale arguments =
  sendOwnedMessage nsAttributedString initWithFormat_options_locale_argumentsSelector (toNSAttributedString format) options (toNSLocale locale) arguments

-- | Formats the string using the current locale and default options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:@
localizedAttributedStringWithFormat :: IsNSAttributedString format => format -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat format =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMessage cls' localizedAttributedStringWithFormatSelector (toNSAttributedString format)

-- | Formats the string using the current locale and the specified options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:options:@
localizedAttributedStringWithFormat_options :: IsNSAttributedString format => format -> NSAttributedStringFormattingOptions -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_options format options =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMessage cls' localizedAttributedStringWithFormat_optionsSelector (toNSAttributedString format) options

-- | Formats the string using the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:context:@
initWithFormat_options_locale_context :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale, IsNSDictionary context) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> context -> IO (Id NSAttributedString)
initWithFormat_options_locale_context nsAttributedString format options locale context =
  sendOwnedMessage nsAttributedString initWithFormat_options_locale_contextSelector (toNSAttributedString format) options (toNSLocale locale) (toNSDictionary context)

-- | Formats the string using the arguments list and the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:context:arguments:@
initWithFormat_options_locale_context_arguments :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale, IsNSDictionary context) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> context -> RawId -> IO (Id NSAttributedString)
initWithFormat_options_locale_context_arguments nsAttributedString format options locale context arguments =
  sendOwnedMessage nsAttributedString initWithFormat_options_locale_context_argumentsSelector (toNSAttributedString format) options (toNSLocale locale) (toNSDictionary context) arguments

-- | Formats the string using the current locale and default options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:context:@
localizedAttributedStringWithFormat_context :: (IsNSAttributedString format, IsNSDictionary context) => format -> context -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_context format context =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMessage cls' localizedAttributedStringWithFormat_contextSelector (toNSAttributedString format) (toNSDictionary context)

-- | Formats the string using the current locale and the specified options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:options:context:@
localizedAttributedStringWithFormat_options_context :: (IsNSAttributedString format, IsNSDictionary context) => format -> NSAttributedStringFormattingOptions -> context -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_options_context format options context =
  do
    cls' <- getRequiredClass "NSAttributedString"
    sendClassMessage cls' localizedAttributedStringWithFormat_options_contextSelector (toNSAttributedString format) options (toNSDictionary context)

-- | @- initWithContentsOfMarkdownFileAtURL:options:baseURL:error:@
initWithContentsOfMarkdownFileAtURL_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSURL markdownFile, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdownFile -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithContentsOfMarkdownFileAtURL_options_baseURL_error nsAttributedString markdownFile options baseURL error_ =
  sendOwnedMessage nsAttributedString initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector (toNSURL markdownFile) (toNSAttributedStringMarkdownParsingOptions options) (toNSURL baseURL) (toNSError error_)

-- | @- initWithMarkdown:options:baseURL:error:@
initWithMarkdown_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSData markdown, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdown -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithMarkdown_options_baseURL_error nsAttributedString markdown options baseURL error_ =
  sendOwnedMessage nsAttributedString initWithMarkdown_options_baseURL_errorSelector (toNSData markdown) (toNSAttributedStringMarkdownParsingOptions options) (toNSURL baseURL) (toNSError error_)

-- | @- initWithMarkdownString:options:baseURL:error:@
initWithMarkdownString_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSString markdownString, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdownString -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithMarkdownString_options_baseURL_error nsAttributedString markdownString options baseURL error_ =
  sendOwnedMessage nsAttributedString initWithMarkdownString_options_baseURL_errorSelector (toNSString markdownString) (toNSAttributedStringMarkdownParsingOptions options) (toNSURL baseURL) (toNSError error_)

-- | @- attribute:atIndex:effectiveRange:@
attribute_atIndex_effectiveRange :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> CULong -> Ptr NSRange -> IO RawId
attribute_atIndex_effectiveRange nsAttributedString attrName location range =
  sendMessage nsAttributedString attribute_atIndex_effectiveRangeSelector (toNSString attrName) location range

-- | @- attributedSubstringFromRange:@
attributedSubstringFromRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO (Id NSAttributedString)
attributedSubstringFromRange nsAttributedString range =
  sendMessage nsAttributedString attributedSubstringFromRangeSelector range

-- | @- attributesAtIndex:longestEffectiveRange:inRange:@
attributesAtIndex_longestEffectiveRange_inRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Ptr NSRange -> NSRange -> IO (Id NSDictionary)
attributesAtIndex_longestEffectiveRange_inRange nsAttributedString location range rangeLimit =
  sendMessage nsAttributedString attributesAtIndex_longestEffectiveRange_inRangeSelector location range rangeLimit

-- | @- attribute:atIndex:longestEffectiveRange:inRange:@
attribute_atIndex_longestEffectiveRange_inRange :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> CULong -> Ptr NSRange -> NSRange -> IO RawId
attribute_atIndex_longestEffectiveRange_inRange nsAttributedString attrName location range rangeLimit =
  sendMessage nsAttributedString attribute_atIndex_longestEffectiveRange_inRangeSelector (toNSString attrName) location range rangeLimit

-- | @- isEqualToAttributedString:@
isEqualToAttributedString :: (IsNSAttributedString nsAttributedString, IsNSAttributedString other) => nsAttributedString -> other -> IO Bool
isEqualToAttributedString nsAttributedString other =
  sendMessage nsAttributedString isEqualToAttributedStringSelector (toNSAttributedString other)

-- | @- initWithString:@
initWithString :: (IsNSAttributedString nsAttributedString, IsNSString str) => nsAttributedString -> str -> IO (Id NSAttributedString)
initWithString nsAttributedString str =
  sendOwnedMessage nsAttributedString initWithStringSelector (toNSString str)

-- | @- initWithString:attributes:@
initWithString_attributes :: (IsNSAttributedString nsAttributedString, IsNSString str, IsNSDictionary attrs) => nsAttributedString -> str -> attrs -> IO (Id NSAttributedString)
initWithString_attributes nsAttributedString str attrs =
  sendOwnedMessage nsAttributedString initWithString_attributesSelector (toNSString str) (toNSDictionary attrs)

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSAttributedString nsAttributedString, IsNSAttributedString attrStr) => nsAttributedString -> attrStr -> IO (Id NSAttributedString)
initWithAttributedString nsAttributedString attrStr =
  sendOwnedMessage nsAttributedString initWithAttributedStringSelector (toNSAttributedString attrStr)

-- | @- enumerateAttribute:inRange:options:usingBlock:@
enumerateAttribute_inRange_options_usingBlock :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> NSRange -> NSAttributedStringEnumerationOptions -> Ptr () -> IO ()
enumerateAttribute_inRange_options_usingBlock nsAttributedString attrName enumerationRange opts block =
  sendMessage nsAttributedString enumerateAttribute_inRange_options_usingBlockSelector (toNSString attrName) enumerationRange opts block

-- | @- string@
string :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO (Id NSString)
string nsAttributedString =
  sendMessage nsAttributedString stringSelector

-- | @- length@
length_ :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO CULong
length_ nsAttributedString =
  sendMessage nsAttributedString lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributesAtIndex:effectiveRange:@
attributesAtIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] (Id NSDictionary)
attributesAtIndex_effectiveRangeSelector = mkSelector "attributesAtIndex:effectiveRange:"

-- | @Selector@ for @attributedStringByInflectingString@
attributedStringByInflectingStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringByInflectingStringSelector = mkSelector "attributedStringByInflectingString"

-- | @Selector@ for @initWithFormat:options:locale:@
initWithFormat_options_localeSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions, Id NSLocale] (Id NSAttributedString)
initWithFormat_options_localeSelector = mkSelector "initWithFormat:options:locale:"

-- | @Selector@ for @initWithFormat:options:locale:arguments:@
initWithFormat_options_locale_argumentsSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions, Id NSLocale, RawId] (Id NSAttributedString)
initWithFormat_options_locale_argumentsSelector = mkSelector "initWithFormat:options:locale:arguments:"

-- | @Selector@ for @localizedAttributedStringWithFormat:@
localizedAttributedStringWithFormatSelector :: Selector '[Id NSAttributedString] (Id NSAttributedString)
localizedAttributedStringWithFormatSelector = mkSelector "localizedAttributedStringWithFormat:"

-- | @Selector@ for @localizedAttributedStringWithFormat:options:@
localizedAttributedStringWithFormat_optionsSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions] (Id NSAttributedString)
localizedAttributedStringWithFormat_optionsSelector = mkSelector "localizedAttributedStringWithFormat:options:"

-- | @Selector@ for @initWithFormat:options:locale:context:@
initWithFormat_options_locale_contextSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions, Id NSLocale, Id NSDictionary] (Id NSAttributedString)
initWithFormat_options_locale_contextSelector = mkSelector "initWithFormat:options:locale:context:"

-- | @Selector@ for @initWithFormat:options:locale:context:arguments:@
initWithFormat_options_locale_context_argumentsSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions, Id NSLocale, Id NSDictionary, RawId] (Id NSAttributedString)
initWithFormat_options_locale_context_argumentsSelector = mkSelector "initWithFormat:options:locale:context:arguments:"

-- | @Selector@ for @localizedAttributedStringWithFormat:context:@
localizedAttributedStringWithFormat_contextSelector :: Selector '[Id NSAttributedString, Id NSDictionary] (Id NSAttributedString)
localizedAttributedStringWithFormat_contextSelector = mkSelector "localizedAttributedStringWithFormat:context:"

-- | @Selector@ for @localizedAttributedStringWithFormat:options:context:@
localizedAttributedStringWithFormat_options_contextSelector :: Selector '[Id NSAttributedString, NSAttributedStringFormattingOptions, Id NSDictionary] (Id NSAttributedString)
localizedAttributedStringWithFormat_options_contextSelector = mkSelector "localizedAttributedStringWithFormat:options:context:"

-- | @Selector@ for @initWithContentsOfMarkdownFileAtURL:options:baseURL:error:@
initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector :: Selector '[Id NSURL, Id NSAttributedStringMarkdownParsingOptions, Id NSURL, Id NSError] (Id NSAttributedString)
initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector = mkSelector "initWithContentsOfMarkdownFileAtURL:options:baseURL:error:"

-- | @Selector@ for @initWithMarkdown:options:baseURL:error:@
initWithMarkdown_options_baseURL_errorSelector :: Selector '[Id NSData, Id NSAttributedStringMarkdownParsingOptions, Id NSURL, Id NSError] (Id NSAttributedString)
initWithMarkdown_options_baseURL_errorSelector = mkSelector "initWithMarkdown:options:baseURL:error:"

-- | @Selector@ for @initWithMarkdownString:options:baseURL:error:@
initWithMarkdownString_options_baseURL_errorSelector :: Selector '[Id NSString, Id NSAttributedStringMarkdownParsingOptions, Id NSURL, Id NSError] (Id NSAttributedString)
initWithMarkdownString_options_baseURL_errorSelector = mkSelector "initWithMarkdownString:options:baseURL:error:"

-- | @Selector@ for @attribute:atIndex:effectiveRange:@
attribute_atIndex_effectiveRangeSelector :: Selector '[Id NSString, CULong, Ptr NSRange] RawId
attribute_atIndex_effectiveRangeSelector = mkSelector "attribute:atIndex:effectiveRange:"

-- | @Selector@ for @attributedSubstringFromRange:@
attributedSubstringFromRangeSelector :: Selector '[NSRange] (Id NSAttributedString)
attributedSubstringFromRangeSelector = mkSelector "attributedSubstringFromRange:"

-- | @Selector@ for @attributesAtIndex:longestEffectiveRange:inRange:@
attributesAtIndex_longestEffectiveRange_inRangeSelector :: Selector '[CULong, Ptr NSRange, NSRange] (Id NSDictionary)
attributesAtIndex_longestEffectiveRange_inRangeSelector = mkSelector "attributesAtIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @attribute:atIndex:longestEffectiveRange:inRange:@
attribute_atIndex_longestEffectiveRange_inRangeSelector :: Selector '[Id NSString, CULong, Ptr NSRange, NSRange] RawId
attribute_atIndex_longestEffectiveRange_inRangeSelector = mkSelector "attribute:atIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @isEqualToAttributedString:@
isEqualToAttributedStringSelector :: Selector '[Id NSAttributedString] Bool
isEqualToAttributedStringSelector = mkSelector "isEqualToAttributedString:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSAttributedString)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:attributes:@
initWithString_attributesSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSAttributedString)
initWithString_attributesSelector = mkSelector "initWithString:attributes:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector '[Id NSAttributedString] (Id NSAttributedString)
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @enumerateAttribute:inRange:options:usingBlock:@
enumerateAttribute_inRange_options_usingBlockSelector :: Selector '[Id NSString, NSRange, NSAttributedStringEnumerationOptions, Ptr ()] ()
enumerateAttribute_inRange_options_usingBlockSelector = mkSelector "enumerateAttribute:inRange:options:usingBlock:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

