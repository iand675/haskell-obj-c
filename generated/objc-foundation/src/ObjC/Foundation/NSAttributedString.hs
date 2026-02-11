{-# LANGUAGE PatternSynonyms #-}
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
  , attributesAtIndex_effectiveRangeSelector
  , attributedStringByInflectingStringSelector
  , initWithFormat_options_localeSelector
  , initWithFormat_options_locale_argumentsSelector
  , localizedAttributedStringWithFormatSelector
  , localizedAttributedStringWithFormat_optionsSelector
  , initWithFormat_options_locale_contextSelector
  , initWithFormat_options_locale_context_argumentsSelector
  , localizedAttributedStringWithFormat_contextSelector
  , localizedAttributedStringWithFormat_options_contextSelector
  , initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector
  , initWithMarkdown_options_baseURL_errorSelector
  , initWithMarkdownString_options_baseURL_errorSelector
  , attribute_atIndex_effectiveRangeSelector
  , attributedSubstringFromRangeSelector
  , attributesAtIndex_longestEffectiveRange_inRangeSelector
  , attribute_atIndex_longestEffectiveRange_inRangeSelector
  , isEqualToAttributedStringSelector
  , initWithStringSelector
  , initWithString_attributesSelector
  , initWithAttributedStringSelector
  , enumerateAttribute_inRange_options_usingBlockSelector
  , stringSelector
  , lengthSelector

  -- * Enum types
  , NSAttributedStringEnumerationOptions(NSAttributedStringEnumerationOptions)
  , pattern NSAttributedStringEnumerationReverse
  , pattern NSAttributedStringEnumerationLongestEffectiveRangeNotRequired
  , NSAttributedStringFormattingOptions(NSAttributedStringFormattingOptions)
  , pattern NSAttributedStringFormattingInsertArgumentAttributesWithoutMerging
  , pattern NSAttributedStringFormattingApplyReplacementIndexAttribute

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- attributesAtIndex:effectiveRange:@
attributesAtIndex_effectiveRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
attributesAtIndex_effectiveRange nsAttributedString  location range =
  sendMsg nsAttributedString (mkSelector "attributesAtIndex:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral location), argPtr range] >>= retainedObject . castPtr

-- | If the string has portions tagged with NSInflectionRuleAttributeName that have no format specifiers, create a new string with those portions inflected by following the rule in the attribute.
--
-- ObjC selector: @- attributedStringByInflectingString@
attributedStringByInflectingString :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO (Id NSAttributedString)
attributedStringByInflectingString nsAttributedString  =
  sendMsg nsAttributedString (mkSelector "attributedStringByInflectingString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Formats the string using the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:@
initWithFormat_options_locale :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> IO (Id NSAttributedString)
initWithFormat_options_locale nsAttributedString  format options locale =
withObjCPtr format $ \raw_format ->
  withObjCPtr locale $ \raw_locale ->
      sendMsg nsAttributedString (mkSelector "initWithFormat:options:locale:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_locale :: Ptr ())] >>= ownedObject . castPtr

-- | Formats the string using the arguments list and the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:arguments:@
initWithFormat_options_locale_arguments :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> RawId -> IO (Id NSAttributedString)
initWithFormat_options_locale_arguments nsAttributedString  format options locale arguments =
withObjCPtr format $ \raw_format ->
  withObjCPtr locale $ \raw_locale ->
      sendMsg nsAttributedString (mkSelector "initWithFormat:options:locale:arguments:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr (unRawId arguments) :: Ptr ())] >>= ownedObject . castPtr

-- | Formats the string using the current locale and default options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:@
localizedAttributedStringWithFormat :: IsNSAttributedString format => format -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat format =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr format $ \raw_format ->
      sendClassMsg cls' (mkSelector "localizedAttributedStringWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the string using the current locale and the specified options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:options:@
localizedAttributedStringWithFormat_options :: IsNSAttributedString format => format -> NSAttributedStringFormattingOptions -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_options format options =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr format $ \raw_format ->
      sendClassMsg cls' (mkSelector "localizedAttributedStringWithFormat:options:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | Formats the string using the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:context:@
initWithFormat_options_locale_context :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale, IsNSDictionary context) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> context -> IO (Id NSAttributedString)
initWithFormat_options_locale_context nsAttributedString  format options locale context =
withObjCPtr format $ \raw_format ->
  withObjCPtr locale $ \raw_locale ->
    withObjCPtr context $ \raw_context ->
        sendMsg nsAttributedString (mkSelector "initWithFormat:options:locale:context:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

-- | Formats the string using the arguments list and the specified locale (or the canonical one, if nil).
--
-- ObjC selector: @- initWithFormat:options:locale:context:arguments:@
initWithFormat_options_locale_context_arguments :: (IsNSAttributedString nsAttributedString, IsNSAttributedString format, IsNSLocale locale, IsNSDictionary context) => nsAttributedString -> format -> NSAttributedStringFormattingOptions -> locale -> context -> RawId -> IO (Id NSAttributedString)
initWithFormat_options_locale_context_arguments nsAttributedString  format options locale context arguments =
withObjCPtr format $ \raw_format ->
  withObjCPtr locale $ \raw_locale ->
    withObjCPtr context $ \raw_context ->
        sendMsg nsAttributedString (mkSelector "initWithFormat:options:locale:context:arguments:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr (unRawId arguments) :: Ptr ())] >>= ownedObject . castPtr

-- | Formats the string using the current locale and default options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:context:@
localizedAttributedStringWithFormat_context :: (IsNSAttributedString format, IsNSDictionary context) => format -> context -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_context format context =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "localizedAttributedStringWithFormat:context:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the string using the current locale and the specified options.
--
-- ObjC selector: @+ localizedAttributedStringWithFormat:options:context:@
localizedAttributedStringWithFormat_options_context :: (IsNSAttributedString format, IsNSDictionary context) => format -> NSAttributedStringFormattingOptions -> context -> IO (Id NSAttributedString)
localizedAttributedStringWithFormat_options_context format options context =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "localizedAttributedStringWithFormat:options:context:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfMarkdownFileAtURL:options:baseURL:error:@
initWithContentsOfMarkdownFileAtURL_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSURL markdownFile, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdownFile -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithContentsOfMarkdownFileAtURL_options_baseURL_error nsAttributedString  markdownFile options baseURL error_ =
withObjCPtr markdownFile $ \raw_markdownFile ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr baseURL $ \raw_baseURL ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsAttributedString (mkSelector "initWithContentsOfMarkdownFileAtURL:options:baseURL:error:") (retPtr retVoid) [argPtr (castPtr raw_markdownFile :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMarkdown:options:baseURL:error:@
initWithMarkdown_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSData markdown, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdown -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithMarkdown_options_baseURL_error nsAttributedString  markdown options baseURL error_ =
withObjCPtr markdown $ \raw_markdown ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr baseURL $ \raw_baseURL ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsAttributedString (mkSelector "initWithMarkdown:options:baseURL:error:") (retPtr retVoid) [argPtr (castPtr raw_markdown :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMarkdownString:options:baseURL:error:@
initWithMarkdownString_options_baseURL_error :: (IsNSAttributedString nsAttributedString, IsNSString markdownString, IsNSAttributedStringMarkdownParsingOptions options, IsNSURL baseURL, IsNSError error_) => nsAttributedString -> markdownString -> options -> baseURL -> error_ -> IO (Id NSAttributedString)
initWithMarkdownString_options_baseURL_error nsAttributedString  markdownString options baseURL error_ =
withObjCPtr markdownString $ \raw_markdownString ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr baseURL $ \raw_baseURL ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg nsAttributedString (mkSelector "initWithMarkdownString:options:baseURL:error:") (retPtr retVoid) [argPtr (castPtr raw_markdownString :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_baseURL :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- attribute:atIndex:effectiveRange:@
attribute_atIndex_effectiveRange :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> CULong -> Ptr NSRange -> IO RawId
attribute_atIndex_effectiveRange nsAttributedString  attrName location range =
withObjCPtr attrName $ \raw_attrName ->
    fmap (RawId . castPtr) $ sendMsg nsAttributedString (mkSelector "attribute:atIndex:effectiveRange:") (retPtr retVoid) [argPtr (castPtr raw_attrName :: Ptr ()), argCULong (fromIntegral location), argPtr range]

-- | @- attributedSubstringFromRange:@
attributedSubstringFromRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> NSRange -> IO (Id NSAttributedString)
attributedSubstringFromRange nsAttributedString  range =
  sendMsg nsAttributedString (mkSelector "attributedSubstringFromRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | @- attributesAtIndex:longestEffectiveRange:inRange:@
attributesAtIndex_longestEffectiveRange_inRange :: IsNSAttributedString nsAttributedString => nsAttributedString -> CULong -> Ptr NSRange -> NSRange -> IO (Id NSDictionary)
attributesAtIndex_longestEffectiveRange_inRange nsAttributedString  location range rangeLimit =
  sendMsg nsAttributedString (mkSelector "attributesAtIndex:longestEffectiveRange:inRange:") (retPtr retVoid) [argCULong (fromIntegral location), argPtr range, argNSRange rangeLimit] >>= retainedObject . castPtr

-- | @- attribute:atIndex:longestEffectiveRange:inRange:@
attribute_atIndex_longestEffectiveRange_inRange :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> CULong -> Ptr NSRange -> NSRange -> IO RawId
attribute_atIndex_longestEffectiveRange_inRange nsAttributedString  attrName location range rangeLimit =
withObjCPtr attrName $ \raw_attrName ->
    fmap (RawId . castPtr) $ sendMsg nsAttributedString (mkSelector "attribute:atIndex:longestEffectiveRange:inRange:") (retPtr retVoid) [argPtr (castPtr raw_attrName :: Ptr ()), argCULong (fromIntegral location), argPtr range, argNSRange rangeLimit]

-- | @- isEqualToAttributedString:@
isEqualToAttributedString :: (IsNSAttributedString nsAttributedString, IsNSAttributedString other) => nsAttributedString -> other -> IO Bool
isEqualToAttributedString nsAttributedString  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributedString (mkSelector "isEqualToAttributedString:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- initWithString:@
initWithString :: (IsNSAttributedString nsAttributedString, IsNSString str) => nsAttributedString -> str -> IO (Id NSAttributedString)
initWithString nsAttributedString  str =
withObjCPtr str $ \raw_str ->
    sendMsg nsAttributedString (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_str :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithString:attributes:@
initWithString_attributes :: (IsNSAttributedString nsAttributedString, IsNSString str, IsNSDictionary attrs) => nsAttributedString -> str -> attrs -> IO (Id NSAttributedString)
initWithString_attributes nsAttributedString  str attrs =
withObjCPtr str $ \raw_str ->
  withObjCPtr attrs $ \raw_attrs ->
      sendMsg nsAttributedString (mkSelector "initWithString:attributes:") (retPtr retVoid) [argPtr (castPtr raw_str :: Ptr ()), argPtr (castPtr raw_attrs :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSAttributedString nsAttributedString, IsNSAttributedString attrStr) => nsAttributedString -> attrStr -> IO (Id NSAttributedString)
initWithAttributedString nsAttributedString  attrStr =
withObjCPtr attrStr $ \raw_attrStr ->
    sendMsg nsAttributedString (mkSelector "initWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_attrStr :: Ptr ())] >>= ownedObject . castPtr

-- | @- enumerateAttribute:inRange:options:usingBlock:@
enumerateAttribute_inRange_options_usingBlock :: (IsNSAttributedString nsAttributedString, IsNSString attrName) => nsAttributedString -> attrName -> NSRange -> NSAttributedStringEnumerationOptions -> Ptr () -> IO ()
enumerateAttribute_inRange_options_usingBlock nsAttributedString  attrName enumerationRange opts block =
withObjCPtr attrName $ \raw_attrName ->
    sendMsg nsAttributedString (mkSelector "enumerateAttribute:inRange:options:usingBlock:") retVoid [argPtr (castPtr raw_attrName :: Ptr ()), argNSRange enumerationRange, argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- string@
string :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO (Id NSString)
string nsAttributedString  =
  sendMsg nsAttributedString (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsNSAttributedString nsAttributedString => nsAttributedString -> IO CULong
length_ nsAttributedString  =
  sendMsg nsAttributedString (mkSelector "length") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributesAtIndex:effectiveRange:@
attributesAtIndex_effectiveRangeSelector :: Selector
attributesAtIndex_effectiveRangeSelector = mkSelector "attributesAtIndex:effectiveRange:"

-- | @Selector@ for @attributedStringByInflectingString@
attributedStringByInflectingStringSelector :: Selector
attributedStringByInflectingStringSelector = mkSelector "attributedStringByInflectingString"

-- | @Selector@ for @initWithFormat:options:locale:@
initWithFormat_options_localeSelector :: Selector
initWithFormat_options_localeSelector = mkSelector "initWithFormat:options:locale:"

-- | @Selector@ for @initWithFormat:options:locale:arguments:@
initWithFormat_options_locale_argumentsSelector :: Selector
initWithFormat_options_locale_argumentsSelector = mkSelector "initWithFormat:options:locale:arguments:"

-- | @Selector@ for @localizedAttributedStringWithFormat:@
localizedAttributedStringWithFormatSelector :: Selector
localizedAttributedStringWithFormatSelector = mkSelector "localizedAttributedStringWithFormat:"

-- | @Selector@ for @localizedAttributedStringWithFormat:options:@
localizedAttributedStringWithFormat_optionsSelector :: Selector
localizedAttributedStringWithFormat_optionsSelector = mkSelector "localizedAttributedStringWithFormat:options:"

-- | @Selector@ for @initWithFormat:options:locale:context:@
initWithFormat_options_locale_contextSelector :: Selector
initWithFormat_options_locale_contextSelector = mkSelector "initWithFormat:options:locale:context:"

-- | @Selector@ for @initWithFormat:options:locale:context:arguments:@
initWithFormat_options_locale_context_argumentsSelector :: Selector
initWithFormat_options_locale_context_argumentsSelector = mkSelector "initWithFormat:options:locale:context:arguments:"

-- | @Selector@ for @localizedAttributedStringWithFormat:context:@
localizedAttributedStringWithFormat_contextSelector :: Selector
localizedAttributedStringWithFormat_contextSelector = mkSelector "localizedAttributedStringWithFormat:context:"

-- | @Selector@ for @localizedAttributedStringWithFormat:options:context:@
localizedAttributedStringWithFormat_options_contextSelector :: Selector
localizedAttributedStringWithFormat_options_contextSelector = mkSelector "localizedAttributedStringWithFormat:options:context:"

-- | @Selector@ for @initWithContentsOfMarkdownFileAtURL:options:baseURL:error:@
initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector :: Selector
initWithContentsOfMarkdownFileAtURL_options_baseURL_errorSelector = mkSelector "initWithContentsOfMarkdownFileAtURL:options:baseURL:error:"

-- | @Selector@ for @initWithMarkdown:options:baseURL:error:@
initWithMarkdown_options_baseURL_errorSelector :: Selector
initWithMarkdown_options_baseURL_errorSelector = mkSelector "initWithMarkdown:options:baseURL:error:"

-- | @Selector@ for @initWithMarkdownString:options:baseURL:error:@
initWithMarkdownString_options_baseURL_errorSelector :: Selector
initWithMarkdownString_options_baseURL_errorSelector = mkSelector "initWithMarkdownString:options:baseURL:error:"

-- | @Selector@ for @attribute:atIndex:effectiveRange:@
attribute_atIndex_effectiveRangeSelector :: Selector
attribute_atIndex_effectiveRangeSelector = mkSelector "attribute:atIndex:effectiveRange:"

-- | @Selector@ for @attributedSubstringFromRange:@
attributedSubstringFromRangeSelector :: Selector
attributedSubstringFromRangeSelector = mkSelector "attributedSubstringFromRange:"

-- | @Selector@ for @attributesAtIndex:longestEffectiveRange:inRange:@
attributesAtIndex_longestEffectiveRange_inRangeSelector :: Selector
attributesAtIndex_longestEffectiveRange_inRangeSelector = mkSelector "attributesAtIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @attribute:atIndex:longestEffectiveRange:inRange:@
attribute_atIndex_longestEffectiveRange_inRangeSelector :: Selector
attribute_atIndex_longestEffectiveRange_inRangeSelector = mkSelector "attribute:atIndex:longestEffectiveRange:inRange:"

-- | @Selector@ for @isEqualToAttributedString:@
isEqualToAttributedStringSelector :: Selector
isEqualToAttributedStringSelector = mkSelector "isEqualToAttributedString:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @initWithString:attributes:@
initWithString_attributesSelector :: Selector
initWithString_attributesSelector = mkSelector "initWithString:attributes:"

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @enumerateAttribute:inRange:options:usingBlock:@
enumerateAttribute_inRange_options_usingBlockSelector :: Selector
enumerateAttribute_inRange_options_usingBlockSelector = mkSelector "enumerateAttribute:inRange:options:usingBlock:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

