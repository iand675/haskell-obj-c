{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRegularExpression@.
module ObjC.Foundation.NSRegularExpression
  ( NSRegularExpression
  , IsNSRegularExpression(..)
  , regularExpressionWithPattern_options_error
  , initWithPattern_options_error
  , escapedPatternForString
  , stringByReplacingMatchesInString_options_range_withTemplate
  , replaceMatchesInString_options_range_withTemplate
  , replacementStringForResult_inString_offset_template
  , escapedTemplateForString
  , enumerateMatchesInString_options_range_usingBlock
  , matchesInString_options_range
  , numberOfMatchesInString_options_range
  , firstMatchInString_options_range
  , rangeOfFirstMatchInString_options_range
  , pattern_
  , options
  , numberOfCaptureGroups
  , enumerateMatchesInString_options_range_usingBlockSelector
  , escapedPatternForStringSelector
  , escapedTemplateForStringSelector
  , firstMatchInString_options_rangeSelector
  , initWithPattern_options_errorSelector
  , matchesInString_options_rangeSelector
  , numberOfCaptureGroupsSelector
  , numberOfMatchesInString_options_rangeSelector
  , optionsSelector
  , patternSelector
  , rangeOfFirstMatchInString_options_rangeSelector
  , regularExpressionWithPattern_options_errorSelector
  , replaceMatchesInString_options_range_withTemplateSelector
  , replacementStringForResult_inString_offset_templateSelector
  , stringByReplacingMatchesInString_options_range_withTemplateSelector

  -- * Enum types
  , NSMatchingOptions(NSMatchingOptions)
  , pattern NSMatchingReportProgress
  , pattern NSMatchingReportCompletion
  , pattern NSMatchingAnchored
  , pattern NSMatchingWithTransparentBounds
  , pattern NSMatchingWithoutAnchoringBounds
  , NSRegularExpressionOptions(NSRegularExpressionOptions)
  , pattern NSRegularExpressionCaseInsensitive
  , pattern NSRegularExpressionAllowCommentsAndWhitespace
  , pattern NSRegularExpressionIgnoreMetacharacters
  , pattern NSRegularExpressionDotMatchesLineSeparators
  , pattern NSRegularExpressionAnchorsMatchLines
  , pattern NSRegularExpressionUseUnixLineSeparators
  , pattern NSRegularExpressionUseUnicodeWordBoundaries

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

-- | @+ regularExpressionWithPattern:options:error:@
regularExpressionWithPattern_options_error :: (IsNSString pattern_, IsNSError error_) => pattern_ -> NSRegularExpressionOptions -> error_ -> IO (Id NSRegularExpression)
regularExpressionWithPattern_options_error pattern_ options error_ =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    sendClassMessage cls' regularExpressionWithPattern_options_errorSelector (toNSString pattern_) options (toNSError error_)

-- | @- initWithPattern:options:error:@
initWithPattern_options_error :: (IsNSRegularExpression nsRegularExpression, IsNSString pattern_, IsNSError error_) => nsRegularExpression -> pattern_ -> NSRegularExpressionOptions -> error_ -> IO (Id NSRegularExpression)
initWithPattern_options_error nsRegularExpression pattern_ options error_ =
  sendOwnedMessage nsRegularExpression initWithPattern_options_errorSelector (toNSString pattern_) options (toNSError error_)

-- | @+ escapedPatternForString:@
escapedPatternForString :: IsNSString string => string -> IO (Id NSString)
escapedPatternForString string =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    sendClassMessage cls' escapedPatternForStringSelector (toNSString string)

-- | @- stringByReplacingMatchesInString:options:range:withTemplate:@
stringByReplacingMatchesInString_options_range_withTemplate :: (IsNSRegularExpression nsRegularExpression, IsNSString string, IsNSString templ) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> templ -> IO (Id NSString)
stringByReplacingMatchesInString_options_range_withTemplate nsRegularExpression string options range templ =
  sendMessage nsRegularExpression stringByReplacingMatchesInString_options_range_withTemplateSelector (toNSString string) options range (toNSString templ)

-- | @- replaceMatchesInString:options:range:withTemplate:@
replaceMatchesInString_options_range_withTemplate :: (IsNSRegularExpression nsRegularExpression, IsNSMutableString string, IsNSString templ) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> templ -> IO CULong
replaceMatchesInString_options_range_withTemplate nsRegularExpression string options range templ =
  sendMessage nsRegularExpression replaceMatchesInString_options_range_withTemplateSelector (toNSMutableString string) options range (toNSString templ)

-- | @- replacementStringForResult:inString:offset:template:@
replacementStringForResult_inString_offset_template :: (IsNSRegularExpression nsRegularExpression, IsNSTextCheckingResult result, IsNSString string, IsNSString templ) => nsRegularExpression -> result -> string -> CLong -> templ -> IO (Id NSString)
replacementStringForResult_inString_offset_template nsRegularExpression result string offset templ =
  sendMessage nsRegularExpression replacementStringForResult_inString_offset_templateSelector (toNSTextCheckingResult result) (toNSString string) offset (toNSString templ)

-- | @+ escapedTemplateForString:@
escapedTemplateForString :: IsNSString string => string -> IO (Id NSString)
escapedTemplateForString string =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    sendClassMessage cls' escapedTemplateForStringSelector (toNSString string)

-- | @- enumerateMatchesInString:options:range:usingBlock:@
enumerateMatchesInString_options_range_usingBlock :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> Ptr () -> IO ()
enumerateMatchesInString_options_range_usingBlock nsRegularExpression string options range block =
  sendMessage nsRegularExpression enumerateMatchesInString_options_range_usingBlockSelector (toNSString string) options range block

-- | @- matchesInString:options:range:@
matchesInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO (Id NSArray)
matchesInString_options_range nsRegularExpression string options range =
  sendMessage nsRegularExpression matchesInString_options_rangeSelector (toNSString string) options range

-- | @- numberOfMatchesInString:options:range:@
numberOfMatchesInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO CULong
numberOfMatchesInString_options_range nsRegularExpression string options range =
  sendMessage nsRegularExpression numberOfMatchesInString_options_rangeSelector (toNSString string) options range

-- | @- firstMatchInString:options:range:@
firstMatchInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO (Id NSTextCheckingResult)
firstMatchInString_options_range nsRegularExpression string options range =
  sendMessage nsRegularExpression firstMatchInString_options_rangeSelector (toNSString string) options range

-- | @- rangeOfFirstMatchInString:options:range:@
rangeOfFirstMatchInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO NSRange
rangeOfFirstMatchInString_options_range nsRegularExpression string options range =
  sendMessage nsRegularExpression rangeOfFirstMatchInString_options_rangeSelector (toNSString string) options range

-- | @- pattern@
pattern_ :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO (Id NSString)
pattern_ nsRegularExpression =
  sendMessage nsRegularExpression patternSelector

-- | @- options@
options :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO NSRegularExpressionOptions
options nsRegularExpression =
  sendMessage nsRegularExpression optionsSelector

-- | @- numberOfCaptureGroups@
numberOfCaptureGroups :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO CULong
numberOfCaptureGroups nsRegularExpression =
  sendMessage nsRegularExpression numberOfCaptureGroupsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @regularExpressionWithPattern:options:error:@
regularExpressionWithPattern_options_errorSelector :: Selector '[Id NSString, NSRegularExpressionOptions, Id NSError] (Id NSRegularExpression)
regularExpressionWithPattern_options_errorSelector = mkSelector "regularExpressionWithPattern:options:error:"

-- | @Selector@ for @initWithPattern:options:error:@
initWithPattern_options_errorSelector :: Selector '[Id NSString, NSRegularExpressionOptions, Id NSError] (Id NSRegularExpression)
initWithPattern_options_errorSelector = mkSelector "initWithPattern:options:error:"

-- | @Selector@ for @escapedPatternForString:@
escapedPatternForStringSelector :: Selector '[Id NSString] (Id NSString)
escapedPatternForStringSelector = mkSelector "escapedPatternForString:"

-- | @Selector@ for @stringByReplacingMatchesInString:options:range:withTemplate:@
stringByReplacingMatchesInString_options_range_withTemplateSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange, Id NSString] (Id NSString)
stringByReplacingMatchesInString_options_range_withTemplateSelector = mkSelector "stringByReplacingMatchesInString:options:range:withTemplate:"

-- | @Selector@ for @replaceMatchesInString:options:range:withTemplate:@
replaceMatchesInString_options_range_withTemplateSelector :: Selector '[Id NSMutableString, NSMatchingOptions, NSRange, Id NSString] CULong
replaceMatchesInString_options_range_withTemplateSelector = mkSelector "replaceMatchesInString:options:range:withTemplate:"

-- | @Selector@ for @replacementStringForResult:inString:offset:template:@
replacementStringForResult_inString_offset_templateSelector :: Selector '[Id NSTextCheckingResult, Id NSString, CLong, Id NSString] (Id NSString)
replacementStringForResult_inString_offset_templateSelector = mkSelector "replacementStringForResult:inString:offset:template:"

-- | @Selector@ for @escapedTemplateForString:@
escapedTemplateForStringSelector :: Selector '[Id NSString] (Id NSString)
escapedTemplateForStringSelector = mkSelector "escapedTemplateForString:"

-- | @Selector@ for @enumerateMatchesInString:options:range:usingBlock:@
enumerateMatchesInString_options_range_usingBlockSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange, Ptr ()] ()
enumerateMatchesInString_options_range_usingBlockSelector = mkSelector "enumerateMatchesInString:options:range:usingBlock:"

-- | @Selector@ for @matchesInString:options:range:@
matchesInString_options_rangeSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange] (Id NSArray)
matchesInString_options_rangeSelector = mkSelector "matchesInString:options:range:"

-- | @Selector@ for @numberOfMatchesInString:options:range:@
numberOfMatchesInString_options_rangeSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange] CULong
numberOfMatchesInString_options_rangeSelector = mkSelector "numberOfMatchesInString:options:range:"

-- | @Selector@ for @firstMatchInString:options:range:@
firstMatchInString_options_rangeSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange] (Id NSTextCheckingResult)
firstMatchInString_options_rangeSelector = mkSelector "firstMatchInString:options:range:"

-- | @Selector@ for @rangeOfFirstMatchInString:options:range:@
rangeOfFirstMatchInString_options_rangeSelector :: Selector '[Id NSString, NSMatchingOptions, NSRange] NSRange
rangeOfFirstMatchInString_options_rangeSelector = mkSelector "rangeOfFirstMatchInString:options:range:"

-- | @Selector@ for @pattern@
patternSelector :: Selector '[] (Id NSString)
patternSelector = mkSelector "pattern"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] NSRegularExpressionOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @numberOfCaptureGroups@
numberOfCaptureGroupsSelector :: Selector '[] CULong
numberOfCaptureGroupsSelector = mkSelector "numberOfCaptureGroups"

