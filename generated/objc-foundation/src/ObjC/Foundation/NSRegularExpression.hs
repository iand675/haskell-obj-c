{-# LANGUAGE PatternSynonyms #-}
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
  , regularExpressionWithPattern_options_errorSelector
  , initWithPattern_options_errorSelector
  , escapedPatternForStringSelector
  , stringByReplacingMatchesInString_options_range_withTemplateSelector
  , replaceMatchesInString_options_range_withTemplateSelector
  , replacementStringForResult_inString_offset_templateSelector
  , escapedTemplateForStringSelector
  , enumerateMatchesInString_options_range_usingBlockSelector
  , matchesInString_options_rangeSelector
  , numberOfMatchesInString_options_rangeSelector
  , firstMatchInString_options_rangeSelector
  , rangeOfFirstMatchInString_options_rangeSelector
  , patternSelector
  , optionsSelector
  , numberOfCaptureGroupsSelector

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

-- | @+ regularExpressionWithPattern:options:error:@
regularExpressionWithPattern_options_error :: (IsNSString pattern_, IsNSError error_) => pattern_ -> NSRegularExpressionOptions -> error_ -> IO (Id NSRegularExpression)
regularExpressionWithPattern_options_error pattern_ options error_ =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    withObjCPtr pattern_ $ \raw_pattern_ ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "regularExpressionWithPattern:options:error:") (retPtr retVoid) [argPtr (castPtr raw_pattern_ :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithPattern:options:error:@
initWithPattern_options_error :: (IsNSRegularExpression nsRegularExpression, IsNSString pattern_, IsNSError error_) => nsRegularExpression -> pattern_ -> NSRegularExpressionOptions -> error_ -> IO (Id NSRegularExpression)
initWithPattern_options_error nsRegularExpression  pattern_ options error_ =
withObjCPtr pattern_ $ \raw_pattern_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsRegularExpression (mkSelector "initWithPattern:options:error:") (retPtr retVoid) [argPtr (castPtr raw_pattern_ :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ escapedPatternForString:@
escapedPatternForString :: IsNSString string => string -> IO (Id NSString)
escapedPatternForString string =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "escapedPatternForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- stringByReplacingMatchesInString:options:range:withTemplate:@
stringByReplacingMatchesInString_options_range_withTemplate :: (IsNSRegularExpression nsRegularExpression, IsNSString string, IsNSString templ) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> templ -> IO (Id NSString)
stringByReplacingMatchesInString_options_range_withTemplate nsRegularExpression  string options range templ =
withObjCPtr string $ \raw_string ->
  withObjCPtr templ $ \raw_templ ->
      sendMsg nsRegularExpression (mkSelector "stringByReplacingMatchesInString:options:range:withTemplate:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range, argPtr (castPtr raw_templ :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceMatchesInString:options:range:withTemplate:@
replaceMatchesInString_options_range_withTemplate :: (IsNSRegularExpression nsRegularExpression, IsNSMutableString string, IsNSString templ) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> templ -> IO CULong
replaceMatchesInString_options_range_withTemplate nsRegularExpression  string options range templ =
withObjCPtr string $ \raw_string ->
  withObjCPtr templ $ \raw_templ ->
      sendMsg nsRegularExpression (mkSelector "replaceMatchesInString:options:range:withTemplate:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range, argPtr (castPtr raw_templ :: Ptr ())]

-- | @- replacementStringForResult:inString:offset:template:@
replacementStringForResult_inString_offset_template :: (IsNSRegularExpression nsRegularExpression, IsNSTextCheckingResult result, IsNSString string, IsNSString templ) => nsRegularExpression -> result -> string -> CLong -> templ -> IO (Id NSString)
replacementStringForResult_inString_offset_template nsRegularExpression  result string offset templ =
withObjCPtr result $ \raw_result ->
  withObjCPtr string $ \raw_string ->
    withObjCPtr templ $ \raw_templ ->
        sendMsg nsRegularExpression (mkSelector "replacementStringForResult:inString:offset:template:") (retPtr retVoid) [argPtr (castPtr raw_result :: Ptr ()), argPtr (castPtr raw_string :: Ptr ()), argCLong (fromIntegral offset), argPtr (castPtr raw_templ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ escapedTemplateForString:@
escapedTemplateForString :: IsNSString string => string -> IO (Id NSString)
escapedTemplateForString string =
  do
    cls' <- getRequiredClass "NSRegularExpression"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "escapedTemplateForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateMatchesInString:options:range:usingBlock:@
enumerateMatchesInString_options_range_usingBlock :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> Ptr () -> IO ()
enumerateMatchesInString_options_range_usingBlock nsRegularExpression  string options range block =
withObjCPtr string $ \raw_string ->
    sendMsg nsRegularExpression (mkSelector "enumerateMatchesInString:options:range:usingBlock:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range, argPtr (castPtr block :: Ptr ())]

-- | @- matchesInString:options:range:@
matchesInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO (Id NSArray)
matchesInString_options_range nsRegularExpression  string options range =
withObjCPtr string $ \raw_string ->
    sendMsg nsRegularExpression (mkSelector "matchesInString:options:range:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range] >>= retainedObject . castPtr

-- | @- numberOfMatchesInString:options:range:@
numberOfMatchesInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO CULong
numberOfMatchesInString_options_range nsRegularExpression  string options range =
withObjCPtr string $ \raw_string ->
    sendMsg nsRegularExpression (mkSelector "numberOfMatchesInString:options:range:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range]

-- | @- firstMatchInString:options:range:@
firstMatchInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO (Id NSTextCheckingResult)
firstMatchInString_options_range nsRegularExpression  string options range =
withObjCPtr string $ \raw_string ->
    sendMsg nsRegularExpression (mkSelector "firstMatchInString:options:range:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range] >>= retainedObject . castPtr

-- | @- rangeOfFirstMatchInString:options:range:@
rangeOfFirstMatchInString_options_range :: (IsNSRegularExpression nsRegularExpression, IsNSString string) => nsRegularExpression -> string -> NSMatchingOptions -> NSRange -> IO NSRange
rangeOfFirstMatchInString_options_range nsRegularExpression  string options range =
withObjCPtr string $ \raw_string ->
    sendMsgStret nsRegularExpression (mkSelector "rangeOfFirstMatchInString:options:range:") retNSRange [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce options), argNSRange range]

-- | @- pattern@
pattern_ :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO (Id NSString)
pattern_ nsRegularExpression  =
  sendMsg nsRegularExpression (mkSelector "pattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO NSRegularExpressionOptions
options nsRegularExpression  =
  fmap (coerce :: CULong -> NSRegularExpressionOptions) $ sendMsg nsRegularExpression (mkSelector "options") retCULong []

-- | @- numberOfCaptureGroups@
numberOfCaptureGroups :: IsNSRegularExpression nsRegularExpression => nsRegularExpression -> IO CULong
numberOfCaptureGroups nsRegularExpression  =
  sendMsg nsRegularExpression (mkSelector "numberOfCaptureGroups") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @regularExpressionWithPattern:options:error:@
regularExpressionWithPattern_options_errorSelector :: Selector
regularExpressionWithPattern_options_errorSelector = mkSelector "regularExpressionWithPattern:options:error:"

-- | @Selector@ for @initWithPattern:options:error:@
initWithPattern_options_errorSelector :: Selector
initWithPattern_options_errorSelector = mkSelector "initWithPattern:options:error:"

-- | @Selector@ for @escapedPatternForString:@
escapedPatternForStringSelector :: Selector
escapedPatternForStringSelector = mkSelector "escapedPatternForString:"

-- | @Selector@ for @stringByReplacingMatchesInString:options:range:withTemplate:@
stringByReplacingMatchesInString_options_range_withTemplateSelector :: Selector
stringByReplacingMatchesInString_options_range_withTemplateSelector = mkSelector "stringByReplacingMatchesInString:options:range:withTemplate:"

-- | @Selector@ for @replaceMatchesInString:options:range:withTemplate:@
replaceMatchesInString_options_range_withTemplateSelector :: Selector
replaceMatchesInString_options_range_withTemplateSelector = mkSelector "replaceMatchesInString:options:range:withTemplate:"

-- | @Selector@ for @replacementStringForResult:inString:offset:template:@
replacementStringForResult_inString_offset_templateSelector :: Selector
replacementStringForResult_inString_offset_templateSelector = mkSelector "replacementStringForResult:inString:offset:template:"

-- | @Selector@ for @escapedTemplateForString:@
escapedTemplateForStringSelector :: Selector
escapedTemplateForStringSelector = mkSelector "escapedTemplateForString:"

-- | @Selector@ for @enumerateMatchesInString:options:range:usingBlock:@
enumerateMatchesInString_options_range_usingBlockSelector :: Selector
enumerateMatchesInString_options_range_usingBlockSelector = mkSelector "enumerateMatchesInString:options:range:usingBlock:"

-- | @Selector@ for @matchesInString:options:range:@
matchesInString_options_rangeSelector :: Selector
matchesInString_options_rangeSelector = mkSelector "matchesInString:options:range:"

-- | @Selector@ for @numberOfMatchesInString:options:range:@
numberOfMatchesInString_options_rangeSelector :: Selector
numberOfMatchesInString_options_rangeSelector = mkSelector "numberOfMatchesInString:options:range:"

-- | @Selector@ for @firstMatchInString:options:range:@
firstMatchInString_options_rangeSelector :: Selector
firstMatchInString_options_rangeSelector = mkSelector "firstMatchInString:options:range:"

-- | @Selector@ for @rangeOfFirstMatchInString:options:range:@
rangeOfFirstMatchInString_options_rangeSelector :: Selector
rangeOfFirstMatchInString_options_rangeSelector = mkSelector "rangeOfFirstMatchInString:options:range:"

-- | @Selector@ for @pattern@
patternSelector :: Selector
patternSelector = mkSelector "pattern"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @numberOfCaptureGroups@
numberOfCaptureGroupsSelector :: Selector
numberOfCaptureGroupsSelector = mkSelector "numberOfCaptureGroups"

