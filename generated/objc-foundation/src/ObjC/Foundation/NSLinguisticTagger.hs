{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLinguisticTagger@.
module ObjC.Foundation.NSLinguisticTagger
  ( NSLinguisticTagger
  , IsNSLinguisticTagger(..)
  , initWithTagSchemes_options
  , availableTagSchemesForUnit_language
  , availableTagSchemesForLanguage
  , setOrthography_range
  , orthographyAtIndex_effectiveRange
  , stringEditedInRange_changeInLength
  , tokenRangeAtIndex_unit
  , sentenceRangeForRange
  , enumerateTagsInRange_unit_scheme_options_usingBlock
  , tagAtIndex_unit_scheme_tokenRange
  , tagsInRange_unit_scheme_options_tokenRanges
  , enumerateTagsInRange_scheme_options_usingBlock
  , tagAtIndex_scheme_tokenRange_sentenceRange
  , tagsInRange_scheme_options_tokenRanges
  , dominantLanguageForString
  , tagForString_atIndex_unit_scheme_orthography_tokenRange
  , tagsForString_range_unit_scheme_options_orthography_tokenRanges
  , enumerateTagsForString_range_unit_scheme_options_orthography_usingBlock
  , possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scores
  , tagSchemes
  , string
  , setString
  , dominantLanguage
  , initWithTagSchemes_optionsSelector
  , availableTagSchemesForUnit_languageSelector
  , availableTagSchemesForLanguageSelector
  , setOrthography_rangeSelector
  , orthographyAtIndex_effectiveRangeSelector
  , stringEditedInRange_changeInLengthSelector
  , tokenRangeAtIndex_unitSelector
  , sentenceRangeForRangeSelector
  , enumerateTagsInRange_unit_scheme_options_usingBlockSelector
  , tagAtIndex_unit_scheme_tokenRangeSelector
  , tagsInRange_unit_scheme_options_tokenRangesSelector
  , enumerateTagsInRange_scheme_options_usingBlockSelector
  , tagAtIndex_scheme_tokenRange_sentenceRangeSelector
  , tagsInRange_scheme_options_tokenRangesSelector
  , dominantLanguageForStringSelector
  , tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector
  , tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector
  , enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector
  , possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector
  , tagSchemesSelector
  , stringSelector
  , setStringSelector
  , dominantLanguageSelector

  -- * Enum types
  , NSLinguisticTaggerOptions(NSLinguisticTaggerOptions)
  , pattern NSLinguisticTaggerOmitWords
  , pattern NSLinguisticTaggerOmitPunctuation
  , pattern NSLinguisticTaggerOmitWhitespace
  , pattern NSLinguisticTaggerOmitOther
  , pattern NSLinguisticTaggerJoinNames
  , NSLinguisticTaggerUnit(NSLinguisticTaggerUnit)
  , pattern NSLinguisticTaggerUnitWord
  , pattern NSLinguisticTaggerUnitSentence
  , pattern NSLinguisticTaggerUnitParagraph
  , pattern NSLinguisticTaggerUnitDocument

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

-- | @- initWithTagSchemes:options:@
initWithTagSchemes_options :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSArray tagSchemes) => nsLinguisticTagger -> tagSchemes -> CULong -> IO (Id NSLinguisticTagger)
initWithTagSchemes_options nsLinguisticTagger  tagSchemes opts =
withObjCPtr tagSchemes $ \raw_tagSchemes ->
    sendMsg nsLinguisticTagger (mkSelector "initWithTagSchemes:options:") (retPtr retVoid) [argPtr (castPtr raw_tagSchemes :: Ptr ()), argCULong (fromIntegral opts)] >>= ownedObject . castPtr

-- | @+ availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_language :: IsNSString language => NSLinguisticTaggerUnit -> language -> IO (Id NSArray)
availableTagSchemesForUnit_language unit language =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "availableTagSchemesForUnit:language:") (retPtr retVoid) [argCLong (coerce unit), argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @+ availableTagSchemesForLanguage:@
availableTagSchemesForLanguage :: IsNSString language => language -> IO (Id NSArray)
availableTagSchemesForLanguage language =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "availableTagSchemesForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- setOrthography:range:@
setOrthography_range :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSOrthography orthography) => nsLinguisticTagger -> orthography -> NSRange -> IO ()
setOrthography_range nsLinguisticTagger  orthography range =
withObjCPtr orthography $ \raw_orthography ->
    sendMsg nsLinguisticTagger (mkSelector "setOrthography:range:") retVoid [argPtr (castPtr raw_orthography :: Ptr ()), argNSRange range]

-- | @- orthographyAtIndex:effectiveRange:@
orthographyAtIndex_effectiveRange :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> CULong -> Ptr NSRange -> IO (Id NSOrthography)
orthographyAtIndex_effectiveRange nsLinguisticTagger  charIndex effectiveRange =
  sendMsg nsLinguisticTagger (mkSelector "orthographyAtIndex:effectiveRange:") (retPtr retVoid) [argCULong (fromIntegral charIndex), argPtr effectiveRange] >>= retainedObject . castPtr

-- | @- stringEditedInRange:changeInLength:@
stringEditedInRange_changeInLength :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> NSRange -> CLong -> IO ()
stringEditedInRange_changeInLength nsLinguisticTagger  newRange delta =
  sendMsg nsLinguisticTagger (mkSelector "stringEditedInRange:changeInLength:") retVoid [argNSRange newRange, argCLong (fromIntegral delta)]

-- | @- tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unit :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> CULong -> NSLinguisticTaggerUnit -> IO NSRange
tokenRangeAtIndex_unit nsLinguisticTagger  charIndex unit =
  sendMsgStret nsLinguisticTagger (mkSelector "tokenRangeAtIndex:unit:") retNSRange [argCULong (fromIntegral charIndex), argCLong (coerce unit)]

-- | @- sentenceRangeForRange:@
sentenceRangeForRange :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> NSRange -> IO NSRange
sentenceRangeForRange nsLinguisticTagger  range =
  sendMsgStret nsLinguisticTagger (mkSelector "sentenceRangeForRange:") retNSRange [argNSRange range]

-- | @- enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlock :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_unit_scheme_options_usingBlock nsLinguisticTagger  range unit scheme options block =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nsLinguisticTagger (mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:") retVoid [argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr block :: Ptr ())]

-- | @- tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRange :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> CULong -> NSLinguisticTaggerUnit -> scheme -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_unit_scheme_tokenRange nsLinguisticTagger  charIndex unit scheme tokenRange =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nsLinguisticTagger (mkSelector "tagAtIndex:unit:scheme:tokenRange:") (retPtr retVoid) [argCULong (fromIntegral charIndex), argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argPtr tokenRange] >>= retainedObject . castPtr

-- | @- tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRanges :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme, IsNSArray tokenRanges) => nsLinguisticTagger -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_unit_scheme_options_tokenRanges nsLinguisticTagger  range unit scheme options tokenRanges =
withObjCPtr scheme $ \raw_scheme ->
  withObjCPtr tokenRanges $ \raw_tokenRanges ->
      sendMsg nsLinguisticTagger (mkSelector "tagsInRange:unit:scheme:options:tokenRanges:") (retPtr retVoid) [argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_tokenRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateTagsInRange:scheme:options:usingBlock:@
enumerateTagsInRange_scheme_options_usingBlock :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme) => nsLinguisticTagger -> NSRange -> tagScheme -> NSLinguisticTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_scheme_options_usingBlock nsLinguisticTagger  range tagScheme opts block =
withObjCPtr tagScheme $ \raw_tagScheme ->
    sendMsg nsLinguisticTagger (mkSelector "enumerateTagsInRange:scheme:options:usingBlock:") retVoid [argNSRange range, argPtr (castPtr raw_tagScheme :: Ptr ()), argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- tagAtIndex:scheme:tokenRange:sentenceRange:@
tagAtIndex_scheme_tokenRange_sentenceRange :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> CULong -> scheme -> Ptr NSRange -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_scheme_tokenRange_sentenceRange nsLinguisticTagger  charIndex scheme tokenRange sentenceRange =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nsLinguisticTagger (mkSelector "tagAtIndex:scheme:tokenRange:sentenceRange:") (retPtr retVoid) [argCULong (fromIntegral charIndex), argPtr (castPtr raw_scheme :: Ptr ()), argPtr tokenRange, argPtr sentenceRange] >>= retainedObject . castPtr

-- | @- tagsInRange:scheme:options:tokenRanges:@
tagsInRange_scheme_options_tokenRanges :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme, IsNSArray tokenRanges) => nsLinguisticTagger -> NSRange -> tagScheme -> NSLinguisticTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_scheme_options_tokenRanges nsLinguisticTagger  range tagScheme opts tokenRanges =
withObjCPtr tagScheme $ \raw_tagScheme ->
  withObjCPtr tokenRanges $ \raw_tokenRanges ->
      sendMsg nsLinguisticTagger (mkSelector "tagsInRange:scheme:options:tokenRanges:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_tagScheme :: Ptr ()), argCULong (coerce opts), argPtr (castPtr raw_tokenRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dominantLanguageForString:@
dominantLanguageForString :: IsNSString string => string -> IO (Id NSString)
dominantLanguageForString string =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "dominantLanguageForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ tagForString:atIndex:unit:scheme:orthography:tokenRange:@
tagForString_atIndex_unit_scheme_orthography_tokenRange :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography) => string -> CULong -> NSLinguisticTaggerUnit -> scheme -> orthography -> Ptr NSRange -> IO (Id NSString)
tagForString_atIndex_unit_scheme_orthography_tokenRange string charIndex unit scheme orthography tokenRange =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr string $ \raw_string ->
      withObjCPtr scheme $ \raw_scheme ->
        withObjCPtr orthography $ \raw_orthography ->
          sendClassMsg cls' (mkSelector "tagForString:atIndex:unit:scheme:orthography:tokenRange:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (fromIntegral charIndex), argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argPtr (castPtr raw_orthography :: Ptr ()), argPtr tokenRange] >>= retainedObject . castPtr

-- | @+ tagsForString:range:unit:scheme:options:orthography:tokenRanges:@
tagsForString_range_unit_scheme_options_orthography_tokenRanges :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography, IsNSArray tokenRanges) => string -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> orthography -> tokenRanges -> IO (Id NSArray)
tagsForString_range_unit_scheme_options_orthography_tokenRanges string range unit scheme options orthography tokenRanges =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr string $ \raw_string ->
      withObjCPtr scheme $ \raw_scheme ->
        withObjCPtr orthography $ \raw_orthography ->
          withObjCPtr tokenRanges $ \raw_tokenRanges ->
            sendClassMsg cls' (mkSelector "tagsForString:range:unit:scheme:options:orthography:tokenRanges:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_orthography :: Ptr ()), argPtr (castPtr raw_tokenRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @+ enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:@
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlock :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography) => string -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> orthography -> Ptr () -> IO ()
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlock string range unit scheme options orthography block =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    withObjCPtr string $ \raw_string ->
      withObjCPtr scheme $ \raw_scheme ->
        withObjCPtr orthography $ \raw_orthography ->
          sendClassMsg cls' (mkSelector "enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_orthography :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:@
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scores :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme, IsNSArray scores) => nsLinguisticTagger -> CULong -> tagScheme -> Ptr NSRange -> Ptr NSRange -> scores -> IO (Id NSArray)
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scores nsLinguisticTagger  charIndex tagScheme tokenRange sentenceRange scores =
withObjCPtr tagScheme $ \raw_tagScheme ->
  withObjCPtr scores $ \raw_scores ->
      sendMsg nsLinguisticTagger (mkSelector "possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:") (retPtr retVoid) [argCULong (fromIntegral charIndex), argPtr (castPtr raw_tagScheme :: Ptr ()), argPtr tokenRange, argPtr sentenceRange, argPtr (castPtr raw_scores :: Ptr ())] >>= retainedObject . castPtr

-- | @- tagSchemes@
tagSchemes :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSArray)
tagSchemes nsLinguisticTagger  =
  sendMsg nsLinguisticTagger (mkSelector "tagSchemes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- string@
string :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSString)
string nsLinguisticTagger  =
  sendMsg nsLinguisticTagger (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setString:@
setString :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString value) => nsLinguisticTagger -> value -> IO ()
setString nsLinguisticTagger  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLinguisticTagger (mkSelector "setString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dominantLanguage@
dominantLanguage :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSString)
dominantLanguage nsLinguisticTagger  =
  sendMsg nsLinguisticTagger (mkSelector "dominantLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTagSchemes:options:@
initWithTagSchemes_optionsSelector :: Selector
initWithTagSchemes_optionsSelector = mkSelector "initWithTagSchemes:options:"

-- | @Selector@ for @availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_languageSelector :: Selector
availableTagSchemesForUnit_languageSelector = mkSelector "availableTagSchemesForUnit:language:"

-- | @Selector@ for @availableTagSchemesForLanguage:@
availableTagSchemesForLanguageSelector :: Selector
availableTagSchemesForLanguageSelector = mkSelector "availableTagSchemesForLanguage:"

-- | @Selector@ for @setOrthography:range:@
setOrthography_rangeSelector :: Selector
setOrthography_rangeSelector = mkSelector "setOrthography:range:"

-- | @Selector@ for @orthographyAtIndex:effectiveRange:@
orthographyAtIndex_effectiveRangeSelector :: Selector
orthographyAtIndex_effectiveRangeSelector = mkSelector "orthographyAtIndex:effectiveRange:"

-- | @Selector@ for @stringEditedInRange:changeInLength:@
stringEditedInRange_changeInLengthSelector :: Selector
stringEditedInRange_changeInLengthSelector = mkSelector "stringEditedInRange:changeInLength:"

-- | @Selector@ for @tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unitSelector :: Selector
tokenRangeAtIndex_unitSelector = mkSelector "tokenRangeAtIndex:unit:"

-- | @Selector@ for @sentenceRangeForRange:@
sentenceRangeForRangeSelector :: Selector
sentenceRangeForRangeSelector = mkSelector "sentenceRangeForRange:"

-- | @Selector@ for @enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlockSelector :: Selector
enumerateTagsInRange_unit_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRangeSelector :: Selector
tagAtIndex_unit_scheme_tokenRangeSelector = mkSelector "tagAtIndex:unit:scheme:tokenRange:"

-- | @Selector@ for @tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRangesSelector :: Selector
tagsInRange_unit_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:unit:scheme:options:tokenRanges:"

-- | @Selector@ for @enumerateTagsInRange:scheme:options:usingBlock:@
enumerateTagsInRange_scheme_options_usingBlockSelector :: Selector
enumerateTagsInRange_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:scheme:tokenRange:sentenceRange:@
tagAtIndex_scheme_tokenRange_sentenceRangeSelector :: Selector
tagAtIndex_scheme_tokenRange_sentenceRangeSelector = mkSelector "tagAtIndex:scheme:tokenRange:sentenceRange:"

-- | @Selector@ for @tagsInRange:scheme:options:tokenRanges:@
tagsInRange_scheme_options_tokenRangesSelector :: Selector
tagsInRange_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:scheme:options:tokenRanges:"

-- | @Selector@ for @dominantLanguageForString:@
dominantLanguageForStringSelector :: Selector
dominantLanguageForStringSelector = mkSelector "dominantLanguageForString:"

-- | @Selector@ for @tagForString:atIndex:unit:scheme:orthography:tokenRange:@
tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector :: Selector
tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector = mkSelector "tagForString:atIndex:unit:scheme:orthography:tokenRange:"

-- | @Selector@ for @tagsForString:range:unit:scheme:options:orthography:tokenRanges:@
tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector :: Selector
tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector = mkSelector "tagsForString:range:unit:scheme:options:orthography:tokenRanges:"

-- | @Selector@ for @enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:@
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector :: Selector
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector = mkSelector "enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:"

-- | @Selector@ for @possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:@
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector :: Selector
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector = mkSelector "possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:"

-- | @Selector@ for @tagSchemes@
tagSchemesSelector :: Selector
tagSchemesSelector = mkSelector "tagSchemes"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector
dominantLanguageSelector = mkSelector "dominantLanguage"

