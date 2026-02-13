{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , availableTagSchemesForLanguageSelector
  , availableTagSchemesForUnit_languageSelector
  , dominantLanguageForStringSelector
  , dominantLanguageSelector
  , enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector
  , enumerateTagsInRange_scheme_options_usingBlockSelector
  , enumerateTagsInRange_unit_scheme_options_usingBlockSelector
  , initWithTagSchemes_optionsSelector
  , orthographyAtIndex_effectiveRangeSelector
  , possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector
  , sentenceRangeForRangeSelector
  , setOrthography_rangeSelector
  , setStringSelector
  , stringEditedInRange_changeInLengthSelector
  , stringSelector
  , tagAtIndex_scheme_tokenRange_sentenceRangeSelector
  , tagAtIndex_unit_scheme_tokenRangeSelector
  , tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector
  , tagSchemesSelector
  , tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector
  , tagsInRange_scheme_options_tokenRangesSelector
  , tagsInRange_unit_scheme_options_tokenRangesSelector
  , tokenRangeAtIndex_unitSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @- initWithTagSchemes:options:@
initWithTagSchemes_options :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSArray tagSchemes) => nsLinguisticTagger -> tagSchemes -> CULong -> IO (Id NSLinguisticTagger)
initWithTagSchemes_options nsLinguisticTagger tagSchemes opts =
  sendOwnedMessage nsLinguisticTagger initWithTagSchemes_optionsSelector (toNSArray tagSchemes) opts

-- | @+ availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_language :: IsNSString language => NSLinguisticTaggerUnit -> language -> IO (Id NSArray)
availableTagSchemesForUnit_language unit language =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' availableTagSchemesForUnit_languageSelector unit (toNSString language)

-- | @+ availableTagSchemesForLanguage:@
availableTagSchemesForLanguage :: IsNSString language => language -> IO (Id NSArray)
availableTagSchemesForLanguage language =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' availableTagSchemesForLanguageSelector (toNSString language)

-- | @- setOrthography:range:@
setOrthography_range :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSOrthography orthography) => nsLinguisticTagger -> orthography -> NSRange -> IO ()
setOrthography_range nsLinguisticTagger orthography range =
  sendMessage nsLinguisticTagger setOrthography_rangeSelector (toNSOrthography orthography) range

-- | @- orthographyAtIndex:effectiveRange:@
orthographyAtIndex_effectiveRange :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> CULong -> Ptr NSRange -> IO (Id NSOrthography)
orthographyAtIndex_effectiveRange nsLinguisticTagger charIndex effectiveRange =
  sendMessage nsLinguisticTagger orthographyAtIndex_effectiveRangeSelector charIndex effectiveRange

-- | @- stringEditedInRange:changeInLength:@
stringEditedInRange_changeInLength :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> NSRange -> CLong -> IO ()
stringEditedInRange_changeInLength nsLinguisticTagger newRange delta =
  sendMessage nsLinguisticTagger stringEditedInRange_changeInLengthSelector newRange delta

-- | @- tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unit :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> CULong -> NSLinguisticTaggerUnit -> IO NSRange
tokenRangeAtIndex_unit nsLinguisticTagger charIndex unit =
  sendMessage nsLinguisticTagger tokenRangeAtIndex_unitSelector charIndex unit

-- | @- sentenceRangeForRange:@
sentenceRangeForRange :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> NSRange -> IO NSRange
sentenceRangeForRange nsLinguisticTagger range =
  sendMessage nsLinguisticTagger sentenceRangeForRangeSelector range

-- | @- enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlock :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_unit_scheme_options_usingBlock nsLinguisticTagger range unit scheme options block =
  sendMessage nsLinguisticTagger enumerateTagsInRange_unit_scheme_options_usingBlockSelector range unit (toNSString scheme) options block

-- | @- tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRange :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> CULong -> NSLinguisticTaggerUnit -> scheme -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_unit_scheme_tokenRange nsLinguisticTagger charIndex unit scheme tokenRange =
  sendMessage nsLinguisticTagger tagAtIndex_unit_scheme_tokenRangeSelector charIndex unit (toNSString scheme) tokenRange

-- | @- tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRanges :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme, IsNSArray tokenRanges) => nsLinguisticTagger -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_unit_scheme_options_tokenRanges nsLinguisticTagger range unit scheme options tokenRanges =
  sendMessage nsLinguisticTagger tagsInRange_unit_scheme_options_tokenRangesSelector range unit (toNSString scheme) options (toNSArray tokenRanges)

-- | @- enumerateTagsInRange:scheme:options:usingBlock:@
enumerateTagsInRange_scheme_options_usingBlock :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme) => nsLinguisticTagger -> NSRange -> tagScheme -> NSLinguisticTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_scheme_options_usingBlock nsLinguisticTagger range tagScheme opts block =
  sendMessage nsLinguisticTagger enumerateTagsInRange_scheme_options_usingBlockSelector range (toNSString tagScheme) opts block

-- | @- tagAtIndex:scheme:tokenRange:sentenceRange:@
tagAtIndex_scheme_tokenRange_sentenceRange :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString scheme) => nsLinguisticTagger -> CULong -> scheme -> Ptr NSRange -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_scheme_tokenRange_sentenceRange nsLinguisticTagger charIndex scheme tokenRange sentenceRange =
  sendMessage nsLinguisticTagger tagAtIndex_scheme_tokenRange_sentenceRangeSelector charIndex (toNSString scheme) tokenRange sentenceRange

-- | @- tagsInRange:scheme:options:tokenRanges:@
tagsInRange_scheme_options_tokenRanges :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme, IsNSArray tokenRanges) => nsLinguisticTagger -> NSRange -> tagScheme -> NSLinguisticTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_scheme_options_tokenRanges nsLinguisticTagger range tagScheme opts tokenRanges =
  sendMessage nsLinguisticTagger tagsInRange_scheme_options_tokenRangesSelector range (toNSString tagScheme) opts (toNSArray tokenRanges)

-- | @+ dominantLanguageForString:@
dominantLanguageForString :: IsNSString string => string -> IO (Id NSString)
dominantLanguageForString string =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' dominantLanguageForStringSelector (toNSString string)

-- | @+ tagForString:atIndex:unit:scheme:orthography:tokenRange:@
tagForString_atIndex_unit_scheme_orthography_tokenRange :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography) => string -> CULong -> NSLinguisticTaggerUnit -> scheme -> orthography -> Ptr NSRange -> IO (Id NSString)
tagForString_atIndex_unit_scheme_orthography_tokenRange string charIndex unit scheme orthography tokenRange =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector (toNSString string) charIndex unit (toNSString scheme) (toNSOrthography orthography) tokenRange

-- | @+ tagsForString:range:unit:scheme:options:orthography:tokenRanges:@
tagsForString_range_unit_scheme_options_orthography_tokenRanges :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography, IsNSArray tokenRanges) => string -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> orthography -> tokenRanges -> IO (Id NSArray)
tagsForString_range_unit_scheme_options_orthography_tokenRanges string range unit scheme options orthography tokenRanges =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector (toNSString string) range unit (toNSString scheme) options (toNSOrthography orthography) (toNSArray tokenRanges)

-- | @+ enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:@
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlock :: (IsNSString string, IsNSString scheme, IsNSOrthography orthography) => string -> NSRange -> NSLinguisticTaggerUnit -> scheme -> NSLinguisticTaggerOptions -> orthography -> Ptr () -> IO ()
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlock string range unit scheme options orthography block =
  do
    cls' <- getRequiredClass "NSLinguisticTagger"
    sendClassMessage cls' enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector (toNSString string) range unit (toNSString scheme) options (toNSOrthography orthography) block

-- | @- possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:@
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scores :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString tagScheme, IsNSArray scores) => nsLinguisticTagger -> CULong -> tagScheme -> Ptr NSRange -> Ptr NSRange -> scores -> IO (Id NSArray)
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scores nsLinguisticTagger charIndex tagScheme tokenRange sentenceRange scores =
  sendMessage nsLinguisticTagger possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector charIndex (toNSString tagScheme) tokenRange sentenceRange (toNSArray scores)

-- | @- tagSchemes@
tagSchemes :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSArray)
tagSchemes nsLinguisticTagger =
  sendMessage nsLinguisticTagger tagSchemesSelector

-- | @- string@
string :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSString)
string nsLinguisticTagger =
  sendMessage nsLinguisticTagger stringSelector

-- | @- setString:@
setString :: (IsNSLinguisticTagger nsLinguisticTagger, IsNSString value) => nsLinguisticTagger -> value -> IO ()
setString nsLinguisticTagger value =
  sendMessage nsLinguisticTagger setStringSelector (toNSString value)

-- | @- dominantLanguage@
dominantLanguage :: IsNSLinguisticTagger nsLinguisticTagger => nsLinguisticTagger -> IO (Id NSString)
dominantLanguage nsLinguisticTagger =
  sendMessage nsLinguisticTagger dominantLanguageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTagSchemes:options:@
initWithTagSchemes_optionsSelector :: Selector '[Id NSArray, CULong] (Id NSLinguisticTagger)
initWithTagSchemes_optionsSelector = mkSelector "initWithTagSchemes:options:"

-- | @Selector@ for @availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_languageSelector :: Selector '[NSLinguisticTaggerUnit, Id NSString] (Id NSArray)
availableTagSchemesForUnit_languageSelector = mkSelector "availableTagSchemesForUnit:language:"

-- | @Selector@ for @availableTagSchemesForLanguage:@
availableTagSchemesForLanguageSelector :: Selector '[Id NSString] (Id NSArray)
availableTagSchemesForLanguageSelector = mkSelector "availableTagSchemesForLanguage:"

-- | @Selector@ for @setOrthography:range:@
setOrthography_rangeSelector :: Selector '[Id NSOrthography, NSRange] ()
setOrthography_rangeSelector = mkSelector "setOrthography:range:"

-- | @Selector@ for @orthographyAtIndex:effectiveRange:@
orthographyAtIndex_effectiveRangeSelector :: Selector '[CULong, Ptr NSRange] (Id NSOrthography)
orthographyAtIndex_effectiveRangeSelector = mkSelector "orthographyAtIndex:effectiveRange:"

-- | @Selector@ for @stringEditedInRange:changeInLength:@
stringEditedInRange_changeInLengthSelector :: Selector '[NSRange, CLong] ()
stringEditedInRange_changeInLengthSelector = mkSelector "stringEditedInRange:changeInLength:"

-- | @Selector@ for @tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unitSelector :: Selector '[CULong, NSLinguisticTaggerUnit] NSRange
tokenRangeAtIndex_unitSelector = mkSelector "tokenRangeAtIndex:unit:"

-- | @Selector@ for @sentenceRangeForRange:@
sentenceRangeForRangeSelector :: Selector '[NSRange] NSRange
sentenceRangeForRangeSelector = mkSelector "sentenceRangeForRange:"

-- | @Selector@ for @enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlockSelector :: Selector '[NSRange, NSLinguisticTaggerUnit, Id NSString, NSLinguisticTaggerOptions, Ptr ()] ()
enumerateTagsInRange_unit_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRangeSelector :: Selector '[CULong, NSLinguisticTaggerUnit, Id NSString, Ptr NSRange] (Id NSString)
tagAtIndex_unit_scheme_tokenRangeSelector = mkSelector "tagAtIndex:unit:scheme:tokenRange:"

-- | @Selector@ for @tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRangesSelector :: Selector '[NSRange, NSLinguisticTaggerUnit, Id NSString, NSLinguisticTaggerOptions, Id NSArray] (Id NSArray)
tagsInRange_unit_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:unit:scheme:options:tokenRanges:"

-- | @Selector@ for @enumerateTagsInRange:scheme:options:usingBlock:@
enumerateTagsInRange_scheme_options_usingBlockSelector :: Selector '[NSRange, Id NSString, NSLinguisticTaggerOptions, Ptr ()] ()
enumerateTagsInRange_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:scheme:tokenRange:sentenceRange:@
tagAtIndex_scheme_tokenRange_sentenceRangeSelector :: Selector '[CULong, Id NSString, Ptr NSRange, Ptr NSRange] (Id NSString)
tagAtIndex_scheme_tokenRange_sentenceRangeSelector = mkSelector "tagAtIndex:scheme:tokenRange:sentenceRange:"

-- | @Selector@ for @tagsInRange:scheme:options:tokenRanges:@
tagsInRange_scheme_options_tokenRangesSelector :: Selector '[NSRange, Id NSString, NSLinguisticTaggerOptions, Id NSArray] (Id NSArray)
tagsInRange_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:scheme:options:tokenRanges:"

-- | @Selector@ for @dominantLanguageForString:@
dominantLanguageForStringSelector :: Selector '[Id NSString] (Id NSString)
dominantLanguageForStringSelector = mkSelector "dominantLanguageForString:"

-- | @Selector@ for @tagForString:atIndex:unit:scheme:orthography:tokenRange:@
tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector :: Selector '[Id NSString, CULong, NSLinguisticTaggerUnit, Id NSString, Id NSOrthography, Ptr NSRange] (Id NSString)
tagForString_atIndex_unit_scheme_orthography_tokenRangeSelector = mkSelector "tagForString:atIndex:unit:scheme:orthography:tokenRange:"

-- | @Selector@ for @tagsForString:range:unit:scheme:options:orthography:tokenRanges:@
tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector :: Selector '[Id NSString, NSRange, NSLinguisticTaggerUnit, Id NSString, NSLinguisticTaggerOptions, Id NSOrthography, Id NSArray] (Id NSArray)
tagsForString_range_unit_scheme_options_orthography_tokenRangesSelector = mkSelector "tagsForString:range:unit:scheme:options:orthography:tokenRanges:"

-- | @Selector@ for @enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:@
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector :: Selector '[Id NSString, NSRange, NSLinguisticTaggerUnit, Id NSString, NSLinguisticTaggerOptions, Id NSOrthography, Ptr ()] ()
enumerateTagsForString_range_unit_scheme_options_orthography_usingBlockSelector = mkSelector "enumerateTagsForString:range:unit:scheme:options:orthography:usingBlock:"

-- | @Selector@ for @possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:@
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector :: Selector '[CULong, Id NSString, Ptr NSRange, Ptr NSRange, Id NSArray] (Id NSArray)
possibleTagsAtIndex_scheme_tokenRange_sentenceRange_scoresSelector = mkSelector "possibleTagsAtIndex:scheme:tokenRange:sentenceRange:scores:"

-- | @Selector@ for @tagSchemes@
tagSchemesSelector :: Selector '[] (Id NSArray)
tagSchemesSelector = mkSelector "tagSchemes"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @setString:@
setStringSelector :: Selector '[Id NSString] ()
setStringSelector = mkSelector "setString:"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector '[] (Id NSString)
dominantLanguageSelector = mkSelector "dominantLanguage"

