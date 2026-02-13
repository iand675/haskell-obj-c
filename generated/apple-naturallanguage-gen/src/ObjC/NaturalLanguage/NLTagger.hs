{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLTagger@.
module ObjC.NaturalLanguage.NLTagger
  ( NLTagger
  , IsNLTagger(..)
  , initWithTagSchemes
  , availableTagSchemesForUnit_language
  , tokenRangeAtIndex_unit
  , tokenRangeForRange_unit
  , enumerateTagsInRange_unit_scheme_options_usingBlock
  , tagAtIndex_unit_scheme_tokenRange
  , tagsInRange_unit_scheme_options_tokenRanges
  , tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRange
  , setLanguage_range
  , setOrthography_range
  , setModels_forTagScheme
  , modelsForTagScheme
  , setGazetteers_forTagScheme
  , gazetteersForTagScheme
  , requestAssetsForLanguage_tagScheme_completionHandler
  , tagSchemes
  , string
  , setString
  , dominantLanguage
  , availableTagSchemesForUnit_languageSelector
  , dominantLanguageSelector
  , enumerateTagsInRange_unit_scheme_options_usingBlockSelector
  , gazetteersForTagSchemeSelector
  , initWithTagSchemesSelector
  , modelsForTagSchemeSelector
  , requestAssetsForLanguage_tagScheme_completionHandlerSelector
  , setGazetteers_forTagSchemeSelector
  , setLanguage_rangeSelector
  , setModels_forTagSchemeSelector
  , setOrthography_rangeSelector
  , setStringSelector
  , stringSelector
  , tagAtIndex_unit_scheme_tokenRangeSelector
  , tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector
  , tagSchemesSelector
  , tagsInRange_unit_scheme_options_tokenRangesSelector
  , tokenRangeAtIndex_unitSelector
  , tokenRangeForRange_unitSelector

  -- * Enum types
  , NLTaggerOptions(NLTaggerOptions)
  , pattern NLTaggerOmitWords
  , pattern NLTaggerOmitPunctuation
  , pattern NLTaggerOmitWhitespace
  , pattern NLTaggerOmitOther
  , pattern NLTaggerJoinNames
  , pattern NLTaggerJoinContractions
  , NLTokenUnit(NLTokenUnit)
  , pattern NLTokenUnitWord
  , pattern NLTokenUnitSentence
  , pattern NLTokenUnitParagraph
  , pattern NLTokenUnitDocument

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTagSchemes:@
initWithTagSchemes :: (IsNLTagger nlTagger, IsNSArray tagSchemes) => nlTagger -> tagSchemes -> IO (Id NLTagger)
initWithTagSchemes nlTagger tagSchemes =
  sendOwnedMessage nlTagger initWithTagSchemesSelector (toNSArray tagSchemes)

-- | @+ availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_language :: IsNSString language => NLTokenUnit -> language -> IO (Id NSArray)
availableTagSchemesForUnit_language unit language =
  do
    cls' <- getRequiredClass "NLTagger"
    sendClassMessage cls' availableTagSchemesForUnit_languageSelector unit (toNSString language)

-- | @- tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unit :: IsNLTagger nlTagger => nlTagger -> CULong -> NLTokenUnit -> IO NSRange
tokenRangeAtIndex_unit nlTagger characterIndex unit =
  sendMessage nlTagger tokenRangeAtIndex_unitSelector characterIndex unit

-- | @- tokenRangeForRange:unit:@
tokenRangeForRange_unit :: IsNLTagger nlTagger => nlTagger -> NSRange -> NLTokenUnit -> IO NSRange
tokenRangeForRange_unit nlTagger range unit =
  sendMessage nlTagger tokenRangeForRange_unitSelector range unit

-- | @- enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlock :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> NSRange -> NLTokenUnit -> scheme -> NLTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_unit_scheme_options_usingBlock nlTagger range unit scheme options block =
  sendMessage nlTagger enumerateTagsInRange_unit_scheme_options_usingBlockSelector range unit (toNSString scheme) options block

-- | @- tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRange :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> CULong -> NLTokenUnit -> scheme -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_unit_scheme_tokenRange nlTagger characterIndex unit scheme tokenRange =
  sendMessage nlTagger tagAtIndex_unit_scheme_tokenRangeSelector characterIndex unit (toNSString scheme) tokenRange

-- | @- tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRanges :: (IsNLTagger nlTagger, IsNSString scheme, IsNSArray tokenRanges) => nlTagger -> NSRange -> NLTokenUnit -> scheme -> NLTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_unit_scheme_options_tokenRanges nlTagger range unit scheme options tokenRanges =
  sendMessage nlTagger tagsInRange_unit_scheme_options_tokenRangesSelector range unit (toNSString scheme) options (toNSArray tokenRanges)

-- | @- tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:@
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRange :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> CULong -> NLTokenUnit -> scheme -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRange nlTagger characterIndex unit scheme maximumCount tokenRange =
  sendMessage nlTagger tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector characterIndex unit (toNSString scheme) maximumCount tokenRange

-- | @- setLanguage:range:@
setLanguage_range :: (IsNLTagger nlTagger, IsNSString language) => nlTagger -> language -> NSRange -> IO ()
setLanguage_range nlTagger language range =
  sendMessage nlTagger setLanguage_rangeSelector (toNSString language) range

-- | @- setOrthography:range:@
setOrthography_range :: (IsNLTagger nlTagger, IsNSOrthography orthography) => nlTagger -> orthography -> NSRange -> IO ()
setOrthography_range nlTagger orthography range =
  sendMessage nlTagger setOrthography_rangeSelector (toNSOrthography orthography) range

-- | @- setModels:forTagScheme:@
setModels_forTagScheme :: (IsNLTagger nlTagger, IsNSArray models, IsNSString tagScheme) => nlTagger -> models -> tagScheme -> IO ()
setModels_forTagScheme nlTagger models tagScheme =
  sendMessage nlTagger setModels_forTagSchemeSelector (toNSArray models) (toNSString tagScheme)

-- | @- modelsForTagScheme:@
modelsForTagScheme :: (IsNLTagger nlTagger, IsNSString tagScheme) => nlTagger -> tagScheme -> IO (Id NSArray)
modelsForTagScheme nlTagger tagScheme =
  sendMessage nlTagger modelsForTagSchemeSelector (toNSString tagScheme)

-- | @- setGazetteers:forTagScheme:@
setGazetteers_forTagScheme :: (IsNLTagger nlTagger, IsNSArray gazetteers, IsNSString tagScheme) => nlTagger -> gazetteers -> tagScheme -> IO ()
setGazetteers_forTagScheme nlTagger gazetteers tagScheme =
  sendMessage nlTagger setGazetteers_forTagSchemeSelector (toNSArray gazetteers) (toNSString tagScheme)

-- | @- gazetteersForTagScheme:@
gazetteersForTagScheme :: (IsNLTagger nlTagger, IsNSString tagScheme) => nlTagger -> tagScheme -> IO (Id NSArray)
gazetteersForTagScheme nlTagger tagScheme =
  sendMessage nlTagger gazetteersForTagSchemeSelector (toNSString tagScheme)

-- | @+ requestAssetsForLanguage:tagScheme:completionHandler:@
requestAssetsForLanguage_tagScheme_completionHandler :: (IsNSString language, IsNSString tagScheme) => language -> tagScheme -> Ptr () -> IO ()
requestAssetsForLanguage_tagScheme_completionHandler language tagScheme completionHandler =
  do
    cls' <- getRequiredClass "NLTagger"
    sendClassMessage cls' requestAssetsForLanguage_tagScheme_completionHandlerSelector (toNSString language) (toNSString tagScheme) completionHandler

-- | @- tagSchemes@
tagSchemes :: IsNLTagger nlTagger => nlTagger -> IO (Id NSArray)
tagSchemes nlTagger =
  sendMessage nlTagger tagSchemesSelector

-- | @- string@
string :: IsNLTagger nlTagger => nlTagger -> IO (Id NSString)
string nlTagger =
  sendMessage nlTagger stringSelector

-- | @- setString:@
setString :: (IsNLTagger nlTagger, IsNSString value) => nlTagger -> value -> IO ()
setString nlTagger value =
  sendMessage nlTagger setStringSelector (toNSString value)

-- | @- dominantLanguage@
dominantLanguage :: IsNLTagger nlTagger => nlTagger -> IO (Id NSString)
dominantLanguage nlTagger =
  sendMessage nlTagger dominantLanguageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTagSchemes:@
initWithTagSchemesSelector :: Selector '[Id NSArray] (Id NLTagger)
initWithTagSchemesSelector = mkSelector "initWithTagSchemes:"

-- | @Selector@ for @availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_languageSelector :: Selector '[NLTokenUnit, Id NSString] (Id NSArray)
availableTagSchemesForUnit_languageSelector = mkSelector "availableTagSchemesForUnit:language:"

-- | @Selector@ for @tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unitSelector :: Selector '[CULong, NLTokenUnit] NSRange
tokenRangeAtIndex_unitSelector = mkSelector "tokenRangeAtIndex:unit:"

-- | @Selector@ for @tokenRangeForRange:unit:@
tokenRangeForRange_unitSelector :: Selector '[NSRange, NLTokenUnit] NSRange
tokenRangeForRange_unitSelector = mkSelector "tokenRangeForRange:unit:"

-- | @Selector@ for @enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlockSelector :: Selector '[NSRange, NLTokenUnit, Id NSString, NLTaggerOptions, Ptr ()] ()
enumerateTagsInRange_unit_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRangeSelector :: Selector '[CULong, NLTokenUnit, Id NSString, Ptr NSRange] (Id NSString)
tagAtIndex_unit_scheme_tokenRangeSelector = mkSelector "tagAtIndex:unit:scheme:tokenRange:"

-- | @Selector@ for @tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRangesSelector :: Selector '[NSRange, NLTokenUnit, Id NSString, NLTaggerOptions, Id NSArray] (Id NSArray)
tagsInRange_unit_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:unit:scheme:options:tokenRanges:"

-- | @Selector@ for @tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:@
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector :: Selector '[CULong, NLTokenUnit, Id NSString, CULong, Ptr NSRange] (Id NSDictionary)
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector = mkSelector "tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:"

-- | @Selector@ for @setLanguage:range:@
setLanguage_rangeSelector :: Selector '[Id NSString, NSRange] ()
setLanguage_rangeSelector = mkSelector "setLanguage:range:"

-- | @Selector@ for @setOrthography:range:@
setOrthography_rangeSelector :: Selector '[Id NSOrthography, NSRange] ()
setOrthography_rangeSelector = mkSelector "setOrthography:range:"

-- | @Selector@ for @setModels:forTagScheme:@
setModels_forTagSchemeSelector :: Selector '[Id NSArray, Id NSString] ()
setModels_forTagSchemeSelector = mkSelector "setModels:forTagScheme:"

-- | @Selector@ for @modelsForTagScheme:@
modelsForTagSchemeSelector :: Selector '[Id NSString] (Id NSArray)
modelsForTagSchemeSelector = mkSelector "modelsForTagScheme:"

-- | @Selector@ for @setGazetteers:forTagScheme:@
setGazetteers_forTagSchemeSelector :: Selector '[Id NSArray, Id NSString] ()
setGazetteers_forTagSchemeSelector = mkSelector "setGazetteers:forTagScheme:"

-- | @Selector@ for @gazetteersForTagScheme:@
gazetteersForTagSchemeSelector :: Selector '[Id NSString] (Id NSArray)
gazetteersForTagSchemeSelector = mkSelector "gazetteersForTagScheme:"

-- | @Selector@ for @requestAssetsForLanguage:tagScheme:completionHandler:@
requestAssetsForLanguage_tagScheme_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Ptr ()] ()
requestAssetsForLanguage_tagScheme_completionHandlerSelector = mkSelector "requestAssetsForLanguage:tagScheme:completionHandler:"

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

