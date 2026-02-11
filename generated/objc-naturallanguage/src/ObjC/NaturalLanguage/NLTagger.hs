{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTagSchemesSelector
  , availableTagSchemesForUnit_languageSelector
  , tokenRangeAtIndex_unitSelector
  , tokenRangeForRange_unitSelector
  , enumerateTagsInRange_unit_scheme_options_usingBlockSelector
  , tagAtIndex_unit_scheme_tokenRangeSelector
  , tagsInRange_unit_scheme_options_tokenRangesSelector
  , tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector
  , setLanguage_rangeSelector
  , setOrthography_rangeSelector
  , setModels_forTagSchemeSelector
  , modelsForTagSchemeSelector
  , setGazetteers_forTagSchemeSelector
  , gazetteersForTagSchemeSelector
  , requestAssetsForLanguage_tagScheme_completionHandlerSelector
  , tagSchemesSelector
  , stringSelector
  , setStringSelector
  , dominantLanguageSelector

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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.NaturalLanguage.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTagSchemes:@
initWithTagSchemes :: (IsNLTagger nlTagger, IsNSArray tagSchemes) => nlTagger -> tagSchemes -> IO (Id NLTagger)
initWithTagSchemes nlTagger  tagSchemes =
withObjCPtr tagSchemes $ \raw_tagSchemes ->
    sendMsg nlTagger (mkSelector "initWithTagSchemes:") (retPtr retVoid) [argPtr (castPtr raw_tagSchemes :: Ptr ())] >>= ownedObject . castPtr

-- | @+ availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_language :: IsNSString language => NLTokenUnit -> language -> IO (Id NSArray)
availableTagSchemesForUnit_language unit language =
  do
    cls' <- getRequiredClass "NLTagger"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "availableTagSchemesForUnit:language:") (retPtr retVoid) [argCLong (coerce unit), argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unit :: IsNLTagger nlTagger => nlTagger -> CULong -> NLTokenUnit -> IO NSRange
tokenRangeAtIndex_unit nlTagger  characterIndex unit =
  sendMsgStret nlTagger (mkSelector "tokenRangeAtIndex:unit:") retNSRange [argCULong (fromIntegral characterIndex), argCLong (coerce unit)]

-- | @- tokenRangeForRange:unit:@
tokenRangeForRange_unit :: IsNLTagger nlTagger => nlTagger -> NSRange -> NLTokenUnit -> IO NSRange
tokenRangeForRange_unit nlTagger  range unit =
  sendMsgStret nlTagger (mkSelector "tokenRangeForRange:unit:") retNSRange [argNSRange range, argCLong (coerce unit)]

-- | @- enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlock :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> NSRange -> NLTokenUnit -> scheme -> NLTaggerOptions -> Ptr () -> IO ()
enumerateTagsInRange_unit_scheme_options_usingBlock nlTagger  range unit scheme options block =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nlTagger (mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:") retVoid [argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr block :: Ptr ())]

-- | @- tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRange :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> CULong -> NLTokenUnit -> scheme -> Ptr NSRange -> IO (Id NSString)
tagAtIndex_unit_scheme_tokenRange nlTagger  characterIndex unit scheme tokenRange =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nlTagger (mkSelector "tagAtIndex:unit:scheme:tokenRange:") (retPtr retVoid) [argCULong (fromIntegral characterIndex), argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argPtr tokenRange] >>= retainedObject . castPtr

-- | @- tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRanges :: (IsNLTagger nlTagger, IsNSString scheme, IsNSArray tokenRanges) => nlTagger -> NSRange -> NLTokenUnit -> scheme -> NLTaggerOptions -> tokenRanges -> IO (Id NSArray)
tagsInRange_unit_scheme_options_tokenRanges nlTagger  range unit scheme options tokenRanges =
withObjCPtr scheme $ \raw_scheme ->
  withObjCPtr tokenRanges $ \raw_tokenRanges ->
      sendMsg nlTagger (mkSelector "tagsInRange:unit:scheme:options:tokenRanges:") (retPtr retVoid) [argNSRange range, argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_tokenRanges :: Ptr ())] >>= retainedObject . castPtr

-- | @- tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:@
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRange :: (IsNLTagger nlTagger, IsNSString scheme) => nlTagger -> CULong -> NLTokenUnit -> scheme -> CULong -> Ptr NSRange -> IO (Id NSDictionary)
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRange nlTagger  characterIndex unit scheme maximumCount tokenRange =
withObjCPtr scheme $ \raw_scheme ->
    sendMsg nlTagger (mkSelector "tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:") (retPtr retVoid) [argCULong (fromIntegral characterIndex), argCLong (coerce unit), argPtr (castPtr raw_scheme :: Ptr ()), argCULong (fromIntegral maximumCount), argPtr tokenRange] >>= retainedObject . castPtr

-- | @- setLanguage:range:@
setLanguage_range :: (IsNLTagger nlTagger, IsNSString language) => nlTagger -> language -> NSRange -> IO ()
setLanguage_range nlTagger  language range =
withObjCPtr language $ \raw_language ->
    sendMsg nlTagger (mkSelector "setLanguage:range:") retVoid [argPtr (castPtr raw_language :: Ptr ()), argNSRange range]

-- | @- setOrthography:range:@
setOrthography_range :: (IsNLTagger nlTagger, IsNSOrthography orthography) => nlTagger -> orthography -> NSRange -> IO ()
setOrthography_range nlTagger  orthography range =
withObjCPtr orthography $ \raw_orthography ->
    sendMsg nlTagger (mkSelector "setOrthography:range:") retVoid [argPtr (castPtr raw_orthography :: Ptr ()), argNSRange range]

-- | @- setModels:forTagScheme:@
setModels_forTagScheme :: (IsNLTagger nlTagger, IsNSArray models, IsNSString tagScheme) => nlTagger -> models -> tagScheme -> IO ()
setModels_forTagScheme nlTagger  models tagScheme =
withObjCPtr models $ \raw_models ->
  withObjCPtr tagScheme $ \raw_tagScheme ->
      sendMsg nlTagger (mkSelector "setModels:forTagScheme:") retVoid [argPtr (castPtr raw_models :: Ptr ()), argPtr (castPtr raw_tagScheme :: Ptr ())]

-- | @- modelsForTagScheme:@
modelsForTagScheme :: (IsNLTagger nlTagger, IsNSString tagScheme) => nlTagger -> tagScheme -> IO (Id NSArray)
modelsForTagScheme nlTagger  tagScheme =
withObjCPtr tagScheme $ \raw_tagScheme ->
    sendMsg nlTagger (mkSelector "modelsForTagScheme:") (retPtr retVoid) [argPtr (castPtr raw_tagScheme :: Ptr ())] >>= retainedObject . castPtr

-- | @- setGazetteers:forTagScheme:@
setGazetteers_forTagScheme :: (IsNLTagger nlTagger, IsNSArray gazetteers, IsNSString tagScheme) => nlTagger -> gazetteers -> tagScheme -> IO ()
setGazetteers_forTagScheme nlTagger  gazetteers tagScheme =
withObjCPtr gazetteers $ \raw_gazetteers ->
  withObjCPtr tagScheme $ \raw_tagScheme ->
      sendMsg nlTagger (mkSelector "setGazetteers:forTagScheme:") retVoid [argPtr (castPtr raw_gazetteers :: Ptr ()), argPtr (castPtr raw_tagScheme :: Ptr ())]

-- | @- gazetteersForTagScheme:@
gazetteersForTagScheme :: (IsNLTagger nlTagger, IsNSString tagScheme) => nlTagger -> tagScheme -> IO (Id NSArray)
gazetteersForTagScheme nlTagger  tagScheme =
withObjCPtr tagScheme $ \raw_tagScheme ->
    sendMsg nlTagger (mkSelector "gazetteersForTagScheme:") (retPtr retVoid) [argPtr (castPtr raw_tagScheme :: Ptr ())] >>= retainedObject . castPtr

-- | @+ requestAssetsForLanguage:tagScheme:completionHandler:@
requestAssetsForLanguage_tagScheme_completionHandler :: (IsNSString language, IsNSString tagScheme) => language -> tagScheme -> Ptr () -> IO ()
requestAssetsForLanguage_tagScheme_completionHandler language tagScheme completionHandler =
  do
    cls' <- getRequiredClass "NLTagger"
    withObjCPtr language $ \raw_language ->
      withObjCPtr tagScheme $ \raw_tagScheme ->
        sendClassMsg cls' (mkSelector "requestAssetsForLanguage:tagScheme:completionHandler:") retVoid [argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_tagScheme :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- tagSchemes@
tagSchemes :: IsNLTagger nlTagger => nlTagger -> IO (Id NSArray)
tagSchemes nlTagger  =
  sendMsg nlTagger (mkSelector "tagSchemes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- string@
string :: IsNLTagger nlTagger => nlTagger -> IO (Id NSString)
string nlTagger  =
  sendMsg nlTagger (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setString:@
setString :: (IsNLTagger nlTagger, IsNSString value) => nlTagger -> value -> IO ()
setString nlTagger  value =
withObjCPtr value $ \raw_value ->
    sendMsg nlTagger (mkSelector "setString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dominantLanguage@
dominantLanguage :: IsNLTagger nlTagger => nlTagger -> IO (Id NSString)
dominantLanguage nlTagger  =
  sendMsg nlTagger (mkSelector "dominantLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTagSchemes:@
initWithTagSchemesSelector :: Selector
initWithTagSchemesSelector = mkSelector "initWithTagSchemes:"

-- | @Selector@ for @availableTagSchemesForUnit:language:@
availableTagSchemesForUnit_languageSelector :: Selector
availableTagSchemesForUnit_languageSelector = mkSelector "availableTagSchemesForUnit:language:"

-- | @Selector@ for @tokenRangeAtIndex:unit:@
tokenRangeAtIndex_unitSelector :: Selector
tokenRangeAtIndex_unitSelector = mkSelector "tokenRangeAtIndex:unit:"

-- | @Selector@ for @tokenRangeForRange:unit:@
tokenRangeForRange_unitSelector :: Selector
tokenRangeForRange_unitSelector = mkSelector "tokenRangeForRange:unit:"

-- | @Selector@ for @enumerateTagsInRange:unit:scheme:options:usingBlock:@
enumerateTagsInRange_unit_scheme_options_usingBlockSelector :: Selector
enumerateTagsInRange_unit_scheme_options_usingBlockSelector = mkSelector "enumerateTagsInRange:unit:scheme:options:usingBlock:"

-- | @Selector@ for @tagAtIndex:unit:scheme:tokenRange:@
tagAtIndex_unit_scheme_tokenRangeSelector :: Selector
tagAtIndex_unit_scheme_tokenRangeSelector = mkSelector "tagAtIndex:unit:scheme:tokenRange:"

-- | @Selector@ for @tagsInRange:unit:scheme:options:tokenRanges:@
tagsInRange_unit_scheme_options_tokenRangesSelector :: Selector
tagsInRange_unit_scheme_options_tokenRangesSelector = mkSelector "tagsInRange:unit:scheme:options:tokenRanges:"

-- | @Selector@ for @tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:@
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector :: Selector
tagHypothesesAtIndex_unit_scheme_maximumCount_tokenRangeSelector = mkSelector "tagHypothesesAtIndex:unit:scheme:maximumCount:tokenRange:"

-- | @Selector@ for @setLanguage:range:@
setLanguage_rangeSelector :: Selector
setLanguage_rangeSelector = mkSelector "setLanguage:range:"

-- | @Selector@ for @setOrthography:range:@
setOrthography_rangeSelector :: Selector
setOrthography_rangeSelector = mkSelector "setOrthography:range:"

-- | @Selector@ for @setModels:forTagScheme:@
setModels_forTagSchemeSelector :: Selector
setModels_forTagSchemeSelector = mkSelector "setModels:forTagScheme:"

-- | @Selector@ for @modelsForTagScheme:@
modelsForTagSchemeSelector :: Selector
modelsForTagSchemeSelector = mkSelector "modelsForTagScheme:"

-- | @Selector@ for @setGazetteers:forTagScheme:@
setGazetteers_forTagSchemeSelector :: Selector
setGazetteers_forTagSchemeSelector = mkSelector "setGazetteers:forTagScheme:"

-- | @Selector@ for @gazetteersForTagScheme:@
gazetteersForTagSchemeSelector :: Selector
gazetteersForTagSchemeSelector = mkSelector "gazetteersForTagScheme:"

-- | @Selector@ for @requestAssetsForLanguage:tagScheme:completionHandler:@
requestAssetsForLanguage_tagScheme_completionHandlerSelector :: Selector
requestAssetsForLanguage_tagScheme_completionHandlerSelector = mkSelector "requestAssetsForLanguage:tagScheme:completionHandler:"

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

