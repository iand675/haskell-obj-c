{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect regions of text and recognize the containing text in an image.
--
-- This request will generate VNRecognizedTextObservation objects describing the locations of text and the actual text recognized.
--
-- Generated bindings for @VNRecognizeTextRequest@.
module ObjC.Vision.VNRecognizeTextRequest
  ( VNRecognizeTextRequest
  , IsVNRecognizeTextRequest(..)
  , supportedRecognitionLanguagesForTextRecognitionLevel_revision_error
  , supportedRecognitionLanguagesAndReturnError
  , recognitionLanguages
  , setRecognitionLanguages
  , customWords
  , setCustomWords
  , recognitionLevel
  , setRecognitionLevel
  , usesLanguageCorrection
  , setUsesLanguageCorrection
  , automaticallyDetectsLanguage
  , setAutomaticallyDetectsLanguage
  , minimumTextHeight
  , setMinimumTextHeight
  , results
  , automaticallyDetectsLanguageSelector
  , customWordsSelector
  , minimumTextHeightSelector
  , recognitionLanguagesSelector
  , recognitionLevelSelector
  , resultsSelector
  , setAutomaticallyDetectsLanguageSelector
  , setCustomWordsSelector
  , setMinimumTextHeightSelector
  , setRecognitionLanguagesSelector
  , setRecognitionLevelSelector
  , setUsesLanguageCorrectionSelector
  , supportedRecognitionLanguagesAndReturnErrorSelector
  , supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector
  , usesLanguageCorrectionSelector

  -- * Enum types
  , VNRequestTextRecognitionLevel(VNRequestTextRecognitionLevel)
  , pattern VNRequestTextRecognitionLevelAccurate
  , pattern VNRequestTextRecognitionLevelFast

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns all the supported languages for a given text recognition level. Note that a language supported in one recognition level might not be available in another.
--
-- ObjC selector: @+ supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:@
supportedRecognitionLanguagesForTextRecognitionLevel_revision_error :: IsNSError error_ => VNRequestTextRecognitionLevel -> CULong -> error_ -> IO (Id NSArray)
supportedRecognitionLanguagesForTextRecognitionLevel_revision_error recognitionLevel requestRevision error_ =
  do
    cls' <- getRequiredClass "VNRecognizeTextRequest"
    sendClassMessage cls' supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector recognitionLevel requestRevision (toNSError error_)

-- | Obtain the collection of supported recognition languages.
--
-- This method will return the collection of all possible language identifiers that are recognized by the target request based on its current state of configuration at the time of the call.
--
-- @error@ — The address of the variable that will be populated with the error if the call fails.
--
-- Returns: The collection of language identifiers, or nil if a failure occurs.
--
-- ObjC selector: @- supportedRecognitionLanguagesAndReturnError:@
supportedRecognitionLanguagesAndReturnError :: (IsVNRecognizeTextRequest vnRecognizeTextRequest, IsNSError error_) => vnRecognizeTextRequest -> error_ -> IO (Id NSArray)
supportedRecognitionLanguagesAndReturnError vnRecognizeTextRequest error_ =
  sendMessage vnRecognizeTextRequest supportedRecognitionLanguagesAndReturnErrorSelector (toNSError error_)

-- | Specify the languages used for the detection. The order of the languages in the array defines the order in which languages will be used during the language processing. The languages are specified as ISO language codes.
--
-- ObjC selector: @- recognitionLanguages@
recognitionLanguages :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
recognitionLanguages vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest recognitionLanguagesSelector

-- | Specify the languages used for the detection. The order of the languages in the array defines the order in which languages will be used during the language processing. The languages are specified as ISO language codes.
--
-- ObjC selector: @- setRecognitionLanguages:@
setRecognitionLanguages :: (IsVNRecognizeTextRequest vnRecognizeTextRequest, IsNSArray value) => vnRecognizeTextRequest -> value -> IO ()
setRecognitionLanguages vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setRecognitionLanguagesSelector (toNSArray value)

-- | An array of strings that will be used at the word recognition stage in addition to the recognition languages. The customWords list takes precedence over the standard lexicon.
--
-- ObjC selector: @- customWords@
customWords :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
customWords vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest customWordsSelector

-- | An array of strings that will be used at the word recognition stage in addition to the recognition languages. The customWords list takes precedence over the standard lexicon.
--
-- ObjC selector: @- setCustomWords:@
setCustomWords :: (IsVNRecognizeTextRequest vnRecognizeTextRequest, IsNSArray value) => vnRecognizeTextRequest -> value -> IO ()
setCustomWords vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setCustomWordsSelector (toNSArray value)

-- | The recognition level selects which techniques will be used during the text recognition. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- recognitionLevel@
recognitionLevel :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO VNRequestTextRecognitionLevel
recognitionLevel vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest recognitionLevelSelector

-- | The recognition level selects which techniques will be used during the text recognition. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- setRecognitionLevel:@
setRecognitionLevel :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> VNRequestTextRecognitionLevel -> IO ()
setRecognitionLevel vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setRecognitionLevelSelector value

-- | Determines whether language correction should be applied during the recognition process. Disabling this will return the raw recognition results providing performance benefits but less accurate results.
--
-- ObjC selector: @- usesLanguageCorrection@
usesLanguageCorrection :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO Bool
usesLanguageCorrection vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest usesLanguageCorrectionSelector

-- | Determines whether language correction should be applied during the recognition process. Disabling this will return the raw recognition results providing performance benefits but less accurate results.
--
-- ObjC selector: @- setUsesLanguageCorrection:@
setUsesLanguageCorrection :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> Bool -> IO ()
setUsesLanguageCorrection vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setUsesLanguageCorrectionSelector value

-- | Language detection will try to automatically identify the script/langauge during the detection and use the appropiate model for recognition and language correction. This can be particularly helpful, if the nature of the content is unkown and with this flag being set it will for instance determine if text is latin vs chinese so you don't have to pick the language model in the first case. But as the language correction cannot always guarantee the correct detection, it is advisable to set the languages, if you have domain knowledge of what language to expect. The default value is NO. Also note that this feature is only available since VNRecognizeTextRequestRevision3 and is a no-op before that.
--
-- ObjC selector: @- automaticallyDetectsLanguage@
automaticallyDetectsLanguage :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO Bool
automaticallyDetectsLanguage vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest automaticallyDetectsLanguageSelector

-- | Language detection will try to automatically identify the script/langauge during the detection and use the appropiate model for recognition and language correction. This can be particularly helpful, if the nature of the content is unkown and with this flag being set it will for instance determine if text is latin vs chinese so you don't have to pick the language model in the first case. But as the language correction cannot always guarantee the correct detection, it is advisable to set the languages, if you have domain knowledge of what language to expect. The default value is NO. Also note that this feature is only available since VNRecognizeTextRequestRevision3 and is a no-op before that.
--
-- ObjC selector: @- setAutomaticallyDetectsLanguage:@
setAutomaticallyDetectsLanguage :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> Bool -> IO ()
setAutomaticallyDetectsLanguage vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setAutomaticallyDetectsLanguageSelector value

-- | @- minimumTextHeight@
minimumTextHeight :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO CFloat
minimumTextHeight vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest minimumTextHeightSelector

-- | @- setMinimumTextHeight:@
setMinimumTextHeight :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> CFloat -> IO ()
setMinimumTextHeight vnRecognizeTextRequest value =
  sendMessage vnRecognizeTextRequest setMinimumTextHeightSelector value

-- | VNRecognizedTextObservation results.
--
-- ObjC selector: @- results@
results :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
results vnRecognizeTextRequest =
  sendMessage vnRecognizeTextRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:@
supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector :: Selector '[VNRequestTextRecognitionLevel, CULong, Id NSError] (Id NSArray)
supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector = mkSelector "supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:"

-- | @Selector@ for @supportedRecognitionLanguagesAndReturnError:@
supportedRecognitionLanguagesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedRecognitionLanguagesAndReturnErrorSelector = mkSelector "supportedRecognitionLanguagesAndReturnError:"

-- | @Selector@ for @recognitionLanguages@
recognitionLanguagesSelector :: Selector '[] (Id NSArray)
recognitionLanguagesSelector = mkSelector "recognitionLanguages"

-- | @Selector@ for @setRecognitionLanguages:@
setRecognitionLanguagesSelector :: Selector '[Id NSArray] ()
setRecognitionLanguagesSelector = mkSelector "setRecognitionLanguages:"

-- | @Selector@ for @customWords@
customWordsSelector :: Selector '[] (Id NSArray)
customWordsSelector = mkSelector "customWords"

-- | @Selector@ for @setCustomWords:@
setCustomWordsSelector :: Selector '[Id NSArray] ()
setCustomWordsSelector = mkSelector "setCustomWords:"

-- | @Selector@ for @recognitionLevel@
recognitionLevelSelector :: Selector '[] VNRequestTextRecognitionLevel
recognitionLevelSelector = mkSelector "recognitionLevel"

-- | @Selector@ for @setRecognitionLevel:@
setRecognitionLevelSelector :: Selector '[VNRequestTextRecognitionLevel] ()
setRecognitionLevelSelector = mkSelector "setRecognitionLevel:"

-- | @Selector@ for @usesLanguageCorrection@
usesLanguageCorrectionSelector :: Selector '[] Bool
usesLanguageCorrectionSelector = mkSelector "usesLanguageCorrection"

-- | @Selector@ for @setUsesLanguageCorrection:@
setUsesLanguageCorrectionSelector :: Selector '[Bool] ()
setUsesLanguageCorrectionSelector = mkSelector "setUsesLanguageCorrection:"

-- | @Selector@ for @automaticallyDetectsLanguage@
automaticallyDetectsLanguageSelector :: Selector '[] Bool
automaticallyDetectsLanguageSelector = mkSelector "automaticallyDetectsLanguage"

-- | @Selector@ for @setAutomaticallyDetectsLanguage:@
setAutomaticallyDetectsLanguageSelector :: Selector '[Bool] ()
setAutomaticallyDetectsLanguageSelector = mkSelector "setAutomaticallyDetectsLanguage:"

-- | @Selector@ for @minimumTextHeight@
minimumTextHeightSelector :: Selector '[] CFloat
minimumTextHeightSelector = mkSelector "minimumTextHeight"

-- | @Selector@ for @setMinimumTextHeight:@
setMinimumTextHeightSelector :: Selector '[CFloat] ()
setMinimumTextHeightSelector = mkSelector "setMinimumTextHeight:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

