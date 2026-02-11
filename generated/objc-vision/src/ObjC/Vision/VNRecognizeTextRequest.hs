{-# LANGUAGE PatternSynonyms #-}
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
  , supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector
  , supportedRecognitionLanguagesAndReturnErrorSelector
  , recognitionLanguagesSelector
  , setRecognitionLanguagesSelector
  , customWordsSelector
  , setCustomWordsSelector
  , recognitionLevelSelector
  , setRecognitionLevelSelector
  , usesLanguageCorrectionSelector
  , setUsesLanguageCorrectionSelector
  , automaticallyDetectsLanguageSelector
  , setAutomaticallyDetectsLanguageSelector
  , minimumTextHeightSelector
  , setMinimumTextHeightSelector
  , resultsSelector

  -- * Enum types
  , VNRequestTextRecognitionLevel(VNRequestTextRecognitionLevel)
  , pattern VNRequestTextRecognitionLevelAccurate
  , pattern VNRequestTextRecognitionLevelFast

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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:") (retPtr retVoid) [argCLong (coerce recognitionLevel), argCULong (fromIntegral requestRevision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
supportedRecognitionLanguagesAndReturnError vnRecognizeTextRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnRecognizeTextRequest (mkSelector "supportedRecognitionLanguagesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Specify the languages used for the detection. The order of the languages in the array defines the order in which languages will be used during the language processing. The languages are specified as ISO language codes.
--
-- ObjC selector: @- recognitionLanguages@
recognitionLanguages :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
recognitionLanguages vnRecognizeTextRequest  =
  sendMsg vnRecognizeTextRequest (mkSelector "recognitionLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specify the languages used for the detection. The order of the languages in the array defines the order in which languages will be used during the language processing. The languages are specified as ISO language codes.
--
-- ObjC selector: @- setRecognitionLanguages:@
setRecognitionLanguages :: (IsVNRecognizeTextRequest vnRecognizeTextRequest, IsNSArray value) => vnRecognizeTextRequest -> value -> IO ()
setRecognitionLanguages vnRecognizeTextRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnRecognizeTextRequest (mkSelector "setRecognitionLanguages:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An array of strings that will be used at the word recognition stage in addition to the recognition languages. The customWords list takes precedence over the standard lexicon.
--
-- ObjC selector: @- customWords@
customWords :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
customWords vnRecognizeTextRequest  =
  sendMsg vnRecognizeTextRequest (mkSelector "customWords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of strings that will be used at the word recognition stage in addition to the recognition languages. The customWords list takes precedence over the standard lexicon.
--
-- ObjC selector: @- setCustomWords:@
setCustomWords :: (IsVNRecognizeTextRequest vnRecognizeTextRequest, IsNSArray value) => vnRecognizeTextRequest -> value -> IO ()
setCustomWords vnRecognizeTextRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vnRecognizeTextRequest (mkSelector "setCustomWords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The recognition level selects which techniques will be used during the text recognition. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- recognitionLevel@
recognitionLevel :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO VNRequestTextRecognitionLevel
recognitionLevel vnRecognizeTextRequest  =
  fmap (coerce :: CLong -> VNRequestTextRecognitionLevel) $ sendMsg vnRecognizeTextRequest (mkSelector "recognitionLevel") retCLong []

-- | The recognition level selects which techniques will be used during the text recognition. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- setRecognitionLevel:@
setRecognitionLevel :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> VNRequestTextRecognitionLevel -> IO ()
setRecognitionLevel vnRecognizeTextRequest  value =
  sendMsg vnRecognizeTextRequest (mkSelector "setRecognitionLevel:") retVoid [argCLong (coerce value)]

-- | Determines whether language correction should be applied during the recognition process. Disabling this will return the raw recognition results providing performance benefits but less accurate results.
--
-- ObjC selector: @- usesLanguageCorrection@
usesLanguageCorrection :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO Bool
usesLanguageCorrection vnRecognizeTextRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnRecognizeTextRequest (mkSelector "usesLanguageCorrection") retCULong []

-- | Determines whether language correction should be applied during the recognition process. Disabling this will return the raw recognition results providing performance benefits but less accurate results.
--
-- ObjC selector: @- setUsesLanguageCorrection:@
setUsesLanguageCorrection :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> Bool -> IO ()
setUsesLanguageCorrection vnRecognizeTextRequest  value =
  sendMsg vnRecognizeTextRequest (mkSelector "setUsesLanguageCorrection:") retVoid [argCULong (if value then 1 else 0)]

-- | Language detection will try to automatically identify the script/langauge during the detection and use the appropiate model for recognition and language correction. This can be particularly helpful, if the nature of the content is unkown and with this flag being set it will for instance determine if text is latin vs chinese so you don't have to pick the language model in the first case. But as the language correction cannot always guarantee the correct detection, it is advisable to set the languages, if you have domain knowledge of what language to expect. The default value is NO. Also note that this feature is only available since VNRecognizeTextRequestRevision3 and is a no-op before that.
--
-- ObjC selector: @- automaticallyDetectsLanguage@
automaticallyDetectsLanguage :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO Bool
automaticallyDetectsLanguage vnRecognizeTextRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnRecognizeTextRequest (mkSelector "automaticallyDetectsLanguage") retCULong []

-- | Language detection will try to automatically identify the script/langauge during the detection and use the appropiate model for recognition and language correction. This can be particularly helpful, if the nature of the content is unkown and with this flag being set it will for instance determine if text is latin vs chinese so you don't have to pick the language model in the first case. But as the language correction cannot always guarantee the correct detection, it is advisable to set the languages, if you have domain knowledge of what language to expect. The default value is NO. Also note that this feature is only available since VNRecognizeTextRequestRevision3 and is a no-op before that.
--
-- ObjC selector: @- setAutomaticallyDetectsLanguage:@
setAutomaticallyDetectsLanguage :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> Bool -> IO ()
setAutomaticallyDetectsLanguage vnRecognizeTextRequest  value =
  sendMsg vnRecognizeTextRequest (mkSelector "setAutomaticallyDetectsLanguage:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minimumTextHeight@
minimumTextHeight :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO CFloat
minimumTextHeight vnRecognizeTextRequest  =
  sendMsg vnRecognizeTextRequest (mkSelector "minimumTextHeight") retCFloat []

-- | @- setMinimumTextHeight:@
setMinimumTextHeight :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> CFloat -> IO ()
setMinimumTextHeight vnRecognizeTextRequest  value =
  sendMsg vnRecognizeTextRequest (mkSelector "setMinimumTextHeight:") retVoid [argCFloat (fromIntegral value)]

-- | VNRecognizedTextObservation results.
--
-- ObjC selector: @- results@
results :: IsVNRecognizeTextRequest vnRecognizeTextRequest => vnRecognizeTextRequest -> IO (Id NSArray)
results vnRecognizeTextRequest  =
  sendMsg vnRecognizeTextRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:@
supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector :: Selector
supportedRecognitionLanguagesForTextRecognitionLevel_revision_errorSelector = mkSelector "supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:"

-- | @Selector@ for @supportedRecognitionLanguagesAndReturnError:@
supportedRecognitionLanguagesAndReturnErrorSelector :: Selector
supportedRecognitionLanguagesAndReturnErrorSelector = mkSelector "supportedRecognitionLanguagesAndReturnError:"

-- | @Selector@ for @recognitionLanguages@
recognitionLanguagesSelector :: Selector
recognitionLanguagesSelector = mkSelector "recognitionLanguages"

-- | @Selector@ for @setRecognitionLanguages:@
setRecognitionLanguagesSelector :: Selector
setRecognitionLanguagesSelector = mkSelector "setRecognitionLanguages:"

-- | @Selector@ for @customWords@
customWordsSelector :: Selector
customWordsSelector = mkSelector "customWords"

-- | @Selector@ for @setCustomWords:@
setCustomWordsSelector :: Selector
setCustomWordsSelector = mkSelector "setCustomWords:"

-- | @Selector@ for @recognitionLevel@
recognitionLevelSelector :: Selector
recognitionLevelSelector = mkSelector "recognitionLevel"

-- | @Selector@ for @setRecognitionLevel:@
setRecognitionLevelSelector :: Selector
setRecognitionLevelSelector = mkSelector "setRecognitionLevel:"

-- | @Selector@ for @usesLanguageCorrection@
usesLanguageCorrectionSelector :: Selector
usesLanguageCorrectionSelector = mkSelector "usesLanguageCorrection"

-- | @Selector@ for @setUsesLanguageCorrection:@
setUsesLanguageCorrectionSelector :: Selector
setUsesLanguageCorrectionSelector = mkSelector "setUsesLanguageCorrection:"

-- | @Selector@ for @automaticallyDetectsLanguage@
automaticallyDetectsLanguageSelector :: Selector
automaticallyDetectsLanguageSelector = mkSelector "automaticallyDetectsLanguage"

-- | @Selector@ for @setAutomaticallyDetectsLanguage:@
setAutomaticallyDetectsLanguageSelector :: Selector
setAutomaticallyDetectsLanguageSelector = mkSelector "setAutomaticallyDetectsLanguage:"

-- | @Selector@ for @minimumTextHeight@
minimumTextHeightSelector :: Selector
minimumTextHeightSelector = mkSelector "minimumTextHeight"

-- | @Selector@ for @setMinimumTextHeight:@
setMinimumTextHeightSelector :: Selector
setMinimumTextHeightSelector = mkSelector "setMinimumTextHeight:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

