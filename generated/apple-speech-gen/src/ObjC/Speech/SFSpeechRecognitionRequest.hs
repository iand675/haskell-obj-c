{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract class that represents a request to recognize speech from an audio source.
--
-- Don't create ``SFSpeechRecognitionRequest`` objects directly. Create an ``SFSpeechURLRecognitionRequest`` or ``SFSpeechAudioBufferRecognitionRequest`` object instead. Use the properties of this class to configure various aspects of your request object before you start the speech recognition process. For example, use the ``shouldReportPartialResults`` property to specify whether you want partial results or only the final result of speech recognition.
--
-- Generated bindings for @SFSpeechRecognitionRequest@.
module ObjC.Speech.SFSpeechRecognitionRequest
  ( SFSpeechRecognitionRequest
  , IsSFSpeechRecognitionRequest(..)
  , taskHint
  , setTaskHint
  , shouldReportPartialResults
  , setShouldReportPartialResults
  , contextualStrings
  , setContextualStrings
  , interactionIdentifier
  , setInteractionIdentifier
  , requiresOnDeviceRecognition
  , setRequiresOnDeviceRecognition
  , addsPunctuation
  , setAddsPunctuation
  , customizedLanguageModel
  , setCustomizedLanguageModel
  , taskHintSelector
  , setTaskHintSelector
  , shouldReportPartialResultsSelector
  , setShouldReportPartialResultsSelector
  , contextualStringsSelector
  , setContextualStringsSelector
  , interactionIdentifierSelector
  , setInteractionIdentifierSelector
  , requiresOnDeviceRecognitionSelector
  , setRequiresOnDeviceRecognitionSelector
  , addsPunctuationSelector
  , setAddsPunctuationSelector
  , customizedLanguageModelSelector
  , setCustomizedLanguageModelSelector

  -- * Enum types
  , SFSpeechRecognitionTaskHint(SFSpeechRecognitionTaskHint)
  , pattern SFSpeechRecognitionTaskHintUnspecified
  , pattern SFSpeechRecognitionTaskHintDictation
  , pattern SFSpeechRecognitionTaskHintSearch
  , pattern SFSpeechRecognitionTaskHintConfirmation

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

import ObjC.Speech.Internal.Classes
import ObjC.Speech.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A value that indicates the type of speech recognition being performed.
--
-- The default value of this property is ``SFSpeechRecognitionTaskHint/unspecified``. For a valid list of values, see ``SFSpeechRecognitionTaskHint``.
--
-- ObjC selector: @- taskHint@
taskHint :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO SFSpeechRecognitionTaskHint
taskHint sfSpeechRecognitionRequest  =
    fmap (coerce :: CLong -> SFSpeechRecognitionTaskHint) $ sendMsg sfSpeechRecognitionRequest (mkSelector "taskHint") retCLong []

-- | A value that indicates the type of speech recognition being performed.
--
-- The default value of this property is ``SFSpeechRecognitionTaskHint/unspecified``. For a valid list of values, see ``SFSpeechRecognitionTaskHint``.
--
-- ObjC selector: @- setTaskHint:@
setTaskHint :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> SFSpeechRecognitionTaskHint -> IO ()
setTaskHint sfSpeechRecognitionRequest  value =
    sendMsg sfSpeechRecognitionRequest (mkSelector "setTaskHint:") retVoid [argCLong (coerce value)]

-- | A Boolean value that indicates whether you want intermediate results returned for each utterance.
--
-- The default value of this property is @true@. If you want only final results (and you don't care about intermediate results), set this property to @false@ to prevent the system from doing extra work.
--
-- ObjC selector: @- shouldReportPartialResults@
shouldReportPartialResults :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO Bool
shouldReportPartialResults sfSpeechRecognitionRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognitionRequest (mkSelector "shouldReportPartialResults") retCULong []

-- | A Boolean value that indicates whether you want intermediate results returned for each utterance.
--
-- The default value of this property is @true@. If you want only final results (and you don't care about intermediate results), set this property to @false@ to prevent the system from doing extra work.
--
-- ObjC selector: @- setShouldReportPartialResults:@
setShouldReportPartialResults :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> Bool -> IO ()
setShouldReportPartialResults sfSpeechRecognitionRequest  value =
    sendMsg sfSpeechRecognitionRequest (mkSelector "setShouldReportPartialResults:") retVoid [argCULong (if value then 1 else 0)]

-- | An array of phrases that should be recognized, even if they are not in the system vocabulary.
--
-- Use this property to specify short custom phrases that are unique to your app. You might include phrases with the names of characters, products, or places that are specific to your app. You might also include domain-specific terminology or unusual or made-up words. Assigning custom phrases to this property improves the likelihood of those phrases being recognized.
--
-- Keep phrases relatively brief, limiting them to one or two words whenever possible. Lengthy phrases are less likely to be recognized. In addition, try to limit each phrase to something the user can say without pausing.
--
-- Limit the total number of phrases to no more than 100.
--
-- ObjC selector: @- contextualStrings@
contextualStrings :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO (Id NSArray)
contextualStrings sfSpeechRecognitionRequest  =
    sendMsg sfSpeechRecognitionRequest (mkSelector "contextualStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of phrases that should be recognized, even if they are not in the system vocabulary.
--
-- Use this property to specify short custom phrases that are unique to your app. You might include phrases with the names of characters, products, or places that are specific to your app. You might also include domain-specific terminology or unusual or made-up words. Assigning custom phrases to this property improves the likelihood of those phrases being recognized.
--
-- Keep phrases relatively brief, limiting them to one or two words whenever possible. Lengthy phrases are less likely to be recognized. In addition, try to limit each phrase to something the user can say without pausing.
--
-- Limit the total number of phrases to no more than 100.
--
-- ObjC selector: @- setContextualStrings:@
setContextualStrings :: (IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest, IsNSArray value) => sfSpeechRecognitionRequest -> value -> IO ()
setContextualStrings sfSpeechRecognitionRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg sfSpeechRecognitionRequest (mkSelector "setContextualStrings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An identifier string that you use to describe the type of interaction associated with the speech recognition request.
--
-- If different parts of your app have different speech recognition needs, you can use this property to identify the part of your app that is making each request. For example, if one part of your app lets users speak phone numbers and another part lets users speak street addresses, consistently identifying the part of the app that makes a recognition request may help improve the accuracy of the results.
--
-- ObjC selector: @- interactionIdentifier@
interactionIdentifier :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO RawId
interactionIdentifier sfSpeechRecognitionRequest  =
    fmap (RawId . castPtr) $ sendMsg sfSpeechRecognitionRequest (mkSelector "interactionIdentifier") (retPtr retVoid) []

-- | An identifier string that you use to describe the type of interaction associated with the speech recognition request.
--
-- If different parts of your app have different speech recognition needs, you can use this property to identify the part of your app that is making each request. For example, if one part of your app lets users speak phone numbers and another part lets users speak street addresses, consistently identifying the part of the app that makes a recognition request may help improve the accuracy of the results.
--
-- ObjC selector: @- setInteractionIdentifier:@
setInteractionIdentifier :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> RawId -> IO ()
setInteractionIdentifier sfSpeechRecognitionRequest  value =
    sendMsg sfSpeechRecognitionRequest (mkSelector "setInteractionIdentifier:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | A Boolean value that determines whether a request must keep its audio data on the device.
--
-- Set this property to @true@ to prevent an ``SFSpeechRecognitionRequest`` from sending audio over the network. However, on-device requests won't be as accurate.
--
-- > Note: > The request only honors this setting if the ``SFSpeechRecognizer/supportsOnDeviceRecognition`` (``SFSpeechRecognizer``) property is also @true@.
--
-- ObjC selector: @- requiresOnDeviceRecognition@
requiresOnDeviceRecognition :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO Bool
requiresOnDeviceRecognition sfSpeechRecognitionRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognitionRequest (mkSelector "requiresOnDeviceRecognition") retCULong []

-- | A Boolean value that determines whether a request must keep its audio data on the device.
--
-- Set this property to @true@ to prevent an ``SFSpeechRecognitionRequest`` from sending audio over the network. However, on-device requests won't be as accurate.
--
-- > Note: > The request only honors this setting if the ``SFSpeechRecognizer/supportsOnDeviceRecognition`` (``SFSpeechRecognizer``) property is also @true@.
--
-- ObjC selector: @- setRequiresOnDeviceRecognition:@
setRequiresOnDeviceRecognition :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> Bool -> IO ()
setRequiresOnDeviceRecognition sfSpeechRecognitionRequest  value =
    sendMsg sfSpeechRecognitionRequest (mkSelector "setRequiresOnDeviceRecognition:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to add punctuation to speech recognition results.
--
-- Set this property to @true@ for the speech framework to automatically include punctuation in the recognition results. Punctuation includes a period or question mark at the end of a sentence, and a comma within a sentence.
--
-- ObjC selector: @- addsPunctuation@
addsPunctuation :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO Bool
addsPunctuation sfSpeechRecognitionRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognitionRequest (mkSelector "addsPunctuation") retCULong []

-- | A Boolean value that indicates whether to add punctuation to speech recognition results.
--
-- Set this property to @true@ for the speech framework to automatically include punctuation in the recognition results. Punctuation includes a period or question mark at the end of a sentence, and a comma within a sentence.
--
-- ObjC selector: @- setAddsPunctuation:@
setAddsPunctuation :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> Bool -> IO ()
setAddsPunctuation sfSpeechRecognitionRequest  value =
    sendMsg sfSpeechRecognitionRequest (mkSelector "setAddsPunctuation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- customizedLanguageModel@
customizedLanguageModel :: IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest => sfSpeechRecognitionRequest -> IO (Id SFSpeechLanguageModelConfiguration)
customizedLanguageModel sfSpeechRecognitionRequest  =
    sendMsg sfSpeechRecognitionRequest (mkSelector "customizedLanguageModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizedLanguageModel:@
setCustomizedLanguageModel :: (IsSFSpeechRecognitionRequest sfSpeechRecognitionRequest, IsSFSpeechLanguageModelConfiguration value) => sfSpeechRecognitionRequest -> value -> IO ()
setCustomizedLanguageModel sfSpeechRecognitionRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg sfSpeechRecognitionRequest (mkSelector "setCustomizedLanguageModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @taskHint@
taskHintSelector :: Selector
taskHintSelector = mkSelector "taskHint"

-- | @Selector@ for @setTaskHint:@
setTaskHintSelector :: Selector
setTaskHintSelector = mkSelector "setTaskHint:"

-- | @Selector@ for @shouldReportPartialResults@
shouldReportPartialResultsSelector :: Selector
shouldReportPartialResultsSelector = mkSelector "shouldReportPartialResults"

-- | @Selector@ for @setShouldReportPartialResults:@
setShouldReportPartialResultsSelector :: Selector
setShouldReportPartialResultsSelector = mkSelector "setShouldReportPartialResults:"

-- | @Selector@ for @contextualStrings@
contextualStringsSelector :: Selector
contextualStringsSelector = mkSelector "contextualStrings"

-- | @Selector@ for @setContextualStrings:@
setContextualStringsSelector :: Selector
setContextualStringsSelector = mkSelector "setContextualStrings:"

-- | @Selector@ for @interactionIdentifier@
interactionIdentifierSelector :: Selector
interactionIdentifierSelector = mkSelector "interactionIdentifier"

-- | @Selector@ for @setInteractionIdentifier:@
setInteractionIdentifierSelector :: Selector
setInteractionIdentifierSelector = mkSelector "setInteractionIdentifier:"

-- | @Selector@ for @requiresOnDeviceRecognition@
requiresOnDeviceRecognitionSelector :: Selector
requiresOnDeviceRecognitionSelector = mkSelector "requiresOnDeviceRecognition"

-- | @Selector@ for @setRequiresOnDeviceRecognition:@
setRequiresOnDeviceRecognitionSelector :: Selector
setRequiresOnDeviceRecognitionSelector = mkSelector "setRequiresOnDeviceRecognition:"

-- | @Selector@ for @addsPunctuation@
addsPunctuationSelector :: Selector
addsPunctuationSelector = mkSelector "addsPunctuation"

-- | @Selector@ for @setAddsPunctuation:@
setAddsPunctuationSelector :: Selector
setAddsPunctuationSelector = mkSelector "setAddsPunctuation:"

-- | @Selector@ for @customizedLanguageModel@
customizedLanguageModelSelector :: Selector
customizedLanguageModelSelector = mkSelector "customizedLanguageModel"

-- | @Selector@ for @setCustomizedLanguageModel:@
setCustomizedLanguageModelSelector :: Selector
setCustomizedLanguageModelSelector = mkSelector "setCustomizedLanguageModel:"

