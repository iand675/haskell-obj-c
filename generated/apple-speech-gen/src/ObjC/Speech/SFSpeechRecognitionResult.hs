{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains the partial or final results of a speech recognition request.
--
-- Use an @SFSpeechRecognitionResult@ object to retrieve the results of a speech recognition request. You don't create these objects directly. Instead, the Speech framework creates them and passes them to the handler block or delegate object you specified when starting your speech recognition task.
--
-- A speech recognition result object contains one or more ``transcriptions`` of the current utterance. Each transcription has a confidence rating indicating how likely it is to be correct. You can also get the transcription with the highest rating directly from the ``bestTranscription`` property.
--
-- If you requested partial results from the speech recognizer, the transcriptions may represent only part of the total audio content. Use the ``isFinal`` property to determine if the request contains partial or final results.
--
-- Generated bindings for @SFSpeechRecognitionResult@.
module ObjC.Speech.SFSpeechRecognitionResult
  ( SFSpeechRecognitionResult
  , IsSFSpeechRecognitionResult(..)
  , bestTranscription
  , transcriptions
  , final
  , speechRecognitionMetadata
  , bestTranscriptionSelector
  , finalSelector
  , speechRecognitionMetadataSelector
  , transcriptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The transcription with the highest confidence level.
--
-- ObjC selector: @- bestTranscription@
bestTranscription :: IsSFSpeechRecognitionResult sfSpeechRecognitionResult => sfSpeechRecognitionResult -> IO (Id SFTranscription)
bestTranscription sfSpeechRecognitionResult =
  sendMessage sfSpeechRecognitionResult bestTranscriptionSelector

-- | An array of potential transcriptions, sorted in descending order of confidence.
--
-- All transcriptions correspond to the same utterance, which can be a partial or final result of the overall request. The first transcription in the array has the highest confidence rating, followed by transcriptions with decreasing confidence ratings.
--
-- ObjC selector: @- transcriptions@
transcriptions :: IsSFSpeechRecognitionResult sfSpeechRecognitionResult => sfSpeechRecognitionResult -> IO (Id NSArray)
transcriptions sfSpeechRecognitionResult =
  sendMessage sfSpeechRecognitionResult transcriptionsSelector

-- | A Boolean value that indicates whether speech recognition is complete and whether the transcriptions are final.
--
-- When a speech recognition request is final, its transcriptions don't change.
--
-- ObjC selector: @- final@
final :: IsSFSpeechRecognitionResult sfSpeechRecognitionResult => sfSpeechRecognitionResult -> IO Bool
final sfSpeechRecognitionResult =
  sendMessage sfSpeechRecognitionResult finalSelector

-- | An object that contains the metadata results for a speech recognition request.
--
-- ObjC selector: @- speechRecognitionMetadata@
speechRecognitionMetadata :: IsSFSpeechRecognitionResult sfSpeechRecognitionResult => sfSpeechRecognitionResult -> IO (Id SFSpeechRecognitionMetadata)
speechRecognitionMetadata sfSpeechRecognitionResult =
  sendMessage sfSpeechRecognitionResult speechRecognitionMetadataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bestTranscription@
bestTranscriptionSelector :: Selector '[] (Id SFTranscription)
bestTranscriptionSelector = mkSelector "bestTranscription"

-- | @Selector@ for @transcriptions@
transcriptionsSelector :: Selector '[] (Id NSArray)
transcriptionsSelector = mkSelector "transcriptions"

-- | @Selector@ for @final@
finalSelector :: Selector '[] Bool
finalSelector = mkSelector "final"

-- | @Selector@ for @speechRecognitionMetadata@
speechRecognitionMetadataSelector :: Selector '[] (Id SFSpeechRecognitionMetadata)
speechRecognitionMetadataSelector = mkSelector "speechRecognitionMetadata"

