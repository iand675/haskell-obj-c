{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A textual representation of the specified speech in its entirety, as recognized by the speech recognizer.
--
-- Use @SFTranscription@ to obtain all the recognized utterances from your audio content. An _utterance_ is a vocalized word or group of words that represent a single meaning to the speech recognizer (``SFSpeechRecognizer``).
--
-- Use the ``formattedString`` property to retrieve the entire transcription of utterances, or use the ``segments`` property to retrieve an individual utterance (``SFTranscriptionSegment``).
--
-- You don't create an @SFTranscription@ directly. Instead, you retrieve it from an ``SFSpeechRecognitionResult`` instance. The speech recognizer sends a speech recognition result to your app in one of two ways, depending on how your app started a speech recognition task.
--
-- You can start a speech recognition task by using the speech recognizer's ``SFSpeechRecognizer/recognitionTask(with:resultHandler:)`` method. When the task is complete, the speech recognizer sends an ``SFSpeechRecognitionResult`` instance to your @resultHandler@ closure. Alternatively, you can use the speech recognizer's ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` method to start a speech recognition task. When the task is complete, the speech recognizer uses your ``SFSpeechRecognitionTaskDelegate`` to send an ``SFSpeechRecognitionResult`` by using the delegate's ``SFSpeechRecognitionTaskDelegate/speechRecognitionTask(_:didFinishRecognition:)`` method.
--
-- An @SFTranscription@ represents only a potential version of the speech. It might not be an accurate representation of the utterances.
--
-- Generated bindings for @SFTranscription@.
module ObjC.Speech.SFTranscription
  ( SFTranscription
  , IsSFTranscription(..)
  , formattedString
  , segments
  , speakingRate
  , averagePauseDuration
  , averagePauseDurationSelector
  , formattedStringSelector
  , segmentsSelector
  , speakingRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The entire transcription of utterances, formatted into a single, user-displayable string.
--
-- ObjC selector: @- formattedString@
formattedString :: IsSFTranscription sfTranscription => sfTranscription -> IO (Id NSString)
formattedString sfTranscription =
  sendMessage sfTranscription formattedStringSelector

-- | An array of transcription segments that represent the parts of the transcription, as identified by the speech recognizer.
--
-- The order of the segments in the array matches the order in which the corresponding utterances occur in the spoken content.
--
-- ObjC selector: @- segments@
segments :: IsSFTranscription sfTranscription => sfTranscription -> IO (Id NSArray)
segments sfTranscription =
  sendMessage sfTranscription segmentsSelector

-- | The number of words spoken per minute.
--
-- ObjC selector: @- speakingRate@
speakingRate :: IsSFTranscription sfTranscription => sfTranscription -> IO CDouble
speakingRate sfTranscription =
  sendMessage sfTranscription speakingRateSelector

-- | The average pause duration between words, measured in seconds.
--
-- ObjC selector: @- averagePauseDuration@
averagePauseDuration :: IsSFTranscription sfTranscription => sfTranscription -> IO CDouble
averagePauseDuration sfTranscription =
  sendMessage sfTranscription averagePauseDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @formattedString@
formattedStringSelector :: Selector '[] (Id NSString)
formattedStringSelector = mkSelector "formattedString"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @speakingRate@
speakingRateSelector :: Selector '[] CDouble
speakingRateSelector = mkSelector "speakingRate"

-- | @Selector@ for @averagePauseDuration@
averagePauseDurationSelector :: Selector '[] CDouble
averagePauseDurationSelector = mkSelector "averagePauseDuration"

