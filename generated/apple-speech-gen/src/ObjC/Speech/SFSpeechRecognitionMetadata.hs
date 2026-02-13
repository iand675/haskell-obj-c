{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The metadata of speech in the audio of a speech recognition request.
--
-- Generated bindings for @SFSpeechRecognitionMetadata@.
module ObjC.Speech.SFSpeechRecognitionMetadata
  ( SFSpeechRecognitionMetadata
  , IsSFSpeechRecognitionMetadata(..)
  , speakingRate
  , averagePauseDuration
  , speechStartTimestamp
  , speechDuration
  , voiceAnalytics
  , averagePauseDurationSelector
  , speakingRateSelector
  , speechDurationSelector
  , speechStartTimestampSelector
  , voiceAnalyticsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The number of words spoken per minute.
--
-- ObjC selector: @- speakingRate@
speakingRate :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speakingRate sfSpeechRecognitionMetadata =
  sendMessage sfSpeechRecognitionMetadata speakingRateSelector

-- | The average pause duration between words, measured in seconds.
--
-- ObjC selector: @- averagePauseDuration@
averagePauseDuration :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
averagePauseDuration sfSpeechRecognitionMetadata =
  sendMessage sfSpeechRecognitionMetadata averagePauseDurationSelector

-- | The start timestamp of speech in the audio.
--
-- ObjC selector: @- speechStartTimestamp@
speechStartTimestamp :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speechStartTimestamp sfSpeechRecognitionMetadata =
  sendMessage sfSpeechRecognitionMetadata speechStartTimestampSelector

-- | The duration in seconds of speech in the audio.
--
-- ObjC selector: @- speechDuration@
speechDuration :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speechDuration sfSpeechRecognitionMetadata =
  sendMessage sfSpeechRecognitionMetadata speechDurationSelector

-- | An analysis of the transcription segment's vocal properties.
--
-- ObjC selector: @- voiceAnalytics@
voiceAnalytics :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO (Id SFVoiceAnalytics)
voiceAnalytics sfSpeechRecognitionMetadata =
  sendMessage sfSpeechRecognitionMetadata voiceAnalyticsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speakingRate@
speakingRateSelector :: Selector '[] CDouble
speakingRateSelector = mkSelector "speakingRate"

-- | @Selector@ for @averagePauseDuration@
averagePauseDurationSelector :: Selector '[] CDouble
averagePauseDurationSelector = mkSelector "averagePauseDuration"

-- | @Selector@ for @speechStartTimestamp@
speechStartTimestampSelector :: Selector '[] CDouble
speechStartTimestampSelector = mkSelector "speechStartTimestamp"

-- | @Selector@ for @speechDuration@
speechDurationSelector :: Selector '[] CDouble
speechDurationSelector = mkSelector "speechDuration"

-- | @Selector@ for @voiceAnalytics@
voiceAnalyticsSelector :: Selector '[] (Id SFVoiceAnalytics)
voiceAnalyticsSelector = mkSelector "voiceAnalytics"

