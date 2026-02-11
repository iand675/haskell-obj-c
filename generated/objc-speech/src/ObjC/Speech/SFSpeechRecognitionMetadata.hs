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
  , speakingRateSelector
  , averagePauseDurationSelector
  , speechStartTimestampSelector
  , speechDurationSelector
  , voiceAnalyticsSelector


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
import ObjC.Foundation.Internal.Classes

-- | The number of words spoken per minute.
--
-- ObjC selector: @- speakingRate@
speakingRate :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speakingRate sfSpeechRecognitionMetadata  =
  sendMsg sfSpeechRecognitionMetadata (mkSelector "speakingRate") retCDouble []

-- | The average pause duration between words, measured in seconds.
--
-- ObjC selector: @- averagePauseDuration@
averagePauseDuration :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
averagePauseDuration sfSpeechRecognitionMetadata  =
  sendMsg sfSpeechRecognitionMetadata (mkSelector "averagePauseDuration") retCDouble []

-- | The start timestamp of speech in the audio.
--
-- ObjC selector: @- speechStartTimestamp@
speechStartTimestamp :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speechStartTimestamp sfSpeechRecognitionMetadata  =
  sendMsg sfSpeechRecognitionMetadata (mkSelector "speechStartTimestamp") retCDouble []

-- | The duration in seconds of speech in the audio.
--
-- ObjC selector: @- speechDuration@
speechDuration :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO CDouble
speechDuration sfSpeechRecognitionMetadata  =
  sendMsg sfSpeechRecognitionMetadata (mkSelector "speechDuration") retCDouble []

-- | An analysis of the transcription segment's vocal properties.
--
-- ObjC selector: @- voiceAnalytics@
voiceAnalytics :: IsSFSpeechRecognitionMetadata sfSpeechRecognitionMetadata => sfSpeechRecognitionMetadata -> IO (Id SFVoiceAnalytics)
voiceAnalytics sfSpeechRecognitionMetadata  =
  sendMsg sfSpeechRecognitionMetadata (mkSelector "voiceAnalytics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @speakingRate@
speakingRateSelector :: Selector
speakingRateSelector = mkSelector "speakingRate"

-- | @Selector@ for @averagePauseDuration@
averagePauseDurationSelector :: Selector
averagePauseDurationSelector = mkSelector "averagePauseDuration"

-- | @Selector@ for @speechStartTimestamp@
speechStartTimestampSelector :: Selector
speechStartTimestampSelector = mkSelector "speechStartTimestamp"

-- | @Selector@ for @speechDuration@
speechDurationSelector :: Selector
speechDurationSelector = mkSelector "speechDuration"

-- | @Selector@ for @voiceAnalytics@
voiceAnalyticsSelector :: Selector
voiceAnalyticsSelector = mkSelector "voiceAnalytics"

