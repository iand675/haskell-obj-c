{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of vocal analysis metrics.
--
-- Use an ``SFAcousticFeature`` object to access the @SFVoiceAnalytics@ insights. Voice analytics include the following features:
--
-- - Use ``jitter`` to measure how pitch varies in audio. - Use ``shimmer`` to measure how amplitude varies in audio. - Use ``pitch`` to measure the highness and lowness of the tone. - Use ``voicing`` to identify voiced regions in speech.
--
-- These results are part of the ``SFTranscriptionSegment`` object and are available when the system sends the ``SFSpeechRecognitionResult/isFinal`` flag.
--
-- Generated bindings for @SFVoiceAnalytics@.
module ObjC.Speech.SFVoiceAnalytics
  ( SFVoiceAnalytics
  , IsSFVoiceAnalytics(..)
  , jitter
  , shimmer
  , pitch
  , voicing
  , jitterSelector
  , pitchSelector
  , shimmerSelector
  , voicingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The variation in pitch in each frame of a transcription segment, expressed as a percentage of the frame's fundamental frequency.
--
-- ObjC selector: @- jitter@
jitter :: IsSFVoiceAnalytics sfVoiceAnalytics => sfVoiceAnalytics -> IO (Id SFAcousticFeature)
jitter sfVoiceAnalytics =
  sendMessage sfVoiceAnalytics jitterSelector

-- | The variation in vocal volume stability (amplitude) in each frame of a transcription segment, expressed in decibels.
--
-- ObjC selector: @- shimmer@
shimmer :: IsSFVoiceAnalytics sfVoiceAnalytics => sfVoiceAnalytics -> IO (Id SFAcousticFeature)
shimmer sfVoiceAnalytics =
  sendMessage sfVoiceAnalytics shimmerSelector

-- | The highness or lowness of the tone (fundamental frequency) in each frame of a transcription segment, expressed as a logarithm.
--
-- The value is a logarithm (base @e@) of the normalized pitch estimate for each frame.
--
-- ObjC selector: @- pitch@
pitch :: IsSFVoiceAnalytics sfVoiceAnalytics => sfVoiceAnalytics -> IO (Id SFAcousticFeature)
pitch sfVoiceAnalytics =
  sendMessage sfVoiceAnalytics pitchSelector

-- | The likelihood of a voice in each frame of a transcription segment.
--
-- The @voicing@ value is expressed as a probability in the range @[0.0, 1.0]@.
--
-- ObjC selector: @- voicing@
voicing :: IsSFVoiceAnalytics sfVoiceAnalytics => sfVoiceAnalytics -> IO (Id SFAcousticFeature)
voicing sfVoiceAnalytics =
  sendMessage sfVoiceAnalytics voicingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jitter@
jitterSelector :: Selector '[] (Id SFAcousticFeature)
jitterSelector = mkSelector "jitter"

-- | @Selector@ for @shimmer@
shimmerSelector :: Selector '[] (Id SFAcousticFeature)
shimmerSelector = mkSelector "shimmer"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] (Id SFAcousticFeature)
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @voicing@
voicingSelector :: Selector '[] (Id SFAcousticFeature)
voicingSelector = mkSelector "voicing"

