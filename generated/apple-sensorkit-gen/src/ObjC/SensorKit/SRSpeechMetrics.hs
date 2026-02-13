{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSpeechMetrics@.
module ObjC.SensorKit.SRSpeechMetrics
  ( SRSpeechMetrics
  , IsSRSpeechMetrics(..)
  , init_
  , new
  , sessionIdentifier
  , sessionFlags
  , timestamp
  , timeSinceAudioStart
  , audioLevel
  , speechExpression
  , audioLevelSelector
  , initSelector
  , newSelector
  , sessionFlagsSelector
  , sessionIdentifierSelector
  , speechExpressionSelector
  , timeSinceAudioStartSelector
  , timestampSelector

  -- * Enum types
  , SRSpeechMetricsSessionFlags(SRSpeechMetricsSessionFlags)
  , pattern SRSpeechMetricsSessionFlagsDefault
  , pattern SRSpeechMetricsSessionFlagsBypassVoiceProcessing

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRSpeechMetrics)
init_ srSpeechMetrics =
  sendOwnedMessage srSpeechMetrics initSelector

-- | @+ new@
new :: IO (Id SRSpeechMetrics)
new  =
  do
    cls' <- getRequiredClass "SRSpeechMetrics"
    sendOwnedClassMessage cls' newSelector

-- | sessionIdentifier
--
-- Identifier of an audio session e.g., a Phone call or Siri utterance
--
-- ObjC selector: @- sessionIdentifier@
sessionIdentifier :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id NSString)
sessionIdentifier srSpeechMetrics =
  sendMessage srSpeechMetrics sessionIdentifierSelector

-- | @- sessionFlags@
sessionFlags :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO SRSpeechMetricsSessionFlags
sessionFlags srSpeechMetrics =
  sendMessage srSpeechMetrics sessionFlagsSelector

-- | timestamp
--
-- The wall time when this sample was generated
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id NSDate)
timestamp srSpeechMetrics =
  sendMessage srSpeechMetrics timestampSelector

-- | timeSinceAudioStart
--
-- The number of seconds since the start of the audio stream
--
-- When an audio stream like a phone call starts, @SRSpeechMetrics@ samples are collected periodically. This field can be used to determine where each sample falls in the audio stream
--
-- ObjC selector: @- timeSinceAudioStart@
timeSinceAudioStart :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO CDouble
timeSinceAudioStart srSpeechMetrics =
  sendMessage srSpeechMetrics timeSinceAudioStartSelector

-- | @- audioLevel@
audioLevel :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRAudioLevel)
audioLevel srSpeechMetrics =
  sendMessage srSpeechMetrics audioLevelSelector

-- | @- speechExpression@
speechExpression :: IsSRSpeechMetrics srSpeechMetrics => srSpeechMetrics -> IO (Id SRSpeechExpression)
speechExpression srSpeechMetrics =
  sendMessage srSpeechMetrics speechExpressionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRSpeechMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRSpeechMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @sessionIdentifier@
sessionIdentifierSelector :: Selector '[] (Id NSString)
sessionIdentifierSelector = mkSelector "sessionIdentifier"

-- | @Selector@ for @sessionFlags@
sessionFlagsSelector :: Selector '[] SRSpeechMetricsSessionFlags
sessionFlagsSelector = mkSelector "sessionFlags"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @timeSinceAudioStart@
timeSinceAudioStartSelector :: Selector '[] CDouble
timeSinceAudioStartSelector = mkSelector "timeSinceAudioStart"

-- | @Selector@ for @audioLevel@
audioLevelSelector :: Selector '[] (Id SRAudioLevel)
audioLevelSelector = mkSelector "audioLevel"

-- | @Selector@ for @speechExpression@
speechExpressionSelector :: Selector '[] (Id SRSpeechExpression)
speechExpressionSelector = mkSelector "speechExpression"

