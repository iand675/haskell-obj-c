{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSpeechExpression@.
module ObjC.SensorKit.SRSpeechExpression
  ( SRSpeechExpression
  , IsSRSpeechExpression(..)
  , init_
  , new
  , version
  , confidence
  , mood
  , valence
  , activation
  , dominance
  , activationSelector
  , confidenceSelector
  , dominanceSelector
  , initSelector
  , moodSelector
  , newSelector
  , valenceSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO (Id SRSpeechExpression)
init_ srSpeechExpression =
  sendOwnedMessage srSpeechExpression initSelector

-- | @+ new@
new :: IO (Id SRSpeechExpression)
new  =
  do
    cls' <- getRequiredClass "SRSpeechExpression"
    sendOwnedClassMessage cls' newSelector

-- | version
--
-- Version of the algorithm used to generate @SRSpeechExpression@
--
-- ObjC selector: @- version@
version :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO (Id NSString)
version srSpeechExpression =
  sendMessage srSpeechExpression versionSelector

-- | confidence
--
-- The level of confidence normalized to [0, 1], where 1 is most confident
--
-- ObjC selector: @- confidence@
confidence :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
confidence srSpeechExpression =
  sendMessage srSpeechExpression confidenceSelector

-- | mood
--
-- Indicator of how slurry/tired/exhausted the speaker sounds as opposed to normal.
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- mood@
mood :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
mood srSpeechExpression =
  sendMessage srSpeechExpression moodSelector

-- | valence
--
-- Degree of (perceived) positive or negative emotion/sentiment from voice
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- valence@
valence :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
valence srSpeechExpression =
  sendMessage srSpeechExpression valenceSelector

-- | activation
--
-- Level of energy or activation (perceived) in voice
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- activation@
activation :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
activation srSpeechExpression =
  sendMessage srSpeechExpression activationSelector

-- | dominance
--
-- Degree of how strong or meek a person sounds (perceptually)
--
-- on a scale from -1 to 1, where negative scores indicate 'negative' sentiment, and positive scores indicate 'positive' sentiment.
--
-- ObjC selector: @- dominance@
dominance :: IsSRSpeechExpression srSpeechExpression => srSpeechExpression -> IO CDouble
dominance srSpeechExpression =
  sendMessage srSpeechExpression dominanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRSpeechExpression)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRSpeechExpression)
newSelector = mkSelector "new"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CDouble
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @mood@
moodSelector :: Selector '[] CDouble
moodSelector = mkSelector "mood"

-- | @Selector@ for @valence@
valenceSelector :: Selector '[] CDouble
valenceSelector = mkSelector "valence"

-- | @Selector@ for @activation@
activationSelector :: Selector '[] CDouble
activationSelector = mkSelector "activation"

-- | @Selector@ for @dominance@
dominanceSelector :: Selector '[] CDouble
dominanceSelector = mkSelector "dominance"

