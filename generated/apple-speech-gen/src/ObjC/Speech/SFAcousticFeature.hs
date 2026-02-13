{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The value of a voice analysis metric.
--
-- Generated bindings for @SFAcousticFeature@.
module ObjC.Speech.SFAcousticFeature
  ( SFAcousticFeature
  , IsSFAcousticFeature(..)
  , acousticFeatureValuePerFrame
  , frameDuration
  , acousticFeatureValuePerFrameSelector
  , frameDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of feature values, one value per audio frame, corresponding to a transcript segment of recorded audio.
--
-- ObjC selector: @- acousticFeatureValuePerFrame@
acousticFeatureValuePerFrame :: IsSFAcousticFeature sfAcousticFeature => sfAcousticFeature -> IO (Id NSArray)
acousticFeatureValuePerFrame sfAcousticFeature =
  sendMessage sfAcousticFeature acousticFeatureValuePerFrameSelector

-- | The duration of the audio frame.
--
-- ObjC selector: @- frameDuration@
frameDuration :: IsSFAcousticFeature sfAcousticFeature => sfAcousticFeature -> IO CDouble
frameDuration sfAcousticFeature =
  sendMessage sfAcousticFeature frameDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acousticFeatureValuePerFrame@
acousticFeatureValuePerFrameSelector :: Selector '[] (Id NSArray)
acousticFeatureValuePerFrameSelector = mkSelector "acousticFeatureValuePerFrame"

-- | @Selector@ for @frameDuration@
frameDurationSelector :: Selector '[] CDouble
frameDurationSelector = mkSelector "frameDuration"

