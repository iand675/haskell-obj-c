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

-- | An array of feature values, one value per audio frame, corresponding to a transcript segment of recorded audio.
--
-- ObjC selector: @- acousticFeatureValuePerFrame@
acousticFeatureValuePerFrame :: IsSFAcousticFeature sfAcousticFeature => sfAcousticFeature -> IO (Id NSArray)
acousticFeatureValuePerFrame sfAcousticFeature  =
  sendMsg sfAcousticFeature (mkSelector "acousticFeatureValuePerFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration of the audio frame.
--
-- ObjC selector: @- frameDuration@
frameDuration :: IsSFAcousticFeature sfAcousticFeature => sfAcousticFeature -> IO CDouble
frameDuration sfAcousticFeature  =
  sendMsg sfAcousticFeature (mkSelector "frameDuration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acousticFeatureValuePerFrame@
acousticFeatureValuePerFrameSelector :: Selector
acousticFeatureValuePerFrameSelector = mkSelector "acousticFeatureValuePerFrame"

-- | @Selector@ for @frameDuration@
frameDurationSelector :: Selector
frameDurationSelector = mkSelector "frameDuration"

