{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangePlaybackRateCommandEvent@.
module ObjC.MediaPlayer.MPChangePlaybackRateCommandEvent
  ( MPChangePlaybackRateCommandEvent
  , IsMPChangePlaybackRateCommandEvent(..)
  , playbackRate
  , playbackRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The chosen playback rate for this command event. This value will be equal to one of the values specified in the supportedPlaybackRates array for the MPChangePlaybackRateCommand object.
--
-- ObjC selector: @- playbackRate@
playbackRate :: IsMPChangePlaybackRateCommandEvent mpChangePlaybackRateCommandEvent => mpChangePlaybackRateCommandEvent -> IO CFloat
playbackRate mpChangePlaybackRateCommandEvent =
  sendMessage mpChangePlaybackRateCommandEvent playbackRateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playbackRate@
playbackRateSelector :: Selector '[] CFloat
playbackRateSelector = mkSelector "playbackRate"

