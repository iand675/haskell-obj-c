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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The chosen playback rate for this command event. This value will be equal to one of the values specified in the supportedPlaybackRates array for the MPChangePlaybackRateCommand object.
--
-- ObjC selector: @- playbackRate@
playbackRate :: IsMPChangePlaybackRateCommandEvent mpChangePlaybackRateCommandEvent => mpChangePlaybackRateCommandEvent -> IO CFloat
playbackRate mpChangePlaybackRateCommandEvent  =
  sendMsg mpChangePlaybackRateCommandEvent (mkSelector "playbackRate") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playbackRate@
playbackRateSelector :: Selector
playbackRateSelector = mkSelector "playbackRate"

