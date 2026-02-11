{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPChangePlaybackPositionCommandEvent@.
module ObjC.MediaPlayer.MPChangePlaybackPositionCommandEvent
  ( MPChangePlaybackPositionCommandEvent
  , IsMPChangePlaybackPositionCommandEvent(..)
  , positionTime
  , positionTimeSelector


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

-- | The desired playback position to use when setting the current time of the player.
--
-- ObjC selector: @- positionTime@
positionTime :: IsMPChangePlaybackPositionCommandEvent mpChangePlaybackPositionCommandEvent => mpChangePlaybackPositionCommandEvent -> IO CDouble
positionTime mpChangePlaybackPositionCommandEvent  =
  sendMsg mpChangePlaybackPositionCommandEvent (mkSelector "positionTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @positionTime@
positionTimeSelector :: Selector
positionTimeSelector = mkSelector "positionTime"

