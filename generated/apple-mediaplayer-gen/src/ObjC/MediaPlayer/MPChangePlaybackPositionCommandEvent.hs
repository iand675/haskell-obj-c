{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The desired playback position to use when setting the current time of the player.
--
-- ObjC selector: @- positionTime@
positionTime :: IsMPChangePlaybackPositionCommandEvent mpChangePlaybackPositionCommandEvent => mpChangePlaybackPositionCommandEvent -> IO CDouble
positionTime mpChangePlaybackPositionCommandEvent =
  sendMessage mpChangePlaybackPositionCommandEvent positionTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @positionTime@
positionTimeSelector :: Selector '[] CDouble
positionTimeSelector = mkSelector "positionTime"

