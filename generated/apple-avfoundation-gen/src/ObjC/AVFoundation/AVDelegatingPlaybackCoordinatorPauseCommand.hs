{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playback command requesting a pause
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorPauseCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorPauseCommand
  ( AVDelegatingPlaybackCoordinatorPauseCommand
  , IsAVDelegatingPlaybackCoordinatorPauseCommand(..)
  , init_
  , new
  , shouldBufferInAnticipationOfPlayback
  , anticipatedPlaybackRate
  , anticipatedPlaybackRateSelector
  , initSelector
  , newSelector
  , shouldBufferInAnticipationOfPlaybackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO (Id AVDelegatingPlaybackCoordinatorPauseCommand)
init_ avDelegatingPlaybackCoordinatorPauseCommand =
  sendOwnedMessage avDelegatingPlaybackCoordinatorPauseCommand initSelector

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPauseCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPauseCommand"
    sendOwnedClassMessage cls' newSelector

-- | Indicates that playback is anticipated and the player should begin buffering if necessary.
--
-- When shouldBufferInAnticipationOfPlayback is YES, some participant wants to resume playback at the rate indicated by the anticipatedPlaybackRate property. This should be treated similar to receiving a separate AVDelegatingPlaybackCoordinatorBufferingCommand. If YES, the command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlayback :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO Bool
shouldBufferInAnticipationOfPlayback avDelegatingPlaybackCoordinatorPauseCommand =
  sendMessage avDelegatingPlaybackCoordinatorPauseCommand shouldBufferInAnticipationOfPlaybackSelector

-- | The rate to prepare for if shouldBufferInAnticipationOfPlayback is YES.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorPauseCommand =
  sendMessage avDelegatingPlaybackCoordinatorPauseCommand anticipatedPlaybackRateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPauseCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPauseCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlaybackSelector :: Selector '[] Bool
shouldBufferInAnticipationOfPlaybackSelector = mkSelector "shouldBufferInAnticipationOfPlayback"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector '[] CFloat
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

