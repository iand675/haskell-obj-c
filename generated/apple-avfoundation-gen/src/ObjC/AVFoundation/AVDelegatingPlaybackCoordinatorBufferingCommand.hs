{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playback command requesting buffering in anticipation of playback.
--
-- Receiving this command should be reflected to the user as playback in a buffering state. To cancel the group intent to begin playback and move back into a paused state, call [AVDelegatingPlaybackCoordinator coordinateRateChangeToRate:0 options: 0]
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorBufferingCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorBufferingCommand
  ( AVDelegatingPlaybackCoordinatorBufferingCommand
  , IsAVDelegatingPlaybackCoordinatorBufferingCommand(..)
  , init_
  , new
  , anticipatedPlaybackRate
  , completionDueDate
  , anticipatedPlaybackRateSelector
  , completionDueDateSelector
  , initSelector
  , newSelector


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
init_ :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
init_ avDelegatingPlaybackCoordinatorBufferingCommand =
  sendOwnedMessage avDelegatingPlaybackCoordinatorBufferingCommand initSelector

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorBufferingCommand"
    sendOwnedClassMessage cls' newSelector

-- | The rate to prepare playback for.
--
-- The command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorBufferingCommand =
  sendMessage avDelegatingPlaybackCoordinatorBufferingCommand anticipatedPlaybackRateSelector

-- | Communicates when the coordinator expects the command's completion handler at the latest.
--
-- A receiver of a buffering command should fire the completion handler by this date at the latest. This is useful in buffering situations where the receiver has not yet buffered enough data to be considered ready to play by the due date. The receiver should then decide to either complete the command as is to try and keep up with the group, or alternatively begin a stall recovery suspension to communicate the situation to the other participants. Completing the command after this date means that the coordinator will likely send a play command for a later time than the receiver buffered for.
--
-- ObjC selector: @- completionDueDate@
completionDueDate :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO (Id NSDate)
completionDueDate avDelegatingPlaybackCoordinatorBufferingCommand =
  sendMessage avDelegatingPlaybackCoordinatorBufferingCommand completionDueDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector '[] CFloat
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

-- | @Selector@ for @completionDueDate@
completionDueDateSelector :: Selector '[] (Id NSDate)
completionDueDateSelector = mkSelector "completionDueDate"

