{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playback command requesting a seek.
--
-- If the current playback rate is non-zero, playback should not automatically resume after the seek. Instead the delegate should pause and wait for the coordinator to issue another PlayCommand. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorSeekCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorSeekCommand
  ( AVDelegatingPlaybackCoordinatorSeekCommand
  , IsAVDelegatingPlaybackCoordinatorSeekCommand(..)
  , init_
  , new
  , shouldBufferInAnticipationOfPlayback
  , anticipatedPlaybackRate
  , completionDueDate
  , anticipatedPlaybackRateSelector
  , completionDueDateSelector
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
init_ :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO (Id AVDelegatingPlaybackCoordinatorSeekCommand)
init_ avDelegatingPlaybackCoordinatorSeekCommand =
  sendOwnedMessage avDelegatingPlaybackCoordinatorSeekCommand initSelector

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorSeekCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorSeekCommand"
    sendOwnedClassMessage cls' newSelector

-- | Indicates that playback is anticipated and the player should begin buffering if necessary.
--
-- When shouldBufferInAnticipationOfPlayback, playback is expected to eventually resume at the rate indicated by the anticipatedPlaybackRate property. This should be treated similar to receiving a separate AVDelegatingPlaybackCoordinatorBufferingCommand. If YES, the command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlayback :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO Bool
shouldBufferInAnticipationOfPlayback avDelegatingPlaybackCoordinatorSeekCommand =
  sendMessage avDelegatingPlaybackCoordinatorSeekCommand shouldBufferInAnticipationOfPlaybackSelector

-- | The rate to prepare for if shouldBufferInAnticipationOfPlayback is YES.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorSeekCommand =
  sendMessage avDelegatingPlaybackCoordinatorSeekCommand anticipatedPlaybackRateSelector

-- | Communicates when the coordinator expects the command's completion handler at the latest.
--
-- A seek command expecting buffering in anticipation of playback does expect the receiver to fire the completion handler by this date at the latest. This is useful in buffering situations where the receiver has not yet buffered enough data to be considered ready to play by the due date. The receiver should then decide to either complete the command as is to try and keep up with the group, or alternatively begin a stall recovery suspension to communicate the situation to the other participants. Completing the command after this date means that the coordinator will likely send a play command for a later time than the receiver buffered for.
--
-- ObjC selector: @- completionDueDate@
completionDueDate :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO (Id NSDate)
completionDueDate avDelegatingPlaybackCoordinatorSeekCommand =
  sendMessage avDelegatingPlaybackCoordinatorSeekCommand completionDueDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorSeekCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorSeekCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlaybackSelector :: Selector '[] Bool
shouldBufferInAnticipationOfPlaybackSelector = mkSelector "shouldBufferInAnticipationOfPlayback"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector '[] CFloat
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

-- | @Selector@ for @completionDueDate@
completionDueDateSelector :: Selector '[] (Id NSDate)
completionDueDateSelector = mkSelector "completionDueDate"

