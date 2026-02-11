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
  , initSelector
  , newSelector
  , shouldBufferInAnticipationOfPlaybackSelector
  , anticipatedPlaybackRateSelector
  , completionDueDateSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO (Id AVDelegatingPlaybackCoordinatorSeekCommand)
init_ avDelegatingPlaybackCoordinatorSeekCommand  =
  sendMsg avDelegatingPlaybackCoordinatorSeekCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorSeekCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorSeekCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates that playback is anticipated and the player should begin buffering if necessary.
--
-- When shouldBufferInAnticipationOfPlayback, playback is expected to eventually resume at the rate indicated by the anticipatedPlaybackRate property. This should be treated similar to receiving a separate AVDelegatingPlaybackCoordinatorBufferingCommand. If YES, the command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlayback :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO Bool
shouldBufferInAnticipationOfPlayback avDelegatingPlaybackCoordinatorSeekCommand  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avDelegatingPlaybackCoordinatorSeekCommand (mkSelector "shouldBufferInAnticipationOfPlayback") retCULong []

-- | The rate to prepare for if shouldBufferInAnticipationOfPlayback is YES.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorSeekCommand  =
  sendMsg avDelegatingPlaybackCoordinatorSeekCommand (mkSelector "anticipatedPlaybackRate") retCFloat []

-- | Communicates when the coordinator expects the command's completion handler at the latest.
--
-- A seek command expecting buffering in anticipation of playback does expect the receiver to fire the completion handler by this date at the latest. This is useful in buffering situations where the receiver has not yet buffered enough data to be considered ready to play by the due date. The receiver should then decide to either complete the command as is to try and keep up with the group, or alternatively begin a stall recovery suspension to communicate the situation to the other participants. Completing the command after this date means that the coordinator will likely send a play command for a later time than the receiver buffered for.
--
-- ObjC selector: @- completionDueDate@
completionDueDate :: IsAVDelegatingPlaybackCoordinatorSeekCommand avDelegatingPlaybackCoordinatorSeekCommand => avDelegatingPlaybackCoordinatorSeekCommand -> IO (Id NSDate)
completionDueDate avDelegatingPlaybackCoordinatorSeekCommand  =
  sendMsg avDelegatingPlaybackCoordinatorSeekCommand (mkSelector "completionDueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlaybackSelector :: Selector
shouldBufferInAnticipationOfPlaybackSelector = mkSelector "shouldBufferInAnticipationOfPlayback"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

-- | @Selector@ for @completionDueDate@
completionDueDateSelector :: Selector
completionDueDateSelector = mkSelector "completionDueDate"

