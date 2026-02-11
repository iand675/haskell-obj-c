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
  , initSelector
  , newSelector
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
init_ :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
init_ avDelegatingPlaybackCoordinatorBufferingCommand  =
  sendMsg avDelegatingPlaybackCoordinatorBufferingCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorBufferingCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorBufferingCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The rate to prepare playback for.
--
-- The command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorBufferingCommand  =
  sendMsg avDelegatingPlaybackCoordinatorBufferingCommand (mkSelector "anticipatedPlaybackRate") retCFloat []

-- | Communicates when the coordinator expects the command's completion handler at the latest.
--
-- A receiver of a buffering command should fire the completion handler by this date at the latest. This is useful in buffering situations where the receiver has not yet buffered enough data to be considered ready to play by the due date. The receiver should then decide to either complete the command as is to try and keep up with the group, or alternatively begin a stall recovery suspension to communicate the situation to the other participants. Completing the command after this date means that the coordinator will likely send a play command for a later time than the receiver buffered for.
--
-- ObjC selector: @- completionDueDate@
completionDueDate :: IsAVDelegatingPlaybackCoordinatorBufferingCommand avDelegatingPlaybackCoordinatorBufferingCommand => avDelegatingPlaybackCoordinatorBufferingCommand -> IO (Id NSDate)
completionDueDate avDelegatingPlaybackCoordinatorBufferingCommand  =
  sendMsg avDelegatingPlaybackCoordinatorBufferingCommand (mkSelector "completionDueDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

-- | @Selector@ for @completionDueDate@
completionDueDateSelector :: Selector
completionDueDateSelector = mkSelector "completionDueDate"

