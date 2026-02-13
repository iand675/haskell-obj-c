{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The playback coordinator negotiates playback state between a player, such as AVPlayer or a custom playback object represented by an implementation of the AVPlaybackCoordinatorPlaybackControlDelegate protocol, and a group of other connected players.
--
-- AVPlaybackCoordinator will match rate and time of all connected players. This means that a local rate change or seek will be reflected in all connected players. Equally, a rate change or seek in any of the connected players will be reflected locally. AVPlaybackCoordinator does not manage the items in the play queue of the connected players, so it is up to player's owner to share and match the play queue across participants. The coordinator does, however, keep track of the identity of items enqueued in each player. This means that for one player's current time and rate to be applied on another player, both players must be playing the same item. If two players are playing different items, they each have independent playback states. When one of the two players transitions to the other's item later, it will match the time and rate of that other player.
--
-- Generated bindings for @AVPlaybackCoordinator@.
module ObjC.AVFoundation.AVPlaybackCoordinator
  ( AVPlaybackCoordinator
  , IsAVPlaybackCoordinator(..)
  , init_
  , new
  , beginSuspensionForReason
  , setParticipantLimit_forWaitingOutSuspensionsWithReason
  , participantLimitForWaitingOutSuspensionsWithReason
  , otherParticipants
  , suspensionReasons
  , suspensionReasonsThatTriggerWaiting
  , setSuspensionReasonsThatTriggerWaiting
  , pauseSnapsToMediaTimeOfOriginator
  , setPauseSnapsToMediaTimeOfOriginator
  , beginSuspensionForReasonSelector
  , initSelector
  , newSelector
  , otherParticipantsSelector
  , participantLimitForWaitingOutSuspensionsWithReasonSelector
  , pauseSnapsToMediaTimeOfOriginatorSelector
  , setParticipantLimit_forWaitingOutSuspensionsWithReasonSelector
  , setPauseSnapsToMediaTimeOfOriginatorSelector
  , setSuspensionReasonsThatTriggerWaitingSelector
  , suspensionReasonsSelector
  , suspensionReasonsThatTriggerWaitingSelector


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
init_ :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> IO (Id AVPlaybackCoordinator)
init_ avPlaybackCoordinator =
  sendOwnedMessage avPlaybackCoordinator initSelector

-- | @+ new@
new :: IO (Id AVPlaybackCoordinator)
new  =
  do
    cls' <- getRequiredClass "AVPlaybackCoordinator"
    sendOwnedClassMessage cls' newSelector

-- | Informs the coordinator that its playback object is detached from the group for some reason and should not receive any playback commands from the coordinator.
--
-- Use this to tell the coordinator that its player cannot, or should not, participate in coordinated playback temporarily. The coordinator will not respond to playback commands coming from the group and it will also not send any commands to the group. To resume in group playback, end a suspension by calling one of the suspension's end methods.
--
-- - Parameter suspensionReason: Indicates the reason for the suspension that is shared with other participants. Can be a system-defined reason (see AVCoordinatedPlaybackSuspensionReason*) or a custom string.
--
-- - NOTE: See the description of AVPlaybackCoordinator subclasses for suspensions automatically begun on behalf of their playback objects, if any.
--
-- ObjC selector: @- beginSuspensionForReason:@
beginSuspensionForReason :: (IsAVPlaybackCoordinator avPlaybackCoordinator, IsNSString suspensionReason) => avPlaybackCoordinator -> suspensionReason -> IO (Id AVCoordinatedPlaybackSuspension)
beginSuspensionForReason avPlaybackCoordinator suspensionReason =
  sendMessage avPlaybackCoordinator beginSuspensionForReasonSelector (toNSString suspensionReason)

-- | Sets the amount of participants that can join a group before the coordinator stops waiting for this particular suspension reason.
--
-- This allows additional configuration for suspension reasons in the suspensionReasonsThatTriggerWaiting array. When the coordinator decides whether one participant's suspensions should cause others to wait, it will also consider this limit of participants currently in the group.
--
-- ObjC selector: @- setParticipantLimit:forWaitingOutSuspensionsWithReason:@
setParticipantLimit_forWaitingOutSuspensionsWithReason :: (IsAVPlaybackCoordinator avPlaybackCoordinator, IsNSString reason) => avPlaybackCoordinator -> CLong -> reason -> IO ()
setParticipantLimit_forWaitingOutSuspensionsWithReason avPlaybackCoordinator participantLimit reason =
  sendMessage avPlaybackCoordinator setParticipantLimit_forWaitingOutSuspensionsWithReasonSelector participantLimit (toNSString reason)

-- | Returns the maximum number of participants that can be in a group before the coordinator stops waiting out this particular suspensions reason. Default value is NSIntegerMax.
--
-- ObjC selector: @- participantLimitForWaitingOutSuspensionsWithReason:@
participantLimitForWaitingOutSuspensionsWithReason :: (IsAVPlaybackCoordinator avPlaybackCoordinator, IsNSString reason) => avPlaybackCoordinator -> reason -> IO CLong
participantLimitForWaitingOutSuspensionsWithReason avPlaybackCoordinator reason =
  sendMessage avPlaybackCoordinator participantLimitForWaitingOutSuspensionsWithReasonSelector (toNSString reason)

-- | The playback states of the other participants in the group.
--
-- Use this property to create UI informing the local user about the state of other participants in the group.
--
-- - NOTE: The coordinator posts AVPlaybackCoordinatorOtherParticipantsDidChangeNotification when the contents of the array changes.
--
-- ObjC selector: @- otherParticipants@
otherParticipants :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> IO (Id NSArray)
otherParticipants avPlaybackCoordinator =
  sendMessage avPlaybackCoordinator otherParticipantsSelector

-- | Describes why the coordinator is currently not able to participate in group playback.
--
-- If the list of reasons is non-empty, the coordinator will not react to any changes of group playback state.
--
-- ObjC selector: @- suspensionReasons@
suspensionReasons :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> IO (Id NSArray)
suspensionReasons avPlaybackCoordinator =
  sendMessage avPlaybackCoordinator suspensionReasonsSelector

-- | If the coordinator decides to delay playback to wait for others, it will wait out these reasons, but not others.
--
-- ObjC selector: @- suspensionReasonsThatTriggerWaiting@
suspensionReasonsThatTriggerWaiting :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> IO (Id NSArray)
suspensionReasonsThatTriggerWaiting avPlaybackCoordinator =
  sendMessage avPlaybackCoordinator suspensionReasonsThatTriggerWaitingSelector

-- | If the coordinator decides to delay playback to wait for others, it will wait out these reasons, but not others.
--
-- ObjC selector: @- setSuspensionReasonsThatTriggerWaiting:@
setSuspensionReasonsThatTriggerWaiting :: (IsAVPlaybackCoordinator avPlaybackCoordinator, IsNSArray value) => avPlaybackCoordinator -> value -> IO ()
setSuspensionReasonsThatTriggerWaiting avPlaybackCoordinator value =
  sendMessage avPlaybackCoordinator setSuspensionReasonsThatTriggerWaitingSelector (toNSArray value)

-- | Determines if participants should mirror the originator's stop time when pausing.
--
-- If YES, all participants will seek to the originator's stop time after they pause. Use this if it is desirable to counteract any network delay incurred by communicating the originator's pause to the other participants. If NO, it's acceptable for participants to stop at slightly different offsets and a pause will not cause other participants' time to jump back.
--
-- ObjC selector: @- pauseSnapsToMediaTimeOfOriginator@
pauseSnapsToMediaTimeOfOriginator :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> IO Bool
pauseSnapsToMediaTimeOfOriginator avPlaybackCoordinator =
  sendMessage avPlaybackCoordinator pauseSnapsToMediaTimeOfOriginatorSelector

-- | Determines if participants should mirror the originator's stop time when pausing.
--
-- If YES, all participants will seek to the originator's stop time after they pause. Use this if it is desirable to counteract any network delay incurred by communicating the originator's pause to the other participants. If NO, it's acceptable for participants to stop at slightly different offsets and a pause will not cause other participants' time to jump back.
--
-- ObjC selector: @- setPauseSnapsToMediaTimeOfOriginator:@
setPauseSnapsToMediaTimeOfOriginator :: IsAVPlaybackCoordinator avPlaybackCoordinator => avPlaybackCoordinator -> Bool -> IO ()
setPauseSnapsToMediaTimeOfOriginator avPlaybackCoordinator value =
  sendMessage avPlaybackCoordinator setPauseSnapsToMediaTimeOfOriginatorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlaybackCoordinator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlaybackCoordinator)
newSelector = mkSelector "new"

-- | @Selector@ for @beginSuspensionForReason:@
beginSuspensionForReasonSelector :: Selector '[Id NSString] (Id AVCoordinatedPlaybackSuspension)
beginSuspensionForReasonSelector = mkSelector "beginSuspensionForReason:"

-- | @Selector@ for @setParticipantLimit:forWaitingOutSuspensionsWithReason:@
setParticipantLimit_forWaitingOutSuspensionsWithReasonSelector :: Selector '[CLong, Id NSString] ()
setParticipantLimit_forWaitingOutSuspensionsWithReasonSelector = mkSelector "setParticipantLimit:forWaitingOutSuspensionsWithReason:"

-- | @Selector@ for @participantLimitForWaitingOutSuspensionsWithReason:@
participantLimitForWaitingOutSuspensionsWithReasonSelector :: Selector '[Id NSString] CLong
participantLimitForWaitingOutSuspensionsWithReasonSelector = mkSelector "participantLimitForWaitingOutSuspensionsWithReason:"

-- | @Selector@ for @otherParticipants@
otherParticipantsSelector :: Selector '[] (Id NSArray)
otherParticipantsSelector = mkSelector "otherParticipants"

-- | @Selector@ for @suspensionReasons@
suspensionReasonsSelector :: Selector '[] (Id NSArray)
suspensionReasonsSelector = mkSelector "suspensionReasons"

-- | @Selector@ for @suspensionReasonsThatTriggerWaiting@
suspensionReasonsThatTriggerWaitingSelector :: Selector '[] (Id NSArray)
suspensionReasonsThatTriggerWaitingSelector = mkSelector "suspensionReasonsThatTriggerWaiting"

-- | @Selector@ for @setSuspensionReasonsThatTriggerWaiting:@
setSuspensionReasonsThatTriggerWaitingSelector :: Selector '[Id NSArray] ()
setSuspensionReasonsThatTriggerWaitingSelector = mkSelector "setSuspensionReasonsThatTriggerWaiting:"

-- | @Selector@ for @pauseSnapsToMediaTimeOfOriginator@
pauseSnapsToMediaTimeOfOriginatorSelector :: Selector '[] Bool
pauseSnapsToMediaTimeOfOriginatorSelector = mkSelector "pauseSnapsToMediaTimeOfOriginator"

-- | @Selector@ for @setPauseSnapsToMediaTimeOfOriginator:@
setPauseSnapsToMediaTimeOfOriginatorSelector :: Selector '[Bool] ()
setPauseSnapsToMediaTimeOfOriginatorSelector = mkSelector "setPauseSnapsToMediaTimeOfOriginator:"

