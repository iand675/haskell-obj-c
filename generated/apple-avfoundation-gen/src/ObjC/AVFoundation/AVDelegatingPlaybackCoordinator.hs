{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlaybackCoordinator subclass for controlling a custom playback object.
--
-- - NOTE: Use AVPlayer's playbackCoordinator property to get an AVPlaybackCoordinator for an AVPlayer.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinator@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinator
  ( AVDelegatingPlaybackCoordinator
  , IsAVDelegatingPlaybackCoordinator(..)
  , initWithPlaybackControlDelegate
  , coordinateRateChangeToRate_options
  , transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebase
  , reapplyCurrentItemStateToPlaybackControlDelegate
  , playbackControlDelegate
  , currentItemIdentifier
  , coordinateRateChangeToRate_optionsSelector
  , currentItemIdentifierSelector
  , initWithPlaybackControlDelegateSelector
  , playbackControlDelegateSelector
  , reapplyCurrentItemStateToPlaybackControlDelegateSelector
  , transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebaseSelector

  -- * Enum types
  , AVDelegatingPlaybackCoordinatorRateChangeOptions(AVDelegatingPlaybackCoordinatorRateChangeOptions)
  , pattern AVDelegatingPlaybackCoordinatorRateChangeOptionPlayImmediately
  , AVDelegatingPlaybackCoordinatorSeekOptions(AVDelegatingPlaybackCoordinatorSeekOptions)
  , pattern AVDelegatingPlaybackCoordinatorSeekOptionResumeImmediately

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates an AVPlaybackCoordinator for a custom playback object.
--
-- Use this to create an AVPlaybackCoordinator when playback is not driven by an AVPlayer.
--
-- - Parameter playbackControlDelegate: An object conforming to the AVPlaybackCoordinatorPlaybackControlDelegate protocol representing a custom playback object. The coordinator will only hold a weak reference to its delegate.
--
-- - NOTE: See AVPlayer's playbackCoordinator property to get an AVPlaybackCoordinator for an AVPlayer.
--
-- ObjC selector: @- initWithPlaybackControlDelegate:@
initWithPlaybackControlDelegate :: IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator => avDelegatingPlaybackCoordinator -> RawId -> IO (Id AVDelegatingPlaybackCoordinator)
initWithPlaybackControlDelegate avDelegatingPlaybackCoordinator playbackControlDelegate =
  sendOwnedMessage avDelegatingPlaybackCoordinator initWithPlaybackControlDelegateSelector playbackControlDelegate

-- | Coordinaties a rate change across the group of connected participants, waiting for other participants to become ready if necessary.
--
-- The coordinator will request a coordinated rate change from all other connected participants. When changing the rate from zero to non-zero, it may also wait out other participant's suspensions as configured by the suspensionReasonsThatTriggerWaiting property.
--
-- This method should not be called when the rate change should not affect the group, or the group should not have control over local playback temporarily, e.g. a pause because of an audio session interruption. In those cases, the coordinator should be informed by beginning a suspension with the appropriate reason instead. If other participants pause is dependent on the coordinator's configuration. The suspension will stop the coordinator from issuing further commands to its playbackControlDelegate. After beginning the suspension, the playback object can be reconfigured as necessary.
--
-- - Parameter rate: The playback rate the group should be using. - Parameter options: Additional configuration of the rate change. For details see AVDelegatingPlaybackCoordinatorRateChangeOptions.
--
-- - NOTE: Calling this method while the coordinator is suspended affects only the local playback object. The group state will not be affected, even after the suspension ends.
--
-- ObjC selector: @- coordinateRateChangeToRate:options:@
coordinateRateChangeToRate_options :: IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator => avDelegatingPlaybackCoordinator -> CFloat -> AVDelegatingPlaybackCoordinatorRateChangeOptions -> IO ()
coordinateRateChangeToRate_options avDelegatingPlaybackCoordinator rate options =
  sendMessage avDelegatingPlaybackCoordinator coordinateRateChangeToRate_optionsSelector rate options

-- | Informs the coordinator to transition to a new current item.
--
-- The coordinator will stop sending commands for any previous item identifier and begin sending commands for the new identifier. The proposed timing will either be used as the new referece timing for the group, or it will be compared to an already existing reference timing. If the proposed timing doesn't match such an existing reference timing, the coordinator will use the playbackControlDelegate to issue appropriate commands to match up the timing.
--
-- - Parameter itemIdentifier: The identifier for the new current item. May be nil if nothing is playing. - Parameter snapshotTimebase: A timebase used to communicate the initial playback state of the new item. If NULL, the coordinator will assume that playback is paused at kCMTimeZero. An appropriate timebase to pass to the completion handler may be retreived from AVFoundation playback objects such as AVSampleBufferRenderSynchronizer. It can also be created manually using CMTimebaseCreateWithSourceClock. The timebase will only be used to take a snapshot of its immediate timing. It will not be observed further.
--
-- - NOTE: This is not a way to affect the play queue of other participants. All other participants must do this independently, e.g. as a side-effect of an automatic item transition or an out-of-band communication requesting a similar item change.
--
-- ObjC selector: @- transitionToItemWithIdentifier:proposingInitialTimingBasedOnTimebase:@
transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebase :: (IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator, IsNSString itemIdentifier) => avDelegatingPlaybackCoordinator -> itemIdentifier -> Ptr () -> IO ()
transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebase avDelegatingPlaybackCoordinator itemIdentifier snapshotTimebase =
  sendMessage avDelegatingPlaybackCoordinator transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebaseSelector (toNSString itemIdentifier) snapshotTimebase

-- | Instructs the coordinator to re-issue commands to synchronize the current item back to the state of the other participants.
--
-- Use this method when the playback object is in a state that doesn't match the group for some reason and should be re-synchronized.
--
-- ObjC selector: @- reapplyCurrentItemStateToPlaybackControlDelegate@
reapplyCurrentItemStateToPlaybackControlDelegate :: IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator => avDelegatingPlaybackCoordinator -> IO ()
reapplyCurrentItemStateToPlaybackControlDelegate avDelegatingPlaybackCoordinator =
  sendMessage avDelegatingPlaybackCoordinator reapplyCurrentItemStateToPlaybackControlDelegateSelector

-- | The custom player implementation controlled by the coordinator.
--
-- ObjC selector: @- playbackControlDelegate@
playbackControlDelegate :: IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator => avDelegatingPlaybackCoordinator -> IO RawId
playbackControlDelegate avDelegatingPlaybackCoordinator =
  sendMessage avDelegatingPlaybackCoordinator playbackControlDelegateSelector

-- | The item identifier of the current item. Previously set by a call to transitionToItemWithIdentifier:proposingInitialTimingBasedOnTimebase:
--
-- ObjC selector: @- currentItemIdentifier@
currentItemIdentifier :: IsAVDelegatingPlaybackCoordinator avDelegatingPlaybackCoordinator => avDelegatingPlaybackCoordinator -> IO (Id NSString)
currentItemIdentifier avDelegatingPlaybackCoordinator =
  sendMessage avDelegatingPlaybackCoordinator currentItemIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlaybackControlDelegate:@
initWithPlaybackControlDelegateSelector :: Selector '[RawId] (Id AVDelegatingPlaybackCoordinator)
initWithPlaybackControlDelegateSelector = mkSelector "initWithPlaybackControlDelegate:"

-- | @Selector@ for @coordinateRateChangeToRate:options:@
coordinateRateChangeToRate_optionsSelector :: Selector '[CFloat, AVDelegatingPlaybackCoordinatorRateChangeOptions] ()
coordinateRateChangeToRate_optionsSelector = mkSelector "coordinateRateChangeToRate:options:"

-- | @Selector@ for @transitionToItemWithIdentifier:proposingInitialTimingBasedOnTimebase:@
transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebaseSelector :: Selector '[Id NSString, Ptr ()] ()
transitionToItemWithIdentifier_proposingInitialTimingBasedOnTimebaseSelector = mkSelector "transitionToItemWithIdentifier:proposingInitialTimingBasedOnTimebase:"

-- | @Selector@ for @reapplyCurrentItemStateToPlaybackControlDelegate@
reapplyCurrentItemStateToPlaybackControlDelegateSelector :: Selector '[] ()
reapplyCurrentItemStateToPlaybackControlDelegateSelector = mkSelector "reapplyCurrentItemStateToPlaybackControlDelegate"

-- | @Selector@ for @playbackControlDelegate@
playbackControlDelegateSelector :: Selector '[] RawId
playbackControlDelegateSelector = mkSelector "playbackControlDelegate"

-- | @Selector@ for @currentItemIdentifier@
currentItemIdentifierSelector :: Selector '[] (Id NSString)
currentItemIdentifierSelector = mkSelector "currentItemIdentifier"

