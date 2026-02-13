{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayer offers a playback interface for single-item playback that's sufficient for the implementation of playback controllers and playback user interfaces.
--
-- AVPlayer works equally well with local and remote media files, providing clients with appropriate information about readiness to play or about the need to await additional data before continuing.
--
-- Visual content of items played by an instance of AVPlayer can be displayed in a CoreAnimation layer of class AVPlayerLayer.
--
-- To allow clients to add and remove their objects as key-value observers safely, AVPlayer serializes notifications of changes that occur dynamically during playback on a dispatch queue. By default, this queue is the main queue. See dispatch_get_main_queue().
--
-- Generated bindings for @AVPlayer@.
module ObjC.AVFoundation.AVPlayer
  ( AVPlayer
  , IsAVPlayer(..)
  , init_
  , playerWithURL
  , playerWithPlayerItem
  , initWithURL
  , initWithPlayerItem
  , setMediaSelectionCriteria_forMediaCharacteristic
  , mediaSelectionCriteriaForMediaCharacteristic
  , addBoundaryTimeObserverForTimes_queue_usingBlock
  , removeTimeObserver
  , prerollAtRate_completionHandler
  , cancelPendingPrerolls
  , seekToDate
  , seekToDate_completionHandler
  , replaceCurrentItemWithPlayerItem
  , play
  , pause
  , playImmediatelyAtRate
  , status
  , error_
  , closedCaptionDisplayEnabled
  , setClosedCaptionDisplayEnabled
  , masterClock
  , setMasterClock
  , observationEnabled
  , setObservationEnabled
  , audioOutputSuppressedDueToNonMixableAudioRoute
  , intendedSpatialAudioExperience
  , setIntendedSpatialAudioExperience
  , networkResourcePriority
  , setNetworkResourcePriority
  , videoOutput
  , setVideoOutput
  , playbackCoordinator
  , audiovisualBackgroundPlaybackPolicy
  , setAudiovisualBackgroundPlaybackPolicy
  , preventsAutomaticBackgroundingDuringVideoPlayback
  , setPreventsAutomaticBackgroundingDuringVideoPlayback
  , preventsDisplaySleepDuringVideoPlayback
  , setPreventsDisplaySleepDuringVideoPlayback
  , preferredVideoDecoderGPURegistryID
  , setPreferredVideoDecoderGPURegistryID
  , availableHDRModes
  , eligibleForHDRPlayback
  , outputObscuredDueToInsufficientExternalProtection
  , allowsExternalPlayback
  , setAllowsExternalPlayback
  , externalPlaybackActive
  , usesExternalPlaybackWhileExternalScreenIsActive
  , setUsesExternalPlaybackWhileExternalScreenIsActive
  , externalPlaybackVideoGravity
  , setExternalPlaybackVideoGravity
  , audioOutputDeviceUniqueID
  , setAudioOutputDeviceUniqueID
  , appliesMediaSelectionCriteriaAutomatically
  , setAppliesMediaSelectionCriteriaAutomatically
  , volume
  , setVolume
  , muted
  , setMuted
  , automaticallyWaitsToMinimizeStalling
  , setAutomaticallyWaitsToMinimizeStalling
  , sourceClock
  , setSourceClock
  , currentItem
  , actionAtItemEnd
  , setActionAtItemEnd
  , rate
  , setRate
  , defaultRate
  , setDefaultRate
  , timeControlStatus
  , reasonForWaitingToPlay
  , actionAtItemEndSelector
  , addBoundaryTimeObserverForTimes_queue_usingBlockSelector
  , allowsExternalPlaybackSelector
  , appliesMediaSelectionCriteriaAutomaticallySelector
  , audioOutputDeviceUniqueIDSelector
  , audioOutputSuppressedDueToNonMixableAudioRouteSelector
  , audiovisualBackgroundPlaybackPolicySelector
  , automaticallyWaitsToMinimizeStallingSelector
  , availableHDRModesSelector
  , cancelPendingPrerollsSelector
  , closedCaptionDisplayEnabledSelector
  , currentItemSelector
  , defaultRateSelector
  , eligibleForHDRPlaybackSelector
  , errorSelector
  , externalPlaybackActiveSelector
  , externalPlaybackVideoGravitySelector
  , initSelector
  , initWithPlayerItemSelector
  , initWithURLSelector
  , intendedSpatialAudioExperienceSelector
  , masterClockSelector
  , mediaSelectionCriteriaForMediaCharacteristicSelector
  , mutedSelector
  , networkResourcePrioritySelector
  , observationEnabledSelector
  , outputObscuredDueToInsufficientExternalProtectionSelector
  , pauseSelector
  , playImmediatelyAtRateSelector
  , playSelector
  , playbackCoordinatorSelector
  , playerWithPlayerItemSelector
  , playerWithURLSelector
  , preferredVideoDecoderGPURegistryIDSelector
  , prerollAtRate_completionHandlerSelector
  , preventsAutomaticBackgroundingDuringVideoPlaybackSelector
  , preventsDisplaySleepDuringVideoPlaybackSelector
  , rateSelector
  , reasonForWaitingToPlaySelector
  , removeTimeObserverSelector
  , replaceCurrentItemWithPlayerItemSelector
  , seekToDateSelector
  , seekToDate_completionHandlerSelector
  , setActionAtItemEndSelector
  , setAllowsExternalPlaybackSelector
  , setAppliesMediaSelectionCriteriaAutomaticallySelector
  , setAudioOutputDeviceUniqueIDSelector
  , setAudiovisualBackgroundPlaybackPolicySelector
  , setAutomaticallyWaitsToMinimizeStallingSelector
  , setClosedCaptionDisplayEnabledSelector
  , setDefaultRateSelector
  , setExternalPlaybackVideoGravitySelector
  , setIntendedSpatialAudioExperienceSelector
  , setMasterClockSelector
  , setMediaSelectionCriteria_forMediaCharacteristicSelector
  , setMutedSelector
  , setNetworkResourcePrioritySelector
  , setObservationEnabledSelector
  , setPreferredVideoDecoderGPURegistryIDSelector
  , setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector
  , setPreventsDisplaySleepDuringVideoPlaybackSelector
  , setRateSelector
  , setSourceClockSelector
  , setUsesExternalPlaybackWhileExternalScreenIsActiveSelector
  , setVideoOutputSelector
  , setVolumeSelector
  , sourceClockSelector
  , statusSelector
  , timeControlStatusSelector
  , usesExternalPlaybackWhileExternalScreenIsActiveSelector
  , videoOutputSelector
  , volumeSelector

  -- * Enum types
  , AVPlayerActionAtItemEnd(AVPlayerActionAtItemEnd)
  , pattern AVPlayerActionAtItemEndAdvance
  , pattern AVPlayerActionAtItemEndPause
  , pattern AVPlayerActionAtItemEndNone
  , AVPlayerAudiovisualBackgroundPlaybackPolicy(AVPlayerAudiovisualBackgroundPlaybackPolicy)
  , pattern AVPlayerAudiovisualBackgroundPlaybackPolicyAutomatic
  , pattern AVPlayerAudiovisualBackgroundPlaybackPolicyPauses
  , pattern AVPlayerAudiovisualBackgroundPlaybackPolicyContinuesIfPossible
  , AVPlayerHDRMode(AVPlayerHDRMode)
  , pattern AVPlayerHDRModeHLG
  , pattern AVPlayerHDRModeHDR10
  , pattern AVPlayerHDRModeDolbyVision
  , AVPlayerNetworkResourcePriority(AVPlayerNetworkResourcePriority)
  , pattern AVPlayerNetworkResourcePriorityDefault
  , pattern AVPlayerNetworkResourcePriorityLow
  , pattern AVPlayerNetworkResourcePriorityHigh
  , AVPlayerStatus(AVPlayerStatus)
  , pattern AVPlayerStatusUnknown
  , pattern AVPlayerStatusReadyToPlay
  , pattern AVPlayerStatusFailed
  , AVPlayerTimeControlStatus(AVPlayerTimeControlStatus)
  , pattern AVPlayerTimeControlStatusPaused
  , pattern AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate
  , pattern AVPlayerTimeControlStatusPlaying

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

-- | Initializes an AVPlayer with no player items.
--
-- ObjC selector: @- init@
init_ :: IsAVPlayer avPlayer => avPlayer -> IO (Id AVPlayer)
init_ avPlayer =
  sendOwnedMessage avPlayer initSelector

-- | Returns an instance of AVPlayer that plays a single audiovisual resource referenced by URL.
--
-- Implicitly creates an AVPlayerItem. Clients can obtain the AVPlayerItem as it becomes the player's currentItem.
--
-- - Parameter URL:
--
-- - Returns: An instance of AVPlayer
--
-- ObjC selector: @+ playerWithURL:@
playerWithURL :: IsNSURL url => url -> IO (Id AVPlayer)
playerWithURL url =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' playerWithURLSelector (toNSURL url)

-- | Create an AVPlayer that plays a single audiovisual item.
--
-- Useful in order to play items for which an AVAsset has previously been created. See -[AVPlayerItem initWithAsset:].
--
-- - Parameter item:
--
-- - Returns: An instance of AVPlayer
--
-- ObjC selector: @+ playerWithPlayerItem:@
playerWithPlayerItem :: IsAVPlayerItem item => item -> IO (Id AVPlayer)
playerWithPlayerItem item =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' playerWithPlayerItemSelector (toAVPlayerItem item)

-- | Initializes an AVPlayer that plays a single audiovisual resource referenced by URL.
--
-- Implicitly creates an AVPlayerItem. Clients can obtain the AVPlayerItem as it becomes the player's currentItem.
--
-- - Parameter URL:
--
-- - Returns: An instance of AVPlayer
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsAVPlayer avPlayer, IsNSURL url) => avPlayer -> url -> IO (Id AVPlayer)
initWithURL avPlayer url =
  sendOwnedMessage avPlayer initWithURLSelector (toNSURL url)

-- | Create an AVPlayer that plays a single audiovisual item.
--
-- Useful in order to play items for which an AVAsset has previously been created. See -[AVPlayerItem initWithAsset:]. This method throws an exception if the item is not an AVPlayerItem, or if the item is associated with another AVPlayer.
--
-- - Parameter item:
--
-- - Returns: An instance of AVPlayer
--
-- ObjC selector: @- initWithPlayerItem:@
initWithPlayerItem :: (IsAVPlayer avPlayer, IsAVPlayerItem item) => avPlayer -> item -> IO (Id AVPlayer)
initWithPlayerItem avPlayer item =
  sendOwnedMessage avPlayer initWithPlayerItemSelector (toAVPlayerItem item)

-- | Applies automatic selection criteria for media that has the specified media characteristic.
--
-- Criteria will be applied to an AVPlayerItem when: a) It is made ready to play b) Specific media selections are made by -[AVPlayerItem selectMediaOption:inMediaSelectionGroup:] in a different group. The automatic choice in one group may be influenced by a specific selection in another group. c) Underlying system preferences change, e.g. system language, accessibility captions.
--
-- Specific selections made by -[AVPlayerItem selectMediaOption:inMediaSelectionGroup:] within any group will override automatic selection in that group until -[AVPlayerItem selectMediaOptionAutomaticallyInMediaSelectionGroup:] is received.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- - Parameter criteria: An instance of AVPlayerMediaSelectionCriteria. - Parameter mediaCharacteristic: The media characteristic for which the selection criteria are to be applied. Supported values include AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, and AVMediaCharacteristicVisual.
--
-- ObjC selector: @- setMediaSelectionCriteria:forMediaCharacteristic:@
setMediaSelectionCriteria_forMediaCharacteristic :: (IsAVPlayer avPlayer, IsAVPlayerMediaSelectionCriteria criteria, IsNSString mediaCharacteristic) => avPlayer -> criteria -> mediaCharacteristic -> IO ()
setMediaSelectionCriteria_forMediaCharacteristic avPlayer criteria mediaCharacteristic =
  sendMessage avPlayer setMediaSelectionCriteria_forMediaCharacteristicSelector (toAVPlayerMediaSelectionCriteria criteria) (toNSString mediaCharacteristic)

-- | Returns the automatic selection criteria for media that has the specified media characteristic.
--
-- - Parameter mediaCharacteristic: The media characteristic for which the selection criteria is to be returned. Supported values include AVMediaCharacteristicAudible, AVMediaCharacteristicLegible, and AVMediaCharacteristicVisual. Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- ObjC selector: @- mediaSelectionCriteriaForMediaCharacteristic:@
mediaSelectionCriteriaForMediaCharacteristic :: (IsAVPlayer avPlayer, IsNSString mediaCharacteristic) => avPlayer -> mediaCharacteristic -> IO (Id AVPlayerMediaSelectionCriteria)
mediaSelectionCriteriaForMediaCharacteristic avPlayer mediaCharacteristic =
  sendMessage avPlayer mediaSelectionCriteriaForMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | Requests invocation of a block when specified times are traversed during normal playback.
--
-- Each call to -addPeriodicTimeObserverForInterval:queue:usingBlock: should be paired with a corresponding call to -removeTimeObserver:. Releasing the observer object without a call to -removeTimeObserver: will result in undefined behavior.
--
-- - Parameter times: The times for which the observer requests notification, supplied as an array of NSValues carrying CMTimes. - Parameter queue: The serial queue onto which block should be enqueued. If you pass NULL, the main queue (obtained using dispatch_get_main_queue()) will be used. Passing a concurrent queue to this method will result in undefined behavior. - Parameter block: The block to be invoked when any of the specified times is crossed during normal playback.
--
-- - Returns: An object conforming to the NSObject protocol.  You must retain this returned value as long as you want the time observer to be invoked by the player. 	  Pass this object to -removeTimeObserver: to cancel time observation.
--
-- ObjC selector: @- addBoundaryTimeObserverForTimes:queue:usingBlock:@
addBoundaryTimeObserverForTimes_queue_usingBlock :: (IsAVPlayer avPlayer, IsNSArray times, IsNSObject queue) => avPlayer -> times -> queue -> Ptr () -> IO RawId
addBoundaryTimeObserverForTimes_queue_usingBlock avPlayer times queue block =
  sendMessage avPlayer addBoundaryTimeObserverForTimes_queue_usingBlockSelector (toNSArray times) (toNSObject queue) block

-- | Cancels a previously registered time observer.
--
-- Upon return, the caller is guaranteed that no new time observer blocks will begin executing. Depending on the calling thread and the queue used to add the time observer, an in-flight block may continue to execute after this method returns. You can guarantee synchronous time  observer removal by enqueuing the call to -removeTimeObserver: on that queue. Alternatively, call dispatch_sync(queue, ^{}) after -removeTimeObserver: to wait for any in-flight blocks to finish executing. -removeTimeObserver: should be used to explicitly cancel each time observer added using -addPeriodicTimeObserverForInterval:queue:usingBlock: and -addBoundaryTimeObserverForTimes:queue:usingBlock:.
--
-- This method throws an exception for any of the following reasons: - observer was added by a different instance of AVPlayer - observer was not returned by -addPeriodicTimeObserverForInterval:queue:usingBlock: - observer was not returned by -addBoundaryTimeObserverForTimes:queue:usingBlock:
--
-- - Parameter observer: An object returned by a previous call to -addPeriodicTimeObserverForInterval:queue:usingBlock: or -addBoundaryTimeObserverForTimes:queue:usingBlock:.
--
-- ObjC selector: @- removeTimeObserver:@
removeTimeObserver :: IsAVPlayer avPlayer => avPlayer -> RawId -> IO ()
removeTimeObserver avPlayer observer =
  sendMessage avPlayer removeTimeObserverSelector observer

-- | Begins loading media data to prime the render pipelines for playback from the current time with the given rate.
--
-- Once the completion handler is called with YES, the player's rate can be set with minimal latency. The completion handler will be called with NO if the preroll is interrupted by a time change or incompatible rate change, or if preroll is not possible for some other reason. Call this method only when the rate is currently zero and only after the AVPlayer's status has become AVPlayerStatusReadyToPlay. This method throws an exception if the status is not AVPlayerStatusReadyToPlay.
--
-- - Parameter rate: The intended rate for subsequent playback. - Parameter completionHandler: The block that will be called when the preroll is either completed or is interrupted.
--
-- ObjC selector: @- prerollAtRate:completionHandler:@
prerollAtRate_completionHandler :: IsAVPlayer avPlayer => avPlayer -> CFloat -> Ptr () -> IO ()
prerollAtRate_completionHandler avPlayer rate completionHandler =
  sendMessage avPlayer prerollAtRate_completionHandlerSelector rate completionHandler

-- | Cancel any pending preroll requests and invoke the corresponding completion handlers if present.
--
-- Use this method to cancel and release the completion handlers for pending prerolls. The finished parameter of the completion handlers will be set to NO.
--
-- ObjC selector: @- cancelPendingPrerolls@
cancelPendingPrerolls :: IsAVPlayer avPlayer => avPlayer -> IO ()
cancelPendingPrerolls avPlayer =
  sendMessage avPlayer cancelPendingPrerollsSelector

-- | Moves the playback cursor.
--
-- Use this method to seek to a specified time for the current player item. The time seeked to may differ from the specified time for efficiency. For sample accurate seeking see seekToTime:toleranceBefore:toleranceAfter:.
--
-- - Parameter date:
--
-- ObjC selector: @- seekToDate:@
seekToDate :: (IsAVPlayer avPlayer, IsNSDate date) => avPlayer -> date -> IO ()
seekToDate avPlayer date =
  sendMessage avPlayer seekToDateSelector (toNSDate date)

-- | Moves the playback cursor and invokes the specified block when the seek operation has either been completed or been interrupted.
--
-- Use this method to seek to a specified time for the current player item and to be notified when the seek operation is complete. The completion handler for any prior seek request that is still in process will be invoked immediately with the finished parameter  set to NO. If the new request completes without being interrupted by another seek request or by any other operation the specified  completion handler will be invoked with the finished parameter set to YES. If no item is attached, the completion handler will be invoked immediately with the finished parameter set to NO.
--
-- - Parameter date: - Parameter completionHandler:
--
-- ObjC selector: @- seekToDate:completionHandler:@
seekToDate_completionHandler :: (IsAVPlayer avPlayer, IsNSDate date) => avPlayer -> date -> Ptr () -> IO ()
seekToDate_completionHandler avPlayer date completionHandler =
  sendMessage avPlayer seekToDate_completionHandlerSelector (toNSDate date) completionHandler

-- | Replaces the player's current item with the specified player item.
--
-- In all releases of iOS 4, invoking replaceCurrentItemWithPlayerItem: with an AVPlayerItem that's already the receiver's currentItem results in an exception being raised. Starting with iOS 5, it's a no-op. This method throws an exception if the item already exists in the play queue.
--
-- - Parameter item: The AVPlayerItem that will become the player's current item.
--
-- ObjC selector: @- replaceCurrentItemWithPlayerItem:@
replaceCurrentItemWithPlayerItem :: (IsAVPlayer avPlayer, IsAVPlayerItem item) => avPlayer -> item -> IO ()
replaceCurrentItemWithPlayerItem avPlayer item =
  sendMessage avPlayer replaceCurrentItemWithPlayerItemSelector (toAVPlayerItem item)

-- | Signals the desire to begin playback at the rate set in the defaultRate.
--
-- For releases up to iOS version 16.0, macOS versions 13.0, tvOS 16.0 and watchOS 9.0, this is equivalent to setting the value of rate to @1.0@. Starting from iOS version 16.0, macOS versions 13.0, tvOS 16.0 and watchOS 9.0, this will attempt to use the rate set in the @defaultRate@ property. The effective rate of playback may differ from the @defaultRate@ due to the reasons mentioned in the documentation of the @rate@ property. Clients interested in knowing the effective rate can listen for @AVPlayerRateDidChangeNotification@ notification.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- ObjC selector: @- play@
play :: IsAVPlayer avPlayer => avPlayer -> IO ()
play avPlayer =
  sendMessage avPlayer playSelector

-- | Pauses playback.
--
-- Equivalent to setting the value of rate to 0.0.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- ObjC selector: @- pause@
pause :: IsAVPlayer avPlayer => avPlayer -> IO ()
pause avPlayer =
  sendMessage avPlayer pauseSelector

-- | Immediately plays the available media data at the specified rate.
--
-- When the player's currentItem has a value of NO for playbackBufferEmpty, this method causes the value of rate to change to the specified rate, the value of timeControlStatus to change to AVPlayerTimeControlStatusPlaying, and the receiver to play the available media immediately, whether or not prior buffering of media data is sufficient to ensure smooth playback. If insufficient media data is buffered for playback to start (e.g. if the current item has a value of YES for playbackBufferEmpty), the receiver will act as if the buffer became empty during playback, except that no AVPlayerItemPlaybackStalledNotification will be posted.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- ObjC selector: @- playImmediatelyAtRate:@
playImmediatelyAtRate :: IsAVPlayer avPlayer => avPlayer -> CFloat -> IO ()
playImmediatelyAtRate avPlayer rate =
  sendMessage avPlayer playImmediatelyAtRateSelector rate

-- | The ability of the receiver to be used for playback.
--
-- The value of this property is an AVPlayerStatus that indicates whether the receiver can be used for playback. When the value of this property is AVPlayerStatusFailed, the receiver can no longer be used for playback and a new instance needs to be created in its place. When this happens, clients can check the value of the error property to determine the nature of the failure. This property is key value observable.
--
-- ObjC selector: @- status@
status :: IsAVPlayer avPlayer => avPlayer -> IO AVPlayerStatus
status avPlayer =
  sendMessage avPlayer statusSelector

-- | If the receiver's status is AVPlayerStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the receiver to no longer be able to play items. If the receiver's status is not AVPlayerStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVPlayer avPlayer => avPlayer -> IO (Id NSError)
error_ avPlayer =
  sendMessage avPlayer errorSelector

-- | Indicates whether display of closed captions is enabled.
--
-- This property is deprecated.
--
-- When the value of appliesMediaSelectionCriteriaAutomatically is YES, the receiver will enable closed captions automatically either according to user preferences or, if you provide them, according to AVPlayerMediaSelectionCriteria for the media characteristic AVMediaCharacteristicLegible.
--
-- If you want to determine whether closed captions may be available for a given AVPlayerItem, you can examine the AVMediaSelectionOptions in the AVMediaSelectionGroup for the characteristic AVMediaCharacteristicLegible, as vended by -[AVAsset mediaSelectionGroupForMediaCharacteristic:]. See AVMediaCharacteristicTranscribesSpokenDialogForAccessibility and AVMediaCharacteristicDescribesMusicAndSoundForAccessibility as documented in AVMediaFormat.h for information about how to identify legible media selection options that offer the features of closed captions for accessibility purposes.
--
-- You can select or deselect a specific AVMediaSelectionOption via -[AVPlayerItem selectMediaOption:inMediaSelectionGroup:].
--
-- For further information about Media Accessibility preferences, see MediaAccessibility framework documentation.
--
-- ObjC selector: @- closedCaptionDisplayEnabled@
closedCaptionDisplayEnabled :: IsAVPlayer avPlayer => avPlayer -> IO Bool
closedCaptionDisplayEnabled avPlayer =
  sendMessage avPlayer closedCaptionDisplayEnabledSelector

-- | Indicates whether display of closed captions is enabled.
--
-- This property is deprecated.
--
-- When the value of appliesMediaSelectionCriteriaAutomatically is YES, the receiver will enable closed captions automatically either according to user preferences or, if you provide them, according to AVPlayerMediaSelectionCriteria for the media characteristic AVMediaCharacteristicLegible.
--
-- If you want to determine whether closed captions may be available for a given AVPlayerItem, you can examine the AVMediaSelectionOptions in the AVMediaSelectionGroup for the characteristic AVMediaCharacteristicLegible, as vended by -[AVAsset mediaSelectionGroupForMediaCharacteristic:]. See AVMediaCharacteristicTranscribesSpokenDialogForAccessibility and AVMediaCharacteristicDescribesMusicAndSoundForAccessibility as documented in AVMediaFormat.h for information about how to identify legible media selection options that offer the features of closed captions for accessibility purposes.
--
-- You can select or deselect a specific AVMediaSelectionOption via -[AVPlayerItem selectMediaOption:inMediaSelectionGroup:].
--
-- For further information about Media Accessibility preferences, see MediaAccessibility framework documentation.
--
-- ObjC selector: @- setClosedCaptionDisplayEnabled:@
setClosedCaptionDisplayEnabled :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setClosedCaptionDisplayEnabled avPlayer value =
  sendMessage avPlayer setClosedCaptionDisplayEnabledSelector value

-- | Use sourceClock instead.
--
-- ObjC selector: @- masterClock@
masterClock :: IsAVPlayer avPlayer => avPlayer -> IO (Ptr ())
masterClock avPlayer =
  sendMessage avPlayer masterClockSelector

-- | Use sourceClock instead.
--
-- ObjC selector: @- setMasterClock:@
setMasterClock :: IsAVPlayer avPlayer => avPlayer -> Ptr () -> IO ()
setMasterClock avPlayer value =
  sendMessage avPlayer setMasterClockSelector value

-- | AVPlayer and other AVFoundation types can optionally be observed using Swift Observation.
--
-- When set to YES, new instances of AVPlayer, AVQueuePlayer, AVPlayerItem, and AVPlayerItemTrack are observable with Swift Observation. The default value is NO (not observable).  An exception is thrown if this property is set YES after initializing any objects of these types, or if it is set to NO after any observable objects are initialized.  In other words, all objects of these types must either be observable or not observable in an application instance.
--
-- For more information regarding management of class objects in SwiftUI, please refer to https://developer.apple.com/documentation/swiftui/state.
--
-- ObjC selector: @+ observationEnabled@
observationEnabled :: IO Bool
observationEnabled  =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' observationEnabledSelector

-- | AVPlayer and other AVFoundation types can optionally be observed using Swift Observation.
--
-- When set to YES, new instances of AVPlayer, AVQueuePlayer, AVPlayerItem, and AVPlayerItemTrack are observable with Swift Observation. The default value is NO (not observable).  An exception is thrown if this property is set YES after initializing any objects of these types, or if it is set to NO after any observable objects are initialized.  In other words, all objects of these types must either be observable or not observable in an application instance.
--
-- For more information regarding management of class objects in SwiftUI, please refer to https://developer.apple.com/documentation/swiftui/state.
--
-- ObjC selector: @+ setObservationEnabled:@
setObservationEnabled :: Bool -> IO ()
setObservationEnabled value =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' setObservationEnabledSelector value

-- | Whether the player's audio output is suppressed due to being on a non-mixable audio route.
--
-- If YES, the player's audio output is suppressed. The player is muted while on a non-mixable audio route and cannot play audio. The player's mute property does not reflect the true mute status. If NO, the player's audio output is not suppressed. The player may be muted or unmuted while on a non-mixable audio route and can play audio. The player's mute property reflects the true mute status. In a non-mixable audio route, only one player can play audio. To play audio in non-mixable states, the player must be specified as the priority participant in AVRoutingPlaybackArbiter.preferredParticipantForNonMixableAudioRoutes. If this player becomes the preferred player, it will gain audio priority and suppress the audio of all other players. If another participant becomes the preferred participant, this player will lose audio priority and have their audio suppressed. This property is key-value observed.
--
-- ObjC selector: @- audioOutputSuppressedDueToNonMixableAudioRoute@
audioOutputSuppressedDueToNonMixableAudioRoute :: IsAVPlayer avPlayer => avPlayer -> IO Bool
audioOutputSuppressedDueToNonMixableAudioRoute avPlayer =
  sendMessage avPlayer audioOutputSuppressedDueToNonMixableAudioRouteSelector

-- | The AVPlayer's intended spatial audio experience.
--
-- The default value of CAAutomaticSpatialAudio means the player uses its AVAudioSession's intended spatial experience. If the anchoring strategy is impossible (e.g. it uses a destroyed UIScene's identifier), the player follows a "front" anchoring strategy instead.
--
-- ObjC selector: @- intendedSpatialAudioExperience@
intendedSpatialAudioExperience :: IsAVPlayer avPlayer => avPlayer -> IO RawId
intendedSpatialAudioExperience avPlayer =
  sendMessage avPlayer intendedSpatialAudioExperienceSelector

-- | The AVPlayer's intended spatial audio experience.
--
-- The default value of CAAutomaticSpatialAudio means the player uses its AVAudioSession's intended spatial experience. If the anchoring strategy is impossible (e.g. it uses a destroyed UIScene's identifier), the player follows a "front" anchoring strategy instead.
--
-- ObjC selector: @- setIntendedSpatialAudioExperience:@
setIntendedSpatialAudioExperience :: IsAVPlayer avPlayer => avPlayer -> RawId -> IO ()
setIntendedSpatialAudioExperience avPlayer value =
  sendMessage avPlayer setIntendedSpatialAudioExperienceSelector value

-- | Indicates the priority of this player for network bandwidth resource distribution.
--
-- This value determines the priority of the player during network resource allocation among all other players within the same application process. The default value for this is AVPlayerNetworkResourcePriorityDefault.
--
-- ObjC selector: @- networkResourcePriority@
networkResourcePriority :: IsAVPlayer avPlayer => avPlayer -> IO AVPlayerNetworkResourcePriority
networkResourcePriority avPlayer =
  sendMessage avPlayer networkResourcePrioritySelector

-- | Indicates the priority of this player for network bandwidth resource distribution.
--
-- This value determines the priority of the player during network resource allocation among all other players within the same application process. The default value for this is AVPlayerNetworkResourcePriorityDefault.
--
-- ObjC selector: @- setNetworkResourcePriority:@
setNetworkResourcePriority :: IsAVPlayer avPlayer => avPlayer -> AVPlayerNetworkResourcePriority -> IO ()
setNetworkResourcePriority avPlayer value =
  sendMessage avPlayer setNetworkResourcePrioritySelector value

-- | The video output for this player, if one was set.
--
-- When an AVPlayerVideoOutput is associated with an AVPlayer, the AVPlayerVideoOutput can then be used to receive video-related samples during playback.
--
-- - NOTE: If an output is set while AVPlayer has a current item it may cause different data channels to be selected for that item, which can have a performance impact.  				As a result, when possible, it is best to set an output before setting items on an AVPlayer.
--
-- ObjC selector: @- videoOutput@
videoOutput :: IsAVPlayer avPlayer => avPlayer -> IO (Id AVPlayerVideoOutput)
videoOutput avPlayer =
  sendMessage avPlayer videoOutputSelector

-- | The video output for this player, if one was set.
--
-- When an AVPlayerVideoOutput is associated with an AVPlayer, the AVPlayerVideoOutput can then be used to receive video-related samples during playback.
--
-- - NOTE: If an output is set while AVPlayer has a current item it may cause different data channels to be selected for that item, which can have a performance impact.  				As a result, when possible, it is best to set an output before setting items on an AVPlayer.
--
-- ObjC selector: @- setVideoOutput:@
setVideoOutput :: (IsAVPlayer avPlayer, IsAVPlayerVideoOutput value) => avPlayer -> value -> IO ()
setVideoOutput avPlayer value =
  sendMessage avPlayer setVideoOutputSelector (toAVPlayerVideoOutput value)

-- | The playback coordinator for this player.
--
-- If the playback coordinator is connected to other participants, rate changes and seeks on the current item will be automatically mirrored to all connected participants. Depending on policies, the coordinator may also intercept rate changes to non-zero to coordinate playback start with the rest of the group. Use [AVPlayer playImmediatelyAtRate:] to override the coordinated startup behavior and start playback immediately. This is useful to give users an opportunity to override waiting caused by other participants' suspensions. Player configuration other than rate and seeks are not communicated to other participants and can be configured independently by each participant. A player with a connected playbackCoordinator will change behavior in situations that require the player to pause for internal reasons, such as a route change or a stall. When resuming after these events, the player will not resume at the stop time. Instead, it will attempt to rejoin the group, potentially seeking to match the other participant's progress. It is left to the owner of the AVPlayer to ensure that all participants are playing the same item. See the discussion of AVPlaybackCoordinator for considerations about item transitions.
--
-- ObjC selector: @- playbackCoordinator@
playbackCoordinator :: IsAVPlayer avPlayer => avPlayer -> IO (Id AVPlayerPlaybackCoordinator)
playbackCoordinator avPlayer =
  sendMessage avPlayer playbackCoordinatorSelector

-- | Controls the policy to be used in deciding how playback of audiovisual content should continue while the application transitions to background.
--
-- By default, the system is free to decide the background playback policy (AVPlayerAudiovisualBackgroundPlaybackPolicyAutomatic). If set to AVPlayerAudiovisualBackgroundPlaybackPolicyPauses, player will be paused on entering background. If set to AVPlayerAudiovisualBackgroundPlaybackPolicyContinuesIfPossible, the system makes the best effort to continue playback but the app also needs appropriate UIBackgroundModes for the system to let it continue running in the background. Note that this policy only applies to items with enabled video.
--
-- ObjC selector: @- audiovisualBackgroundPlaybackPolicy@
audiovisualBackgroundPlaybackPolicy :: IsAVPlayer avPlayer => avPlayer -> IO AVPlayerAudiovisualBackgroundPlaybackPolicy
audiovisualBackgroundPlaybackPolicy avPlayer =
  sendMessage avPlayer audiovisualBackgroundPlaybackPolicySelector

-- | Controls the policy to be used in deciding how playback of audiovisual content should continue while the application transitions to background.
--
-- By default, the system is free to decide the background playback policy (AVPlayerAudiovisualBackgroundPlaybackPolicyAutomatic). If set to AVPlayerAudiovisualBackgroundPlaybackPolicyPauses, player will be paused on entering background. If set to AVPlayerAudiovisualBackgroundPlaybackPolicyContinuesIfPossible, the system makes the best effort to continue playback but the app also needs appropriate UIBackgroundModes for the system to let it continue running in the background. Note that this policy only applies to items with enabled video.
--
-- ObjC selector: @- setAudiovisualBackgroundPlaybackPolicy:@
setAudiovisualBackgroundPlaybackPolicy :: IsAVPlayer avPlayer => avPlayer -> AVPlayerAudiovisualBackgroundPlaybackPolicy -> IO ()
setAudiovisualBackgroundPlaybackPolicy avPlayer value =
  sendMessage avPlayer setAudiovisualBackgroundPlaybackPolicySelector value

-- | Indicates whether video playback prevents the app from automatically getting backgrounded.
--
-- Default value is YES. Setting this property to YES prevents an application that is playing video from automatically getting backgrounded. This property does not prevent the user from backgrounding the application.
--
-- ObjC selector: @- preventsAutomaticBackgroundingDuringVideoPlayback@
preventsAutomaticBackgroundingDuringVideoPlayback :: IsAVPlayer avPlayer => avPlayer -> IO Bool
preventsAutomaticBackgroundingDuringVideoPlayback avPlayer =
  sendMessage avPlayer preventsAutomaticBackgroundingDuringVideoPlaybackSelector

-- | Indicates whether video playback prevents the app from automatically getting backgrounded.
--
-- Default value is YES. Setting this property to YES prevents an application that is playing video from automatically getting backgrounded. This property does not prevent the user from backgrounding the application.
--
-- ObjC selector: @- setPreventsAutomaticBackgroundingDuringVideoPlayback:@
setPreventsAutomaticBackgroundingDuringVideoPlayback :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setPreventsAutomaticBackgroundingDuringVideoPlayback avPlayer value =
  sendMessage avPlayer setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector value

-- | Indicates whether video playback prevents display and device sleep.
--
-- Default is YES on iOS, tvOS and in Mac Catalyst apps. Default is NO on macOS. Setting this property to NO does not force the display to sleep, it simply stops preventing display sleep. Other apps or frameworks within your app may still be preventing display sleep for various reasons.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- preventsDisplaySleepDuringVideoPlayback@
preventsDisplaySleepDuringVideoPlayback :: IsAVPlayer avPlayer => avPlayer -> IO Bool
preventsDisplaySleepDuringVideoPlayback avPlayer =
  sendMessage avPlayer preventsDisplaySleepDuringVideoPlaybackSelector

-- | Indicates whether video playback prevents display and device sleep.
--
-- Default is YES on iOS, tvOS and in Mac Catalyst apps. Default is NO on macOS. Setting this property to NO does not force the display to sleep, it simply stops preventing display sleep. Other apps or frameworks within your app may still be preventing display sleep for various reasons.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setPreventsDisplaySleepDuringVideoPlayback:@
setPreventsDisplaySleepDuringVideoPlayback :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setPreventsDisplaySleepDuringVideoPlayback avPlayer value =
  sendMessage avPlayer setPreventsDisplaySleepDuringVideoPlaybackSelector value

-- | Specifies a registryID associated with a GPU that should be used for video decode.
--
-- By default, whenever possible, video decode will be performed on the GPU associated with the display on which the presenting CALayer is located. Decode will be transitioned to a new GPU if appropriate when the CALayer moves to a new display. This property overrides this default behavior, forcing decode to prefer an affinity to the GPU specified regardless of which GPU is being used to display the associated CALayer.
--
-- The GPU registryID can be obtained from the GPU MTLDevice using [MTLDevice registryID] or can be obtained from OpenGL or OpenCL.
--
-- ObjC selector: @- preferredVideoDecoderGPURegistryID@
preferredVideoDecoderGPURegistryID :: IsAVPlayer avPlayer => avPlayer -> IO CULong
preferredVideoDecoderGPURegistryID avPlayer =
  sendMessage avPlayer preferredVideoDecoderGPURegistryIDSelector

-- | Specifies a registryID associated with a GPU that should be used for video decode.
--
-- By default, whenever possible, video decode will be performed on the GPU associated with the display on which the presenting CALayer is located. Decode will be transitioned to a new GPU if appropriate when the CALayer moves to a new display. This property overrides this default behavior, forcing decode to prefer an affinity to the GPU specified regardless of which GPU is being used to display the associated CALayer.
--
-- The GPU registryID can be obtained from the GPU MTLDevice using [MTLDevice registryID] or can be obtained from OpenGL or OpenCL.
--
-- ObjC selector: @- setPreferredVideoDecoderGPURegistryID:@
setPreferredVideoDecoderGPURegistryID :: IsAVPlayer avPlayer => avPlayer -> CULong -> IO ()
setPreferredVideoDecoderGPURegistryID avPlayer value =
  sendMessage avPlayer setPreferredVideoDecoderGPURegistryIDSelector value

-- | An AVPlayerHDRMode value that indicates the HDR modes the device can play to an appropriate display. A value of 0 indicates that no HDR modes are supported.
--
-- This property indicates all of the HDR modes that the device can play. Each value indicates that an appropriate HDR display is available for the specified HDR mode. Additionally, the device must be capable of playing the specified HDR type. This property does not indicate whether video contains HDR content, whether HDR video is currently playing, or whether video is playing on an HDR display.
--
-- ObjC selector: @+ availableHDRModes@
availableHDRModes :: IO AVPlayerHDRMode
availableHDRModes  =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' availableHDRModesSelector

-- | Indicates whether HDR content can be played to an appropriate display.
--
-- This property is YES if an HDR display is available and the device is capable of playing HDR content from an appropriate AVAsset, NO otherwise. This property does not indicate whether video contains HDR content, whether HDR video is currently playing, or whether video is playing on an HDR display. This property is not KVO observable.
--
-- ObjC selector: @+ eligibleForHDRPlayback@
eligibleForHDRPlayback :: IO Bool
eligibleForHDRPlayback  =
  do
    cls' <- getRequiredClass "AVPlayer"
    sendClassMessage cls' eligibleForHDRPlaybackSelector

-- | Whether or not decoded output is being obscured due to insufficient external protection.
--
-- The value of this property indicates whether the player is purposefully obscuring the visual output of the current item because the requirement for an external protection mechanism is not met by the current device configuration. It is highly recommended that clients whose content requires external protection observe this property and set the playback rate to zero and display an appropriate user interface when the value changes to YES. This property is key value observable.
--
-- Note that the value of this property is dependent on the external protection requirements of the current item. These requirements are inherent to the content itself and cannot be externally specified. If the current item does not require external protection, the value of this property will be NO.
--
-- ObjC selector: @- outputObscuredDueToInsufficientExternalProtection@
outputObscuredDueToInsufficientExternalProtection :: IsAVPlayer avPlayer => avPlayer -> IO Bool
outputObscuredDueToInsufficientExternalProtection avPlayer =
  sendMessage avPlayer outputObscuredDueToInsufficientExternalProtectionSelector

-- | Indicates whether the player allows switching to "external playback" mode. The default value is YES.
--
-- ObjC selector: @- allowsExternalPlayback@
allowsExternalPlayback :: IsAVPlayer avPlayer => avPlayer -> IO Bool
allowsExternalPlayback avPlayer =
  sendMessage avPlayer allowsExternalPlaybackSelector

-- | Indicates whether the player allows switching to "external playback" mode. The default value is YES.
--
-- ObjC selector: @- setAllowsExternalPlayback:@
setAllowsExternalPlayback :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setAllowsExternalPlayback avPlayer value =
  sendMessage avPlayer setAllowsExternalPlaybackSelector value

-- | Indicates whether the player is currently playing video in "external playback" mode.
--
-- ObjC selector: @- externalPlaybackActive@
externalPlaybackActive :: IsAVPlayer avPlayer => avPlayer -> IO Bool
externalPlaybackActive avPlayer =
  sendMessage avPlayer externalPlaybackActiveSelector

-- | Indicates whether the player should automatically switch to "external playback" mode while the "external screen" mode is active in order to play video content and switching back to "external screen" mode as soon as playback is done. Brief transition may be visible on the external display when automatically switching between the two modes. The default value is NO. Has no effect if allowsExternalPlayback is NO.
--
-- ObjC selector: @- usesExternalPlaybackWhileExternalScreenIsActive@
usesExternalPlaybackWhileExternalScreenIsActive :: IsAVPlayer avPlayer => avPlayer -> IO Bool
usesExternalPlaybackWhileExternalScreenIsActive avPlayer =
  sendMessage avPlayer usesExternalPlaybackWhileExternalScreenIsActiveSelector

-- | Indicates whether the player should automatically switch to "external playback" mode while the "external screen" mode is active in order to play video content and switching back to "external screen" mode as soon as playback is done. Brief transition may be visible on the external display when automatically switching between the two modes. The default value is NO. Has no effect if allowsExternalPlayback is NO.
--
-- ObjC selector: @- setUsesExternalPlaybackWhileExternalScreenIsActive:@
setUsesExternalPlaybackWhileExternalScreenIsActive :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setUsesExternalPlaybackWhileExternalScreenIsActive avPlayer value =
  sendMessage avPlayer setUsesExternalPlaybackWhileExternalScreenIsActiveSelector value

-- | Video gravity strictly for "external playback" mode, one of AVLayerVideoGravity* defined in AVAnimation.h
--
-- ObjC selector: @- externalPlaybackVideoGravity@
externalPlaybackVideoGravity :: IsAVPlayer avPlayer => avPlayer -> IO (Id NSString)
externalPlaybackVideoGravity avPlayer =
  sendMessage avPlayer externalPlaybackVideoGravitySelector

-- | Video gravity strictly for "external playback" mode, one of AVLayerVideoGravity* defined in AVAnimation.h
--
-- ObjC selector: @- setExternalPlaybackVideoGravity:@
setExternalPlaybackVideoGravity :: (IsAVPlayer avPlayer, IsNSString value) => avPlayer -> value -> IO ()
setExternalPlaybackVideoGravity avPlayer value =
  sendMessage avPlayer setExternalPlaybackVideoGravitySelector (toNSString value)

-- | Specifies the unique ID of the Core Audio output device used to play audio.
--
-- By default, the value of this property is nil, indicating that the default audio output device is used. Otherwise the value of this property is an NSString containing the unique ID of the Core Audio output device to be used for audio output.
--
-- Core Audio's kAudioDevicePropertyDeviceUID is a suitable source of audio output device unique IDs.
--
-- ObjC selector: @- audioOutputDeviceUniqueID@
audioOutputDeviceUniqueID :: IsAVPlayer avPlayer => avPlayer -> IO (Id NSString)
audioOutputDeviceUniqueID avPlayer =
  sendMessage avPlayer audioOutputDeviceUniqueIDSelector

-- | Specifies the unique ID of the Core Audio output device used to play audio.
--
-- By default, the value of this property is nil, indicating that the default audio output device is used. Otherwise the value of this property is an NSString containing the unique ID of the Core Audio output device to be used for audio output.
--
-- Core Audio's kAudioDevicePropertyDeviceUID is a suitable source of audio output device unique IDs.
--
-- ObjC selector: @- setAudioOutputDeviceUniqueID:@
setAudioOutputDeviceUniqueID :: (IsAVPlayer avPlayer, IsNSString value) => avPlayer -> value -> IO ()
setAudioOutputDeviceUniqueID avPlayer value =
  sendMessage avPlayer setAudioOutputDeviceUniqueIDSelector (toNSString value)

-- | Indicates whether the receiver should apply the current selection criteria automatically to AVPlayerItems.
--
-- For clients linked against the iOS 7 SDK or later or against the macOS 10.9 SDK or later, the default is YES. For all others, the default is NO.
--
-- By default, AVPlayer applies selection criteria based on system preferences. To override the default criteria for any media selection group, use -[AVPlayer setMediaSelectionCriteria:forMediaCharacteristic:].
--
-- ObjC selector: @- appliesMediaSelectionCriteriaAutomatically@
appliesMediaSelectionCriteriaAutomatically :: IsAVPlayer avPlayer => avPlayer -> IO Bool
appliesMediaSelectionCriteriaAutomatically avPlayer =
  sendMessage avPlayer appliesMediaSelectionCriteriaAutomaticallySelector

-- | Indicates whether the receiver should apply the current selection criteria automatically to AVPlayerItems.
--
-- For clients linked against the iOS 7 SDK or later or against the macOS 10.9 SDK or later, the default is YES. For all others, the default is NO.
--
-- By default, AVPlayer applies selection criteria based on system preferences. To override the default criteria for any media selection group, use -[AVPlayer setMediaSelectionCriteria:forMediaCharacteristic:].
--
-- ObjC selector: @- setAppliesMediaSelectionCriteriaAutomatically:@
setAppliesMediaSelectionCriteriaAutomatically :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setAppliesMediaSelectionCriteriaAutomatically avPlayer value =
  sendMessage avPlayer setAppliesMediaSelectionCriteriaAutomaticallySelector value

-- | Indicates the current audio volume of the player; 0.0 means "silence all audio", 1.0 means "play at the full volume of the current item".
--
-- iOS note: Do not use this property to implement a volume slider for media playback. For that purpose, use MPVolumeView, which is customizable in appearance and provides standard media playback behaviors that users expect. This property is most useful on iOS to control the volume of the AVPlayer relative to other audio output, not for volume control by end users.
--
-- ObjC selector: @- volume@
volume :: IsAVPlayer avPlayer => avPlayer -> IO CFloat
volume avPlayer =
  sendMessage avPlayer volumeSelector

-- | Indicates the current audio volume of the player; 0.0 means "silence all audio", 1.0 means "play at the full volume of the current item".
--
-- iOS note: Do not use this property to implement a volume slider for media playback. For that purpose, use MPVolumeView, which is customizable in appearance and provides standard media playback behaviors that users expect. This property is most useful on iOS to control the volume of the AVPlayer relative to other audio output, not for volume control by end users.
--
-- ObjC selector: @- setVolume:@
setVolume :: IsAVPlayer avPlayer => avPlayer -> CFloat -> IO ()
setVolume avPlayer value =
  sendMessage avPlayer setVolumeSelector value

-- | Indicates whether or not audio output of the player is muted. Only affects audio muting for the player instance and not for the device.
--
-- ObjC selector: @- muted@
muted :: IsAVPlayer avPlayer => avPlayer -> IO Bool
muted avPlayer =
  sendMessage avPlayer mutedSelector

-- | Indicates whether or not audio output of the player is muted. Only affects audio muting for the player instance and not for the device.
--
-- ObjC selector: @- setMuted:@
setMuted :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setMuted avPlayer value =
  sendMessage avPlayer setMutedSelector value

-- | Indicates that the player is allowed to delay playback at the specified rate in order to minimize stalling
--
-- When this property is YES, whenever 1) the rate is set from zero to non-zero or 2) the playback buffer becomes empty and playback stalls, the player will attempt to determine if, at the specified rate, its currentItem will play to the end without interruptions. Should it determine that such interruptions would occur and these interruptions can be avoided by delaying the start or resumption of playback, the value of timeControlStatus will become AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and playback will start automatically when the likelihood of stalling has been minimized.
--
-- You may want to set this property to NO when you need precise control over playback start times, e.g., when synchronizing multiple instances of AVPlayer, and you should set it to NO if you use an AVAssetResourceLoader delegate to load media data (more on this below). If the value of this property is NO, reasonForWaitingToPlay cannot assume a value of AVPlayerWaitingToMinimizeStallsReason. This implies that setting rate to a non-zero value in AVPlayerTimeControlStatusPaused will cause playback to start immediately as long as the playback buffer is not empty. When the playback buffer becomes empty during AVPlayerTimeControlStatusPlaying and playback stalls, playback state will switch to AVPlayerTimeControlStatusPaused and the rate will become 0.0.
--
-- Changing the value of this property to NO while the value of timeControlStatus is AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate with a reasonForWaitingToPlay of AVPlayerWaitingToMinimizeStallsReason will cause the player to attempt playback at the specified rate immediately.
--
-- For clients linked against iOS 10.0 and running on that version or later or linked against macOS 10.12 and running on that version or later, the default value of this property is YES. In versions of iOS prior to iOS 10.0 and versions of macOS prior to 10.12, this property is unavailable, and the behavior of the AVPlayer corresponds to the type of content being played. For streaming content, including HTTP Live Streaming, the AVPlayer acts as if automaticallyWaitsToMinimizeStalling is YES. For file-based content, including file-based content accessed via progressive http download, the AVPlayer acts as if automaticallyWaitsToMinimizeStalling is NO.
--
-- If you employ an AVAssetResourceLoader delegate that loads media data for playback, you should set the value of your AVPlayer’s automaticallyWaitsToMinimizeStalling property to NO. Allowing the value of automaticallyWaitsToMinimizeStalling to remain YES when an AVAssetResourceLoader delegate is used for the loading of media data can result in poor start-up times for playback and poor recovery from stalls, because the behaviors provided by AVPlayer when automaticallyWaitsToMinimizeStalling has a value of YES depend on predictions of the future availability of media data that that do not function as expected when data is loaded via a client-controlled means, using the AVAssetResourceLoader delegate interface.
--
-- You can allow the value of automaticallyWaitsToMinimizeStalling to remain YES if you use an AVAssetResourceLoader delegate to manage content keys for FairPlay Streaming, to provide dynamically-generated master playlists for HTTP Live Streaming, or to respond to authentication challenges, but not to load media data for playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- automaticallyWaitsToMinimizeStalling@
automaticallyWaitsToMinimizeStalling :: IsAVPlayer avPlayer => avPlayer -> IO Bool
automaticallyWaitsToMinimizeStalling avPlayer =
  sendMessage avPlayer automaticallyWaitsToMinimizeStallingSelector

-- | Indicates that the player is allowed to delay playback at the specified rate in order to minimize stalling
--
-- When this property is YES, whenever 1) the rate is set from zero to non-zero or 2) the playback buffer becomes empty and playback stalls, the player will attempt to determine if, at the specified rate, its currentItem will play to the end without interruptions. Should it determine that such interruptions would occur and these interruptions can be avoided by delaying the start or resumption of playback, the value of timeControlStatus will become AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and playback will start automatically when the likelihood of stalling has been minimized.
--
-- You may want to set this property to NO when you need precise control over playback start times, e.g., when synchronizing multiple instances of AVPlayer, and you should set it to NO if you use an AVAssetResourceLoader delegate to load media data (more on this below). If the value of this property is NO, reasonForWaitingToPlay cannot assume a value of AVPlayerWaitingToMinimizeStallsReason. This implies that setting rate to a non-zero value in AVPlayerTimeControlStatusPaused will cause playback to start immediately as long as the playback buffer is not empty. When the playback buffer becomes empty during AVPlayerTimeControlStatusPlaying and playback stalls, playback state will switch to AVPlayerTimeControlStatusPaused and the rate will become 0.0.
--
-- Changing the value of this property to NO while the value of timeControlStatus is AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate with a reasonForWaitingToPlay of AVPlayerWaitingToMinimizeStallsReason will cause the player to attempt playback at the specified rate immediately.
--
-- For clients linked against iOS 10.0 and running on that version or later or linked against macOS 10.12 and running on that version or later, the default value of this property is YES. In versions of iOS prior to iOS 10.0 and versions of macOS prior to 10.12, this property is unavailable, and the behavior of the AVPlayer corresponds to the type of content being played. For streaming content, including HTTP Live Streaming, the AVPlayer acts as if automaticallyWaitsToMinimizeStalling is YES. For file-based content, including file-based content accessed via progressive http download, the AVPlayer acts as if automaticallyWaitsToMinimizeStalling is NO.
--
-- If you employ an AVAssetResourceLoader delegate that loads media data for playback, you should set the value of your AVPlayer’s automaticallyWaitsToMinimizeStalling property to NO. Allowing the value of automaticallyWaitsToMinimizeStalling to remain YES when an AVAssetResourceLoader delegate is used for the loading of media data can result in poor start-up times for playback and poor recovery from stalls, because the behaviors provided by AVPlayer when automaticallyWaitsToMinimizeStalling has a value of YES depend on predictions of the future availability of media data that that do not function as expected when data is loaded via a client-controlled means, using the AVAssetResourceLoader delegate interface.
--
-- You can allow the value of automaticallyWaitsToMinimizeStalling to remain YES if you use an AVAssetResourceLoader delegate to manage content keys for FairPlay Streaming, to provide dynamically-generated master playlists for HTTP Live Streaming, or to respond to authentication challenges, but not to load media data for playback.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setAutomaticallyWaitsToMinimizeStalling:@
setAutomaticallyWaitsToMinimizeStalling :: IsAVPlayer avPlayer => avPlayer -> Bool -> IO ()
setAutomaticallyWaitsToMinimizeStalling avPlayer value =
  sendMessage avPlayer setAutomaticallyWaitsToMinimizeStallingSelector value

-- | Set to override the automatic choice of source clock for item timebases.
--
-- NULL by default. This is most useful for synchronizing video-only movies with audio played via other means. IMPORTANT NOTE: If you specify a source clock other than the appropriate audio device clock, audio may drift out of sync.
--
-- ObjC selector: @- sourceClock@
sourceClock :: IsAVPlayer avPlayer => avPlayer -> IO (Ptr ())
sourceClock avPlayer =
  sendMessage avPlayer sourceClockSelector

-- | Set to override the automatic choice of source clock for item timebases.
--
-- NULL by default. This is most useful for synchronizing video-only movies with audio played via other means. IMPORTANT NOTE: If you specify a source clock other than the appropriate audio device clock, audio may drift out of sync.
--
-- ObjC selector: @- setSourceClock:@
setSourceClock :: IsAVPlayer avPlayer => avPlayer -> Ptr () -> IO ()
setSourceClock avPlayer value =
  sendMessage avPlayer setSourceClockSelector value

-- | Indicates the current item of the player
--
-- ObjC selector: @- currentItem@
currentItem :: IsAVPlayer avPlayer => avPlayer -> IO (Id AVPlayerItem)
currentItem avPlayer =
  sendMessage avPlayer currentItemSelector

-- | Indicates the action that the player should perform when playback of an item reaches its end time.
--
-- This property throws an exception if set to AVPlayerActionAtItemEndAdvance on an AVPlayer which is not an AVQueuePlayer.
--
-- ObjC selector: @- actionAtItemEnd@
actionAtItemEnd :: IsAVPlayer avPlayer => avPlayer -> IO AVPlayerActionAtItemEnd
actionAtItemEnd avPlayer =
  sendMessage avPlayer actionAtItemEndSelector

-- | Indicates the action that the player should perform when playback of an item reaches its end time.
--
-- This property throws an exception if set to AVPlayerActionAtItemEndAdvance on an AVPlayer which is not an AVQueuePlayer.
--
-- ObjC selector: @- setActionAtItemEnd:@
setActionAtItemEnd :: IsAVPlayer avPlayer => avPlayer -> AVPlayerActionAtItemEnd -> IO ()
setActionAtItemEnd avPlayer value =
  sendMessage avPlayer setActionAtItemEndSelector value

-- | Indicates the desired rate of playback; 0.0 means "paused", 1.0 indicates a desire to play at the natural rate of the current item.
--
-- Setting the value of rate to 0.0 pauses playback, causing the value of timeControlStatus to change to AVPlayerTimeControlStatusPaused. Setting the rate to a non-zero value causes the value of timeControlStatus to become either AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate or AVPlayerTimeControlStatusPlaying, depending on whether sufficient media data has been buffered for playback to occur and whether the player's default behavior of waiting in order to minimize stalling is permitted. See discussion of AVPlayerTimeControlStatus for more details.
--
-- AVPlayer can reset the desired rate to 0.0 when a change in overall state requires playback to be halted, such as when an interruption occurs on iOS, as announced by AVAudioSession, or when the playback buffer becomes empty and playback stalls while automaticallyWaitsToMinimizeStalling is NO.
--
-- The effective rate of playback may differ from the desired rate even while timeControlStatus is AVPlayerTimeControlStatusPlaying, if the processing algorithm in use for managing audio pitch requires quantization of playback rate. For information about quantization of rates for audio processing, see AVAudioProcessingSettings.h. You can always obtain the effective rate of playback from the currentItem's timebase; see the timebase property of AVPlayerItem.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- rate@
rate :: IsAVPlayer avPlayer => avPlayer -> IO CFloat
rate avPlayer =
  sendMessage avPlayer rateSelector

-- | Indicates the desired rate of playback; 0.0 means "paused", 1.0 indicates a desire to play at the natural rate of the current item.
--
-- Setting the value of rate to 0.0 pauses playback, causing the value of timeControlStatus to change to AVPlayerTimeControlStatusPaused. Setting the rate to a non-zero value causes the value of timeControlStatus to become either AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate or AVPlayerTimeControlStatusPlaying, depending on whether sufficient media data has been buffered for playback to occur and whether the player's default behavior of waiting in order to minimize stalling is permitted. See discussion of AVPlayerTimeControlStatus for more details.
--
-- AVPlayer can reset the desired rate to 0.0 when a change in overall state requires playback to be halted, such as when an interruption occurs on iOS, as announced by AVAudioSession, or when the playback buffer becomes empty and playback stalls while automaticallyWaitsToMinimizeStalling is NO.
--
-- The effective rate of playback may differ from the desired rate even while timeControlStatus is AVPlayerTimeControlStatusPlaying, if the processing algorithm in use for managing audio pitch requires quantization of playback rate. For information about quantization of rates for audio processing, see AVAudioProcessingSettings.h. You can always obtain the effective rate of playback from the currentItem's timebase; see the timebase property of AVPlayerItem.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVPlayer avPlayer => avPlayer -> CFloat -> IO ()
setRate avPlayer value =
  sendMessage avPlayer setRateSelector value

-- | Indicates the rate at which to start playback when play is called; defaults to 1.0.
--
-- Setting this property does not imply playback starts automatically at this rate. Clients still have to kick off playback using @play@. Note that using setRate to start playback will skip using the value in this property nor would it update this property. Therefore, @setRate:1.0@ is no longer recommended as a means to start playback. Use @play@ instead. Use @setRate@ for operations like scanning where the rate is to be updated instantaneously. Invoking @play@ again would restore playback at the rate set in this property.
--
-- The effective rate of playback may still differ from the default rate subject to restrictions imposed by the system. See documentation for the rate property for a discussion on when the desired rate does not translate to effective rate.
--
-- ObjC selector: @- defaultRate@
defaultRate :: IsAVPlayer avPlayer => avPlayer -> IO CFloat
defaultRate avPlayer =
  sendMessage avPlayer defaultRateSelector

-- | Indicates the rate at which to start playback when play is called; defaults to 1.0.
--
-- Setting this property does not imply playback starts automatically at this rate. Clients still have to kick off playback using @play@. Note that using setRate to start playback will skip using the value in this property nor would it update this property. Therefore, @setRate:1.0@ is no longer recommended as a means to start playback. Use @play@ instead. Use @setRate@ for operations like scanning where the rate is to be updated instantaneously. Invoking @play@ again would restore playback at the rate set in this property.
--
-- The effective rate of playback may still differ from the default rate subject to restrictions imposed by the system. See documentation for the rate property for a discussion on when the desired rate does not translate to effective rate.
--
-- ObjC selector: @- setDefaultRate:@
setDefaultRate :: IsAVPlayer avPlayer => avPlayer -> CFloat -> IO ()
setDefaultRate avPlayer value =
  sendMessage avPlayer setDefaultRateSelector value

-- | Indicates whether playback is currently paused indefinitely, suspended while waiting for appropriate conditions, or in progress.
--
-- For possible values and discussion, see AVPlayerTimeControlStatus.
--
-- When automaticallyWaitsToMinimizeStalling is YES, absent intervention in the form of invocations of -setRate: or -pause or, on iOS, an interruption that requires user intervention before playback can resume, the value of the property timeControlStatus automatically changes between AVPlayerTimeControlStatusPlaying and AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate depending on whether sufficient media data is available to continue playback. This property is key value observable.
--
-- ObjC selector: @- timeControlStatus@
timeControlStatus :: IsAVPlayer avPlayer => avPlayer -> IO AVPlayerTimeControlStatus
timeControlStatus avPlayer =
  sendMessage avPlayer timeControlStatusSelector

-- | Indicates the reason for waiting when the value of timeControlStatus is AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate
--
-- When the value of timeControlStatus is AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate, this property describes why the player is currently waiting. It is nil otherwise. You can use the value of reasonForWaitingToPlay to show UI indicating the player's waiting state conditionally. This property is key value observable. Possible values are AVPlayerWaitingWithNoItemToPlayReason, AVPlayerWaitingWhileEvaluatingBufferingRateReason, and AVPlayerWaitingToMinimizeStallsReason.
--
-- ObjC selector: @- reasonForWaitingToPlay@
reasonForWaitingToPlay :: IsAVPlayer avPlayer => avPlayer -> IO (Id NSString)
reasonForWaitingToPlay avPlayer =
  sendMessage avPlayer reasonForWaitingToPlaySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayer)
initSelector = mkSelector "init"

-- | @Selector@ for @playerWithURL:@
playerWithURLSelector :: Selector '[Id NSURL] (Id AVPlayer)
playerWithURLSelector = mkSelector "playerWithURL:"

-- | @Selector@ for @playerWithPlayerItem:@
playerWithPlayerItemSelector :: Selector '[Id AVPlayerItem] (Id AVPlayer)
playerWithPlayerItemSelector = mkSelector "playerWithPlayerItem:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id AVPlayer)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithPlayerItem:@
initWithPlayerItemSelector :: Selector '[Id AVPlayerItem] (Id AVPlayer)
initWithPlayerItemSelector = mkSelector "initWithPlayerItem:"

-- | @Selector@ for @setMediaSelectionCriteria:forMediaCharacteristic:@
setMediaSelectionCriteria_forMediaCharacteristicSelector :: Selector '[Id AVPlayerMediaSelectionCriteria, Id NSString] ()
setMediaSelectionCriteria_forMediaCharacteristicSelector = mkSelector "setMediaSelectionCriteria:forMediaCharacteristic:"

-- | @Selector@ for @mediaSelectionCriteriaForMediaCharacteristic:@
mediaSelectionCriteriaForMediaCharacteristicSelector :: Selector '[Id NSString] (Id AVPlayerMediaSelectionCriteria)
mediaSelectionCriteriaForMediaCharacteristicSelector = mkSelector "mediaSelectionCriteriaForMediaCharacteristic:"

-- | @Selector@ for @addBoundaryTimeObserverForTimes:queue:usingBlock:@
addBoundaryTimeObserverForTimes_queue_usingBlockSelector :: Selector '[Id NSArray, Id NSObject, Ptr ()] RawId
addBoundaryTimeObserverForTimes_queue_usingBlockSelector = mkSelector "addBoundaryTimeObserverForTimes:queue:usingBlock:"

-- | @Selector@ for @removeTimeObserver:@
removeTimeObserverSelector :: Selector '[RawId] ()
removeTimeObserverSelector = mkSelector "removeTimeObserver:"

-- | @Selector@ for @prerollAtRate:completionHandler:@
prerollAtRate_completionHandlerSelector :: Selector '[CFloat, Ptr ()] ()
prerollAtRate_completionHandlerSelector = mkSelector "prerollAtRate:completionHandler:"

-- | @Selector@ for @cancelPendingPrerolls@
cancelPendingPrerollsSelector :: Selector '[] ()
cancelPendingPrerollsSelector = mkSelector "cancelPendingPrerolls"

-- | @Selector@ for @seekToDate:@
seekToDateSelector :: Selector '[Id NSDate] ()
seekToDateSelector = mkSelector "seekToDate:"

-- | @Selector@ for @seekToDate:completionHandler:@
seekToDate_completionHandlerSelector :: Selector '[Id NSDate, Ptr ()] ()
seekToDate_completionHandlerSelector = mkSelector "seekToDate:completionHandler:"

-- | @Selector@ for @replaceCurrentItemWithPlayerItem:@
replaceCurrentItemWithPlayerItemSelector :: Selector '[Id AVPlayerItem] ()
replaceCurrentItemWithPlayerItemSelector = mkSelector "replaceCurrentItemWithPlayerItem:"

-- | @Selector@ for @play@
playSelector :: Selector '[] ()
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @playImmediatelyAtRate:@
playImmediatelyAtRateSelector :: Selector '[CFloat] ()
playImmediatelyAtRateSelector = mkSelector "playImmediatelyAtRate:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVPlayerStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @closedCaptionDisplayEnabled@
closedCaptionDisplayEnabledSelector :: Selector '[] Bool
closedCaptionDisplayEnabledSelector = mkSelector "closedCaptionDisplayEnabled"

-- | @Selector@ for @setClosedCaptionDisplayEnabled:@
setClosedCaptionDisplayEnabledSelector :: Selector '[Bool] ()
setClosedCaptionDisplayEnabledSelector = mkSelector "setClosedCaptionDisplayEnabled:"

-- | @Selector@ for @masterClock@
masterClockSelector :: Selector '[] (Ptr ())
masterClockSelector = mkSelector "masterClock"

-- | @Selector@ for @setMasterClock:@
setMasterClockSelector :: Selector '[Ptr ()] ()
setMasterClockSelector = mkSelector "setMasterClock:"

-- | @Selector@ for @observationEnabled@
observationEnabledSelector :: Selector '[] Bool
observationEnabledSelector = mkSelector "observationEnabled"

-- | @Selector@ for @setObservationEnabled:@
setObservationEnabledSelector :: Selector '[Bool] ()
setObservationEnabledSelector = mkSelector "setObservationEnabled:"

-- | @Selector@ for @audioOutputSuppressedDueToNonMixableAudioRoute@
audioOutputSuppressedDueToNonMixableAudioRouteSelector :: Selector '[] Bool
audioOutputSuppressedDueToNonMixableAudioRouteSelector = mkSelector "audioOutputSuppressedDueToNonMixableAudioRoute"

-- | @Selector@ for @intendedSpatialAudioExperience@
intendedSpatialAudioExperienceSelector :: Selector '[] RawId
intendedSpatialAudioExperienceSelector = mkSelector "intendedSpatialAudioExperience"

-- | @Selector@ for @setIntendedSpatialAudioExperience:@
setIntendedSpatialAudioExperienceSelector :: Selector '[RawId] ()
setIntendedSpatialAudioExperienceSelector = mkSelector "setIntendedSpatialAudioExperience:"

-- | @Selector@ for @networkResourcePriority@
networkResourcePrioritySelector :: Selector '[] AVPlayerNetworkResourcePriority
networkResourcePrioritySelector = mkSelector "networkResourcePriority"

-- | @Selector@ for @setNetworkResourcePriority:@
setNetworkResourcePrioritySelector :: Selector '[AVPlayerNetworkResourcePriority] ()
setNetworkResourcePrioritySelector = mkSelector "setNetworkResourcePriority:"

-- | @Selector@ for @videoOutput@
videoOutputSelector :: Selector '[] (Id AVPlayerVideoOutput)
videoOutputSelector = mkSelector "videoOutput"

-- | @Selector@ for @setVideoOutput:@
setVideoOutputSelector :: Selector '[Id AVPlayerVideoOutput] ()
setVideoOutputSelector = mkSelector "setVideoOutput:"

-- | @Selector@ for @playbackCoordinator@
playbackCoordinatorSelector :: Selector '[] (Id AVPlayerPlaybackCoordinator)
playbackCoordinatorSelector = mkSelector "playbackCoordinator"

-- | @Selector@ for @audiovisualBackgroundPlaybackPolicy@
audiovisualBackgroundPlaybackPolicySelector :: Selector '[] AVPlayerAudiovisualBackgroundPlaybackPolicy
audiovisualBackgroundPlaybackPolicySelector = mkSelector "audiovisualBackgroundPlaybackPolicy"

-- | @Selector@ for @setAudiovisualBackgroundPlaybackPolicy:@
setAudiovisualBackgroundPlaybackPolicySelector :: Selector '[AVPlayerAudiovisualBackgroundPlaybackPolicy] ()
setAudiovisualBackgroundPlaybackPolicySelector = mkSelector "setAudiovisualBackgroundPlaybackPolicy:"

-- | @Selector@ for @preventsAutomaticBackgroundingDuringVideoPlayback@
preventsAutomaticBackgroundingDuringVideoPlaybackSelector :: Selector '[] Bool
preventsAutomaticBackgroundingDuringVideoPlaybackSelector = mkSelector "preventsAutomaticBackgroundingDuringVideoPlayback"

-- | @Selector@ for @setPreventsAutomaticBackgroundingDuringVideoPlayback:@
setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector :: Selector '[Bool] ()
setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector = mkSelector "setPreventsAutomaticBackgroundingDuringVideoPlayback:"

-- | @Selector@ for @preventsDisplaySleepDuringVideoPlayback@
preventsDisplaySleepDuringVideoPlaybackSelector :: Selector '[] Bool
preventsDisplaySleepDuringVideoPlaybackSelector = mkSelector "preventsDisplaySleepDuringVideoPlayback"

-- | @Selector@ for @setPreventsDisplaySleepDuringVideoPlayback:@
setPreventsDisplaySleepDuringVideoPlaybackSelector :: Selector '[Bool] ()
setPreventsDisplaySleepDuringVideoPlaybackSelector = mkSelector "setPreventsDisplaySleepDuringVideoPlayback:"

-- | @Selector@ for @preferredVideoDecoderGPURegistryID@
preferredVideoDecoderGPURegistryIDSelector :: Selector '[] CULong
preferredVideoDecoderGPURegistryIDSelector = mkSelector "preferredVideoDecoderGPURegistryID"

-- | @Selector@ for @setPreferredVideoDecoderGPURegistryID:@
setPreferredVideoDecoderGPURegistryIDSelector :: Selector '[CULong] ()
setPreferredVideoDecoderGPURegistryIDSelector = mkSelector "setPreferredVideoDecoderGPURegistryID:"

-- | @Selector@ for @availableHDRModes@
availableHDRModesSelector :: Selector '[] AVPlayerHDRMode
availableHDRModesSelector = mkSelector "availableHDRModes"

-- | @Selector@ for @eligibleForHDRPlayback@
eligibleForHDRPlaybackSelector :: Selector '[] Bool
eligibleForHDRPlaybackSelector = mkSelector "eligibleForHDRPlayback"

-- | @Selector@ for @outputObscuredDueToInsufficientExternalProtection@
outputObscuredDueToInsufficientExternalProtectionSelector :: Selector '[] Bool
outputObscuredDueToInsufficientExternalProtectionSelector = mkSelector "outputObscuredDueToInsufficientExternalProtection"

-- | @Selector@ for @allowsExternalPlayback@
allowsExternalPlaybackSelector :: Selector '[] Bool
allowsExternalPlaybackSelector = mkSelector "allowsExternalPlayback"

-- | @Selector@ for @setAllowsExternalPlayback:@
setAllowsExternalPlaybackSelector :: Selector '[Bool] ()
setAllowsExternalPlaybackSelector = mkSelector "setAllowsExternalPlayback:"

-- | @Selector@ for @externalPlaybackActive@
externalPlaybackActiveSelector :: Selector '[] Bool
externalPlaybackActiveSelector = mkSelector "externalPlaybackActive"

-- | @Selector@ for @usesExternalPlaybackWhileExternalScreenIsActive@
usesExternalPlaybackWhileExternalScreenIsActiveSelector :: Selector '[] Bool
usesExternalPlaybackWhileExternalScreenIsActiveSelector = mkSelector "usesExternalPlaybackWhileExternalScreenIsActive"

-- | @Selector@ for @setUsesExternalPlaybackWhileExternalScreenIsActive:@
setUsesExternalPlaybackWhileExternalScreenIsActiveSelector :: Selector '[Bool] ()
setUsesExternalPlaybackWhileExternalScreenIsActiveSelector = mkSelector "setUsesExternalPlaybackWhileExternalScreenIsActive:"

-- | @Selector@ for @externalPlaybackVideoGravity@
externalPlaybackVideoGravitySelector :: Selector '[] (Id NSString)
externalPlaybackVideoGravitySelector = mkSelector "externalPlaybackVideoGravity"

-- | @Selector@ for @setExternalPlaybackVideoGravity:@
setExternalPlaybackVideoGravitySelector :: Selector '[Id NSString] ()
setExternalPlaybackVideoGravitySelector = mkSelector "setExternalPlaybackVideoGravity:"

-- | @Selector@ for @audioOutputDeviceUniqueID@
audioOutputDeviceUniqueIDSelector :: Selector '[] (Id NSString)
audioOutputDeviceUniqueIDSelector = mkSelector "audioOutputDeviceUniqueID"

-- | @Selector@ for @setAudioOutputDeviceUniqueID:@
setAudioOutputDeviceUniqueIDSelector :: Selector '[Id NSString] ()
setAudioOutputDeviceUniqueIDSelector = mkSelector "setAudioOutputDeviceUniqueID:"

-- | @Selector@ for @appliesMediaSelectionCriteriaAutomatically@
appliesMediaSelectionCriteriaAutomaticallySelector :: Selector '[] Bool
appliesMediaSelectionCriteriaAutomaticallySelector = mkSelector "appliesMediaSelectionCriteriaAutomatically"

-- | @Selector@ for @setAppliesMediaSelectionCriteriaAutomatically:@
setAppliesMediaSelectionCriteriaAutomaticallySelector :: Selector '[Bool] ()
setAppliesMediaSelectionCriteriaAutomaticallySelector = mkSelector "setAppliesMediaSelectionCriteriaAutomatically:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @muted@
mutedSelector :: Selector '[] Bool
mutedSelector = mkSelector "muted"

-- | @Selector@ for @setMuted:@
setMutedSelector :: Selector '[Bool] ()
setMutedSelector = mkSelector "setMuted:"

-- | @Selector@ for @automaticallyWaitsToMinimizeStalling@
automaticallyWaitsToMinimizeStallingSelector :: Selector '[] Bool
automaticallyWaitsToMinimizeStallingSelector = mkSelector "automaticallyWaitsToMinimizeStalling"

-- | @Selector@ for @setAutomaticallyWaitsToMinimizeStalling:@
setAutomaticallyWaitsToMinimizeStallingSelector :: Selector '[Bool] ()
setAutomaticallyWaitsToMinimizeStallingSelector = mkSelector "setAutomaticallyWaitsToMinimizeStalling:"

-- | @Selector@ for @sourceClock@
sourceClockSelector :: Selector '[] (Ptr ())
sourceClockSelector = mkSelector "sourceClock"

-- | @Selector@ for @setSourceClock:@
setSourceClockSelector :: Selector '[Ptr ()] ()
setSourceClockSelector = mkSelector "setSourceClock:"

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector '[] (Id AVPlayerItem)
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @actionAtItemEnd@
actionAtItemEndSelector :: Selector '[] AVPlayerActionAtItemEnd
actionAtItemEndSelector = mkSelector "actionAtItemEnd"

-- | @Selector@ for @setActionAtItemEnd:@
setActionAtItemEndSelector :: Selector '[AVPlayerActionAtItemEnd] ()
setActionAtItemEndSelector = mkSelector "setActionAtItemEnd:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @defaultRate@
defaultRateSelector :: Selector '[] CFloat
defaultRateSelector = mkSelector "defaultRate"

-- | @Selector@ for @setDefaultRate:@
setDefaultRateSelector :: Selector '[CFloat] ()
setDefaultRateSelector = mkSelector "setDefaultRate:"

-- | @Selector@ for @timeControlStatus@
timeControlStatusSelector :: Selector '[] AVPlayerTimeControlStatus
timeControlStatusSelector = mkSelector "timeControlStatus"

-- | @Selector@ for @reasonForWaitingToPlay@
reasonForWaitingToPlaySelector :: Selector '[] (Id NSString)
reasonForWaitingToPlaySelector = mkSelector "reasonForWaitingToPlay"

