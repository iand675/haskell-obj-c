{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVSampleBufferRenderSynchronizer can synchronize multiple objects conforming to AVQueuedSampleBufferRendering to a single timebase.
--
-- Generated bindings for @AVSampleBufferRenderSynchronizer@.
module ObjC.AVFoundation.AVSampleBufferRenderSynchronizer
  ( AVSampleBufferRenderSynchronizer
  , IsAVSampleBufferRenderSynchronizer(..)
  , addBoundaryTimeObserverForTimes_queue_usingBlock
  , removeTimeObserver
  , addRenderer
  , timebase
  , rate
  , setRate
  , delaysRateChangeUntilHasSufficientMediaData
  , setDelaysRateChangeUntilHasSufficientMediaData
  , intendedSpatialAudioExperience
  , setIntendedSpatialAudioExperience
  , renderers
  , addBoundaryTimeObserverForTimes_queue_usingBlockSelector
  , addRendererSelector
  , delaysRateChangeUntilHasSufficientMediaDataSelector
  , intendedSpatialAudioExperienceSelector
  , rateSelector
  , removeTimeObserverSelector
  , renderersSelector
  , setDelaysRateChangeUntilHasSufficientMediaDataSelector
  , setIntendedSpatialAudioExperienceSelector
  , setRateSelector
  , timebaseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Requests invocation of a block when specified times are traversed during normal rendering.
--
-- Each call to -addPeriodicTimeObserverForInterval:queue:usingBlock: should be paired with a corresponding call to -removeTimeObserver:. Releasing the observer object without a call to -removeTimeObserver: will result in undefined behavior.
--
-- - Parameter times: The times for which the observer requests notification, supplied as an array of NSValues carrying CMTimes. - Parameter queue: The serial queue onto which block should be enqueued. If you pass NULL, the main queue (obtained using dispatch_get_main_queue()) will be used. Passing a concurrent queue to this method will result in undefined behavior. - Parameter block: The block to be invoked when any of the specified times is crossed during normal rendering.
--
-- - Returns: An object conforming to the NSObject protocol.  You must retain this returned value as long as you want the time observer to be invoked by the synchronizer.  Pass this object to -removeTimeObserver: to cancel time observation.
--
-- ObjC selector: @- addBoundaryTimeObserverForTimes:queue:usingBlock:@
addBoundaryTimeObserverForTimes_queue_usingBlock :: (IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer, IsNSArray times, IsNSObject queue) => avSampleBufferRenderSynchronizer -> times -> queue -> Ptr () -> IO RawId
addBoundaryTimeObserverForTimes_queue_usingBlock avSampleBufferRenderSynchronizer times queue block =
  sendMessage avSampleBufferRenderSynchronizer addBoundaryTimeObserverForTimes_queue_usingBlockSelector (toNSArray times) (toNSObject queue) block

-- | Cancels a previously registered time observer.
--
-- Upon return, the caller is guaranteed that no new time observer blocks will begin executing. Depending on the calling thread and the queue used to add the time observer, an in-flight block may continue to execute after this method returns. You can guarantee synchronous time observer removal by enqueuing the call to -removeTimeObserver: on that queue. Alternatively, call dispatch_sync(queue, ^{}) after -removeTimeObserver: to wait for any in-flight blocks to finish executing. -removeTimeObserver: should be used to explicitly cancel each time observer added using -addPeriodicTimeObserverForInterval:queue:usingBlock: and -addBoundaryTimeObserverForTimes:queue:usingBlock:.
--
-- This method throws an exception for any of the following reasons: - observer was added by another AVSampleBufferRenderSynchronizer - observer was not returned by either 	-addPeriodicTimeObserverForInterval:queue:usingBlock: 	-addBoundaryTimeObserverForTimes:queue:usingBlock:
--
-- - Parameter observer: An object returned by a previous call to -addPeriodicTimeObserverForInterval:queue:usingBlock: or -addBoundaryTimeObserverForTimes:queue:usingBlock:.
--
-- ObjC selector: @- removeTimeObserver:@
removeTimeObserver :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> RawId -> IO ()
removeTimeObserver avSampleBufferRenderSynchronizer observer =
  sendMessage avSampleBufferRenderSynchronizer removeTimeObserverSelector observer

-- | Adds a renderer to the list of renderers under the synchronizer's control.
--
-- Adds a renderer to begin operating with the synchronizer's timebase.
--
-- This method can be called while rate is non-0.0.
--
-- - Parameter renderer: An object conforming to AVQueuedSampleBufferRendering to be synchronized by this synchronizer.
--
-- ObjC selector: @- addRenderer:@
addRenderer :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> RawId -> IO ()
addRenderer avSampleBufferRenderSynchronizer renderer =
  sendMessage avSampleBufferRenderSynchronizer addRendererSelector renderer

-- | The synchronizer's rendering timebase, which governs how time stamps are interpreted.
--
-- By default, this timebase will be driven by the clock of an added AVSampleBufferAudioRenderer.
--
-- If no AVSampleBufferAudioRenderer has been added, the source clock will be the host time clock (mach_absolute_time with the appropriate timescale conversion; this is the same as Core Animation's CACurrentMediaTime).
--
-- The timebase is a read-only timebase. Use the rate property and corresponding methods to adjust the timebase.
--
-- ObjC selector: @- timebase@
timebase :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO (Ptr ())
timebase avSampleBufferRenderSynchronizer =
  sendMessage avSampleBufferRenderSynchronizer timebaseSelector

-- | Playback rate.
--
-- Indicates the current rate of rendering. A value of 0.0 means "stopped"; a value of 1.0 means "play at the natural rate of the media". Must be greater than or equal to 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO CFloat
rate avSampleBufferRenderSynchronizer =
  sendMessage avSampleBufferRenderSynchronizer rateSelector

-- | Playback rate.
--
-- Indicates the current rate of rendering. A value of 0.0 means "stopped"; a value of 1.0 means "play at the natural rate of the media". Must be greater than or equal to 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> CFloat -> IO ()
setRate avSampleBufferRenderSynchronizer value =
  sendMessage avSampleBufferRenderSynchronizer setRateSelector value

-- | Indicates whether the playback should be started immediately on rate change request.
--
-- If set to YES, playback will be delayed if the value of hasSufficientMediaDataForReliablePlaybackStart of any added renderer is NO. If set to NO, playback will attempt to start immediately regardless of the value of hasSufficientMediaDataForReliablePlaybackStart of added renderers. Default is YES.
--
-- ObjC selector: @- delaysRateChangeUntilHasSufficientMediaData@
delaysRateChangeUntilHasSufficientMediaData :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO Bool
delaysRateChangeUntilHasSufficientMediaData avSampleBufferRenderSynchronizer =
  sendMessage avSampleBufferRenderSynchronizer delaysRateChangeUntilHasSufficientMediaDataSelector

-- | Indicates whether the playback should be started immediately on rate change request.
--
-- If set to YES, playback will be delayed if the value of hasSufficientMediaDataForReliablePlaybackStart of any added renderer is NO. If set to NO, playback will attempt to start immediately regardless of the value of hasSufficientMediaDataForReliablePlaybackStart of added renderers. Default is YES.
--
-- ObjC selector: @- setDelaysRateChangeUntilHasSufficientMediaData:@
setDelaysRateChangeUntilHasSufficientMediaData :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> Bool -> IO ()
setDelaysRateChangeUntilHasSufficientMediaData avSampleBufferRenderSynchronizer value =
  sendMessage avSampleBufferRenderSynchronizer setDelaysRateChangeUntilHasSufficientMediaDataSelector value

-- | The intended spatial audio experience applied to all AVSampleBufferAudioRenderers within this synchronizer.
--
-- The default value of CAAutomaticSpatialAudio means the renderers use their AVAudioSession's intended spatial experience. If the anchoring strategy is impossible (e.g. it uses a destroyed UIScene's identifier), the renderers follow a "front" anchoring strategy instead.
--
-- ObjC selector: @- intendedSpatialAudioExperience@
intendedSpatialAudioExperience :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO RawId
intendedSpatialAudioExperience avSampleBufferRenderSynchronizer =
  sendMessage avSampleBufferRenderSynchronizer intendedSpatialAudioExperienceSelector

-- | The intended spatial audio experience applied to all AVSampleBufferAudioRenderers within this synchronizer.
--
-- The default value of CAAutomaticSpatialAudio means the renderers use their AVAudioSession's intended spatial experience. If the anchoring strategy is impossible (e.g. it uses a destroyed UIScene's identifier), the renderers follow a "front" anchoring strategy instead.
--
-- ObjC selector: @- setIntendedSpatialAudioExperience:@
setIntendedSpatialAudioExperience :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> RawId -> IO ()
setIntendedSpatialAudioExperience avSampleBufferRenderSynchronizer value =
  sendMessage avSampleBufferRenderSynchronizer setIntendedSpatialAudioExperienceSelector value

-- | Array of id<AVQueuedSampleBufferRendering> currently attached to the synchronizer.
--
-- A list of renderers added to and not removed from the synchronizer. The list also includes renderers that have been scheduled to be removed but have not yet been removed.
--
-- This property is not KVO observable.
--
-- ObjC selector: @- renderers@
renderers :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO (Id NSArray)
renderers avSampleBufferRenderSynchronizer =
  sendMessage avSampleBufferRenderSynchronizer renderersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBoundaryTimeObserverForTimes:queue:usingBlock:@
addBoundaryTimeObserverForTimes_queue_usingBlockSelector :: Selector '[Id NSArray, Id NSObject, Ptr ()] RawId
addBoundaryTimeObserverForTimes_queue_usingBlockSelector = mkSelector "addBoundaryTimeObserverForTimes:queue:usingBlock:"

-- | @Selector@ for @removeTimeObserver:@
removeTimeObserverSelector :: Selector '[RawId] ()
removeTimeObserverSelector = mkSelector "removeTimeObserver:"

-- | @Selector@ for @addRenderer:@
addRendererSelector :: Selector '[RawId] ()
addRendererSelector = mkSelector "addRenderer:"

-- | @Selector@ for @timebase@
timebaseSelector :: Selector '[] (Ptr ())
timebaseSelector = mkSelector "timebase"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @delaysRateChangeUntilHasSufficientMediaData@
delaysRateChangeUntilHasSufficientMediaDataSelector :: Selector '[] Bool
delaysRateChangeUntilHasSufficientMediaDataSelector = mkSelector "delaysRateChangeUntilHasSufficientMediaData"

-- | @Selector@ for @setDelaysRateChangeUntilHasSufficientMediaData:@
setDelaysRateChangeUntilHasSufficientMediaDataSelector :: Selector '[Bool] ()
setDelaysRateChangeUntilHasSufficientMediaDataSelector = mkSelector "setDelaysRateChangeUntilHasSufficientMediaData:"

-- | @Selector@ for @intendedSpatialAudioExperience@
intendedSpatialAudioExperienceSelector :: Selector '[] RawId
intendedSpatialAudioExperienceSelector = mkSelector "intendedSpatialAudioExperience"

-- | @Selector@ for @setIntendedSpatialAudioExperience:@
setIntendedSpatialAudioExperienceSelector :: Selector '[RawId] ()
setIntendedSpatialAudioExperienceSelector = mkSelector "setIntendedSpatialAudioExperience:"

-- | @Selector@ for @renderers@
renderersSelector :: Selector '[] (Id NSArray)
renderersSelector = mkSelector "renderers"

