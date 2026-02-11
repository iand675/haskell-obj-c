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
  , addBoundaryTimeObserverForTimes_queue_usingBlockSelector
  , removeTimeObserverSelector
  , addRendererSelector
  , timebaseSelector
  , rateSelector
  , setRateSelector
  , delaysRateChangeUntilHasSufficientMediaDataSelector
  , setDelaysRateChangeUntilHasSufficientMediaDataSelector


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
addBoundaryTimeObserverForTimes_queue_usingBlock avSampleBufferRenderSynchronizer  times queue block =
withObjCPtr times $ \raw_times ->
  withObjCPtr queue $ \raw_queue ->
      fmap (RawId . castPtr) $ sendMsg avSampleBufferRenderSynchronizer (mkSelector "addBoundaryTimeObserverForTimes:queue:usingBlock:") (retPtr retVoid) [argPtr (castPtr raw_times :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

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
removeTimeObserver avSampleBufferRenderSynchronizer  observer =
  sendMsg avSampleBufferRenderSynchronizer (mkSelector "removeTimeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

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
addRenderer avSampleBufferRenderSynchronizer  renderer =
  sendMsg avSampleBufferRenderSynchronizer (mkSelector "addRenderer:") retVoid [argPtr (castPtr (unRawId renderer) :: Ptr ())]

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
timebase avSampleBufferRenderSynchronizer  =
  fmap castPtr $ sendMsg avSampleBufferRenderSynchronizer (mkSelector "timebase") (retPtr retVoid) []

-- | Playback rate.
--
-- Indicates the current rate of rendering. A value of 0.0 means "stopped"; a value of 1.0 means "play at the natural rate of the media". Must be greater than or equal to 0.0.
--
-- ObjC selector: @- rate@
rate :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO CFloat
rate avSampleBufferRenderSynchronizer  =
  sendMsg avSampleBufferRenderSynchronizer (mkSelector "rate") retCFloat []

-- | Playback rate.
--
-- Indicates the current rate of rendering. A value of 0.0 means "stopped"; a value of 1.0 means "play at the natural rate of the media". Must be greater than or equal to 0.0.
--
-- ObjC selector: @- setRate:@
setRate :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> CFloat -> IO ()
setRate avSampleBufferRenderSynchronizer  value =
  sendMsg avSampleBufferRenderSynchronizer (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | Indicates whether the playback should be started immediately on rate change request.
--
-- If set to YES, playback will be delayed if the value of hasSufficientMediaDataForReliablePlaybackStart of any added renderer is NO. If set to NO, playback will attempt to start immediately regardless of the value of hasSufficientMediaDataForReliablePlaybackStart of added renderers. Default is YES.
--
-- ObjC selector: @- delaysRateChangeUntilHasSufficientMediaData@
delaysRateChangeUntilHasSufficientMediaData :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> IO Bool
delaysRateChangeUntilHasSufficientMediaData avSampleBufferRenderSynchronizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferRenderSynchronizer (mkSelector "delaysRateChangeUntilHasSufficientMediaData") retCULong []

-- | Indicates whether the playback should be started immediately on rate change request.
--
-- If set to YES, playback will be delayed if the value of hasSufficientMediaDataForReliablePlaybackStart of any added renderer is NO. If set to NO, playback will attempt to start immediately regardless of the value of hasSufficientMediaDataForReliablePlaybackStart of added renderers. Default is YES.
--
-- ObjC selector: @- setDelaysRateChangeUntilHasSufficientMediaData:@
setDelaysRateChangeUntilHasSufficientMediaData :: IsAVSampleBufferRenderSynchronizer avSampleBufferRenderSynchronizer => avSampleBufferRenderSynchronizer -> Bool -> IO ()
setDelaysRateChangeUntilHasSufficientMediaData avSampleBufferRenderSynchronizer  value =
  sendMsg avSampleBufferRenderSynchronizer (mkSelector "setDelaysRateChangeUntilHasSufficientMediaData:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBoundaryTimeObserverForTimes:queue:usingBlock:@
addBoundaryTimeObserverForTimes_queue_usingBlockSelector :: Selector
addBoundaryTimeObserverForTimes_queue_usingBlockSelector = mkSelector "addBoundaryTimeObserverForTimes:queue:usingBlock:"

-- | @Selector@ for @removeTimeObserver:@
removeTimeObserverSelector :: Selector
removeTimeObserverSelector = mkSelector "removeTimeObserver:"

-- | @Selector@ for @addRenderer:@
addRendererSelector :: Selector
addRendererSelector = mkSelector "addRenderer:"

-- | @Selector@ for @timebase@
timebaseSelector :: Selector
timebaseSelector = mkSelector "timebase"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @delaysRateChangeUntilHasSufficientMediaData@
delaysRateChangeUntilHasSufficientMediaDataSelector :: Selector
delaysRateChangeUntilHasSufficientMediaDataSelector = mkSelector "delaysRateChangeUntilHasSufficientMediaData"

-- | @Selector@ for @setDelaysRateChangeUntilHasSufficientMediaData:@
setDelaysRateChangeUntilHasSufficientMediaDataSelector :: Selector
setDelaysRateChangeUntilHasSufficientMediaDataSelector = mkSelector "setDelaysRateChangeUntilHasSufficientMediaData:"

