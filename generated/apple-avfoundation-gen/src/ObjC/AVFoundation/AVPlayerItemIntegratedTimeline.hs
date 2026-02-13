{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemIntegratedTimeline
--
-- An AVPlayerItemIntegratedTimeline provides detailed timing information and control for the sequence of playback of a primary AVPlayerItem and scheduled AVPlayerInterstitialEvents.
--
-- An object that models the timeline and sequence of playback of primary AVPlayerItem and scheduled AVPlayerInterstitialEvents. The timeline models all regions expected to be traversed during playback. Notably portions of the primary item may not be presented when exiting an interstitial event with a positive resumption offset.	Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemIntegratedTimeline@.
module ObjC.AVFoundation.AVPlayerItemIntegratedTimeline
  ( AVPlayerItemIntegratedTimeline
  , IsAVPlayerItemIntegratedTimeline(..)
  , init_
  , new
  , addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlock
  , removeTimeObserver
  , seekToDate_completionHandler
  , currentSnapshot
  , currentDate
  , addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlockSelector
  , currentDateSelector
  , currentSnapshotSelector
  , initSelector
  , newSelector
  , removeTimeObserverSelector
  , seekToDate_completionHandlerSelector


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
init_ :: IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline => avPlayerItemIntegratedTimeline -> IO (Id AVPlayerItemIntegratedTimeline)
init_ avPlayerItemIntegratedTimeline =
  sendOwnedMessage avPlayerItemIntegratedTimeline initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemIntegratedTimeline)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemIntegratedTimeline"
    sendOwnedClassMessage cls' newSelector

-- | addBoundaryTimeObserverForSegment
--
-- Requests invocation of a block when an offset in a segment is traversed during playback.
--
-- @segment@ — AVPlayerItemSegment to monitor playback traversal of.
--
-- @offsetsIntoSegment@ — Offsets in the segment for which the observer requests notification, supplied as an array of NSValues carrying CMTimes.
--
-- @queue@ — The serial queue onto which block should be enqueued. If you pass NULL, the main queue (obtained using dispatch_get_main_queue()) will be used. Passing a concurrent queue to this method will result in undefined behavior.
--
-- @block@ — The block to be invoked when the offset is crossed during playback of a segment.
--
-- Returns: An object conforming to the AVPlayerItemIntegratedTimelineObserver protocol. You must retain this returned value as long as you want the time observer to be invoked by the timeline. Pass this object to -removeTimeObserver: to cancel time observation. One can also configure single point segments with segmentTimes to trigger during traversal of the segment's playback. As the timeline duration and segments change, the installed time observer will be automatically adjusted to fire at the desired offset in the segment. A segment that is removed from the timeline will trigger the invocation of the block immediately with success set as false. Each call to -addBoundaryTimeObserverForSegment:segment:offsetsInSegment:queue:usingBlock: should be paired with a corresponding call to -removeTimeObserver:. Releasing the observer object without a call to -removeTimeObserver: will result in undefined behavior
--
-- ObjC selector: @- addBoundaryTimeObserverForSegment:offsetsIntoSegment:queue:usingBlock:@
addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlock :: (IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline, IsAVPlayerItemSegment segment, IsNSArray offsetsIntoSegment, IsNSObject queue) => avPlayerItemIntegratedTimeline -> segment -> offsetsIntoSegment -> queue -> Ptr () -> IO RawId
addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlock avPlayerItemIntegratedTimeline segment offsetsIntoSegment queue block =
  sendMessage avPlayerItemIntegratedTimeline addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlockSelector (toAVPlayerItemSegment segment) (toNSArray offsetsIntoSegment) (toNSObject queue) block

-- | removeTimeObserver:
--
-- Cancels a previously registered time observer.
--
-- @observer@ — An object returned by a previous call to -addPeriodicTimeObserverForInterval or -addBoundaryTimeObserverForSegment.
--
-- ObjC selector: @- removeTimeObserver:@
removeTimeObserver :: IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline => avPlayerItemIntegratedTimeline -> RawId -> IO ()
removeTimeObserver avPlayerItemIntegratedTimeline observer =
  sendMessage avPlayerItemIntegratedTimeline removeTimeObserverSelector observer

-- | seekToDate
--
-- Seeks playhead to corresponding date and invokes the completionHandler.
--
-- @date@ — The new position for the playhead.
--
-- @completionHandler@ — CompletionHandler callback after seek completes. Success will be true if the playhead moved to the new date.
--
-- The integrated timeline will seek playhead to the coresponding date.
--
-- ObjC selector: @- seekToDate:completionHandler:@
seekToDate_completionHandler :: (IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline, IsNSDate date) => avPlayerItemIntegratedTimeline -> date -> Ptr () -> IO ()
seekToDate_completionHandler avPlayerItemIntegratedTimeline date completionHandler =
  sendMessage avPlayerItemIntegratedTimeline seekToDate_completionHandlerSelector (toNSDate date) completionHandler

-- | currentSnapshot
--
-- This property provides an immutable representation of the timeline state at time of request.
--
-- Returns an immutable representation of the timeline state at time of request. A timeline snapshot provides accessors for obtaining inspectable details of the timeline.  Because a snapshot is immutable, the snapshot's properties will not update as playback continues.
--
-- ObjC selector: @- currentSnapshot@
currentSnapshot :: IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline => avPlayerItemIntegratedTimeline -> IO (Id AVPlayerItemIntegratedTimelineSnapshot)
currentSnapshot avPlayerItemIntegratedTimeline =
  sendMessage avPlayerItemIntegratedTimeline currentSnapshotSelector

-- | currentDate
--
-- Returns the date of current playback, or nil if playback is not mapped to any date.
--
-- ObjC selector: @- currentDate@
currentDate :: IsAVPlayerItemIntegratedTimeline avPlayerItemIntegratedTimeline => avPlayerItemIntegratedTimeline -> IO (Id NSDate)
currentDate avPlayerItemIntegratedTimeline =
  sendMessage avPlayerItemIntegratedTimeline currentDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemIntegratedTimeline)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemIntegratedTimeline)
newSelector = mkSelector "new"

-- | @Selector@ for @addBoundaryTimeObserverForSegment:offsetsIntoSegment:queue:usingBlock:@
addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlockSelector :: Selector '[Id AVPlayerItemSegment, Id NSArray, Id NSObject, Ptr ()] RawId
addBoundaryTimeObserverForSegment_offsetsIntoSegment_queue_usingBlockSelector = mkSelector "addBoundaryTimeObserverForSegment:offsetsIntoSegment:queue:usingBlock:"

-- | @Selector@ for @removeTimeObserver:@
removeTimeObserverSelector :: Selector '[RawId] ()
removeTimeObserverSelector = mkSelector "removeTimeObserver:"

-- | @Selector@ for @seekToDate:completionHandler:@
seekToDate_completionHandlerSelector :: Selector '[Id NSDate, Ptr ()] ()
seekToDate_completionHandlerSelector = mkSelector "seekToDate:completionHandler:"

-- | @Selector@ for @currentSnapshot@
currentSnapshotSelector :: Selector '[] (Id AVPlayerItemIntegratedTimelineSnapshot)
currentSnapshotSelector = mkSelector "currentSnapshot"

-- | @Selector@ for @currentDate@
currentDateSelector :: Selector '[] (Id NSDate)
currentDateSelector = mkSelector "currentDate"

