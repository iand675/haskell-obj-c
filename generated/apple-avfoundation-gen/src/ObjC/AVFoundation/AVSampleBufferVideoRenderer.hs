{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSampleBufferVideoRenderer@.
module ObjC.AVFoundation.AVSampleBufferVideoRenderer
  ( AVSampleBufferVideoRenderer
  , IsAVSampleBufferVideoRenderer(..)
  , flushWithRemovalOfDisplayedImage_completionHandler
  , loadVideoPerformanceMetricsWithCompletionHandler
  , expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes
  , resetUpcomingSampleBufferPresentationTimeExpectations
  , copyDisplayedPixelBuffer
  , status
  , error_
  , requiresFlushToResumeDecoding
  , recommendedPixelBufferAttributes
  , copyDisplayedPixelBufferSelector
  , errorSelector
  , expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimesSelector
  , flushWithRemovalOfDisplayedImage_completionHandlerSelector
  , loadVideoPerformanceMetricsWithCompletionHandlerSelector
  , recommendedPixelBufferAttributesSelector
  , requiresFlushToResumeDecodingSelector
  , resetUpcomingSampleBufferPresentationTimeExpectationsSelector
  , statusSelector

  -- * Enum types
  , AVQueuedSampleBufferRenderingStatus(AVQueuedSampleBufferRenderingStatus)
  , pattern AVQueuedSampleBufferRenderingStatusUnknown
  , pattern AVQueuedSampleBufferRenderingStatusRendering
  , pattern AVQueuedSampleBufferRenderingStatusFailed

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

-- | flushWithRemovalOfDisplayedImage:completionHandler:
--
-- Instructs the video renderer to discard pending enqueued sample buffers and call the provided block when complete.
--
-- @removeDisplayedImage@ — Set YES to remove any currently displayed image, NO to preserve any current image.
--
-- @handler@ — The handler to invoke when flush operation is complete. May be nil.
--
-- A flush resets decoder state. The next frame passed to enqueueSampleBuffer: should be an IDR frame (also known as a key frame or sync sample).
--
-- ObjC selector: @- flushWithRemovalOfDisplayedImage:completionHandler:@
flushWithRemovalOfDisplayedImage_completionHandler :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> Bool -> Ptr () -> IO ()
flushWithRemovalOfDisplayedImage_completionHandler avSampleBufferVideoRenderer removeDisplayedImage handler =
  sendMessage avSampleBufferVideoRenderer flushWithRemovalOfDisplayedImage_completionHandlerSelector removeDisplayedImage handler

-- | loadVideoPerformanceMetricsWithCompletionHandler:
--
-- Gathers a snapshot of the video performance metrics and calls the completion handler with the results.
--
-- @completionHandler@ — The handler to invoke with the video performance metrics.
--
-- If there are no performance metrics available, the completion handler will be called with nil videoPerformanceMetrics.
--
-- ObjC selector: @- loadVideoPerformanceMetricsWithCompletionHandler:@
loadVideoPerformanceMetricsWithCompletionHandler :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> Ptr () -> IO ()
loadVideoPerformanceMetricsWithCompletionHandler avSampleBufferVideoRenderer completionHandler =
  sendMessage avSampleBufferVideoRenderer loadVideoPerformanceMetricsWithCompletionHandlerSelector completionHandler

-- | expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes
--
-- Promises, for the purpose of enabling power optimizations, that future sample buffers will have monotonically increasing PTS values.
--
-- Only applicable for forward playback.					Sending this message and later calling -enqueueSampleBuffer: with a buffer with a lower PTS than any previously enqueued PTS has the potential to lead to dropped buffers.					Messaging -flush resets such expectations.
--
-- ObjC selector: @- expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes@
expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO ()
expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimesSelector

-- | resetUpcomingSampleBufferPresentationTimeExpectations:
--
-- Resets previously-promised expectations about upcoming sample buffer PTSs.
--
-- This undoes the state set by messaging -expectMinimumUpcomingSampleBufferPresentationTime: or -expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes.					If you didn't use either of those, you don't have to use this.
--
-- ObjC selector: @- resetUpcomingSampleBufferPresentationTimeExpectations@
resetUpcomingSampleBufferPresentationTimeExpectations :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO ()
resetUpcomingSampleBufferPresentationTimeExpectations avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer resetUpcomingSampleBufferPresentationTimeExpectationsSelector

-- | copyDisplayedPixelBuffer
--
-- Returns a retained reference to the pixel buffer currently displayed in the AVSampleBufferVideoRenderer's target. This will return NULL if the displayed pixel buffer is protected, no image is currently being displayed, or if the image is unavailable.
--
-- This will return NULL if the rate is non-zero.  Clients must release the pixel buffer after use.
--
-- Do not write to the returned CVPixelBuffer's attachments or pixel data.
--
-- ObjC selector: @- copyDisplayedPixelBuffer@
copyDisplayedPixelBuffer :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO (Ptr ())
copyDisplayedPixelBuffer avSampleBufferVideoRenderer =
  sendOwnedMessage avSampleBufferVideoRenderer copyDisplayedPixelBufferSelector

-- | status
--
-- The ability of the video renderer to be used for enqueueing sample buffers.
--
-- The value of this property is an AVQueuedSampleBufferRenderingStatus that indicates whether the receiver can be used for enqueueing and rendering sample buffers. When the value of this property is AVQueuedSampleBufferRenderingStatusFailed, clients can check the value of the error property to determine the failure. To resume rendering sample buffers using the video renderer after a failure, clients must first reset the status to AVQueuedSampleBufferRenderingStatusUnknown. This can be achieved by invoking -flush on the video renderer.					This property is key value observable.
--
-- ObjC selector: @- status@
status :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO AVQueuedSampleBufferRenderingStatus
status avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer statusSelector

-- | error
--
-- If the video renderer's status is AVQueuedSampleBufferRenderingStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the video renderer to no longer be able to enqueue sample buffers. If the status is not AVQueuedSampleBufferRenderingStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO (Id NSError)
error_ avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer errorSelector

-- | requiresFlushToResumeDecoding
--
-- Indicates that the receiver is in a state where it requires a call to -flush to continue decoding frames.
--
-- When the application enters a state where use of video decoder resources is not permissible, the value of this property changes to YES along with the video renderer's status changing to AVQueuedSampleBufferRenderingStatusFailed.					To resume rendering sample buffers using the video renderer after this property's value is YES, clients must first reset the video renderer by calling flush or flushWithRemovalOfDisplayedImage:completionHandler:.					Clients can track changes to this property via AVSampleBufferVideoRendererRequiresFlushToResumeDecodingDidChangeNotification.					This property is not key value observable.
--
-- ObjC selector: @- requiresFlushToResumeDecoding@
requiresFlushToResumeDecoding :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO Bool
requiresFlushToResumeDecoding avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer requiresFlushToResumeDecodingSelector

-- | recommendedPixelBufferAttributes
--
-- Recommended pixel buffer attributes for optimal performance when using CMSampleBuffers containing CVPixelBuffers.
--
-- The returned dictionary does not contain all of the attributes needed for creating pixel buffers.					Use ``CVPixelBufferCreateResolvedAttributesDictionary()`` to reconcile these attributes with the pixel buffer creation attributes.
--
-- ObjC selector: @- recommendedPixelBufferAttributes@
recommendedPixelBufferAttributes :: IsAVSampleBufferVideoRenderer avSampleBufferVideoRenderer => avSampleBufferVideoRenderer -> IO (Id NSDictionary)
recommendedPixelBufferAttributes avSampleBufferVideoRenderer =
  sendMessage avSampleBufferVideoRenderer recommendedPixelBufferAttributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @flushWithRemovalOfDisplayedImage:completionHandler:@
flushWithRemovalOfDisplayedImage_completionHandlerSelector :: Selector '[Bool, Ptr ()] ()
flushWithRemovalOfDisplayedImage_completionHandlerSelector = mkSelector "flushWithRemovalOfDisplayedImage:completionHandler:"

-- | @Selector@ for @loadVideoPerformanceMetricsWithCompletionHandler:@
loadVideoPerformanceMetricsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadVideoPerformanceMetricsWithCompletionHandlerSelector = mkSelector "loadVideoPerformanceMetricsWithCompletionHandler:"

-- | @Selector@ for @expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes@
expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimesSelector :: Selector '[] ()
expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimesSelector = mkSelector "expectMonotonicallyIncreasingUpcomingSampleBufferPresentationTimes"

-- | @Selector@ for @resetUpcomingSampleBufferPresentationTimeExpectations@
resetUpcomingSampleBufferPresentationTimeExpectationsSelector :: Selector '[] ()
resetUpcomingSampleBufferPresentationTimeExpectationsSelector = mkSelector "resetUpcomingSampleBufferPresentationTimeExpectations"

-- | @Selector@ for @copyDisplayedPixelBuffer@
copyDisplayedPixelBufferSelector :: Selector '[] (Ptr ())
copyDisplayedPixelBufferSelector = mkSelector "copyDisplayedPixelBuffer"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVQueuedSampleBufferRenderingStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @requiresFlushToResumeDecoding@
requiresFlushToResumeDecodingSelector :: Selector '[] Bool
requiresFlushToResumeDecodingSelector = mkSelector "requiresFlushToResumeDecoding"

-- | @Selector@ for @recommendedPixelBufferAttributes@
recommendedPixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
recommendedPixelBufferAttributesSelector = mkSelector "recommendedPixelBufferAttributes"

