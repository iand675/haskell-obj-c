{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSampleBufferDisplayLayer@.
module ObjC.AVFoundation.AVSampleBufferDisplayLayer
  ( AVSampleBufferDisplayLayer
  , IsAVSampleBufferDisplayLayer(..)
  , enqueueSampleBuffer
  , flush
  , flushAndRemoveImage
  , requestMediaDataWhenReadyOnQueue_usingBlock
  , stopRequestingMediaData
  , controlTimebase
  , setControlTimebase
  , videoGravity
  , setVideoGravity
  , readyForDisplay
  , sampleBufferRenderer
  , outputObscuredDueToInsufficientExternalProtection
  , preventsAutomaticBackgroundingDuringVideoPlayback
  , setPreventsAutomaticBackgroundingDuringVideoPlayback
  , preventsDisplaySleepDuringVideoPlayback
  , setPreventsDisplaySleepDuringVideoPlayback
  , preventsCapture
  , setPreventsCapture
  , timebase
  , status
  , error_
  , requiresFlushToResumeDecoding
  , readyForMoreMediaData
  , hasSufficientMediaDataForReliablePlaybackStart
  , enqueueSampleBufferSelector
  , flushSelector
  , flushAndRemoveImageSelector
  , requestMediaDataWhenReadyOnQueue_usingBlockSelector
  , stopRequestingMediaDataSelector
  , controlTimebaseSelector
  , setControlTimebaseSelector
  , videoGravitySelector
  , setVideoGravitySelector
  , readyForDisplaySelector
  , sampleBufferRendererSelector
  , outputObscuredDueToInsufficientExternalProtectionSelector
  , preventsAutomaticBackgroundingDuringVideoPlaybackSelector
  , setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector
  , preventsDisplaySleepDuringVideoPlaybackSelector
  , setPreventsDisplaySleepDuringVideoPlaybackSelector
  , preventsCaptureSelector
  , setPreventsCaptureSelector
  , timebaseSelector
  , statusSelector
  , errorSelector
  , requiresFlushToResumeDecodingSelector
  , readyForMoreMediaDataSelector
  , hasSufficientMediaDataForReliablePlaybackStartSelector

  -- * Enum types
  , AVQueuedSampleBufferRenderingStatus(AVQueuedSampleBufferRenderingStatus)
  , pattern AVQueuedSampleBufferRenderingStatusUnknown
  , pattern AVQueuedSampleBufferRenderingStatusRendering
  , pattern AVQueuedSampleBufferRenderingStatusFailed

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | enqueueSampleBuffer:
--
-- Sends a sample buffer for display.
--
-- If sampleBuffer has the kCMSampleAttachmentKey_DoNotDisplay attachment set to					kCFBooleanTrue, the frame will be decoded but not displayed.					Otherwise, if sampleBuffer has the kCMSampleAttachmentKey_DisplayImmediately					attachment set to kCFBooleanTrue, the decoded image will be displayed as soon 					as possible, replacing all previously enqueued images regardless of their timestamps.					Otherwise, the decoded image will be displayed at sampleBuffer's output presentation					timestamp, as interpreted by the control timebase (or the mach_absolute_time timeline					if there is no control timebase).
--
-- To schedule the removal of previous images at a specific timestamp, enqueue 					a marker sample buffer containing no samples, with the					kCMSampleBufferAttachmentKey_EmptyMedia attachment set to kCFBooleanTrue.
--
-- IMPORTANT NOTE: attachments with the kCMSampleAttachmentKey_ prefix must be set via					CMSampleBufferGetSampleAttachmentsArray and CFDictionarySetValue. 					Attachments with the kCMSampleBufferAttachmentKey_ prefix must be set via					CMSetAttachment.
--
-- IMPORTANT NOTE:  When using CMSampleBuffers that wrap CVPixelBuffer, it is important that such CVPixelBuffers be IOSurface-backed.					CoreVideo allocates IOSurface-backed CVPixelBuffers when the pixel buffer attribute dictionary passed to CVPixelBufferPoolCreate contains					an entry with key kCVPixelBufferIOSurfacePropertiesKey and value being a dictionary (which can be an empty dictionary).
--
-- The combination of either a non-NULL controlTimebase or an AVSampleBufferRenderSynchronizer with the use of kCMSampleAttachmentKey_DisplayImmediately as an attachment to the CMSampleBuffers that are enqueued for display is not recommended.
--
-- ObjC selector: @- enqueueSampleBuffer:@
enqueueSampleBuffer :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> Ptr () -> IO ()
enqueueSampleBuffer avSampleBufferDisplayLayer  sampleBuffer =
    sendMsg avSampleBufferDisplayLayer (mkSelector "enqueueSampleBuffer:") retVoid [argPtr sampleBuffer]

-- | flush
--
-- Instructs the layer to discard pending enqueued sample buffers.
--
-- It is not possible to determine which sample buffers have been decoded, 					so the next frame passed to enqueueSampleBuffer: should be an IDR frame					(also known as a key frame or sync sample).
--
-- ObjC selector: @- flush@
flush :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO ()
flush avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "flush") retVoid []

-- | flushAndRemoveImage
--
-- Instructs the layer to discard pending enqueued sample buffers and remove any					currently displayed image.
--
-- It is not possible to determine which sample buffers have been decoded, 					so the next frame passed to enqueueSampleBuffer: should be an IDR frame					(also known as a key frame or sync sample).
--
-- ObjC selector: @- flushAndRemoveImage@
flushAndRemoveImage :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO ()
flushAndRemoveImage avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "flushAndRemoveImage") retVoid []

-- | requestMediaDataWhenReadyOnQueue:usingBlock:
--
-- Instructs the target to invoke a client-supplied block repeatedly, 					at its convenience, in order to gather sample buffers for display.
--
-- The block should enqueue sample buffers to the layer either until the layer's					readyForMoreMediaData property becomes NO or until there is no more data 					to supply. When the layer has decoded enough of the media data it has received 					that it becomes ready for more media data again, it will invoke the block again 					in order to obtain more.					If this function is called multiple times, only the last call is effective.					Call stopRequestingMediaData to cancel this request.					Each call to requestMediaDataWhenReadyOnQueue:usingBlock: should be paired					with a corresponding call to stopRequestingMediaData:. Releasing the					AVSampleBufferDisplayLayer without a call to stopRequestingMediaData will result					in undefined behavior.
--
-- ObjC selector: @- requestMediaDataWhenReadyOnQueue:usingBlock:@
requestMediaDataWhenReadyOnQueue_usingBlock :: (IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer, IsNSObject queue) => avSampleBufferDisplayLayer -> queue -> Ptr () -> IO ()
requestMediaDataWhenReadyOnQueue_usingBlock avSampleBufferDisplayLayer  queue block =
  withObjCPtr queue $ \raw_queue ->
      sendMsg avSampleBufferDisplayLayer (mkSelector "requestMediaDataWhenReadyOnQueue:usingBlock:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | stopRequestingMediaData
--
-- Cancels any current requestMediaDataWhenReadyOnQueue:usingBlock: call.
--
-- This method may be called from outside the block or from within the block.
--
-- ObjC selector: @- stopRequestingMediaData@
stopRequestingMediaData :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO ()
stopRequestingMediaData avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "stopRequestingMediaData") retVoid []

-- | controlTimebase
--
-- The layer's control timebase, which governs how time stamps are interpreted.
--
-- By default, this property is NULL, in which case time stamps will be interpreted					according to the host time clock (mach_absolute_time with the appropriate timescale					conversion; this is the same as Core Animation's CACurrentMediaTime).  With no 					control timebase, once frames are enqueued, it is not possible to adjust exactly 					when they are displayed.
--
-- If a non-NULL control timebase is set, it will be used to interpret time stamps.					You can control the timing of frame display by setting the rate and time of the					control timebase.  					If you are synchronizing video to audio, you can use a timebase whose source clock					is a CMAudioDeviceClock for the appropriate audio device to prevent drift.
--
-- Note that prior to OSX 10.10 and iOS 8.0, the control timebase could not be changed after enqueueSampleBuffer: was called.  As of OSX 10.10 and iOS 8.0, the control timebase may be changed at any time.
--
-- ObjC selector: @- controlTimebase@
controlTimebase :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO (Ptr ())
controlTimebase avSampleBufferDisplayLayer  =
    fmap castPtr $ sendMsg avSampleBufferDisplayLayer (mkSelector "controlTimebase") (retPtr retVoid) []

-- | controlTimebase
--
-- The layer's control timebase, which governs how time stamps are interpreted.
--
-- By default, this property is NULL, in which case time stamps will be interpreted					according to the host time clock (mach_absolute_time with the appropriate timescale					conversion; this is the same as Core Animation's CACurrentMediaTime).  With no 					control timebase, once frames are enqueued, it is not possible to adjust exactly 					when they are displayed.
--
-- If a non-NULL control timebase is set, it will be used to interpret time stamps.					You can control the timing of frame display by setting the rate and time of the					control timebase.  					If you are synchronizing video to audio, you can use a timebase whose source clock					is a CMAudioDeviceClock for the appropriate audio device to prevent drift.
--
-- Note that prior to OSX 10.10 and iOS 8.0, the control timebase could not be changed after enqueueSampleBuffer: was called.  As of OSX 10.10 and iOS 8.0, the control timebase may be changed at any time.
--
-- ObjC selector: @- setControlTimebase:@
setControlTimebase :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> Ptr () -> IO ()
setControlTimebase avSampleBufferDisplayLayer  value =
    sendMsg avSampleBufferDisplayLayer (mkSelector "setControlTimebase:") retVoid [argPtr value]

-- | videoGravity
--
-- A string defining how the video is displayed within an AVSampleBufferDisplayLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- videoGravity@
videoGravity :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO (Id NSString)
videoGravity avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "videoGravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoGravity
--
-- A string defining how the video is displayed within an AVSampleBufferDisplayLayer bounds rect.
--
-- Options are AVLayerVideoGravityResizeAspect, AVLayerVideoGravityResizeAspectFill  					and AVLayerVideoGravityResize. AVLayerVideoGravityResizeAspect is default. 					See <AVFoundation/AVAnimation.h> for a description of these options.
--
-- ObjC selector: @- setVideoGravity:@
setVideoGravity :: (IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer, IsNSString value) => avSampleBufferDisplayLayer -> value -> IO ()
setVideoGravity avSampleBufferDisplayLayer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avSampleBufferDisplayLayer (mkSelector "setVideoGravity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | readyForDisplay
--
-- Boolean indicating that the first video frame has been made ready for display.
--
-- Use this property as an indicator of when best to show or animate-in an AVSampleBufferDisplayLayer into view.					An AVSampleBufferDisplayLayer may be displayed, or made visible, while this property is NO, however the layer will not have any user-visible content until the value becomes YES. Note that if an animation is added to an AVSampleBufferDisplayLayer before it becomes readyForDisplay the video image displayed inside might not animate with the receiver.					readyForDisplay will change to NO when the layer can no longer display frames. readyForDisplay will be YES when the first video frame has been made ready for display.					This property is not key-value observable.  AVSampleBufferDisplayLayerReadyForDisplayDidChangeNotification is posted when this value changes.
--
-- ObjC selector: @- readyForDisplay@
readyForDisplay :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
readyForDisplay avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "readyForDisplay") retCULong []

-- | sampleBufferRenderer
--
-- An AVSampleBufferVideoRenderer instance that allows enqueuing sample buffers for rendering.
--
-- Although AVSampleBufferDisplayLayer conforms to the AVQueuedSampleBufferRendering protocol, the sampleBufferRenderer should be used to enqueue sample buffers. sampleBufferRenderer allows the client to safely enqueue sample buffers from a background thread. NOTE: Do not use AVSampleBufferDisplayLayer's AVQueuedSampleBufferRendering functions when using sampleBufferRenderer.
--
-- ObjC selector: @- sampleBufferRenderer@
sampleBufferRenderer :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO (Id AVSampleBufferVideoRenderer)
sampleBufferRenderer avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "sampleBufferRenderer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputObscuredDueToInsufficientExternalProtection
--
-- Whether or not decoded output is being obscured due to insufficient external protection.
--
-- The value of this property indicates whether the layer is purposefully obscuring its visual output	 because the requirement for an external protection mechanism is not met by the current device	 configuration. The change of this property can be observed through AVSampleBufferDisplayLayerOutputObscuredDueToInsufficientExternalProtectionDidChangeNotification
--
-- It is highly recommended that clients whose content requires external	 protection observe this property and set the playback rate to zero and display an appropriate user	 interface when the value changes to YES.
--
-- Note that the value of this property is dependent on the external protection requirements of the	 media being displayed by the layer. These requirements are inherent to the content itself and cannot	 be externally specified. If the content does not require external protection, the value of this	 property will be NO.
--
-- ObjC selector: @- outputObscuredDueToInsufficientExternalProtection@
outputObscuredDueToInsufficientExternalProtection :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
outputObscuredDueToInsufficientExternalProtection avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "outputObscuredDueToInsufficientExternalProtection") retCULong []

-- | preventsAutomaticBackgroundingDuringVideoPlayback
--
-- Indicates whether video playback prevents the app from automatically getting backgrounded.
--
-- Default is YES.	 Setting this property to YES prevents an application that is playing video from automatically getting backgrounded.  This property does not prevent the user from backgrounding the application.	 Note: If sample buffers are being enqueued for playback at the user's request, you should ensure that the value of this property is set to YES. If video is not being displayed as part of the user's primary focus, you should ensure that the value of this property is set to NO.
--
-- ObjC selector: @- preventsAutomaticBackgroundingDuringVideoPlayback@
preventsAutomaticBackgroundingDuringVideoPlayback :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
preventsAutomaticBackgroundingDuringVideoPlayback avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "preventsAutomaticBackgroundingDuringVideoPlayback") retCULong []

-- | preventsAutomaticBackgroundingDuringVideoPlayback
--
-- Indicates whether video playback prevents the app from automatically getting backgrounded.
--
-- Default is YES.	 Setting this property to YES prevents an application that is playing video from automatically getting backgrounded.  This property does not prevent the user from backgrounding the application.	 Note: If sample buffers are being enqueued for playback at the user's request, you should ensure that the value of this property is set to YES. If video is not being displayed as part of the user's primary focus, you should ensure that the value of this property is set to NO.
--
-- ObjC selector: @- setPreventsAutomaticBackgroundingDuringVideoPlayback:@
setPreventsAutomaticBackgroundingDuringVideoPlayback :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> Bool -> IO ()
setPreventsAutomaticBackgroundingDuringVideoPlayback avSampleBufferDisplayLayer  value =
    sendMsg avSampleBufferDisplayLayer (mkSelector "setPreventsAutomaticBackgroundingDuringVideoPlayback:") retVoid [argCULong (if value then 1 else 0)]

-- | preventsDisplaySleepDuringVideoPlayback
--
-- Indicates whether video playback prevents display and device sleep.
--
-- Default is YES on iOS, tvOS and in Mac Catalyst apps.  Default is NO on macOS. Setting this property to NO does not force the display to sleep, it simply stops preventing display sleep.  Other apps or frameworks within your app may still be preventing display sleep for various reasons. Note: If sample buffers are being enqueued for playback at the user's request, you should ensure that the value of this property is set to YES. If video is not being displayed as part of the user's primary focus, you should ensure that the value of this property is set to NO.
--
-- ObjC selector: @- preventsDisplaySleepDuringVideoPlayback@
preventsDisplaySleepDuringVideoPlayback :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
preventsDisplaySleepDuringVideoPlayback avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "preventsDisplaySleepDuringVideoPlayback") retCULong []

-- | preventsDisplaySleepDuringVideoPlayback
--
-- Indicates whether video playback prevents display and device sleep.
--
-- Default is YES on iOS, tvOS and in Mac Catalyst apps.  Default is NO on macOS. Setting this property to NO does not force the display to sleep, it simply stops preventing display sleep.  Other apps or frameworks within your app may still be preventing display sleep for various reasons. Note: If sample buffers are being enqueued for playback at the user's request, you should ensure that the value of this property is set to YES. If video is not being displayed as part of the user's primary focus, you should ensure that the value of this property is set to NO.
--
-- ObjC selector: @- setPreventsDisplaySleepDuringVideoPlayback:@
setPreventsDisplaySleepDuringVideoPlayback :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> Bool -> IO ()
setPreventsDisplaySleepDuringVideoPlayback avSampleBufferDisplayLayer  value =
    sendMsg avSampleBufferDisplayLayer (mkSelector "setPreventsDisplaySleepDuringVideoPlayback:") retVoid [argCULong (if value then 1 else 0)]

-- | preventsCapture
--
-- Indicates that image data should be protected from capture.
--
-- ObjC selector: @- preventsCapture@
preventsCapture :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
preventsCapture avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "preventsCapture") retCULong []

-- | preventsCapture
--
-- Indicates that image data should be protected from capture.
--
-- ObjC selector: @- setPreventsCapture:@
setPreventsCapture :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> Bool -> IO ()
setPreventsCapture avSampleBufferDisplayLayer  value =
    sendMsg avSampleBufferDisplayLayer (mkSelector "setPreventsCapture:") retVoid [argCULong (if value then 1 else 0)]

-- | timebase
--
-- The renderer's timebase, which governs how time stamps are interpreted.
--
-- The timebase is used to interpret time stamps.
--
-- The timebase is read-only.  Use the AVSampleBufferRenderSynchronizer to set the rate or time.
--
-- ObjC selector: @- timebase@
timebase :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO (Ptr ())
timebase avSampleBufferDisplayLayer  =
    fmap castPtr $ sendMsg avSampleBufferDisplayLayer (mkSelector "timebase") (retPtr retVoid) []

-- | status
--
-- The ability of the display layer to be used for enqueuing sample buffers.
--
-- The value of this property is an AVQueuedSampleBufferRenderingStatus that indicates whether the receiver can be used for enqueuing and rendering sample buffers. When the value of this property is AVQueuedSampleBufferRenderingStatusFailed, clients can check the value of the error property to determine the failure. To resume rendering sample buffers using the display layer after a failure, clients must first reset the status to AVQueuedSampleBufferRenderingStatusUnknown. This can be achieved by invoking -flush on the display layer.
--
-- This property is key value observable.
--
-- ObjC selector: @- status@
status :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO AVQueuedSampleBufferRenderingStatus
status avSampleBufferDisplayLayer  =
    fmap (coerce :: CLong -> AVQueuedSampleBufferRenderingStatus) $ sendMsg avSampleBufferDisplayLayer (mkSelector "status") retCLong []

-- | error
--
-- If the display layer's status is AVQueuedSampleBufferRenderingStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the display layer to no longer be able to enqueue sample buffers. If the status is not AVQueuedSampleBufferRenderingStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO (Id NSError)
error_ avSampleBufferDisplayLayer  =
    sendMsg avSampleBufferDisplayLayer (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | requiresFlushToResumeDecoding
--
-- Indicates that the receiver is in a state where it requires a call to -flush to continue decoding frames.
--
-- When the application enters a state where use of video decoder resources is not permissible, the value of this property changes to YES along with the display layer's status changing to AVQueuedSampleBufferRenderingStatusFailed.					To resume rendering sample buffers using the display layer after this property's value is YES, clients must first reset the display layer's status to AVQueuedSampleBufferRenderingStatusUnknown. This can be achieved by invoking -flush on the display layer.					Clients can track changes to this property via AVSampleBufferDisplayLayerRequiresFlushToResumeDecodingDidChangeNotification.					This property is not key value observable.
--
-- ObjC selector: @- requiresFlushToResumeDecoding@
requiresFlushToResumeDecoding :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
requiresFlushToResumeDecoding avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "requiresFlushToResumeDecoding") retCULong []

-- | readyForMoreMediaData
--
-- Indicates the readiness of the layer to accept more sample buffers.
--
-- AVSampleBufferDisplayLayer keeps track of the occupancy levels of its internal queues					for the benefit of clients that enqueue sample buffers from non-real-time sources --					i.e., clients that can supply sample buffers faster than they are consumed, and so					need to decide when to hold back.
--
-- Clients enqueueing sample buffers from non-real-time sources may hold off from					generating or obtaining more sample buffers to enqueue when the value of					readyForMoreMediaData is NO.
--
-- It is safe to call enqueueSampleBuffer: when readyForMoreMediaData is NO, but 					it is a bad idea to enqueue sample buffers without bound.
--
-- To help with control of the non-real-time supply of sample buffers, such clients can use					-requestMediaDataWhenReadyOnQueue:usingBlock					in order to specify a block that the layer should invoke whenever it's ready for 					sample buffers to be appended.
--
-- The value of readyForMoreMediaData will often change from NO to YES asynchronously, 					as previously supplied sample buffers are decoded and displayed.
--
-- This property is not key value observable.
--
-- ObjC selector: @- readyForMoreMediaData@
readyForMoreMediaData :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
readyForMoreMediaData avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "readyForMoreMediaData") retCULong []

-- | hasSufficientMediaDataForReliablePlaybackStart
--
-- Indicates whether the enqueued media data meets the renderer's preroll level.
--
-- Clients should fetch the value of this property to learn if the renderer has had enough media data enqueued to start playback reliably. Starting playback when this property is NO may prevent smooth playback following an immediate start.
--
-- ObjC selector: @- hasSufficientMediaDataForReliablePlaybackStart@
hasSufficientMediaDataForReliablePlaybackStart :: IsAVSampleBufferDisplayLayer avSampleBufferDisplayLayer => avSampleBufferDisplayLayer -> IO Bool
hasSufficientMediaDataForReliablePlaybackStart avSampleBufferDisplayLayer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avSampleBufferDisplayLayer (mkSelector "hasSufficientMediaDataForReliablePlaybackStart") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enqueueSampleBuffer:@
enqueueSampleBufferSelector :: Selector
enqueueSampleBufferSelector = mkSelector "enqueueSampleBuffer:"

-- | @Selector@ for @flush@
flushSelector :: Selector
flushSelector = mkSelector "flush"

-- | @Selector@ for @flushAndRemoveImage@
flushAndRemoveImageSelector :: Selector
flushAndRemoveImageSelector = mkSelector "flushAndRemoveImage"

-- | @Selector@ for @requestMediaDataWhenReadyOnQueue:usingBlock:@
requestMediaDataWhenReadyOnQueue_usingBlockSelector :: Selector
requestMediaDataWhenReadyOnQueue_usingBlockSelector = mkSelector "requestMediaDataWhenReadyOnQueue:usingBlock:"

-- | @Selector@ for @stopRequestingMediaData@
stopRequestingMediaDataSelector :: Selector
stopRequestingMediaDataSelector = mkSelector "stopRequestingMediaData"

-- | @Selector@ for @controlTimebase@
controlTimebaseSelector :: Selector
controlTimebaseSelector = mkSelector "controlTimebase"

-- | @Selector@ for @setControlTimebase:@
setControlTimebaseSelector :: Selector
setControlTimebaseSelector = mkSelector "setControlTimebase:"

-- | @Selector@ for @videoGravity@
videoGravitySelector :: Selector
videoGravitySelector = mkSelector "videoGravity"

-- | @Selector@ for @setVideoGravity:@
setVideoGravitySelector :: Selector
setVideoGravitySelector = mkSelector "setVideoGravity:"

-- | @Selector@ for @readyForDisplay@
readyForDisplaySelector :: Selector
readyForDisplaySelector = mkSelector "readyForDisplay"

-- | @Selector@ for @sampleBufferRenderer@
sampleBufferRendererSelector :: Selector
sampleBufferRendererSelector = mkSelector "sampleBufferRenderer"

-- | @Selector@ for @outputObscuredDueToInsufficientExternalProtection@
outputObscuredDueToInsufficientExternalProtectionSelector :: Selector
outputObscuredDueToInsufficientExternalProtectionSelector = mkSelector "outputObscuredDueToInsufficientExternalProtection"

-- | @Selector@ for @preventsAutomaticBackgroundingDuringVideoPlayback@
preventsAutomaticBackgroundingDuringVideoPlaybackSelector :: Selector
preventsAutomaticBackgroundingDuringVideoPlaybackSelector = mkSelector "preventsAutomaticBackgroundingDuringVideoPlayback"

-- | @Selector@ for @setPreventsAutomaticBackgroundingDuringVideoPlayback:@
setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector :: Selector
setPreventsAutomaticBackgroundingDuringVideoPlaybackSelector = mkSelector "setPreventsAutomaticBackgroundingDuringVideoPlayback:"

-- | @Selector@ for @preventsDisplaySleepDuringVideoPlayback@
preventsDisplaySleepDuringVideoPlaybackSelector :: Selector
preventsDisplaySleepDuringVideoPlaybackSelector = mkSelector "preventsDisplaySleepDuringVideoPlayback"

-- | @Selector@ for @setPreventsDisplaySleepDuringVideoPlayback:@
setPreventsDisplaySleepDuringVideoPlaybackSelector :: Selector
setPreventsDisplaySleepDuringVideoPlaybackSelector = mkSelector "setPreventsDisplaySleepDuringVideoPlayback:"

-- | @Selector@ for @preventsCapture@
preventsCaptureSelector :: Selector
preventsCaptureSelector = mkSelector "preventsCapture"

-- | @Selector@ for @setPreventsCapture:@
setPreventsCaptureSelector :: Selector
setPreventsCaptureSelector = mkSelector "setPreventsCapture:"

-- | @Selector@ for @timebase@
timebaseSelector :: Selector
timebaseSelector = mkSelector "timebase"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @requiresFlushToResumeDecoding@
requiresFlushToResumeDecodingSelector :: Selector
requiresFlushToResumeDecodingSelector = mkSelector "requiresFlushToResumeDecoding"

-- | @Selector@ for @readyForMoreMediaData@
readyForMoreMediaDataSelector :: Selector
readyForMoreMediaDataSelector = mkSelector "readyForMoreMediaData"

-- | @Selector@ for @hasSufficientMediaDataForReliablePlaybackStart@
hasSufficientMediaDataForReliablePlaybackStartSelector :: Selector
hasSufficientMediaDataForReliablePlaybackStartSelector = mkSelector "hasSufficientMediaDataForReliablePlaybackStart"

