{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSynchronizedSampleBufferData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureVideoDataOutput or AVCaptureAudioDataOutput.
--
-- Synchronized sample buffer data is valid for the duration of AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback. To extend the sample buffer data beyond the callback, you must CFRetain it, and later call CFRelease when you're done with it.
--
-- Generated bindings for @AVCaptureSynchronizedSampleBufferData@.
module ObjC.AVFoundation.AVCaptureSynchronizedSampleBufferData
  ( AVCaptureSynchronizedSampleBufferData
  , IsAVCaptureSynchronizedSampleBufferData(..)
  , sampleBuffer
  , sampleBufferWasDropped
  , droppedReason
  , droppedReasonSelector
  , sampleBufferSelector
  , sampleBufferWasDroppedSelector

  -- * Enum types
  , AVCaptureOutputDataDroppedReason(AVCaptureOutputDataDroppedReason)
  , pattern AVCaptureOutputDataDroppedReasonNone
  , pattern AVCaptureOutputDataDroppedReasonLateData
  , pattern AVCaptureOutputDataDroppedReasonOutOfBuffers
  , pattern AVCaptureOutputDataDroppedReasonDiscontinuity

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

-- | sampleBuffer
--
-- A sample buffer containing video or audio data.
--
-- If sampleBufferWasDropped is YES, the returned sampleBuffer was dropped before it could be delivered to you, and thus this sample buffer is a shell containing metadata and format information, but no actual pixel data. This property is never NULL. If a data output has no data to return, it is simply not present in the dictionary of synchronized data returned by AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback.
--
-- ObjC selector: @- sampleBuffer@
sampleBuffer :: IsAVCaptureSynchronizedSampleBufferData avCaptureSynchronizedSampleBufferData => avCaptureSynchronizedSampleBufferData -> IO (Ptr ())
sampleBuffer avCaptureSynchronizedSampleBufferData =
  sendMessage avCaptureSynchronizedSampleBufferData sampleBufferSelector

-- | sampleBufferWasDropped
--
-- YES if the sample buffer was dropped.
--
-- AVCaptureVideoDataOutput has a delegate callback for dropped sample buffers. AVCaptureAudioDataOutput does not. Therefore, sampleBufferWasDropped may be YES for video, but never for audio.
--
-- ObjC selector: @- sampleBufferWasDropped@
sampleBufferWasDropped :: IsAVCaptureSynchronizedSampleBufferData avCaptureSynchronizedSampleBufferData => avCaptureSynchronizedSampleBufferData -> IO Bool
sampleBufferWasDropped avCaptureSynchronizedSampleBufferData =
  sendMessage avCaptureSynchronizedSampleBufferData sampleBufferWasDroppedSelector

-- | droppedReason
--
-- If sampleBufferWasDropped is YES, the reason for the drop, otherwise AVCaptureOutputDataDroppedReasonNone.
--
-- AVCaptureOutputDataDroppedReasons are defined in AVCaptureOutputBase.h.
--
-- ObjC selector: @- droppedReason@
droppedReason :: IsAVCaptureSynchronizedSampleBufferData avCaptureSynchronizedSampleBufferData => avCaptureSynchronizedSampleBufferData -> IO AVCaptureOutputDataDroppedReason
droppedReason avCaptureSynchronizedSampleBufferData =
  sendMessage avCaptureSynchronizedSampleBufferData droppedReasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleBuffer@
sampleBufferSelector :: Selector '[] (Ptr ())
sampleBufferSelector = mkSelector "sampleBuffer"

-- | @Selector@ for @sampleBufferWasDropped@
sampleBufferWasDroppedSelector :: Selector '[] Bool
sampleBufferWasDroppedSelector = mkSelector "sampleBufferWasDropped"

-- | @Selector@ for @droppedReason@
droppedReasonSelector :: Selector '[] AVCaptureOutputDataDroppedReason
droppedReasonSelector = mkSelector "droppedReason"

