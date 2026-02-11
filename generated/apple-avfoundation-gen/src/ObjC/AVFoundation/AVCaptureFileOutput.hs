{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureFileOutput
--
-- AVCaptureFileOutput is an abstract subclass of AVCaptureOutput that provides an interface for writing captured media to files.
--
-- This abstract superclass defines the interface for outputs that record media samples to files. File outputs can start recording to a new file using the startRecordingToOutputFileURL:recordingDelegate: method. On successive invocations of this method on macOS, the output file can by changed dynamically without losing media samples. A file output can stop recording using the stopRecording method. Because files are recorded in the background, applications will need to specify a delegate for each new file so that they can be notified when recorded files are finished.
--
-- On macOS, clients can also set a delegate on the file output itself that can be used to control recording along exact media sample boundaries using the captureOutput:didOutputSampleBuffer:fromConnection: method.
--
-- The concrete subclasses of AVCaptureFileOutput are AVCaptureMovieFileOutput, which records media to a QuickTime movie file, and AVCaptureAudioFileOutput, which writes audio media to a variety of audio file formats.
--
-- Generated bindings for @AVCaptureFileOutput@.
module ObjC.AVFoundation.AVCaptureFileOutput
  ( AVCaptureFileOutput
  , IsAVCaptureFileOutput(..)
  , startRecordingToOutputFileURL_recordingDelegate
  , stopRecording
  , pauseRecording
  , resumeRecording
  , delegate
  , setDelegate
  , outputFileURL
  , recording
  , recordingPaused
  , recordedFileSize
  , maxRecordedFileSize
  , setMaxRecordedFileSize
  , minFreeDiskSpaceLimit
  , setMinFreeDiskSpaceLimit
  , startRecordingToOutputFileURL_recordingDelegateSelector
  , stopRecordingSelector
  , pauseRecordingSelector
  , resumeRecordingSelector
  , delegateSelector
  , setDelegateSelector
  , outputFileURLSelector
  , recordingSelector
  , recordingPausedSelector
  , recordedFileSizeSelector
  , maxRecordedFileSizeSelector
  , setMaxRecordedFileSizeSelector
  , minFreeDiskSpaceLimitSelector
  , setMinFreeDiskSpaceLimitSelector


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

-- | startRecordingToOutputFileURL:recordingDelegate:
--
-- Tells the receiver to start recording to a new file, and specifies a delegate that will be notified when recording is finished.
--
-- @outputFileURL@ — An NSURL object containing the URL of the output file. This method throws an NSInvalidArgumentException if the URL is not a valid file URL.
--
-- @delegate@ — An object conforming to the AVCaptureFileOutputRecordingDelegate protocol. Clients must specify a delegate so that they can be notified when recording to the given URL is finished.
--
-- The method sets the file URL to which the receiver is currently writing output media. If a file at the given URL already exists when capturing starts, recording to the new file will fail.
--
-- Clients need not call stopRecording before calling this method while another recording is in progress. On macOS, if this method is invoked while an existing output file was already being recorded, no media samples will be discarded between the old file and the new file.
--
-- When recording is stopped either by calling stopRecording, by changing files using this method, or because of an error, the remaining data that needs to be included to the file will be written in the background. Therefore, clients must specify a delegate that will be notified when all data has been written to the file using the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: method. The recording delegate can also optionally implement methods that inform it when data starts being written, when recording is paused and resumed, and when recording is about to be finished.
--
-- On macOS, if this method is called within the captureOutput:didOutputSampleBuffer:fromConnection: delegate method, the first samples written to the new file are guaranteed to be those contained in the sample buffer passed to that method.
--
-- Note: AVCaptureAudioFileOutput does not support -startRecordingToOutputFileURL:recordingDelegate:. Use -startRecordingToOutputFileURL:outputFileType:recordingDelegate: instead.
--
-- ObjC selector: @- startRecordingToOutputFileURL:recordingDelegate:@
startRecordingToOutputFileURL_recordingDelegate :: (IsAVCaptureFileOutput avCaptureFileOutput, IsNSURL outputFileURL) => avCaptureFileOutput -> outputFileURL -> RawId -> IO ()
startRecordingToOutputFileURL_recordingDelegate avCaptureFileOutput  outputFileURL delegate =
  withObjCPtr outputFileURL $ \raw_outputFileURL ->
      sendMsg avCaptureFileOutput (mkSelector "startRecordingToOutputFileURL:recordingDelegate:") retVoid [argPtr (castPtr raw_outputFileURL :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | stopRecording
--
-- Tells the receiver to stop recording to the current file.
--
-- Clients can call this method when they want to stop recording new samples to the current file, and do not want to continue recording to another file. Clients that want to switch from one file to another should not call this method. Instead they should simply call startRecordingToOutputFileURL:recordingDelegate: with the new file URL.
--
-- When recording is stopped either by calling this method, by changing files using startRecordingToOutputFileURL:recordingDelegate:, or because of an error, the remaining data that needs to be included to the file will be written in the background. Therefore, before using the file, clients must wait until the delegate that was specified in startRecordingToOutputFileURL:recordingDelegate: is notified when all data has been written to the file using the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: method.
--
-- On macOS, if this method is called within the captureOutput:didOutputSampleBuffer:fromConnection: delegate method, the last samples written to the current file are guaranteed to be those that were output immediately before those in the sample buffer passed to that method.
--
-- ObjC selector: @- stopRecording@
stopRecording :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO ()
stopRecording avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "stopRecording") retVoid []

-- | pauseRecording
--
-- Pauses recording to the current output file.
--
-- This method causes the receiver to stop writing captured samples to the current output file returned by outputFileURL, but leaves the file open so that samples can be written to it in the future, when resumeRecording is called. This allows clients to record multiple media segments that are not contiguous in time to a single file.
--
-- On macOS, if this method is called within the captureOutput:didOutputSampleBuffer:fromConnection: delegate method, the last samples written to the current file are guaranteed to be those that were output immediately before those in the sample buffer passed to that method.
--
-- A recording can be stopped as normal, even when it's paused.
--
-- A format or device change will result in the recording being stopped, even when it's paused.
--
-- ObjC selector: @- pauseRecording@
pauseRecording :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO ()
pauseRecording avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "pauseRecording") retVoid []

-- | resumeRecording
--
-- Resumes recording to the current output file after it was previously paused using pauseRecording.
--
-- This method causes the receiver to resume writing captured samples to the current output file returned by outputFileURL, after recording was previously paused using pauseRecording. This allows clients to record multiple media segments that are not contiguous in time to a single file.
--
-- On macOS, if this method is called within the captureOutput:didOutputSampleBuffer:fromConnection: delegate method, the first samples written to the current file are guaranteed to be those contained in the sample buffer passed to that method.
--
-- ObjC selector: @- resumeRecording@
resumeRecording :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO ()
resumeRecording avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "resumeRecording") retVoid []

-- | delegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureFileOutputDelegate protocol that will be able to monitor and control recording along exact sample boundaries.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO RawId
delegate avCaptureFileOutput  =
    fmap (RawId . castPtr) $ sendMsg avCaptureFileOutput (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureFileOutputDelegate protocol that will be able to monitor and control recording along exact sample boundaries.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> RawId -> IO ()
setDelegate avCaptureFileOutput  value =
    sendMsg avCaptureFileOutput (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | outputFileURL
--
-- The file URL of the file to which the receiver is currently recording incoming buffers.
--
-- The value of this property is an NSURL object containing the file URL of the file currently being written by the receiver. Returns nil if the receiver is not recording to any file.
--
-- ObjC selector: @- outputFileURL@
outputFileURL :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO (Id NSURL)
outputFileURL avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "outputFileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | recording
--
-- Indicates whether the receiver is currently recording.
--
-- The value of this property is YES when the receiver currently has a file to which it is writing new samples, NO otherwise.
--
-- ObjC selector: @- recording@
recording :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO Bool
recording avCaptureFileOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureFileOutput (mkSelector "recording") retCULong []

-- | recordingPaused
--
-- Indicates whether recording to the current output file is paused.
--
-- This property indicates recording to the file returned by outputFileURL has been previously paused using the pauseRecording method. When a recording is paused, captured samples are not written to the output file, but new samples can be written to the same file in the future by calling resumeRecording.
--
-- ObjC selector: @- recordingPaused@
recordingPaused :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO Bool
recordingPaused avCaptureFileOutput  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureFileOutput (mkSelector "recordingPaused") retCULong []

-- | recordedFileSize
--
-- Indicates the size, in bytes, of the data recorded to the current output file.
--
-- If a recording is in progress, this property returns the size in bytes of the data recorded so far.
--
-- ObjC selector: @- recordedFileSize@
recordedFileSize :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO CLong
recordedFileSize avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "recordedFileSize") retCLong []

-- | maxRecordedFileSize
--
-- Specifies the maximum size, in bytes, of the data that should be recorded by the receiver.
--
-- This property specifies a hard limit on the data size of recorded files. Recording is stopped when the limit is reached and the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: delegate method is invoked with an appropriate error. The default value of this property is 0, which indicates no limit.
--
-- ObjC selector: @- maxRecordedFileSize@
maxRecordedFileSize :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO CLong
maxRecordedFileSize avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "maxRecordedFileSize") retCLong []

-- | maxRecordedFileSize
--
-- Specifies the maximum size, in bytes, of the data that should be recorded by the receiver.
--
-- This property specifies a hard limit on the data size of recorded files. Recording is stopped when the limit is reached and the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: delegate method is invoked with an appropriate error. The default value of this property is 0, which indicates no limit.
--
-- ObjC selector: @- setMaxRecordedFileSize:@
setMaxRecordedFileSize :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> CLong -> IO ()
setMaxRecordedFileSize avCaptureFileOutput  value =
    sendMsg avCaptureFileOutput (mkSelector "setMaxRecordedFileSize:") retVoid [argCLong value]

-- | minFreeDiskSpaceLimit
--
-- Specifies the minimum amount of free space, in bytes, required for recording to continue on a given volume.
--
-- This property specifies a hard lower limit on the amount of free space that must remain on a target volume for recording to continue. Recording is stopped when the limit is reached and the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: delegate method is invoked with an appropriate error.
--
-- ObjC selector: @- minFreeDiskSpaceLimit@
minFreeDiskSpaceLimit :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> IO CLong
minFreeDiskSpaceLimit avCaptureFileOutput  =
    sendMsg avCaptureFileOutput (mkSelector "minFreeDiskSpaceLimit") retCLong []

-- | minFreeDiskSpaceLimit
--
-- Specifies the minimum amount of free space, in bytes, required for recording to continue on a given volume.
--
-- This property specifies a hard lower limit on the amount of free space that must remain on a target volume for recording to continue. Recording is stopped when the limit is reached and the captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error: delegate method is invoked with an appropriate error.
--
-- ObjC selector: @- setMinFreeDiskSpaceLimit:@
setMinFreeDiskSpaceLimit :: IsAVCaptureFileOutput avCaptureFileOutput => avCaptureFileOutput -> CLong -> IO ()
setMinFreeDiskSpaceLimit avCaptureFileOutput  value =
    sendMsg avCaptureFileOutput (mkSelector "setMinFreeDiskSpaceLimit:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startRecordingToOutputFileURL:recordingDelegate:@
startRecordingToOutputFileURL_recordingDelegateSelector :: Selector
startRecordingToOutputFileURL_recordingDelegateSelector = mkSelector "startRecordingToOutputFileURL:recordingDelegate:"

-- | @Selector@ for @stopRecording@
stopRecordingSelector :: Selector
stopRecordingSelector = mkSelector "stopRecording"

-- | @Selector@ for @pauseRecording@
pauseRecordingSelector :: Selector
pauseRecordingSelector = mkSelector "pauseRecording"

-- | @Selector@ for @resumeRecording@
resumeRecordingSelector :: Selector
resumeRecordingSelector = mkSelector "resumeRecording"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @outputFileURL@
outputFileURLSelector :: Selector
outputFileURLSelector = mkSelector "outputFileURL"

-- | @Selector@ for @recording@
recordingSelector :: Selector
recordingSelector = mkSelector "recording"

-- | @Selector@ for @recordingPaused@
recordingPausedSelector :: Selector
recordingPausedSelector = mkSelector "recordingPaused"

-- | @Selector@ for @recordedFileSize@
recordedFileSizeSelector :: Selector
recordedFileSizeSelector = mkSelector "recordedFileSize"

-- | @Selector@ for @maxRecordedFileSize@
maxRecordedFileSizeSelector :: Selector
maxRecordedFileSizeSelector = mkSelector "maxRecordedFileSize"

-- | @Selector@ for @setMaxRecordedFileSize:@
setMaxRecordedFileSizeSelector :: Selector
setMaxRecordedFileSizeSelector = mkSelector "setMaxRecordedFileSize:"

-- | @Selector@ for @minFreeDiskSpaceLimit@
minFreeDiskSpaceLimitSelector :: Selector
minFreeDiskSpaceLimitSelector = mkSelector "minFreeDiskSpaceLimit"

-- | @Selector@ for @setMinFreeDiskSpaceLimit:@
setMinFreeDiskSpaceLimitSelector :: Selector
setMinFreeDiskSpaceLimitSelector = mkSelector "setMinFreeDiskSpaceLimit:"

