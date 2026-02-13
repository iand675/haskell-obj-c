{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | RPScreenRecorder
--
-- Singleton class used to control app recording.
--
-- Generated bindings for @RPScreenRecorder@.
module ObjC.ReplayKit.RPScreenRecorder
  ( RPScreenRecorder
  , IsRPScreenRecorder(..)
  , sharedRecorder
  , init_
  , startRecordingWithMicrophoneEnabled_handler
  , startRecordingWithHandler
  , stopRecordingWithHandler
  , stopRecordingWithOutputURL_completionHandler
  , discardRecordingWithHandler
  , startCaptureWithHandler_completionHandler
  , stopCaptureWithHandler
  , startClipBufferingWithCompletionHandler
  , stopClipBufferingWithCompletionHandler
  , exportClipToURL_duration_completionHandler
  , delegate
  , setDelegate
  , available
  , recording
  , microphoneEnabled
  , setMicrophoneEnabled
  , cameraEnabled
  , setCameraEnabled
  , cameraPosition
  , setCameraPosition
  , cameraPreviewView
  , availableSelector
  , cameraEnabledSelector
  , cameraPositionSelector
  , cameraPreviewViewSelector
  , delegateSelector
  , discardRecordingWithHandlerSelector
  , exportClipToURL_duration_completionHandlerSelector
  , initSelector
  , microphoneEnabledSelector
  , recordingSelector
  , setCameraEnabledSelector
  , setCameraPositionSelector
  , setDelegateSelector
  , setMicrophoneEnabledSelector
  , sharedRecorderSelector
  , startCaptureWithHandler_completionHandlerSelector
  , startClipBufferingWithCompletionHandlerSelector
  , startRecordingWithHandlerSelector
  , startRecordingWithMicrophoneEnabled_handlerSelector
  , stopCaptureWithHandlerSelector
  , stopClipBufferingWithCompletionHandlerSelector
  , stopRecordingWithHandlerSelector
  , stopRecordingWithOutputURL_completionHandlerSelector

  -- * Enum types
  , RPCameraPosition(RPCameraPosition)
  , pattern RPCameraPositionFront
  , pattern RPCameraPositionBack

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ReplayKit.Internal.Classes
import ObjC.ReplayKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedRecorder@
sharedRecorder :: IO (Id RPScreenRecorder)
sharedRecorder  =
  do
    cls' <- getRequiredClass "RPScreenRecorder"
    sendClassMessage cls' sharedRecorderSelector

-- | @- init@
init_ :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO (Id RPScreenRecorder)
init_ rpScreenRecorder =
  sendOwnedMessage rpScreenRecorder initSelector

-- | Deprecated. Use startRecordingWithHandler: instead.
--
-- Starts app recording with a completion handler. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- @microphoneEnabled@ — Determines whether the microphone input should be included in the recorded movie audio.
--
-- handler Called after user interactions are complete. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting the recording.
--
-- ObjC selector: @- startRecordingWithMicrophoneEnabled:handler:@
startRecordingWithMicrophoneEnabled_handler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Bool -> Ptr () -> IO ()
startRecordingWithMicrophoneEnabled_handler rpScreenRecorder microphoneEnabled handler =
  sendMessage rpScreenRecorder startRecordingWithMicrophoneEnabled_handlerSelector microphoneEnabled handler

-- | Starts app recording with a completion handler. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called after user interactions are complete. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting the recording.
--
-- ObjC selector: @- startRecordingWithHandler:@
startRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
startRecordingWithHandler rpScreenRecorder handler =
  sendMessage rpScreenRecorder startRecordingWithHandlerSelector handler

-- | Stops app recording with a completion handler.
--
-- handler Called when the movie is ready. Will return an instance of RPPreviewViewController on success which should be presented using [UIViewController presentViewController:animated:completion:]. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the recording.
--
-- ObjC selector: @- stopRecordingWithHandler:@
stopRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopRecordingWithHandler rpScreenRecorder handler =
  sendMessage rpScreenRecorder stopRecordingWithHandlerSelector handler

-- | Stops app recording with output URL and completion handler.
--
-- @url@ — Output URL for app recording movie.
--
-- handler Called when  movie is written to specified output URL. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the recording and writing the output URL.
--
-- ObjC selector: @- stopRecordingWithOutputURL:completionHandler:@
stopRecordingWithOutputURL_completionHandler :: (IsRPScreenRecorder rpScreenRecorder, IsNSURL url) => rpScreenRecorder -> url -> Ptr () -> IO ()
stopRecordingWithOutputURL_completionHandler rpScreenRecorder url completionHandler =
  sendMessage rpScreenRecorder stopRecordingWithOutputURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Discards the current recording. This can only be called after the handler block in stopRecordingWithHandler: is executed.
--
-- ObjC selector: @- discardRecordingWithHandler:@
discardRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
discardRecordingWithHandler rpScreenRecorder handler =
  sendMessage rpScreenRecorder discardRecordingWithHandlerSelector handler

-- | Starts screen and audio capture and continually calls the supplied handler with the current sampleBuffer and bufferType and passed it back to the application. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called continually with sampleBuffers and the bufferType. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting the capture.
--
-- ObjC selector: @- startCaptureWithHandler:completionHandler:@
startCaptureWithHandler_completionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> Ptr () -> IO ()
startCaptureWithHandler_completionHandler rpScreenRecorder captureHandler completionHandler =
  sendMessage rpScreenRecorder startCaptureWithHandler_completionHandlerSelector captureHandler completionHandler

-- | Stops screen capture with a completion handler
--
-- handler Called after the screen capture has stopped. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the capture
--
-- ObjC selector: @- stopCaptureWithHandler:@
stopCaptureWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopCaptureWithHandler rpScreenRecorder handler =
  sendMessage rpScreenRecorder stopCaptureWithHandlerSelector handler

-- | Start clip recording buffering with a completion handler. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called after clip recording is started. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting clip record buffering.
--
-- ObjC selector: @- startClipBufferingWithCompletionHandler:@
startClipBufferingWithCompletionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
startClipBufferingWithCompletionHandler rpScreenRecorder completionHandler =
  sendMessage rpScreenRecorder startClipBufferingWithCompletionHandlerSelector completionHandler

-- | Stop clip recording buffering with a completion handler.
--
-- handler Called after clip recording session is stopped. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping clip record buffering.
--
-- ObjC selector: @- stopClipBufferingWithCompletionHandler:@
stopClipBufferingWithCompletionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopClipBufferingWithCompletionHandler rpScreenRecorder completionHandler =
  sendMessage rpScreenRecorder stopClipBufferingWithCompletionHandlerSelector completionHandler

-- | Exports clip recording
--
-- @url@ — URL containing absolute path for where to save the clip
--
-- @duration@ — Length of time in seconds for clip recording, capped at either the elapsed time, or a maximum of 15 seconds, depending on which is the shorter amount of time
--
-- Must be called after startClipBufferingWithCompletionHandler:, otherwise this will return an error. Exports clip recording from newest samples in buffer for duration. handler Will be called after asset is finished writing to output path. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue generating the clip recording.
--
-- ObjC selector: @- exportClipToURL:duration:completionHandler:@
exportClipToURL_duration_completionHandler :: (IsRPScreenRecorder rpScreenRecorder, IsNSURL url) => rpScreenRecorder -> url -> CDouble -> Ptr () -> IO ()
exportClipToURL_duration_completionHandler rpScreenRecorder url duration completionHandler =
  sendMessage rpScreenRecorder exportClipToURL_duration_completionHandlerSelector (toNSURL url) duration completionHandler

-- | @- delegate@
delegate :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO RawId
delegate rpScreenRecorder =
  sendMessage rpScreenRecorder delegateSelector

-- | @- setDelegate:@
setDelegate :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> RawId -> IO ()
setDelegate rpScreenRecorder value =
  sendMessage rpScreenRecorder setDelegateSelector value

-- | @- available@
available :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
available rpScreenRecorder =
  sendMessage rpScreenRecorder availableSelector

-- | @- recording@
recording :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
recording rpScreenRecorder =
  sendMessage rpScreenRecorder recordingSelector

-- | @- microphoneEnabled@
microphoneEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
microphoneEnabled rpScreenRecorder =
  sendMessage rpScreenRecorder microphoneEnabledSelector

-- | @- setMicrophoneEnabled:@
setMicrophoneEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Bool -> IO ()
setMicrophoneEnabled rpScreenRecorder value =
  sendMessage rpScreenRecorder setMicrophoneEnabledSelector value

-- | @- cameraEnabled@
cameraEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
cameraEnabled rpScreenRecorder =
  sendMessage rpScreenRecorder cameraEnabledSelector

-- | @- setCameraEnabled:@
setCameraEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Bool -> IO ()
setCameraEnabled rpScreenRecorder value =
  sendMessage rpScreenRecorder setCameraEnabledSelector value

-- | @- cameraPosition@
cameraPosition :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO RPCameraPosition
cameraPosition rpScreenRecorder =
  sendMessage rpScreenRecorder cameraPositionSelector

-- | @- setCameraPosition:@
setCameraPosition :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> RPCameraPosition -> IO ()
setCameraPosition rpScreenRecorder value =
  sendMessage rpScreenRecorder setCameraPositionSelector value

-- | @- cameraPreviewView@
cameraPreviewView :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO (Id NSView)
cameraPreviewView rpScreenRecorder =
  sendMessage rpScreenRecorder cameraPreviewViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedRecorder@
sharedRecorderSelector :: Selector '[] (Id RPScreenRecorder)
sharedRecorderSelector = mkSelector "sharedRecorder"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id RPScreenRecorder)
initSelector = mkSelector "init"

-- | @Selector@ for @startRecordingWithMicrophoneEnabled:handler:@
startRecordingWithMicrophoneEnabled_handlerSelector :: Selector '[Bool, Ptr ()] ()
startRecordingWithMicrophoneEnabled_handlerSelector = mkSelector "startRecordingWithMicrophoneEnabled:handler:"

-- | @Selector@ for @startRecordingWithHandler:@
startRecordingWithHandlerSelector :: Selector '[Ptr ()] ()
startRecordingWithHandlerSelector = mkSelector "startRecordingWithHandler:"

-- | @Selector@ for @stopRecordingWithHandler:@
stopRecordingWithHandlerSelector :: Selector '[Ptr ()] ()
stopRecordingWithHandlerSelector = mkSelector "stopRecordingWithHandler:"

-- | @Selector@ for @stopRecordingWithOutputURL:completionHandler:@
stopRecordingWithOutputURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
stopRecordingWithOutputURL_completionHandlerSelector = mkSelector "stopRecordingWithOutputURL:completionHandler:"

-- | @Selector@ for @discardRecordingWithHandler:@
discardRecordingWithHandlerSelector :: Selector '[Ptr ()] ()
discardRecordingWithHandlerSelector = mkSelector "discardRecordingWithHandler:"

-- | @Selector@ for @startCaptureWithHandler:completionHandler:@
startCaptureWithHandler_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
startCaptureWithHandler_completionHandlerSelector = mkSelector "startCaptureWithHandler:completionHandler:"

-- | @Selector@ for @stopCaptureWithHandler:@
stopCaptureWithHandlerSelector :: Selector '[Ptr ()] ()
stopCaptureWithHandlerSelector = mkSelector "stopCaptureWithHandler:"

-- | @Selector@ for @startClipBufferingWithCompletionHandler:@
startClipBufferingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startClipBufferingWithCompletionHandlerSelector = mkSelector "startClipBufferingWithCompletionHandler:"

-- | @Selector@ for @stopClipBufferingWithCompletionHandler:@
stopClipBufferingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
stopClipBufferingWithCompletionHandlerSelector = mkSelector "stopClipBufferingWithCompletionHandler:"

-- | @Selector@ for @exportClipToURL:duration:completionHandler:@
exportClipToURL_duration_completionHandlerSelector :: Selector '[Id NSURL, CDouble, Ptr ()] ()
exportClipToURL_duration_completionHandlerSelector = mkSelector "exportClipToURL:duration:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @available@
availableSelector :: Selector '[] Bool
availableSelector = mkSelector "available"

-- | @Selector@ for @recording@
recordingSelector :: Selector '[] Bool
recordingSelector = mkSelector "recording"

-- | @Selector@ for @microphoneEnabled@
microphoneEnabledSelector :: Selector '[] Bool
microphoneEnabledSelector = mkSelector "microphoneEnabled"

-- | @Selector@ for @setMicrophoneEnabled:@
setMicrophoneEnabledSelector :: Selector '[Bool] ()
setMicrophoneEnabledSelector = mkSelector "setMicrophoneEnabled:"

-- | @Selector@ for @cameraEnabled@
cameraEnabledSelector :: Selector '[] Bool
cameraEnabledSelector = mkSelector "cameraEnabled"

-- | @Selector@ for @setCameraEnabled:@
setCameraEnabledSelector :: Selector '[Bool] ()
setCameraEnabledSelector = mkSelector "setCameraEnabled:"

-- | @Selector@ for @cameraPosition@
cameraPositionSelector :: Selector '[] RPCameraPosition
cameraPositionSelector = mkSelector "cameraPosition"

-- | @Selector@ for @setCameraPosition:@
setCameraPositionSelector :: Selector '[RPCameraPosition] ()
setCameraPositionSelector = mkSelector "setCameraPosition:"

-- | @Selector@ for @cameraPreviewView@
cameraPreviewViewSelector :: Selector '[] (Id NSView)
cameraPreviewViewSelector = mkSelector "cameraPreviewView"

