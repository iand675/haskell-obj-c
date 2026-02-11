{-# LANGUAGE PatternSynonyms #-}
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
  , sharedRecorderSelector
  , initSelector
  , startRecordingWithMicrophoneEnabled_handlerSelector
  , startRecordingWithHandlerSelector
  , stopRecordingWithHandlerSelector
  , stopRecordingWithOutputURL_completionHandlerSelector
  , discardRecordingWithHandlerSelector
  , startCaptureWithHandler_completionHandlerSelector
  , stopCaptureWithHandlerSelector
  , startClipBufferingWithCompletionHandlerSelector
  , stopClipBufferingWithCompletionHandlerSelector
  , exportClipToURL_duration_completionHandlerSelector
  , delegateSelector
  , setDelegateSelector
  , availableSelector
  , recordingSelector
  , microphoneEnabledSelector
  , setMicrophoneEnabledSelector
  , cameraEnabledSelector
  , setCameraEnabledSelector
  , cameraPositionSelector
  , setCameraPositionSelector
  , cameraPreviewViewSelector

  -- * Enum types
  , RPCameraPosition(RPCameraPosition)
  , pattern RPCameraPositionFront
  , pattern RPCameraPositionBack

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

import ObjC.ReplayKit.Internal.Classes
import ObjC.ReplayKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedRecorder@
sharedRecorder :: IO (Id RPScreenRecorder)
sharedRecorder  =
  do
    cls' <- getRequiredClass "RPScreenRecorder"
    sendClassMsg cls' (mkSelector "sharedRecorder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO (Id RPScreenRecorder)
init_ rpScreenRecorder  =
    sendMsg rpScreenRecorder (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
startRecordingWithMicrophoneEnabled_handler rpScreenRecorder  microphoneEnabled handler =
    sendMsg rpScreenRecorder (mkSelector "startRecordingWithMicrophoneEnabled:handler:") retVoid [argCULong (if microphoneEnabled then 1 else 0), argPtr (castPtr handler :: Ptr ())]

-- | Starts app recording with a completion handler. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called after user interactions are complete. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting the recording.
--
-- ObjC selector: @- startRecordingWithHandler:@
startRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
startRecordingWithHandler rpScreenRecorder  handler =
    sendMsg rpScreenRecorder (mkSelector "startRecordingWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Stops app recording with a completion handler.
--
-- handler Called when the movie is ready. Will return an instance of RPPreviewViewController on success which should be presented using [UIViewController presentViewController:animated:completion:]. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the recording.
--
-- ObjC selector: @- stopRecordingWithHandler:@
stopRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopRecordingWithHandler rpScreenRecorder  handler =
    sendMsg rpScreenRecorder (mkSelector "stopRecordingWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Stops app recording with output URL and completion handler.
--
-- @url@ — Output URL for app recording movie.
--
-- handler Called when  movie is written to specified output URL. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the recording and writing the output URL.
--
-- ObjC selector: @- stopRecordingWithOutputURL:completionHandler:@
stopRecordingWithOutputURL_completionHandler :: (IsRPScreenRecorder rpScreenRecorder, IsNSURL url) => rpScreenRecorder -> url -> Ptr () -> IO ()
stopRecordingWithOutputURL_completionHandler rpScreenRecorder  url completionHandler =
  withObjCPtr url $ \raw_url ->
      sendMsg rpScreenRecorder (mkSelector "stopRecordingWithOutputURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Discards the current recording. This can only be called after the handler block in stopRecordingWithHandler: is executed.
--
-- ObjC selector: @- discardRecordingWithHandler:@
discardRecordingWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
discardRecordingWithHandler rpScreenRecorder  handler =
    sendMsg rpScreenRecorder (mkSelector "discardRecordingWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Starts screen and audio capture and continually calls the supplied handler with the current sampleBuffer and bufferType and passed it back to the application. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called continually with sampleBuffers and the bufferType. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting the capture.
--
-- ObjC selector: @- startCaptureWithHandler:completionHandler:@
startCaptureWithHandler_completionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> Ptr () -> IO ()
startCaptureWithHandler_completionHandler rpScreenRecorder  captureHandler completionHandler =
    sendMsg rpScreenRecorder (mkSelector "startCaptureWithHandler:completionHandler:") retVoid [argPtr (castPtr captureHandler :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Stops screen capture with a completion handler
--
-- handler Called after the screen capture has stopped. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping the capture
--
-- ObjC selector: @- stopCaptureWithHandler:@
stopCaptureWithHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopCaptureWithHandler rpScreenRecorder  handler =
    sendMsg rpScreenRecorder (mkSelector "stopCaptureWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Start clip recording buffering with a completion handler. Note that before recording actually starts, the user may be prompted with UI to confirm recording.
--
-- handler Called after clip recording is started. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue starting clip record buffering.
--
-- ObjC selector: @- startClipBufferingWithCompletionHandler:@
startClipBufferingWithCompletionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
startClipBufferingWithCompletionHandler rpScreenRecorder  completionHandler =
    sendMsg rpScreenRecorder (mkSelector "startClipBufferingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Stop clip recording buffering with a completion handler.
--
-- handler Called after clip recording session is stopped. Will be passed an optional NSError in the RPRecordingErrorDomain domain if there was an issue stopping clip record buffering.
--
-- ObjC selector: @- stopClipBufferingWithCompletionHandler:@
stopClipBufferingWithCompletionHandler :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Ptr () -> IO ()
stopClipBufferingWithCompletionHandler rpScreenRecorder  completionHandler =
    sendMsg rpScreenRecorder (mkSelector "stopClipBufferingWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

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
exportClipToURL_duration_completionHandler rpScreenRecorder  url duration completionHandler =
  withObjCPtr url $ \raw_url ->
      sendMsg rpScreenRecorder (mkSelector "exportClipToURL:duration:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCDouble duration, argPtr (castPtr completionHandler :: Ptr ())]

-- | @- delegate@
delegate :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO RawId
delegate rpScreenRecorder  =
    fmap (RawId . castPtr) $ sendMsg rpScreenRecorder (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> RawId -> IO ()
setDelegate rpScreenRecorder  value =
    sendMsg rpScreenRecorder (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- available@
available :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
available rpScreenRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpScreenRecorder (mkSelector "available") retCULong []

-- | @- recording@
recording :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
recording rpScreenRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpScreenRecorder (mkSelector "recording") retCULong []

-- | @- microphoneEnabled@
microphoneEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
microphoneEnabled rpScreenRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpScreenRecorder (mkSelector "microphoneEnabled") retCULong []

-- | @- setMicrophoneEnabled:@
setMicrophoneEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Bool -> IO ()
setMicrophoneEnabled rpScreenRecorder  value =
    sendMsg rpScreenRecorder (mkSelector "setMicrophoneEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cameraEnabled@
cameraEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO Bool
cameraEnabled rpScreenRecorder  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg rpScreenRecorder (mkSelector "cameraEnabled") retCULong []

-- | @- setCameraEnabled:@
setCameraEnabled :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> Bool -> IO ()
setCameraEnabled rpScreenRecorder  value =
    sendMsg rpScreenRecorder (mkSelector "setCameraEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cameraPosition@
cameraPosition :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO RPCameraPosition
cameraPosition rpScreenRecorder  =
    fmap (coerce :: CLong -> RPCameraPosition) $ sendMsg rpScreenRecorder (mkSelector "cameraPosition") retCLong []

-- | @- setCameraPosition:@
setCameraPosition :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> RPCameraPosition -> IO ()
setCameraPosition rpScreenRecorder  value =
    sendMsg rpScreenRecorder (mkSelector "setCameraPosition:") retVoid [argCLong (coerce value)]

-- | @- cameraPreviewView@
cameraPreviewView :: IsRPScreenRecorder rpScreenRecorder => rpScreenRecorder -> IO (Id NSView)
cameraPreviewView rpScreenRecorder  =
    sendMsg rpScreenRecorder (mkSelector "cameraPreviewView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedRecorder@
sharedRecorderSelector :: Selector
sharedRecorderSelector = mkSelector "sharedRecorder"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @startRecordingWithMicrophoneEnabled:handler:@
startRecordingWithMicrophoneEnabled_handlerSelector :: Selector
startRecordingWithMicrophoneEnabled_handlerSelector = mkSelector "startRecordingWithMicrophoneEnabled:handler:"

-- | @Selector@ for @startRecordingWithHandler:@
startRecordingWithHandlerSelector :: Selector
startRecordingWithHandlerSelector = mkSelector "startRecordingWithHandler:"

-- | @Selector@ for @stopRecordingWithHandler:@
stopRecordingWithHandlerSelector :: Selector
stopRecordingWithHandlerSelector = mkSelector "stopRecordingWithHandler:"

-- | @Selector@ for @stopRecordingWithOutputURL:completionHandler:@
stopRecordingWithOutputURL_completionHandlerSelector :: Selector
stopRecordingWithOutputURL_completionHandlerSelector = mkSelector "stopRecordingWithOutputURL:completionHandler:"

-- | @Selector@ for @discardRecordingWithHandler:@
discardRecordingWithHandlerSelector :: Selector
discardRecordingWithHandlerSelector = mkSelector "discardRecordingWithHandler:"

-- | @Selector@ for @startCaptureWithHandler:completionHandler:@
startCaptureWithHandler_completionHandlerSelector :: Selector
startCaptureWithHandler_completionHandlerSelector = mkSelector "startCaptureWithHandler:completionHandler:"

-- | @Selector@ for @stopCaptureWithHandler:@
stopCaptureWithHandlerSelector :: Selector
stopCaptureWithHandlerSelector = mkSelector "stopCaptureWithHandler:"

-- | @Selector@ for @startClipBufferingWithCompletionHandler:@
startClipBufferingWithCompletionHandlerSelector :: Selector
startClipBufferingWithCompletionHandlerSelector = mkSelector "startClipBufferingWithCompletionHandler:"

-- | @Selector@ for @stopClipBufferingWithCompletionHandler:@
stopClipBufferingWithCompletionHandlerSelector :: Selector
stopClipBufferingWithCompletionHandlerSelector = mkSelector "stopClipBufferingWithCompletionHandler:"

-- | @Selector@ for @exportClipToURL:duration:completionHandler:@
exportClipToURL_duration_completionHandlerSelector :: Selector
exportClipToURL_duration_completionHandlerSelector = mkSelector "exportClipToURL:duration:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

-- | @Selector@ for @recording@
recordingSelector :: Selector
recordingSelector = mkSelector "recording"

-- | @Selector@ for @microphoneEnabled@
microphoneEnabledSelector :: Selector
microphoneEnabledSelector = mkSelector "microphoneEnabled"

-- | @Selector@ for @setMicrophoneEnabled:@
setMicrophoneEnabledSelector :: Selector
setMicrophoneEnabledSelector = mkSelector "setMicrophoneEnabled:"

-- | @Selector@ for @cameraEnabled@
cameraEnabledSelector :: Selector
cameraEnabledSelector = mkSelector "cameraEnabled"

-- | @Selector@ for @setCameraEnabled:@
setCameraEnabledSelector :: Selector
setCameraEnabledSelector = mkSelector "setCameraEnabled:"

-- | @Selector@ for @cameraPosition@
cameraPositionSelector :: Selector
cameraPositionSelector = mkSelector "cameraPosition"

-- | @Selector@ for @setCameraPosition:@
setCameraPositionSelector :: Selector
setCameraPositionSelector = mkSelector "setCameraPosition:"

-- | @Selector@ for @cameraPreviewView@
cameraPreviewViewSelector :: Selector
cameraPreviewViewSelector = mkSelector "cameraPreviewView"

