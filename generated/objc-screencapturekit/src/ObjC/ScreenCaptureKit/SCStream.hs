{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCStream@.
module ObjC.ScreenCaptureKit.SCStream
  ( SCStream
  , IsSCStream(..)
  , init_
  , new
  , initWithFilter_configuration_delegate
  , addStreamOutput_type_sampleHandlerQueue_error
  , removeStreamOutput_type_error
  , updateContentFilter_completionHandler
  , updateConfiguration_completionHandler
  , startCaptureWithCompletionHandler
  , stopCaptureWithCompletionHandler
  , addRecordingOutput_error
  , removeRecordingOutput_error
  , synchronizationClock
  , initSelector
  , newSelector
  , initWithFilter_configuration_delegateSelector
  , addStreamOutput_type_sampleHandlerQueue_errorSelector
  , removeStreamOutput_type_errorSelector
  , updateContentFilter_completionHandlerSelector
  , updateConfiguration_completionHandlerSelector
  , startCaptureWithCompletionHandlerSelector
  , stopCaptureWithCompletionHandlerSelector
  , addRecordingOutput_errorSelector
  , removeRecordingOutput_errorSelector
  , synchronizationClockSelector

  -- * Enum types
  , SCStreamOutputType(SCStreamOutputType)
  , pattern SCStreamOutputTypeScreen
  , pattern SCStreamOutputTypeAudio
  , pattern SCStreamOutputTypeMicrophone

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

import ObjC.ScreenCaptureKit.Internal.Classes
import ObjC.ScreenCaptureKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCStream scStream => scStream -> IO (Id SCStream)
init_ scStream  =
  sendMsg scStream (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SCStream)
new  =
  do
    cls' <- getRequiredClass "SCStream"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithFilter:configuration:delegate:
--
-- @contentFilter@ — the requested content filter to be captured
--
-- @streamConfig@ — the requested stream configuration to be applied to the SCStream
--
-- @delegate@ — the SCStream delegate object
--
-- this method create a SCStream object that has the particular output settings for the content stream
--
-- ObjC selector: @- initWithFilter:configuration:delegate:@
initWithFilter_configuration_delegate :: (IsSCStream scStream, IsSCContentFilter contentFilter, IsSCStreamConfiguration streamConfig) => scStream -> contentFilter -> streamConfig -> RawId -> IO (Id SCStream)
initWithFilter_configuration_delegate scStream  contentFilter streamConfig delegate =
withObjCPtr contentFilter $ \raw_contentFilter ->
  withObjCPtr streamConfig $ \raw_streamConfig ->
      sendMsg scStream (mkSelector "initWithFilter:configuration:delegate:") (retPtr retVoid) [argPtr (castPtr raw_contentFilter :: Ptr ()), argPtr (castPtr raw_streamConfig :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | addStreamOutput:type:sampleHandlerQueue:error:
--
-- @output@ — an object that adheres to the SCStreamOutput protocol that will receive the frames and call its delegate frame call back on its sample handler queue
--
-- @type@ — the SCStreamOutput type
--
-- @sampleHandlerQueue@ — the return queue for the sample handler
--
-- @error@ — the error pertaining to the add stream output
--
-- An SCStreamOutput protocol object instance can only be added to a session using -addStreamOutput: Returns a BOOL denoting if the add was successful
--
-- ObjC selector: @- addStreamOutput:type:sampleHandlerQueue:error:@
addStreamOutput_type_sampleHandlerQueue_error :: (IsSCStream scStream, IsNSObject sampleHandlerQueue, IsNSError error_) => scStream -> RawId -> SCStreamOutputType -> sampleHandlerQueue -> error_ -> IO Bool
addStreamOutput_type_sampleHandlerQueue_error scStream  output type_ sampleHandlerQueue error_ =
withObjCPtr sampleHandlerQueue $ \raw_sampleHandlerQueue ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStream (mkSelector "addStreamOutput:type:sampleHandlerQueue:error:") retCULong [argPtr (castPtr (unRawId output) :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_sampleHandlerQueue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeStreamOutput:type:error:
--
-- @output@ — an object that adheres to the SCStreamOutput protocol that will received the frames and call its delegate frame call back on its sample handler queue
--
-- @type@ — the SCStreamOutput type
--
-- @error@ — the error pertaining to the removing stream output
--
-- An SCStreamOutput protocol object instance can only be removed to a session using -addStreamOutput: Returns a BOOL denoting if the remove was successful
--
-- ObjC selector: @- removeStreamOutput:type:error:@
removeStreamOutput_type_error :: (IsSCStream scStream, IsNSError error_) => scStream -> RawId -> SCStreamOutputType -> error_ -> IO Bool
removeStreamOutput_type_error scStream  output type_ error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStream (mkSelector "removeStreamOutput:type:error:") retCULong [argPtr (castPtr (unRawId output) :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_error_ :: Ptr ())]

-- | updateContentFilter:completionHandler:
--
-- @contentFilter@ — the requested content filter to be updated
--
-- @completionHandler@ — the handler to be called when the function completes
--
-- this method will update the content filter for a content stream. A completion handler will be called when the update is complete with an error denoting if the update has failed.
--
-- ObjC selector: @- updateContentFilter:completionHandler:@
updateContentFilter_completionHandler :: (IsSCStream scStream, IsSCContentFilter contentFilter) => scStream -> contentFilter -> Ptr () -> IO ()
updateContentFilter_completionHandler scStream  contentFilter completionHandler =
withObjCPtr contentFilter $ \raw_contentFilter ->
    sendMsg scStream (mkSelector "updateContentFilter:completionHandler:") retVoid [argPtr (castPtr raw_contentFilter :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | updateConfiguration:completionHandler:
--
-- @streamConfig@ — the requested content filter to be updated
--
-- @completionHandler@ — the handler to be called when the function completes
--
-- this method will update the stream configuration for a content stream. A completion handler will be called when the update is complete with an error denoting if the update has failed.
--
-- ObjC selector: @- updateConfiguration:completionHandler:@
updateConfiguration_completionHandler :: (IsSCStream scStream, IsSCStreamConfiguration streamConfig) => scStream -> streamConfig -> Ptr () -> IO ()
updateConfiguration_completionHandler scStream  streamConfig completionHandler =
withObjCPtr streamConfig $ \raw_streamConfig ->
    sendMsg scStream (mkSelector "updateConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_streamConfig :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | startCaptureWithCompletionHandler:
--
-- @completionHandler@ — the handler to be called when the function completes
--
-- this method starts the content stream. The handler will be called when the content stream start has completed with an error denoting if the start has failed.
--
-- ObjC selector: @- startCaptureWithCompletionHandler:@
startCaptureWithCompletionHandler :: IsSCStream scStream => scStream -> Ptr () -> IO ()
startCaptureWithCompletionHandler scStream  completionHandler =
  sendMsg scStream (mkSelector "startCaptureWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | stopCaptureWithCompletionHandler:
--
-- @completionHandler@ — the handler to be called when the function completes
--
-- this method stops the content stream. The handler will be called when the content stream stop has completed with an error denoting if the stop has failed.
--
-- ObjC selector: @- stopCaptureWithCompletionHandler:@
stopCaptureWithCompletionHandler :: IsSCStream scStream => scStream -> Ptr () -> IO ()
stopCaptureWithCompletionHandler scStream  completionHandler =
  sendMsg scStream (mkSelector "stopCaptureWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | addRecordingOutput
--
-- Add a SCRecordingOutput to the SCStream. Starts Recording if stream is already capturing, otherwise recording will be started after capture starts. Recording will be written into a file url specified in SCRecordingOutput. Media(Screen/Audio/Microphone) to be recorded will be based on the SCStream configuration.
--
-- @recordingOutput@ — an SCRecordingOutput that including configuration of recording, and delegate for recording event.
--
-- @error@ — the error pertaining to the add recording output
--
-- Returns a BOOL denoting if the add was successful. Currently only support one recordingOutput on a stream. To guarantee the first sample captured in the stream to be written into the recording file, client need to add recordingOutput before startCapture. Delegate for recordingDidStart will be notified in SCRecordingOutput or recordingDidFinishWithError will be notified with an error associated if recording failed to start.
--
-- ObjC selector: @- addRecordingOutput:error:@
addRecordingOutput_error :: (IsSCStream scStream, IsSCRecordingOutput recordingOutput, IsNSError error_) => scStream -> recordingOutput -> error_ -> IO Bool
addRecordingOutput_error scStream  recordingOutput error_ =
withObjCPtr recordingOutput $ \raw_recordingOutput ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStream (mkSelector "addRecordingOutput:error:") retCULong [argPtr (castPtr raw_recordingOutput :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | removeRecordingOutput
--
-- Remove SCRecordingOutput from the SCStream. Stops Recording if the stream is currently recording.
--
-- @recordingOutput@ — an SCRecordingOutput that including configuration of recording, and delegate for recording event.
--
-- @error@ — the error pertaining to the remove recording output
--
-- Returns a BOOL denoting if the remove was successful. Delegate for recordingDidFinishWithError will be notified in SCRecordingOutput, associate with an error code if recording failed to finish written to the file. If stopCapture is called without removing recordingOutput, recording will be stopped and finish writting into the file. In case client update the stream configuration during recording, recording will be stopped as well.
--
-- ObjC selector: @- removeRecordingOutput:error:@
removeRecordingOutput_error :: (IsSCStream scStream, IsSCRecordingOutput recordingOutput, IsNSError error_) => scStream -> recordingOutput -> error_ -> IO Bool
removeRecordingOutput_error scStream  recordingOutput error_ =
withObjCPtr recordingOutput $ \raw_recordingOutput ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg scStream (mkSelector "removeRecordingOutput:error:") retCULong [argPtr (castPtr raw_recordingOutput :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Synchronization clock used for media capture.
--
-- ObjC selector: @- synchronizationClock@
synchronizationClock :: IsSCStream scStream => scStream -> IO (Ptr ())
synchronizationClock scStream  =
  fmap castPtr $ sendMsg scStream (mkSelector "synchronizationClock") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithFilter:configuration:delegate:@
initWithFilter_configuration_delegateSelector :: Selector
initWithFilter_configuration_delegateSelector = mkSelector "initWithFilter:configuration:delegate:"

-- | @Selector@ for @addStreamOutput:type:sampleHandlerQueue:error:@
addStreamOutput_type_sampleHandlerQueue_errorSelector :: Selector
addStreamOutput_type_sampleHandlerQueue_errorSelector = mkSelector "addStreamOutput:type:sampleHandlerQueue:error:"

-- | @Selector@ for @removeStreamOutput:type:error:@
removeStreamOutput_type_errorSelector :: Selector
removeStreamOutput_type_errorSelector = mkSelector "removeStreamOutput:type:error:"

-- | @Selector@ for @updateContentFilter:completionHandler:@
updateContentFilter_completionHandlerSelector :: Selector
updateContentFilter_completionHandlerSelector = mkSelector "updateContentFilter:completionHandler:"

-- | @Selector@ for @updateConfiguration:completionHandler:@
updateConfiguration_completionHandlerSelector :: Selector
updateConfiguration_completionHandlerSelector = mkSelector "updateConfiguration:completionHandler:"

-- | @Selector@ for @startCaptureWithCompletionHandler:@
startCaptureWithCompletionHandlerSelector :: Selector
startCaptureWithCompletionHandlerSelector = mkSelector "startCaptureWithCompletionHandler:"

-- | @Selector@ for @stopCaptureWithCompletionHandler:@
stopCaptureWithCompletionHandlerSelector :: Selector
stopCaptureWithCompletionHandlerSelector = mkSelector "stopCaptureWithCompletionHandler:"

-- | @Selector@ for @addRecordingOutput:error:@
addRecordingOutput_errorSelector :: Selector
addRecordingOutput_errorSelector = mkSelector "addRecordingOutput:error:"

-- | @Selector@ for @removeRecordingOutput:error:@
removeRecordingOutput_errorSelector :: Selector
removeRecordingOutput_errorSelector = mkSelector "removeRecordingOutput:error:"

-- | @Selector@ for @synchronizationClock@
synchronizationClockSelector :: Selector
synchronizationClockSelector = mkSelector "synchronizationClock"

