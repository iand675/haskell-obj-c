{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSession
--
-- AVCaptureSession is the central hub of the AVFoundation capture classes.
--
-- To perform a real-time capture, a client may instantiate AVCaptureSession and add appropriate AVCaptureInputs, such as AVCaptureDeviceInput, and outputs, such as AVCaptureMovieFileOutput. [AVCaptureSession startRunning] starts the flow of data from the inputs to the outputs, and [AVCaptureSession stopRunning] stops the flow. A client may set the sessionPreset property to customize the quality level or bitrate of the output.
--
-- Generated bindings for @AVCaptureSession@.
module ObjC.AVFoundation.AVCaptureSession
  ( AVCaptureSession
  , IsAVCaptureSession(..)
  , canSetSessionPreset
  , canAddInput
  , addInput
  , removeInput
  , canAddOutput
  , addOutput
  , removeOutput
  , addInputWithNoConnections
  , addOutputWithNoConnections
  , canAddConnection
  , addConnection
  , removeConnection
  , setControlsDelegate_queue
  , canAddControl
  , addControl
  , removeControl
  , beginConfiguration
  , commitConfiguration
  , startRunning
  , stopRunning
  , runDeferredStartWhenNeeded
  , setDeferredStartDelegate_deferredStartDelegateCallbackQueue
  , sessionPreset
  , setSessionPreset
  , inputs
  , outputs
  , supportsControls
  , maxControlsCount
  , controlsDelegateCallbackQueue
  , running
  , interrupted
  , multitaskingCameraAccessSupported
  , multitaskingCameraAccessEnabled
  , setMultitaskingCameraAccessEnabled
  , usesApplicationAudioSession
  , setUsesApplicationAudioSession
  , automaticallyConfiguresApplicationAudioSession
  , setAutomaticallyConfiguresApplicationAudioSession
  , configuresApplicationAudioSessionToMixWithOthers
  , setConfiguresApplicationAudioSessionToMixWithOthers
  , configuresApplicationAudioSessionForBluetoothHighQualityRecording
  , setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording
  , automaticallyConfiguresCaptureDeviceForWideColor
  , setAutomaticallyConfiguresCaptureDeviceForWideColor
  , synchronizationClock
  , masterClock
  , hardwareCost
  , manualDeferredStartSupported
  , automaticallyRunsDeferredStart
  , setAutomaticallyRunsDeferredStart
  , deferredStartDelegateCallbackQueue
  , canSetSessionPresetSelector
  , canAddInputSelector
  , addInputSelector
  , removeInputSelector
  , canAddOutputSelector
  , addOutputSelector
  , removeOutputSelector
  , addInputWithNoConnectionsSelector
  , addOutputWithNoConnectionsSelector
  , canAddConnectionSelector
  , addConnectionSelector
  , removeConnectionSelector
  , setControlsDelegate_queueSelector
  , canAddControlSelector
  , addControlSelector
  , removeControlSelector
  , beginConfigurationSelector
  , commitConfigurationSelector
  , startRunningSelector
  , stopRunningSelector
  , runDeferredStartWhenNeededSelector
  , setDeferredStartDelegate_deferredStartDelegateCallbackQueueSelector
  , sessionPresetSelector
  , setSessionPresetSelector
  , inputsSelector
  , outputsSelector
  , supportsControlsSelector
  , maxControlsCountSelector
  , controlsDelegateCallbackQueueSelector
  , runningSelector
  , interruptedSelector
  , multitaskingCameraAccessSupportedSelector
  , multitaskingCameraAccessEnabledSelector
  , setMultitaskingCameraAccessEnabledSelector
  , usesApplicationAudioSessionSelector
  , setUsesApplicationAudioSessionSelector
  , automaticallyConfiguresApplicationAudioSessionSelector
  , setAutomaticallyConfiguresApplicationAudioSessionSelector
  , configuresApplicationAudioSessionToMixWithOthersSelector
  , setConfiguresApplicationAudioSessionToMixWithOthersSelector
  , configuresApplicationAudioSessionForBluetoothHighQualityRecordingSelector
  , setConfiguresApplicationAudioSessionForBluetoothHighQualityRecordingSelector
  , automaticallyConfiguresCaptureDeviceForWideColorSelector
  , setAutomaticallyConfiguresCaptureDeviceForWideColorSelector
  , synchronizationClockSelector
  , masterClockSelector
  , hardwareCostSelector
  , manualDeferredStartSupportedSelector
  , automaticallyRunsDeferredStartSelector
  , setAutomaticallyRunsDeferredStartSelector
  , deferredStartDelegateCallbackQueueSelector


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

-- | canSetSessionPreset:
--
-- Returns whether the receiver can be configured with the given preset.
--
-- @preset@ — An AVCaptureSession preset.
--
-- Returns: YES if the receiver can be set to the given preset, NO otherwise.
--
-- An AVCaptureSession instance can be associated with a preset that configures its inputs and outputs to fulfill common use cases. This method can be used to determine if the receiver supports the desired preset given its current input and output configuration. The receiver's sessionPreset property may only be set to a certain preset if this method returns YES for that preset.
--
-- ObjC selector: @- canSetSessionPreset:@
canSetSessionPreset :: (IsAVCaptureSession avCaptureSession, IsNSString preset) => avCaptureSession -> preset -> IO Bool
canSetSessionPreset avCaptureSession  preset =
withObjCPtr preset $ \raw_preset ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "canSetSessionPreset:") retCULong [argPtr (castPtr raw_preset :: Ptr ())]

-- | canAddInput:
--
-- Returns whether the proposed input can be added to the receiver.
--
-- @input@ — An AVCaptureInput instance.
--
-- Returns: YES if the proposed input can be added to the receiver, NO otherwise.
--
-- An AVCaptureInput instance can only be added to a session using -addInput: if -canAddInput: returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- ObjC selector: @- canAddInput:@
canAddInput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureInput input) => avCaptureSession -> input -> IO Bool
canAddInput avCaptureSession  input =
withObjCPtr input $ \raw_input ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "canAddInput:") retCULong [argPtr (castPtr raw_input :: Ptr ())]

-- | addInput:
--
-- Adds an AVCaptureInput to the session.
--
-- @input@ — An AVCaptureInput instance.
--
-- An AVCaptureInput instance can only be added to a session using -addInput: if -canAddInput: returns YES, otherwise an NSInvalidArgumentException is thrown. -addInput: may be called while the session is running.
--
-- ObjC selector: @- addInput:@
addInput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureInput input) => avCaptureSession -> input -> IO ()
addInput avCaptureSession  input =
withObjCPtr input $ \raw_input ->
    sendMsg avCaptureSession (mkSelector "addInput:") retVoid [argPtr (castPtr raw_input :: Ptr ())]

-- | removeInput:
--
-- Removes an AVCaptureInput from the session.
--
-- @input@ — An AVCaptureInput instance.
--
-- -removeInput: may be called while the session is running.
--
-- ObjC selector: @- removeInput:@
removeInput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureInput input) => avCaptureSession -> input -> IO ()
removeInput avCaptureSession  input =
withObjCPtr input $ \raw_input ->
    sendMsg avCaptureSession (mkSelector "removeInput:") retVoid [argPtr (castPtr raw_input :: Ptr ())]

-- | canAddOutput:
--
-- Returns whether the proposed output can be added to the receiver.
--
-- @output@ — An AVCaptureOutput instance.
--
-- Returns: YES if the proposed output can be added to the receiver, NO otherwise.
--
-- An AVCaptureOutput instance can only be added to a session using -addOutput: if -canAddOutput: returns YES, otherwise an NSInvalidArgumentException is thrown.
--
-- On iOS and Mac Catalyst, some limitations to adding combinations of different types of outputs apply:     - A maximum of one output of each type may be added. For applications linked on or after iOS 16.0, this restriction no longer applies to AVCaptureVideoDataOutputs. When adding more than one AVCaptureVideoDataOutput, AVCaptureSession.hardwareCost must be taken into account.     - A session cannot contain both an AVCaptureStillImageOutput and an AVCapturePhotoOutput at the same time.     - Prior to iOS 16.0, an AVCaptureVideoDataOutput and an AVCaptureMovieFileOutput may be added to the same session, but only one may have its connection active. When both have their connections enabled, the AVCaptureMovieFileOutput "wins" and the AVCaptureVideoDataOutput's connection becomes inactive. For applications linked on or after iOS 16.0, this restriction has been lifted. When adding multiple AVCaptureVideoDataOutputs or a combination of AVCaptureVideoDataOutputs and an AVCaptureMovieFileOutput, AVCaptureSession.hardwareCost must be taken into account.     - Similarly, prior to iOS 16.0, an AVCaptureAudioDataOutput and an AVCaptureMovieFileOutput may be added to the same session, but only one may have its connection active. When both have their connections enabled, the AVCaptureMovieFileOutput "wins" and the AVCaptureAudioDataOutput's connection becomes inactive. For applications linked on or after iOS 16.0, this restriction has been lifted.
--
-- ObjC selector: @- canAddOutput:@
canAddOutput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureOutput output) => avCaptureSession -> output -> IO Bool
canAddOutput avCaptureSession  output =
withObjCPtr output $ \raw_output ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "canAddOutput:") retCULong [argPtr (castPtr raw_output :: Ptr ())]

-- | addOutput:
--
-- Adds an AVCaptureOutput to the session.
--
-- @output@ — An AVCaptureOutput instance.
--
-- An AVCaptureOutput instance can only be added to a session using -addOutput: if -canAddOutput: returns YES, otherwise an NSInvalidArgumentException is thrown. -addOutput: may be called while the session is running.
--
-- ObjC selector: @- addOutput:@
addOutput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureOutput output) => avCaptureSession -> output -> IO ()
addOutput avCaptureSession  output =
withObjCPtr output $ \raw_output ->
    sendMsg avCaptureSession (mkSelector "addOutput:") retVoid [argPtr (castPtr raw_output :: Ptr ())]

-- | removeOutput:
--
-- Removes an AVCaptureOutput from the session.
--
-- @output@ — An AVCaptureOutput instance.
--
-- -removeOutput: may be called while the session is running.
--
-- ObjC selector: @- removeOutput:@
removeOutput :: (IsAVCaptureSession avCaptureSession, IsAVCaptureOutput output) => avCaptureSession -> output -> IO ()
removeOutput avCaptureSession  output =
withObjCPtr output $ \raw_output ->
    sendMsg avCaptureSession (mkSelector "removeOutput:") retVoid [argPtr (castPtr raw_output :: Ptr ())]

-- | addInputWithNoConnections:
--
-- Adds an AVCaptureInput to the session without forming any connections.
--
-- @input@ — An AVCaptureInput instance.
--
-- An AVCaptureInput instance can only be added to a session using -addInputWithNoConnections: if -canAddInput: returns YES, otherwise an NSInvalidArgumentException is thrown. -addInputWithNoConnections: may be called while the session is running. The -addInput: method is the preferred method for adding an input to an AVCaptureSession. -addInputWithNoConnections: may be called if you need fine-grained control over which inputs are connected to which outputs.
--
-- ObjC selector: @- addInputWithNoConnections:@
addInputWithNoConnections :: (IsAVCaptureSession avCaptureSession, IsAVCaptureInput input) => avCaptureSession -> input -> IO ()
addInputWithNoConnections avCaptureSession  input =
withObjCPtr input $ \raw_input ->
    sendMsg avCaptureSession (mkSelector "addInputWithNoConnections:") retVoid [argPtr (castPtr raw_input :: Ptr ())]

-- | addOutputWithNoConnections:
--
-- Adds an AVCaptureOutput to the session without forming any connections.
--
-- @output@ — An AVCaptureOutput instance.
--
-- An AVCaptureOutput instance can only be added to a session using -addOutputWithNoConnections: if -canAddOutput: returns YES, otherwise an NSInvalidArgumentException is thrown. -addOutputWithNoConnections: may be called while the session is running. The -addOutput: method is the preferred method for adding an output to an AVCaptureSession. -addOutputWithNoConnections: may be called if you need fine-grained control over which inputs are connected to which outputs.
--
-- ObjC selector: @- addOutputWithNoConnections:@
addOutputWithNoConnections :: (IsAVCaptureSession avCaptureSession, IsAVCaptureOutput output) => avCaptureSession -> output -> IO ()
addOutputWithNoConnections avCaptureSession  output =
withObjCPtr output $ \raw_output ->
    sendMsg avCaptureSession (mkSelector "addOutputWithNoConnections:") retVoid [argPtr (castPtr raw_output :: Ptr ())]

-- | canAddConnection:
--
-- Returns whether the proposed connection can be added to the receiver.
--
-- @connection@ — An AVCaptureConnection instance.
--
-- An AVCaptureConnection instance can only be added to a session using -addConnection: if -canAddConnection: returns YES, otherwise an NSInvalidArgumentException is thrown. When using -addInput: or -addOutput:, connections are formed automatically between all compatible inputs and outputs. Manually adding connections is only necessary when adding an input or output with no connections.
--
-- ObjC selector: @- canAddConnection:@
canAddConnection :: (IsAVCaptureSession avCaptureSession, IsAVCaptureConnection connection) => avCaptureSession -> connection -> IO Bool
canAddConnection avCaptureSession  connection =
withObjCPtr connection $ \raw_connection ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "canAddConnection:") retCULong [argPtr (castPtr raw_connection :: Ptr ())]

-- | addConnection:
--
-- Adds an AVCaptureConnection to the session.
--
-- @connection@ — An AVCaptureConnection instance.
--
-- An AVCaptureConnection instance can only be added to a session using -addConnection: if canAddConnection: returns YES, otherwise an NSInvalidArgumentException is thrown. When using -addInput: or -addOutput:, connections are formed automatically between all compatible inputs and outputs. Manually adding connections is only necessary when adding an input or output with no connections. -addConnection: may be called while the session is running.
--
-- ObjC selector: @- addConnection:@
addConnection :: (IsAVCaptureSession avCaptureSession, IsAVCaptureConnection connection) => avCaptureSession -> connection -> IO ()
addConnection avCaptureSession  connection =
withObjCPtr connection $ \raw_connection ->
    sendMsg avCaptureSession (mkSelector "addConnection:") retVoid [argPtr (castPtr raw_connection :: Ptr ())]

-- | removeConnection:
--
-- Removes an AVCaptureConnection from the session.
--
-- @connection@ — An AVCaptureConnection instance.
--
-- -removeConnection: may be called while the session is running.
--
-- ObjC selector: @- removeConnection:@
removeConnection :: (IsAVCaptureSession avCaptureSession, IsAVCaptureConnection connection) => avCaptureSession -> connection -> IO ()
removeConnection avCaptureSession  connection =
withObjCPtr connection $ \raw_connection ->
    sendMsg avCaptureSession (mkSelector "removeConnection:") retVoid [argPtr (castPtr raw_connection :: Ptr ())]

-- | setControlsDelegate:queue:
--
-- Sets the receiver's controls delegate that receives events about the session's controls and the dispatch queue on which the delegate is called.
--
-- @controlsDelegate@ — An object conforming to the @AVCaptureSessionControlsDelegate@ protocol that receives events about the session's controls.
--
-- @controlsDelegateCallbackQueue@ — A dispatch queue on which all delegate methods are called.
--
-- Users can interact with an @AVCaptureSession@'s controls by performing specific gestures to enable their visibility. A delegate may be specified to be informed when the controls can be interacted with and are dismissed. All delegate methods will be called on the specified dispatch queue.
--
-- A serial dispatch queue must be used to guarantee that delegate callbacks will be delivered in order. The @controlsDelegateCallbackQueue@ parameter may not be @NULL@, except when setting the @controlsDelegate@ to @nil@ otherwise @-setControlsDelegate:queue:@ throws an @NSInvalidArgumentException@.
--
-- ObjC selector: @- setControlsDelegate:queue:@
setControlsDelegate_queue :: (IsAVCaptureSession avCaptureSession, IsNSObject controlsDelegateCallbackQueue) => avCaptureSession -> RawId -> controlsDelegateCallbackQueue -> IO ()
setControlsDelegate_queue avCaptureSession  controlsDelegate controlsDelegateCallbackQueue =
withObjCPtr controlsDelegateCallbackQueue $ \raw_controlsDelegateCallbackQueue ->
    sendMsg avCaptureSession (mkSelector "setControlsDelegate:queue:") retVoid [argPtr (castPtr (unRawId controlsDelegate) :: Ptr ()), argPtr (castPtr raw_controlsDelegateCallbackQueue :: Ptr ())]

-- | canAddControl:
--
-- Returns whether the proposed control can be added to the session.
--
-- @control@ — An @AVCaptureControl@ instance.
--
-- Returns: @YES@ if the proposed control can be added to the session, @NO@ otherwise.
--
-- An @AVCaptureControl@ instance can only be added to a session using @-addControl:@ if @-canAddControl:@ returns @YES@. For example, some platforms do not support controls. Instances of @AVCaptureSlider@, @AVCaptureToggle@ and @AVCaptureIndexPicker@ must have an action and an action queue set before being added to a session.
--
-- ObjC selector: @- canAddControl:@
canAddControl :: (IsAVCaptureSession avCaptureSession, IsAVCaptureControl control) => avCaptureSession -> control -> IO Bool
canAddControl avCaptureSession  control =
withObjCPtr control $ \raw_control ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "canAddControl:") retCULong [argPtr (castPtr raw_control :: Ptr ())]

-- | addControl:
--
-- Adds an @AVCaptureControl@ instance to the session.
--
-- @control@ — An @AVCaptureControl@ instance.
--
-- An @AVCaptureControl@ instance can only be added to a session using @-addControl:@ if @-canAddControl:@ returns @YES@, otherwise an @NSInvalidArgumentException@ is thrown. @-addControl:@ may be called while the session is running.
--
-- For an @AVCaptureControl@ instance to become active, an @AVCaptureSessionControlsDelegate@ must be set on the session.
--
-- ObjC selector: @- addControl:@
addControl :: (IsAVCaptureSession avCaptureSession, IsAVCaptureControl control) => avCaptureSession -> control -> IO ()
addControl avCaptureSession  control =
withObjCPtr control $ \raw_control ->
    sendMsg avCaptureSession (mkSelector "addControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | removeControl:
--
-- Removes an @AVCaptureControl@ instance from the session.
--
-- @control@ — An @AVCaptureControl@ instance.
--
-- @-removeControl:@ may be called while the session is running.
--
-- ObjC selector: @- removeControl:@
removeControl :: (IsAVCaptureSession avCaptureSession, IsAVCaptureControl control) => avCaptureSession -> control -> IO ()
removeControl avCaptureSession  control =
withObjCPtr control $ \raw_control ->
    sendMsg avCaptureSession (mkSelector "removeControl:") retVoid [argPtr (castPtr raw_control :: Ptr ())]

-- | beginConfiguration
--
-- When paired with commitConfiguration, allows a client to batch multiple configuration operations on a running session into atomic updates.
--
-- -beginConfiguration / -commitConfiguration are AVCaptureSession's mechanism for batching multiple configuration operations on a running session into atomic updates. After calling [session beginConfiguration], clients may add or remove outputs, alter the sessionPreset, or configure individual AVCaptureInput or Output properties. All changes will be pended until the client calls [session commitConfiguration], at which time they will be applied together. -beginConfiguration / -commitConfiguration pairs may be nested, and will only be applied when the outermost commit is invoked. If you've called -beginConfiguration, you must call -commitConfiguration before invoking -startRunning or -stopRunning, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- beginConfiguration@
beginConfiguration :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO ()
beginConfiguration avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "beginConfiguration") retVoid []

-- | commitConfiguration
--
-- When preceded by beginConfiguration, allows a client to batch multiple configuration operations on a running session into atomic updates.
--
-- -beginConfiguration / -commitConfiguration are AVCaptureSession's mechanism for batching multiple configuration operations on a running session into atomic updates. After calling [session beginConfiguration], clients may add or remove outputs, alter the sessionPreset, or configure individual AVCaptureInput or Output properties. All changes will be pended until the client calls [session commitConfiguration], at which time they will be applied together. -beginConfiguration / -commitConfiguration pairs may be nested, and will only be applied when the outermost commit is invoked. If you've called -beginConfiguration, you must call -commitConfiguration before invoking -startRunning or -stopRunning, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- commitConfiguration@
commitConfiguration :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO ()
commitConfiguration avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "commitConfiguration") retVoid []

-- | startRunning
--
-- Starts an AVCaptureSession instance running.
--
-- Clients invoke -startRunning to start the flow of data from inputs to outputs connected to the AVCaptureSession instance. This call blocks until the session object has completely started up or failed. A failure to start running is reported through the AVCaptureSessionRuntimeErrorNotification mechanism. If you've called -beginConfiguration, you must call -commitConfiguration before invoking -startRunning, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- startRunning@
startRunning :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO ()
startRunning avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "startRunning") retVoid []

-- | stopRunning
--
-- Stops an AVCaptureSession instance that is currently running.
--
-- Clients invoke -stopRunning to stop the flow of data from inputs to outputs connected to the AVCaptureSession instance. This call blocks until the session object has completely stopped. -stopRunning may not be called while the session is being configured. If you've called -beginConfiguration, you must call -commitConfiguration before invoking -stopRunning, otherwise an NSGenericException is thrown.
--
-- ObjC selector: @- stopRunning@
stopRunning :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO ()
stopRunning avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "stopRunning") retVoid []

-- | Tells the session to run deferred start when appropriate.
--
-- For best perceived startup performance, call this after displaying the first frame, so that deferred start processing doesn't interfere with other initialization operations. For example, if using a <doc://com.apple.documentation/documentation/quartzcore/cametallayer> to draw camera frames, add a @presentHandler@ (using <doc://com.apple.documentation/metal/mtldrawable/addpresentedhandler>) to the first drawable and call ``runDeferredStartWhenNeeded`` from there.
--
-- If one or more outputs need to start to perform a capture operation, and ``runDeferredStartWhenNeeded`` has not run yet, the session runs the deferred start on your app's behalf. Only call this method once for each configuration commit - after the first call, subsequent calls to ``runDeferredStartWhenNeeded`` have no effect. The deferred start runs asynchronously, so this method returns immediately.
--
-- - Note: You can only call this when ``automaticallyRunsDeferredStart`` is @false@. Otherwise, the session throws an @NSInvalidArgumentException@.
--
-- - Important: To avoid blocking your app's UI, don't call this method from the application's main actor or queue.
--
-- ObjC selector: @- runDeferredStartWhenNeeded@
runDeferredStartWhenNeeded :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO ()
runDeferredStartWhenNeeded avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "runDeferredStartWhenNeeded") retVoid []

-- | Sets a delegate object for the session to call when performing deferred start.
--
-- This delegate receives a call to the ``AVCaptureSessionDeferredStartDelegate/sessionWillRunDeferredStart:`` method when deferred start is about to run. It is non-blocking, so by the time this method is called, the deferred start may already be underway. If you want your app to perform initialization (potentially) concurrently with deferred start (e.g. user-facing camera features that are not needed to display the first preview frame, but are available to the user as soon as possible) it may be done in the delegate's ``AVCaptureSessionDeferredStartDelegate/sessionWillRunDeferredStart:`` method. To wait until deferred start is finished to perform some remaining initialization work, use the ``AVCaptureSessionDeferredStartDelegate/sessionDidRunDeferredStart:`` method instead.
--
-- The delegate receives a call to the ``AVCaptureSessionDeferredStartDelegate/sessionDidRunDeferredStart:`` method when the deferred start finishes running. This allows you to run less-critical application initialization code. For example, if you've deferred an ``AVCapturePhotoOutput`` by setting its ``AVCaptureOutput/deferredStartEnabled`` property to @true@, and you'd like to do some app-specific initialization related to still capture, here might be a good place to put it.
--
-- If the delegate is non-nil, the session still calls the ``AVCaptureSessionDeferredStartDelegate/sessionWillRunDeferredStart:`` and ``AVCaptureSessionDeferredStartDelegate/sessionDidRunDeferredStart:`` methods regardless of the value of the session's ``automaticallyRunsDeferredStart`` property.
--
-- To minimize the capture session's startup latency, defer all unnecessary work until after the session starts. This delegate provides callbacks for you to schedule deferred work without impacting session startup performance.
--
-- To perform initialization prior to deferred start but after the user interface displays, set ``automaticallyRunsDeferredStart`` to @false@, and then run the custom initialization prior to calling ``runDeferredStartWhenNeeded``.
--
-- If ``deferredStartDelegate`` is not @NULL@, the session throws an exception if ``deferredStartDelegateCallbackQueue`` is @nil@.
--
-- - Parameter deferredStartDelegate: An object conforming to the ``AVCaptureSessionDeferredStartDelegate`` protocol that receives events about deferred start. - Parameter deferredStartDelegateCallbackQueue: A dispatch queue on which deferredStart delegate methods are called.
--
-- ObjC selector: @- setDeferredStartDelegate:deferredStartDelegateCallbackQueue:@
setDeferredStartDelegate_deferredStartDelegateCallbackQueue :: (IsAVCaptureSession avCaptureSession, IsNSObject deferredStartDelegateCallbackQueue) => avCaptureSession -> RawId -> deferredStartDelegateCallbackQueue -> IO ()
setDeferredStartDelegate_deferredStartDelegateCallbackQueue avCaptureSession  deferredStartDelegate deferredStartDelegateCallbackQueue =
withObjCPtr deferredStartDelegateCallbackQueue $ \raw_deferredStartDelegateCallbackQueue ->
    sendMsg avCaptureSession (mkSelector "setDeferredStartDelegate:deferredStartDelegateCallbackQueue:") retVoid [argPtr (castPtr (unRawId deferredStartDelegate) :: Ptr ()), argPtr (castPtr raw_deferredStartDelegateCallbackQueue :: Ptr ())]

-- | sessionPreset
--
-- Indicates the session preset currently in use by the receiver.
--
-- The value of this property is an AVCaptureSessionPreset indicating the current session preset in use by the receiver. The sessionPreset property may be set while the receiver is running.
--
-- ObjC selector: @- sessionPreset@
sessionPreset :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Id NSString)
sessionPreset avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "sessionPreset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sessionPreset
--
-- Indicates the session preset currently in use by the receiver.
--
-- The value of this property is an AVCaptureSessionPreset indicating the current session preset in use by the receiver. The sessionPreset property may be set while the receiver is running.
--
-- ObjC selector: @- setSessionPreset:@
setSessionPreset :: (IsAVCaptureSession avCaptureSession, IsNSString value) => avCaptureSession -> value -> IO ()
setSessionPreset avCaptureSession  value =
withObjCPtr value $ \raw_value ->
    sendMsg avCaptureSession (mkSelector "setSessionPreset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | inputs
--
-- An NSArray of AVCaptureInputs currently added to the receiver.
--
-- The value of this property is an NSArray of AVCaptureInputs currently added to the receiver. Clients can add AVCaptureInputs to a session by calling -addInput:.
--
-- ObjC selector: @- inputs@
inputs :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Id NSArray)
inputs avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputs
--
-- An NSArray of AVCaptureOutputs currently added to the receiver.
--
-- The value of this property is an NSArray of AVCaptureOutputs currently added to the receiver. Clients can add AVCaptureOutputs to a session by calling -addOutput:.
--
-- ObjC selector: @- outputs@
outputs :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Id NSArray)
outputs avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "outputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | supportsControls
--
-- Indicates whether session controls are supported on this platform.
--
-- @AVCaptureControl@s are only supported on platforms with necessary hardware.
--
-- ObjC selector: @- supportsControls@
supportsControls :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
supportsControls avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "supportsControls") retCULong []

-- | maxControlsCount
--
-- Specifies the maximum number of controls that can be added to a session.
--
-- ObjC selector: @- maxControlsCount@
maxControlsCount :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO CLong
maxControlsCount avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "maxControlsCount") retCLong []

-- | controlsDelegateCallbackQueue
--
-- The dispatch queue on which all controls delegate methods will be called.
--
-- The value of this property is a @dispatch_queue_t@. The queue is set using the @-setControlsDelegate:queue:@ method.
--
-- ObjC selector: @- controlsDelegateCallbackQueue@
controlsDelegateCallbackQueue :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Id NSObject)
controlsDelegateCallbackQueue avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "controlsDelegateCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | running
--
-- Indicates whether the session is currently running.
--
-- The value of this property is a BOOL indicating whether the receiver is running. Clients can key value observe the value of this property to be notified when the session automatically starts or stops running.
--
-- ObjC selector: @- running@
running :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
running avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "running") retCULong []

-- | interrupted
--
-- Indicates whether the session is being interrupted.
--
-- The value of this property is a BOOL indicating whether the receiver is currently being interrupted, such as by a phone call or alarm. Clients can key value observe the value of this property to be notified when the session ceases to be interrupted and again has access to needed hardware resources.
--
-- ObjC selector: @- interrupted@
interrupted :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
interrupted avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "interrupted") retCULong []

-- | multitaskingCameraAccessSupported
--
-- Returns whether the session can be configured to use the camera while multitasking.
--
-- This property can be used to determine whether multitaskingCameraAccessEnabled may be set to YES. When this property changes from YES to NO, multitaskingCameraAccessEnabled also reverts to NO.
--
-- Prior to iOS 18, this property returns YES on iPads that support Stage Manager with an extended display. In applications linked on or after iOS 18, this property returns YES for video conferencing applications (apps that use "voip" as one of their UIBackgroundModes).
--
-- This property also returns YES for iOS applications that have the com.apple.developer.avfoundation.multitasking-camera-access entitlement.
--
-- This property returns YES on Apple TV.
--
-- This property is key-value observable.
--
-- ObjC selector: @- multitaskingCameraAccessSupported@
multitaskingCameraAccessSupported :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
multitaskingCameraAccessSupported avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "multitaskingCameraAccessSupported") retCULong []

-- | multitaskingCameraAccessEnabled
--
-- Indicates whether the session is configured to use the camera while multitasking.
--
-- The default value is NO. This property may only be set if -isMultitaskingCameraAccessSupported returns YES. This property must be set before the session starts running.
--
-- AVCaptureSessions that are configured to use the camera while multitasking will not be interrupted with AVCaptureSessionInterruptionReasonVideoDeviceNotAvailableWithMultipleForegroundApps.
--
-- For applications that have the com.apple.developer.avfoundation.multitasking-camera-access entitlement, this property defaults to YES if -isMultitaskingCameraAccessSupported returns YES.
--
-- To learn about best practices for using the camera while multitasking, refer to the Accessing the Camera While Multitasking article on developer.apple.com. See https://developer.apple.com/documentation/avkit/accessing_the_camera_while_multitasking.
--
-- This property is key-value observable.
--
-- ObjC selector: @- multitaskingCameraAccessEnabled@
multitaskingCameraAccessEnabled :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
multitaskingCameraAccessEnabled avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "multitaskingCameraAccessEnabled") retCULong []

-- | multitaskingCameraAccessEnabled
--
-- Indicates whether the session is configured to use the camera while multitasking.
--
-- The default value is NO. This property may only be set if -isMultitaskingCameraAccessSupported returns YES. This property must be set before the session starts running.
--
-- AVCaptureSessions that are configured to use the camera while multitasking will not be interrupted with AVCaptureSessionInterruptionReasonVideoDeviceNotAvailableWithMultipleForegroundApps.
--
-- For applications that have the com.apple.developer.avfoundation.multitasking-camera-access entitlement, this property defaults to YES if -isMultitaskingCameraAccessSupported returns YES.
--
-- To learn about best practices for using the camera while multitasking, refer to the Accessing the Camera While Multitasking article on developer.apple.com. See https://developer.apple.com/documentation/avkit/accessing_the_camera_while_multitasking.
--
-- This property is key-value observable.
--
-- ObjC selector: @- setMultitaskingCameraAccessEnabled:@
setMultitaskingCameraAccessEnabled :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setMultitaskingCameraAccessEnabled avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setMultitaskingCameraAccessEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | usesApplicationAudioSession
--
-- Indicates whether the receiver will use the application's AVAudioSession for recording.
--
-- The value of this property is a BOOL indicating whether the receiver is currently using the application's AVAudioSession (see AVAudioSession.h). Prior to iOS 7, AVCaptureSession uses its own audio session, which can lead to unwanted interruptions when interacting with the application's audio session. In applications linked on or after iOS 7, AVCaptureSession shares the application's audio session, allowing for simultaneous play back and recording without unwanted interruptions. Clients desiring the pre-iOS 7 behavior may opt out by setting usesApplicationAudioSession to NO. The default value is YES.
--
-- ObjC selector: @- usesApplicationAudioSession@
usesApplicationAudioSession :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
usesApplicationAudioSession avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "usesApplicationAudioSession") retCULong []

-- | usesApplicationAudioSession
--
-- Indicates whether the receiver will use the application's AVAudioSession for recording.
--
-- The value of this property is a BOOL indicating whether the receiver is currently using the application's AVAudioSession (see AVAudioSession.h). Prior to iOS 7, AVCaptureSession uses its own audio session, which can lead to unwanted interruptions when interacting with the application's audio session. In applications linked on or after iOS 7, AVCaptureSession shares the application's audio session, allowing for simultaneous play back and recording without unwanted interruptions. Clients desiring the pre-iOS 7 behavior may opt out by setting usesApplicationAudioSession to NO. The default value is YES.
--
-- ObjC selector: @- setUsesApplicationAudioSession:@
setUsesApplicationAudioSession :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setUsesApplicationAudioSession avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setUsesApplicationAudioSession:") retVoid [argCULong (if value then 1 else 0)]

-- | automaticallyConfiguresApplicationAudioSession
--
-- Indicates whether the receiver should configure the application's audio session for recording.
--
-- The value of this property is a BOOL indicating whether the receiver should configure the application's audio session when needed for optimal recording. When set to YES, the receiver ensures the application's audio session is set to the PlayAndRecord category, and picks an appropriate microphone and polar pattern to match the video camera being used. When set to NO, and -usesApplicationAudioSession is set to YES, the receiver will use the application's audio session, but will not change any of its properties. If the session is not set up correctly for input, audio recording may fail. The default value is YES.
--
-- ObjC selector: @- automaticallyConfiguresApplicationAudioSession@
automaticallyConfiguresApplicationAudioSession :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
automaticallyConfiguresApplicationAudioSession avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "automaticallyConfiguresApplicationAudioSession") retCULong []

-- | automaticallyConfiguresApplicationAudioSession
--
-- Indicates whether the receiver should configure the application's audio session for recording.
--
-- The value of this property is a BOOL indicating whether the receiver should configure the application's audio session when needed for optimal recording. When set to YES, the receiver ensures the application's audio session is set to the PlayAndRecord category, and picks an appropriate microphone and polar pattern to match the video camera being used. When set to NO, and -usesApplicationAudioSession is set to YES, the receiver will use the application's audio session, but will not change any of its properties. If the session is not set up correctly for input, audio recording may fail. The default value is YES.
--
-- ObjC selector: @- setAutomaticallyConfiguresApplicationAudioSession:@
setAutomaticallyConfiguresApplicationAudioSession :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setAutomaticallyConfiguresApplicationAudioSession avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setAutomaticallyConfiguresApplicationAudioSession:") retVoid [argCULong (if value then 1 else 0)]

-- | configuresApplicationAudioSessionToMixWithOthers
--
-- Indicates whether the receiver should configure the application's audio session to mix with others.
--
-- The value of this property is a BOOL indicating whether the receiver should configure the application's audio session to mix with, instead of interrupting, any ongoing audio sessions. It has no effect when usesApplicationAudioSession is set to NO. It also has no effect on Live Photo movie complement capture (where music is always mixed with). The default value is NO.
--
-- ObjC selector: @- configuresApplicationAudioSessionToMixWithOthers@
configuresApplicationAudioSessionToMixWithOthers :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
configuresApplicationAudioSessionToMixWithOthers avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "configuresApplicationAudioSessionToMixWithOthers") retCULong []

-- | configuresApplicationAudioSessionToMixWithOthers
--
-- Indicates whether the receiver should configure the application's audio session to mix with others.
--
-- The value of this property is a BOOL indicating whether the receiver should configure the application's audio session to mix with, instead of interrupting, any ongoing audio sessions. It has no effect when usesApplicationAudioSession is set to NO. It also has no effect on Live Photo movie complement capture (where music is always mixed with). The default value is NO.
--
-- ObjC selector: @- setConfiguresApplicationAudioSessionToMixWithOthers:@
setConfiguresApplicationAudioSessionToMixWithOthers :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setConfiguresApplicationAudioSessionToMixWithOthers avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setConfiguresApplicationAudioSessionToMixWithOthers:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether the receiver should configure the application's audio session for bluetooth high quality recording.
--
-- The value of this property is a @BOOL@ indicating whether the receiver should configure the application's audio session for bluetooth high quality recording (AirPods as a high quality microphone). When this property is set to @true@, the ``AVCaptureSession`` will opt in for high quality bluetooth recording, allowing users of your app to select AirPods as the active mic source for capture. This property has no effect when ``usesApplicationAudioSession`` is set to @false@. The default value is @false@.
--
-- ObjC selector: @- configuresApplicationAudioSessionForBluetoothHighQualityRecording@
configuresApplicationAudioSessionForBluetoothHighQualityRecording :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
configuresApplicationAudioSessionForBluetoothHighQualityRecording avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "configuresApplicationAudioSessionForBluetoothHighQualityRecording") retCULong []

-- | Indicates whether the receiver should configure the application's audio session for bluetooth high quality recording.
--
-- The value of this property is a @BOOL@ indicating whether the receiver should configure the application's audio session for bluetooth high quality recording (AirPods as a high quality microphone). When this property is set to @true@, the ``AVCaptureSession`` will opt in for high quality bluetooth recording, allowing users of your app to select AirPods as the active mic source for capture. This property has no effect when ``usesApplicationAudioSession`` is set to @false@. The default value is @false@.
--
-- ObjC selector: @- setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording:@
setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording:") retVoid [argCULong (if value then 1 else 0)]

-- | automaticallyConfiguresCaptureDeviceForWideColor
--
-- Indicates whether the receiver automatically configures its video device's activeFormat and activeColorSpace properties, preferring wide color for photos.
--
-- The default value is YES. By default, the receiver automatically adjusts its source video AVCaptureDevice's activeFormat and activeColorSpace properties based on the supportedColorSpaces of the device's formats and the current AVCaptureSession topology. Wide color spaces are preferred over sRGB if an AVCapturePhotoOutput is present in the session. If you wish to set AVCaptureDevice's activeColorSpace manually, and prevent the AVCaptureSession from undoing your work, you must set automaticallyConfiguresCaptureDeviceForWideColor to NO. If the receiver's sessionPreset is set to AVCaptureSessionPresetInputPriority, the session will not alter the capture device's activeFormat, but might still alter its activeColorSpace.
--
-- ObjC selector: @- automaticallyConfiguresCaptureDeviceForWideColor@
automaticallyConfiguresCaptureDeviceForWideColor :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
automaticallyConfiguresCaptureDeviceForWideColor avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "automaticallyConfiguresCaptureDeviceForWideColor") retCULong []

-- | automaticallyConfiguresCaptureDeviceForWideColor
--
-- Indicates whether the receiver automatically configures its video device's activeFormat and activeColorSpace properties, preferring wide color for photos.
--
-- The default value is YES. By default, the receiver automatically adjusts its source video AVCaptureDevice's activeFormat and activeColorSpace properties based on the supportedColorSpaces of the device's formats and the current AVCaptureSession topology. Wide color spaces are preferred over sRGB if an AVCapturePhotoOutput is present in the session. If you wish to set AVCaptureDevice's activeColorSpace manually, and prevent the AVCaptureSession from undoing your work, you must set automaticallyConfiguresCaptureDeviceForWideColor to NO. If the receiver's sessionPreset is set to AVCaptureSessionPresetInputPriority, the session will not alter the capture device's activeFormat, but might still alter its activeColorSpace.
--
-- ObjC selector: @- setAutomaticallyConfiguresCaptureDeviceForWideColor:@
setAutomaticallyConfiguresCaptureDeviceForWideColor :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setAutomaticallyConfiguresCaptureDeviceForWideColor avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setAutomaticallyConfiguresCaptureDeviceForWideColor:") retVoid [argCULong (if value then 1 else 0)]

-- | synchronizationClock
--
-- Provides the clock being used for synchronization.
--
-- synchronizationClock is readonly. Use synchronizationClock to synchronize AVCaptureOutput data with external data sources (e.g motion samples). All capture output sample buffer timestamps are on the synchronizationClock timebase.
--
-- For example, if you want to reverse synchronize the output timestamps to the original timestamps, you can do the following: In captureOutput:didOutputSampleBuffer:fromConnection:
--
-- AVCaptureInputPort *port = [[connection inputPorts] objectAtIndex:0];    CMClockRef originalClock = [port clock];
--
-- CMTime syncedPTS = CMSampleBufferGetPresentationTime( sampleBuffer );    CMTime originalPTS = CMSyncConvertTime( syncedPTS, [session synchronizationClock], originalClock );
--
-- This property is key-value observable.
--
-- ObjC selector: @- synchronizationClock@
synchronizationClock :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Ptr ())
synchronizationClock avCaptureSession  =
  fmap castPtr $ sendMsg avCaptureSession (mkSelector "synchronizationClock") (retPtr retVoid) []

-- | masterClock
--
-- Provides the clock being used for synchronization.
--
-- Deprecated. Please use synchronizationClock instead.
--
-- ObjC selector: @- masterClock@
masterClock :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Ptr ())
masterClock avCaptureSession  =
  fmap castPtr $ sendMsg avCaptureSession (mkSelector "masterClock") (retPtr retVoid) []

-- | hardwareCost
--
-- Indicates the percentage of the session's available hardware budget currently in use.
--
-- The value of this property is a float from 0.0 => 1.0 indicating how much of the session's available hardware is in use as a percentage, given the currently connected inputs and outputs and the features for which you've opted in. When your hardwareCost is greater than 1.0, the capture session cannot run your desired configuration due to hardware constraints, so you receive an AVCaptureSessionRuntimeErrorNotification when attempting to start it running. Default value is 0.
--
-- Contributors to hardwareCost include:        - Whether the source device's active format uses the full sensor (4:3) or a crop (16:9). Cropped formats require lower hardware bandwidth, and therefore lower the cost.        - The max frame rate supported by the source device's active format. The higher the max frame rate, the higher the cost.        - Whether the source device's active format is binned or not. Binned formats require substantially less hardware bandwidth, and therefore result in a lower cost.        - The number of sources configured to deliver streaming disparity / depth via AVCaptureDepthDataOutput. The higher the number of cameras configured to produce depth, the higher the cost.    For AVCaptureMultiCamSessions, all of the source devices' active formats contribute to hardwareCost.    In order to reduce hardwareCost, consider picking a sensor-cropped activeFormat, or a binned format. You may also use AVCaptureDeviceInput's videoMinFrameDurationOverride property to artificially limit the max frame rate (which is the reciprocal of the min frame duration) of a source device to a lower value. By doing so, you only pay the hardware cost for the max frame rate you intend to use.
--
-- AVCaptureMultiCamSessions always computes this hardwareCost. AVCaptureSessions only computes a non-zero hardwareCost when multiple AVCaptureVideoDataOutputs or an AVCaptureMovieFileOutput and one or more AVCaptureVideoDataOutputs are added to the session.
--
-- ObjC selector: @- hardwareCost@
hardwareCost :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO CFloat
hardwareCost avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "hardwareCost") retCFloat []

-- | A @BOOL@ value that indicates whether the session supports manually running deferred start.
--
-- Deferred Start is a feature that allows you to control, on a per-output basis, whether output objects start when or after the session is started. The session defers starting an output when its ``deferredStartEnabled`` property is set to @true@, and starts it after the session is started.
--
-- You can only set the ``automaticallyRunsDeferredStart`` property value to @false@ if the session supports manual deferred start.
--
-- ObjC selector: @- manualDeferredStartSupported@
manualDeferredStartSupported :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
manualDeferredStartSupported avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "manualDeferredStartSupported") retCULong []

-- | A @BOOL@ value that indicates whether deferred start runs automatically.
--
-- Deferred Start is a feature that allows you to control, on a per-output basis, whether output objects start when or after the session is started. The session defers starting an output when its ``AVCaptureOutput/deferredStartEnabled`` property is set to @true@, and starts it after the session is started.
--
-- When this value is @true@, ``AVCaptureSession`` automatically runs deferred start. If only ``AVCaptureVideoPreviewLayer`` objects have ``AVCaptureVideoPreviewLayer/deferredStartEnabled`` set to @false@, the session runs deferred start a short time after displaying the first frame. If there are ``AVCaptureOutput`` objects that have ``AVCaptureOutput/deferredStartEnabled`` set to @false@, then the session waits until each output that provides streaming data to your app sends its first frame.
--
-- If you set this value to @false@, call ``runDeferredStartWhenNeeded`` to indicate when to run deferred start.
--
-- By default, for apps that are linked on or after iOS 26, this value is @true@.
--
-- - Note: If ``manualDeferredStartSupported`` is @false@, setting this property value to @false@ results in the session throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before committing the configuration.
--
-- ObjC selector: @- automaticallyRunsDeferredStart@
automaticallyRunsDeferredStart :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO Bool
automaticallyRunsDeferredStart avCaptureSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSession (mkSelector "automaticallyRunsDeferredStart") retCULong []

-- | A @BOOL@ value that indicates whether deferred start runs automatically.
--
-- Deferred Start is a feature that allows you to control, on a per-output basis, whether output objects start when or after the session is started. The session defers starting an output when its ``AVCaptureOutput/deferredStartEnabled`` property is set to @true@, and starts it after the session is started.
--
-- When this value is @true@, ``AVCaptureSession`` automatically runs deferred start. If only ``AVCaptureVideoPreviewLayer`` objects have ``AVCaptureVideoPreviewLayer/deferredStartEnabled`` set to @false@, the session runs deferred start a short time after displaying the first frame. If there are ``AVCaptureOutput`` objects that have ``AVCaptureOutput/deferredStartEnabled`` set to @false@, then the session waits until each output that provides streaming data to your app sends its first frame.
--
-- If you set this value to @false@, call ``runDeferredStartWhenNeeded`` to indicate when to run deferred start.
--
-- By default, for apps that are linked on or after iOS 26, this value is @true@.
--
-- - Note: If ``manualDeferredStartSupported`` is @false@, setting this property value to @false@ results in the session throwing an @NSInvalidArgumentException@.
--
-- - Note: Set this value before committing the configuration.
--
-- ObjC selector: @- setAutomaticallyRunsDeferredStart:@
setAutomaticallyRunsDeferredStart :: IsAVCaptureSession avCaptureSession => avCaptureSession -> Bool -> IO ()
setAutomaticallyRunsDeferredStart avCaptureSession  value =
  sendMsg avCaptureSession (mkSelector "setAutomaticallyRunsDeferredStart:") retVoid [argCULong (if value then 1 else 0)]

-- | The dispatch queue on which the session calls deferred start delegate methods.
--
-- Call the ``setDeferredStartDelegate:deferredStartDelegateCallbackQueue:`` method to specify the dispatch queue on which to call the deferred start delegate methods.
--
-- ObjC selector: @- deferredStartDelegateCallbackQueue@
deferredStartDelegateCallbackQueue :: IsAVCaptureSession avCaptureSession => avCaptureSession -> IO (Id NSObject)
deferredStartDelegateCallbackQueue avCaptureSession  =
  sendMsg avCaptureSession (mkSelector "deferredStartDelegateCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canSetSessionPreset:@
canSetSessionPresetSelector :: Selector
canSetSessionPresetSelector = mkSelector "canSetSessionPreset:"

-- | @Selector@ for @canAddInput:@
canAddInputSelector :: Selector
canAddInputSelector = mkSelector "canAddInput:"

-- | @Selector@ for @addInput:@
addInputSelector :: Selector
addInputSelector = mkSelector "addInput:"

-- | @Selector@ for @removeInput:@
removeInputSelector :: Selector
removeInputSelector = mkSelector "removeInput:"

-- | @Selector@ for @canAddOutput:@
canAddOutputSelector :: Selector
canAddOutputSelector = mkSelector "canAddOutput:"

-- | @Selector@ for @addOutput:@
addOutputSelector :: Selector
addOutputSelector = mkSelector "addOutput:"

-- | @Selector@ for @removeOutput:@
removeOutputSelector :: Selector
removeOutputSelector = mkSelector "removeOutput:"

-- | @Selector@ for @addInputWithNoConnections:@
addInputWithNoConnectionsSelector :: Selector
addInputWithNoConnectionsSelector = mkSelector "addInputWithNoConnections:"

-- | @Selector@ for @addOutputWithNoConnections:@
addOutputWithNoConnectionsSelector :: Selector
addOutputWithNoConnectionsSelector = mkSelector "addOutputWithNoConnections:"

-- | @Selector@ for @canAddConnection:@
canAddConnectionSelector :: Selector
canAddConnectionSelector = mkSelector "canAddConnection:"

-- | @Selector@ for @addConnection:@
addConnectionSelector :: Selector
addConnectionSelector = mkSelector "addConnection:"

-- | @Selector@ for @removeConnection:@
removeConnectionSelector :: Selector
removeConnectionSelector = mkSelector "removeConnection:"

-- | @Selector@ for @setControlsDelegate:queue:@
setControlsDelegate_queueSelector :: Selector
setControlsDelegate_queueSelector = mkSelector "setControlsDelegate:queue:"

-- | @Selector@ for @canAddControl:@
canAddControlSelector :: Selector
canAddControlSelector = mkSelector "canAddControl:"

-- | @Selector@ for @addControl:@
addControlSelector :: Selector
addControlSelector = mkSelector "addControl:"

-- | @Selector@ for @removeControl:@
removeControlSelector :: Selector
removeControlSelector = mkSelector "removeControl:"

-- | @Selector@ for @beginConfiguration@
beginConfigurationSelector :: Selector
beginConfigurationSelector = mkSelector "beginConfiguration"

-- | @Selector@ for @commitConfiguration@
commitConfigurationSelector :: Selector
commitConfigurationSelector = mkSelector "commitConfiguration"

-- | @Selector@ for @startRunning@
startRunningSelector :: Selector
startRunningSelector = mkSelector "startRunning"

-- | @Selector@ for @stopRunning@
stopRunningSelector :: Selector
stopRunningSelector = mkSelector "stopRunning"

-- | @Selector@ for @runDeferredStartWhenNeeded@
runDeferredStartWhenNeededSelector :: Selector
runDeferredStartWhenNeededSelector = mkSelector "runDeferredStartWhenNeeded"

-- | @Selector@ for @setDeferredStartDelegate:deferredStartDelegateCallbackQueue:@
setDeferredStartDelegate_deferredStartDelegateCallbackQueueSelector :: Selector
setDeferredStartDelegate_deferredStartDelegateCallbackQueueSelector = mkSelector "setDeferredStartDelegate:deferredStartDelegateCallbackQueue:"

-- | @Selector@ for @sessionPreset@
sessionPresetSelector :: Selector
sessionPresetSelector = mkSelector "sessionPreset"

-- | @Selector@ for @setSessionPreset:@
setSessionPresetSelector :: Selector
setSessionPresetSelector = mkSelector "setSessionPreset:"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector
outputsSelector = mkSelector "outputs"

-- | @Selector@ for @supportsControls@
supportsControlsSelector :: Selector
supportsControlsSelector = mkSelector "supportsControls"

-- | @Selector@ for @maxControlsCount@
maxControlsCountSelector :: Selector
maxControlsCountSelector = mkSelector "maxControlsCount"

-- | @Selector@ for @controlsDelegateCallbackQueue@
controlsDelegateCallbackQueueSelector :: Selector
controlsDelegateCallbackQueueSelector = mkSelector "controlsDelegateCallbackQueue"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @interrupted@
interruptedSelector :: Selector
interruptedSelector = mkSelector "interrupted"

-- | @Selector@ for @multitaskingCameraAccessSupported@
multitaskingCameraAccessSupportedSelector :: Selector
multitaskingCameraAccessSupportedSelector = mkSelector "multitaskingCameraAccessSupported"

-- | @Selector@ for @multitaskingCameraAccessEnabled@
multitaskingCameraAccessEnabledSelector :: Selector
multitaskingCameraAccessEnabledSelector = mkSelector "multitaskingCameraAccessEnabled"

-- | @Selector@ for @setMultitaskingCameraAccessEnabled:@
setMultitaskingCameraAccessEnabledSelector :: Selector
setMultitaskingCameraAccessEnabledSelector = mkSelector "setMultitaskingCameraAccessEnabled:"

-- | @Selector@ for @usesApplicationAudioSession@
usesApplicationAudioSessionSelector :: Selector
usesApplicationAudioSessionSelector = mkSelector "usesApplicationAudioSession"

-- | @Selector@ for @setUsesApplicationAudioSession:@
setUsesApplicationAudioSessionSelector :: Selector
setUsesApplicationAudioSessionSelector = mkSelector "setUsesApplicationAudioSession:"

-- | @Selector@ for @automaticallyConfiguresApplicationAudioSession@
automaticallyConfiguresApplicationAudioSessionSelector :: Selector
automaticallyConfiguresApplicationAudioSessionSelector = mkSelector "automaticallyConfiguresApplicationAudioSession"

-- | @Selector@ for @setAutomaticallyConfiguresApplicationAudioSession:@
setAutomaticallyConfiguresApplicationAudioSessionSelector :: Selector
setAutomaticallyConfiguresApplicationAudioSessionSelector = mkSelector "setAutomaticallyConfiguresApplicationAudioSession:"

-- | @Selector@ for @configuresApplicationAudioSessionToMixWithOthers@
configuresApplicationAudioSessionToMixWithOthersSelector :: Selector
configuresApplicationAudioSessionToMixWithOthersSelector = mkSelector "configuresApplicationAudioSessionToMixWithOthers"

-- | @Selector@ for @setConfiguresApplicationAudioSessionToMixWithOthers:@
setConfiguresApplicationAudioSessionToMixWithOthersSelector :: Selector
setConfiguresApplicationAudioSessionToMixWithOthersSelector = mkSelector "setConfiguresApplicationAudioSessionToMixWithOthers:"

-- | @Selector@ for @configuresApplicationAudioSessionForBluetoothHighQualityRecording@
configuresApplicationAudioSessionForBluetoothHighQualityRecordingSelector :: Selector
configuresApplicationAudioSessionForBluetoothHighQualityRecordingSelector = mkSelector "configuresApplicationAudioSessionForBluetoothHighQualityRecording"

-- | @Selector@ for @setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording:@
setConfiguresApplicationAudioSessionForBluetoothHighQualityRecordingSelector :: Selector
setConfiguresApplicationAudioSessionForBluetoothHighQualityRecordingSelector = mkSelector "setConfiguresApplicationAudioSessionForBluetoothHighQualityRecording:"

-- | @Selector@ for @automaticallyConfiguresCaptureDeviceForWideColor@
automaticallyConfiguresCaptureDeviceForWideColorSelector :: Selector
automaticallyConfiguresCaptureDeviceForWideColorSelector = mkSelector "automaticallyConfiguresCaptureDeviceForWideColor"

-- | @Selector@ for @setAutomaticallyConfiguresCaptureDeviceForWideColor:@
setAutomaticallyConfiguresCaptureDeviceForWideColorSelector :: Selector
setAutomaticallyConfiguresCaptureDeviceForWideColorSelector = mkSelector "setAutomaticallyConfiguresCaptureDeviceForWideColor:"

-- | @Selector@ for @synchronizationClock@
synchronizationClockSelector :: Selector
synchronizationClockSelector = mkSelector "synchronizationClock"

-- | @Selector@ for @masterClock@
masterClockSelector :: Selector
masterClockSelector = mkSelector "masterClock"

-- | @Selector@ for @hardwareCost@
hardwareCostSelector :: Selector
hardwareCostSelector = mkSelector "hardwareCost"

-- | @Selector@ for @manualDeferredStartSupported@
manualDeferredStartSupportedSelector :: Selector
manualDeferredStartSupportedSelector = mkSelector "manualDeferredStartSupported"

-- | @Selector@ for @automaticallyRunsDeferredStart@
automaticallyRunsDeferredStartSelector :: Selector
automaticallyRunsDeferredStartSelector = mkSelector "automaticallyRunsDeferredStart"

-- | @Selector@ for @setAutomaticallyRunsDeferredStart:@
setAutomaticallyRunsDeferredStartSelector :: Selector
setAutomaticallyRunsDeferredStartSelector = mkSelector "setAutomaticallyRunsDeferredStart:"

-- | @Selector@ for @deferredStartDelegateCallbackQueue@
deferredStartDelegateCallbackQueueSelector :: Selector
deferredStartDelegateCallbackQueueSelector = mkSelector "deferredStartDelegateCallbackQueue"

