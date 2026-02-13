{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRSensorReader@.
module ObjC.SensorKit.SRSensorReader
  ( SRSensorReader
  , IsSRSensorReader(..)
  , initWithSensor
  , init_
  , new
  , startRecording
  , stopRecording
  , fetchDevices
  , fetch
  , requestAuthorizationForSensors_completion
  , authorizationStatus
  , sensor
  , delegate
  , setDelegate
  , authorizationStatusSelector
  , delegateSelector
  , fetchDevicesSelector
  , fetchSelector
  , initSelector
  , initWithSensorSelector
  , newSelector
  , requestAuthorizationForSensors_completionSelector
  , sensorSelector
  , setDelegateSelector
  , startRecordingSelector
  , stopRecordingSelector

  -- * Enum types
  , SRAuthorizationStatus(SRAuthorizationStatus)
  , pattern SRAuthorizationStatusNotDetermined
  , pattern SRAuthorizationStatusAuthorized
  , pattern SRAuthorizationStatusDenied

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes an SRSensorReader that will fetch data for the current device and any companion
--
-- ObjC selector: @- initWithSensor:@
initWithSensor :: (IsSRSensorReader srSensorReader, IsNSString sensor) => srSensorReader -> sensor -> IO (Id SRSensorReader)
initWithSensor srSensorReader sensor =
  sendOwnedMessage srSensorReader initWithSensorSelector (toNSString sensor)

-- | @- init@
init_ :: IsSRSensorReader srSensorReader => srSensorReader -> IO (Id SRSensorReader)
init_ srSensorReader =
  sendOwnedMessage srSensorReader initSelector

-- | @+ new@
new :: IO (Id SRSensorReader)
new  =
  do
    cls' <- getRequiredClass "SRSensorReader"
    sendOwnedClassMessage cls' newSelector

-- | Starts recording for the reader's sensor
--
-- The reader must be authorized for the sensor for this to succeed. This starts recording on this device and any paired devices. If other readers have already started the sensor recording this reader's interest in recording will be maintained. Other readers in other apps for the same sensor will not affect the recording status of this reader.
--
-- In the case of success, completion notification will be delivered to the delegate in the sensorReaderWillStartRecording: delegate method.
--
-- In the case of failure, error notification will be delivered to the delegate in the sensorReader:startRecordingFailedWithError: delegate method.
--
-- ObjC selector: @- startRecording@
startRecording :: IsSRSensorReader srSensorReader => srSensorReader -> IO ()
startRecording srSensorReader =
  sendMessage srSensorReader startRecordingSelector

-- | Stops recording for the reader's sensor
--
-- The reader must be authorized for the sensor for this to succeed. This stops recording on this device and any paired devices. Sensor recording will continue until the last interested reader has stopped recording.
--
-- In the case of success, completion notification will be delivered to the delegate in the sensorReaderDidStopRecording: delegate method.
--
-- In the case of failure, error notification will be delivered to the delegate in the sensorReader:stopRecordingFailedWithError: delegate method.
--
-- ObjC selector: @- stopRecording@
stopRecording :: IsSRSensorReader srSensorReader => srSensorReader -> IO ()
stopRecording srSensorReader =
  sendMessage srSensorReader stopRecordingSelector

-- | Fetches device information for all devices that have stored data for the given sensor in SensorKit
--
-- If the request completes successfully, devices will be returned to the delegate in the sensorReader:fetchedDevices: callback. If the request failed, an error will be returned to the delegate in the sensorReader:fetchDevicesFailedWithError: method
--
-- ObjC selector: @- fetchDevices@
fetchDevices :: IsSRSensorReader srSensorReader => srSensorReader -> IO ()
fetchDevices srSensorReader =
  sendMessage srSensorReader fetchDevicesSelector

-- | Fetches samples for the reader's sensor for given request parameters
--
-- The reader must be authorized for the sensor for this to succeed.
--
-- Samples will be delivered to the delegate through multiple calls to the sensorReader:fetchingRequest:didFetchResult: delegate method
--
-- In the case of a failure, any error will be delivered to the delegate in the sensorReader:fetchingRequest:failedWithError: method.
--
-- In the case of success, completion notification will be delivered to the delegate in the sensorReader:didCompleteFetch: method.
--
-- @request@ — The query parameters for this fetch
--
-- ObjC selector: @- fetch:@
fetch :: (IsSRSensorReader srSensorReader, IsSRFetchRequest request) => srSensorReader -> request -> IO ()
fetch srSensorReader request =
  sendMessage srSensorReader fetchSelector (toSRFetchRequest request)

-- | Request authorization to a given set of sensors
--
-- If the SRSensorReader instance is not authorized, this method must be called before any other methods. Failure to request authorization will cause errors to be returned from the other methods.
--
-- When SensorKit prepares the prompt for display, it will look at the NSSensorKitUsageDetail key in your Info.plist.  The value should be a dictionary containing usage descriptions for all of the sensors being requested.  The description key you provide to this method must correspond to an entry in that dictionary.  To retrieve a localized string, SensorKit will load your InfoPlist.strings file and try to look up a string using the description key you provided.  If that fails, SensorKit will use the content provided in your Info.plist.
--
-- SensorKit may decide against showing the user a prompt.  For example, if the user has already chosen whether to grant the application access to all of the types provided.  When that happens, your completion block will be called with an appropriate  NSError.  If the user responded to the prompt, your completion block will be called with a nil error. Changes in authorization status will delivered to the delegate in the sensorReader:didChangeAuthorizationStatus: method.
--
-- ObjC selector: @+ requestAuthorizationForSensors:completion:@
requestAuthorizationForSensors_completion :: IsNSSet sensors => sensors -> Ptr () -> IO ()
requestAuthorizationForSensors_completion sensors completion =
  do
    cls' <- getRequiredClass "SRSensorReader"
    sendClassMessage cls' requestAuthorizationForSensors_completionSelector (toNSSet sensors) completion

-- | The current authorization status of the calling application.
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsSRSensorReader srSensorReader => srSensorReader -> IO SRAuthorizationStatus
authorizationStatus srSensorReader =
  sendMessage srSensorReader authorizationStatusSelector

-- | the sensor this reader was initialized with
--
-- ObjC selector: @- sensor@
sensor :: IsSRSensorReader srSensorReader => srSensorReader -> IO (Id NSString)
sensor srSensorReader =
  sendMessage srSensorReader sensorSelector

-- | @- delegate@
delegate :: IsSRSensorReader srSensorReader => srSensorReader -> IO RawId
delegate srSensorReader =
  sendMessage srSensorReader delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSRSensorReader srSensorReader => srSensorReader -> RawId -> IO ()
setDelegate srSensorReader value =
  sendMessage srSensorReader setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSensor:@
initWithSensorSelector :: Selector '[Id NSString] (Id SRSensorReader)
initWithSensorSelector = mkSelector "initWithSensor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRSensorReader)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRSensorReader)
newSelector = mkSelector "new"

-- | @Selector@ for @startRecording@
startRecordingSelector :: Selector '[] ()
startRecordingSelector = mkSelector "startRecording"

-- | @Selector@ for @stopRecording@
stopRecordingSelector :: Selector '[] ()
stopRecordingSelector = mkSelector "stopRecording"

-- | @Selector@ for @fetchDevices@
fetchDevicesSelector :: Selector '[] ()
fetchDevicesSelector = mkSelector "fetchDevices"

-- | @Selector@ for @fetch:@
fetchSelector :: Selector '[Id SRFetchRequest] ()
fetchSelector = mkSelector "fetch:"

-- | @Selector@ for @requestAuthorizationForSensors:completion:@
requestAuthorizationForSensors_completionSelector :: Selector '[Id NSSet, Ptr ()] ()
requestAuthorizationForSensors_completionSelector = mkSelector "requestAuthorizationForSensors:completion:"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] SRAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @sensor@
sensorSelector :: Selector '[] (Id NSString)
sensorSelector = mkSelector "sensor"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

