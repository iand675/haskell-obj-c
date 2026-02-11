{-# LANGUAGE PatternSynonyms #-}
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
  , initWithSensorSelector
  , initSelector
  , newSelector
  , startRecordingSelector
  , stopRecordingSelector
  , fetchDevicesSelector
  , fetchSelector
  , requestAuthorizationForSensors_completionSelector
  , authorizationStatusSelector
  , sensorSelector
  , delegateSelector
  , setDelegateSelector

  -- * Enum types
  , SRAuthorizationStatus(SRAuthorizationStatus)
  , pattern SRAuthorizationStatusNotDetermined
  , pattern SRAuthorizationStatusAuthorized
  , pattern SRAuthorizationStatusDenied

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes an SRSensorReader that will fetch data for the current device and any companion
--
-- ObjC selector: @- initWithSensor:@
initWithSensor :: (IsSRSensorReader srSensorReader, IsNSString sensor) => srSensorReader -> sensor -> IO (Id SRSensorReader)
initWithSensor srSensorReader  sensor =
  withObjCPtr sensor $ \raw_sensor ->
      sendMsg srSensorReader (mkSelector "initWithSensor:") (retPtr retVoid) [argPtr (castPtr raw_sensor :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSRSensorReader srSensorReader => srSensorReader -> IO (Id SRSensorReader)
init_ srSensorReader  =
    sendMsg srSensorReader (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRSensorReader)
new  =
  do
    cls' <- getRequiredClass "SRSensorReader"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
startRecording srSensorReader  =
    sendMsg srSensorReader (mkSelector "startRecording") retVoid []

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
stopRecording srSensorReader  =
    sendMsg srSensorReader (mkSelector "stopRecording") retVoid []

-- | Fetches device information for all devices that have stored data for the given sensor in SensorKit
--
-- If the request completes successfully, devices will be returned to the delegate in the sensorReader:fetchedDevices: callback. If the request failed, an error will be returned to the delegate in the sensorReader:fetchDevicesFailedWithError: method
--
-- ObjC selector: @- fetchDevices@
fetchDevices :: IsSRSensorReader srSensorReader => srSensorReader -> IO ()
fetchDevices srSensorReader  =
    sendMsg srSensorReader (mkSelector "fetchDevices") retVoid []

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
fetch srSensorReader  request =
  withObjCPtr request $ \raw_request ->
      sendMsg srSensorReader (mkSelector "fetch:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

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
    withObjCPtr sensors $ \raw_sensors ->
      sendClassMsg cls' (mkSelector "requestAuthorizationForSensors:completion:") retVoid [argPtr (castPtr raw_sensors :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | The current authorization status of the calling application.
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsSRSensorReader srSensorReader => srSensorReader -> IO SRAuthorizationStatus
authorizationStatus srSensorReader  =
    fmap (coerce :: CLong -> SRAuthorizationStatus) $ sendMsg srSensorReader (mkSelector "authorizationStatus") retCLong []

-- | the sensor this reader was initialized with
--
-- ObjC selector: @- sensor@
sensor :: IsSRSensorReader srSensorReader => srSensorReader -> IO (Id NSString)
sensor srSensorReader  =
    sendMsg srSensorReader (mkSelector "sensor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsSRSensorReader srSensorReader => srSensorReader -> IO RawId
delegate srSensorReader  =
    fmap (RawId . castPtr) $ sendMsg srSensorReader (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSRSensorReader srSensorReader => srSensorReader -> RawId -> IO ()
setDelegate srSensorReader  value =
    sendMsg srSensorReader (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSensor:@
initWithSensorSelector :: Selector
initWithSensorSelector = mkSelector "initWithSensor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @startRecording@
startRecordingSelector :: Selector
startRecordingSelector = mkSelector "startRecording"

-- | @Selector@ for @stopRecording@
stopRecordingSelector :: Selector
stopRecordingSelector = mkSelector "stopRecording"

-- | @Selector@ for @fetchDevices@
fetchDevicesSelector :: Selector
fetchDevicesSelector = mkSelector "fetchDevices"

-- | @Selector@ for @fetch:@
fetchSelector :: Selector
fetchSelector = mkSelector "fetch:"

-- | @Selector@ for @requestAuthorizationForSensors:completion:@
requestAuthorizationForSensors_completionSelector :: Selector
requestAuthorizationForSensors_completionSelector = mkSelector "requestAuthorizationForSensors:completion:"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @sensor@
sensorSelector :: Selector
sensorSelector = mkSelector "sensor"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

