{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBPeripheralManager
--
-- The CBPeripheralManager class is an abstraction of the Peripheral and Broadcaster GAP roles, and the GATT Server              role. Its primary function is to allow you to manage published services within the GATT database, and to advertise these services              to other devices.              Each application has sandboxed access to the shared GATT database. You can add services to the database by calling {
--
-- addService:};
-- they can be removed via {@link removeService:} and {@link removeAllServices}, as appropriate. While a service is in the database,
-- it is visible to and can be accessed by any connected GATT Client. However, applications that have not specified the "bluetooth-peripheral"
-- background mode will have the contents of their service(s) "disabled" when in the background. Any remote device trying to access
-- characteristic values or descriptors during this time will receive an error response.
-- Once you've published services that you want to share, you can ask to advertise their availability and allow other devices to connect
-- to you by calling {@link startAdvertising:}. Like the GATT database, advertisement is managed at the system level and shared by all
-- applications. This means that even if you aren't advertising at the moment, someone else might be!
--
-- Generated bindings for @CBPeripheralManager@.
module ObjC.CoreBluetooth.CBPeripheralManager
  ( CBPeripheralManager
  , IsCBPeripheralManager(..)
  , authorizationStatus
  , init_
  , initWithDelegate_queue
  , initWithDelegate_queue_options
  , startAdvertising
  , stopAdvertising
  , setDesiredConnectionLatency_forCentral
  , addService
  , removeService
  , removeAllServices
  , respondToRequest_withResult
  , updateValue_forCharacteristic_onSubscribedCentrals
  , publishL2CAPChannelWithEncryption
  , unpublishL2CAPChannel
  , isAdvertising
  , authorizationStatusSelector
  , initSelector
  , initWithDelegate_queueSelector
  , initWithDelegate_queue_optionsSelector
  , startAdvertisingSelector
  , stopAdvertisingSelector
  , setDesiredConnectionLatency_forCentralSelector
  , addServiceSelector
  , removeServiceSelector
  , removeAllServicesSelector
  , respondToRequest_withResultSelector
  , updateValue_forCharacteristic_onSubscribedCentralsSelector
  , publishL2CAPChannelWithEncryptionSelector
  , unpublishL2CAPChannelSelector
  , isAdvertisingSelector

  -- * Enum types
  , CBATTError(CBATTError)
  , pattern CBATTErrorSuccess
  , pattern CBATTErrorInvalidHandle
  , pattern CBATTErrorReadNotPermitted
  , pattern CBATTErrorWriteNotPermitted
  , pattern CBATTErrorInvalidPdu
  , pattern CBATTErrorInsufficientAuthentication
  , pattern CBATTErrorRequestNotSupported
  , pattern CBATTErrorInvalidOffset
  , pattern CBATTErrorInsufficientAuthorization
  , pattern CBATTErrorPrepareQueueFull
  , pattern CBATTErrorAttributeNotFound
  , pattern CBATTErrorAttributeNotLong
  , pattern CBATTErrorInsufficientEncryptionKeySize
  , pattern CBATTErrorInvalidAttributeValueLength
  , pattern CBATTErrorUnlikelyError
  , pattern CBATTErrorInsufficientEncryption
  , pattern CBATTErrorUnsupportedGroupType
  , pattern CBATTErrorInsufficientResources
  , CBPeripheralManagerAuthorizationStatus(CBPeripheralManagerAuthorizationStatus)
  , pattern CBPeripheralManagerAuthorizationStatusNotDetermined
  , pattern CBPeripheralManagerAuthorizationStatusRestricted
  , pattern CBPeripheralManagerAuthorizationStatusDenied
  , pattern CBPeripheralManagerAuthorizationStatusAuthorized
  , CBPeripheralManagerConnectionLatency(CBPeripheralManagerConnectionLatency)
  , pattern CBPeripheralManagerConnectionLatencyLow
  , pattern CBPeripheralManagerConnectionLatencyMedium
  , pattern CBPeripheralManagerConnectionLatencyHigh

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

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | authorizationStatus
--
-- This method does not prompt the user for access. You can use it to detect restricted access and simply hide UI instead of				prompting for access.
--
-- Returns: The current authorization status for sharing data while backgrounded. For the constants returned, see {
--
-- CBPeripheralManagerAuthorizationStatus}.
--
-- @see		CBPeripheralManagerAuthorizationStatus
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO CBPeripheralManagerAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CBPeripheralManager"
    fmap (coerce :: CLong -> CBPeripheralManagerAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- init@
init_ :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> IO (Id CBPeripheralManager)
init_ cbPeripheralManager  =
  sendMsg cbPeripheralManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithDelegate:queue:
--
-- @delegate@ — The delegate that will receive peripheral role events.
--
-- @queue@ — The dispatch queue on which the events will be dispatched.
--
-- The initialization call. The events of the peripheral role will be dispatched on the provided queue.                  If nil, the main queue will be used.
--
-- ObjC selector: @- initWithDelegate:queue:@
initWithDelegate_queue :: (IsCBPeripheralManager cbPeripheralManager, IsNSObject queue) => cbPeripheralManager -> RawId -> queue -> IO (Id CBPeripheralManager)
initWithDelegate_queue cbPeripheralManager  delegate queue =
withObjCPtr queue $ \raw_queue ->
    sendMsg cbPeripheralManager (mkSelector "initWithDelegate:queue:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | initWithDelegate:queue:options:
--
-- @delegate@ — The delegate that will receive peripheral role events.
--
-- @queue@ — The dispatch queue on which the events will be dispatched.
--
-- @options@ — An optional dictionary specifying options for the manager.
--
-- The initialization call. The events of the peripheral role will be dispatched on the provided queue.                  If nil, the main queue will be used.
--
-- CBPeripheralManagerOptionShowPowerAlertKey
--
-- CBPeripheralManagerOptionRestoreIdentifierKey
--
-- ObjC selector: @- initWithDelegate:queue:options:@
initWithDelegate_queue_options :: (IsCBPeripheralManager cbPeripheralManager, IsNSObject queue, IsNSDictionary options) => cbPeripheralManager -> RawId -> queue -> options -> IO (Id CBPeripheralManager)
initWithDelegate_queue_options cbPeripheralManager  delegate queue options =
withObjCPtr queue $ \raw_queue ->
  withObjCPtr options $ \raw_options ->
      sendMsg cbPeripheralManager (mkSelector "initWithDelegate:queue:options:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | startAdvertising:
--
-- @advertisementData@ — An optional dictionary containing the data to be advertised.
--
-- Starts advertising. Supported advertising data types are CBAdvertisementDataLocalNameKey                              and CBAdvertisementDataServiceUUIDsKey.                              When in the foreground, an application can utilize up to 28 bytes of space in the initial advertisement data for                               any combination of the supported advertising data types. If this space is used up, there are an additional 10 bytes of                               space in the scan response that can be used only for the local name. Note that these sizes do not include the 2 bytes                               of header information that are required for each new data type. Any service UUIDs that do not fit in the allotted space                               will be added to a special "overflow" area, and can only be discovered by an iOS device that is explicitly scanning                              for them.                              While an application is in the background, the local name will not be used and all service UUIDs will be placed in the                              "overflow" area. However, applications that have not specified the "bluetooth-peripheral" background mode will not be able                               to advertise anything while in the background.
--
-- See: peripheralManagerDidStartAdvertising:error:
--
-- CBAdvertisementData.h
--
-- ObjC selector: @- startAdvertising:@
startAdvertising :: (IsCBPeripheralManager cbPeripheralManager, IsNSDictionary advertisementData) => cbPeripheralManager -> advertisementData -> IO ()
startAdvertising cbPeripheralManager  advertisementData =
withObjCPtr advertisementData $ \raw_advertisementData ->
    sendMsg cbPeripheralManager (mkSelector "startAdvertising:") retVoid [argPtr (castPtr raw_advertisementData :: Ptr ())]

-- | stopAdvertising
--
-- Stops advertising.
--
-- ObjC selector: @- stopAdvertising@
stopAdvertising :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> IO ()
stopAdvertising cbPeripheralManager  =
  sendMsg cbPeripheralManager (mkSelector "stopAdvertising") retVoid []

-- | setDesiredConnectionLatency:forCentral:
--
-- @latency@ — The desired connection latency.
--
-- @central@ — A connected central.
--
-- Sets the desired connection latency for an existing connection to central. Connection latency changes are not guaranteed, so the                  resultant latency may vary. If a desired latency is not set, the latency chosen by central at the time of connection establishment                  will be used. Typically, it is not necessary to change the latency.
--
-- See: CBPeripheralManagerConnectionLatency
--
-- ObjC selector: @- setDesiredConnectionLatency:forCentral:@
setDesiredConnectionLatency_forCentral :: (IsCBPeripheralManager cbPeripheralManager, IsCBCentral central) => cbPeripheralManager -> CBPeripheralManagerConnectionLatency -> central -> IO ()
setDesiredConnectionLatency_forCentral cbPeripheralManager  latency central =
withObjCPtr central $ \raw_central ->
    sendMsg cbPeripheralManager (mkSelector "setDesiredConnectionLatency:forCentral:") retVoid [argCLong (coerce latency), argPtr (castPtr raw_central :: Ptr ())]

-- | addService:
--
-- @service@ — A GATT service.
--
-- Publishes a service and its associated characteristic(s) to the local database. If the service contains included services,                  they must be published first.
--
-- See: peripheralManager:didAddService:error:
--
-- ObjC selector: @- addService:@
addService :: (IsCBPeripheralManager cbPeripheralManager, IsCBMutableService service) => cbPeripheralManager -> service -> IO ()
addService cbPeripheralManager  service =
withObjCPtr service $ \raw_service ->
    sendMsg cbPeripheralManager (mkSelector "addService:") retVoid [argPtr (castPtr raw_service :: Ptr ())]

-- | removeService:
--
-- @service@ — A GATT service.
--
-- Removes a published service from the local database. If the service is included by other service(s), they must be removed                  first.
--
-- ObjC selector: @- removeService:@
removeService :: (IsCBPeripheralManager cbPeripheralManager, IsCBMutableService service) => cbPeripheralManager -> service -> IO ()
removeService cbPeripheralManager  service =
withObjCPtr service $ \raw_service ->
    sendMsg cbPeripheralManager (mkSelector "removeService:") retVoid [argPtr (castPtr raw_service :: Ptr ())]

-- | removeAllServices
--
-- Removes all published services from the local database.
--
-- ObjC selector: @- removeAllServices@
removeAllServices :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> IO ()
removeAllServices cbPeripheralManager  =
  sendMsg cbPeripheralManager (mkSelector "removeAllServices") retVoid []

-- | respondToRequest:withResult:
--
-- @request@ — The original request that was received from the central.
--
-- @result@ — The result of attempting to fulfill request.
--
-- Used to respond to request(s) received via the
--
-- peripheralManager:didReceiveReadRequest:
--
-- or
--
-- peripheralManager:didReceiveWriteRequests:
--
-- delegate methods.
--
-- See: peripheralManager:didReceiveReadRequest:
--
-- See: peripheralManager:didReceiveWriteRequests:
--
-- ObjC selector: @- respondToRequest:withResult:@
respondToRequest_withResult :: (IsCBPeripheralManager cbPeripheralManager, IsCBATTRequest request) => cbPeripheralManager -> request -> CBATTError -> IO ()
respondToRequest_withResult cbPeripheralManager  request result =
withObjCPtr request $ \raw_request ->
    sendMsg cbPeripheralManager (mkSelector "respondToRequest:withResult:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argCLong (coerce result)]

-- | updateValue:forCharacteristic:onSubscribedCentrals:
--
-- @value@ — The value to be sent via a notification/indication.
--
-- @characteristic@ — The characteristic whose value has changed.
--
-- @centrals@ — A list of CBCentral objects to receive the update. Note that centrals which have not subscribed to                          characteristic will be ignored. If nil, all centrals that are subscribed to characteristic will be updated.
--
-- Sends an updated characteristic value to one or more centrals, via a notification or indication. If value exceeds							{
--
-- maximumUpdateValueLength}, it will be truncated to fit.
--
-- @return                 <i>YES</i> if the update could be sent, or <i>NO</i> if the underlying transmit queue is full. If <i>NO</i> was returned,
-- the delegate method @link peripheralManagerIsReadyToUpdateSubscribers:
--
-- will be called once space has become                          available, and the update should be re-sent if so desired.
--
-- See: peripheralManager:central:didSubscribeToCharacteristic:
--
-- See: peripheralManager:central:didUnsubscribeFromCharacteristic:
--
-- See: peripheralManagerIsReadyToUpdateSubscribers:
--
-- maximumUpdateValueLength
--
-- ObjC selector: @- updateValue:forCharacteristic:onSubscribedCentrals:@
updateValue_forCharacteristic_onSubscribedCentrals :: (IsCBPeripheralManager cbPeripheralManager, IsNSData value, IsCBMutableCharacteristic characteristic, IsNSArray centrals) => cbPeripheralManager -> value -> characteristic -> centrals -> IO Bool
updateValue_forCharacteristic_onSubscribedCentrals cbPeripheralManager  value characteristic centrals =
withObjCPtr value $ \raw_value ->
  withObjCPtr characteristic $ \raw_characteristic ->
    withObjCPtr centrals $ \raw_centrals ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbPeripheralManager (mkSelector "updateValue:forCharacteristic:onSubscribedCentrals:") retCULong [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_characteristic :: Ptr ()), argPtr (castPtr raw_centrals :: Ptr ())]

-- | publishL2CAPChannelWithEncryption:
--
-- @encryptionRequired@ — YES if the service requires the link to be encrypted before a stream can be established.  NO if the service can be used over									an unsecured link.
--
-- Create a listener for incoming L2CAP Channel connections.  The system will determine an unused PSM at the time of publishing, which will be returned					with
--
-- peripheralManager:didPublishL2CAPChannel:error:
--
-- .  L2CAP Channels are not discoverable by themselves, so it is the application's					responsibility to handle PSM discovery on the client.
--
-- ObjC selector: @- publishL2CAPChannelWithEncryption:@
publishL2CAPChannelWithEncryption :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> Bool -> IO ()
publishL2CAPChannelWithEncryption cbPeripheralManager  encryptionRequired =
  sendMsg cbPeripheralManager (mkSelector "publishL2CAPChannelWithEncryption:") retVoid [argCULong (if encryptionRequired then 1 else 0)]

-- | unpublishL2CAPChannel:
--
-- @PSM@ — The service PSM to be removed from the system.
--
-- Removes a published service from the local system.  No new connections for this PSM will be accepted, and any existing L2CAP channels					using this PSM will be closed.
--
-- ObjC selector: @- unpublishL2CAPChannel:@
unpublishL2CAPChannel :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> CUShort -> IO ()
unpublishL2CAPChannel cbPeripheralManager  psm =
  sendMsg cbPeripheralManager (mkSelector "unpublishL2CAPChannel:") retVoid [argCUInt (fromIntegral psm)]

-- | isAdvertising
--
-- Whether or not the peripheral is currently advertising data.
--
-- ObjC selector: @- isAdvertising@
isAdvertising :: IsCBPeripheralManager cbPeripheralManager => cbPeripheralManager -> IO Bool
isAdvertising cbPeripheralManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbPeripheralManager (mkSelector "isAdvertising") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDelegate:queue:@
initWithDelegate_queueSelector :: Selector
initWithDelegate_queueSelector = mkSelector "initWithDelegate:queue:"

-- | @Selector@ for @initWithDelegate:queue:options:@
initWithDelegate_queue_optionsSelector :: Selector
initWithDelegate_queue_optionsSelector = mkSelector "initWithDelegate:queue:options:"

-- | @Selector@ for @startAdvertising:@
startAdvertisingSelector :: Selector
startAdvertisingSelector = mkSelector "startAdvertising:"

-- | @Selector@ for @stopAdvertising@
stopAdvertisingSelector :: Selector
stopAdvertisingSelector = mkSelector "stopAdvertising"

-- | @Selector@ for @setDesiredConnectionLatency:forCentral:@
setDesiredConnectionLatency_forCentralSelector :: Selector
setDesiredConnectionLatency_forCentralSelector = mkSelector "setDesiredConnectionLatency:forCentral:"

-- | @Selector@ for @addService:@
addServiceSelector :: Selector
addServiceSelector = mkSelector "addService:"

-- | @Selector@ for @removeService:@
removeServiceSelector :: Selector
removeServiceSelector = mkSelector "removeService:"

-- | @Selector@ for @removeAllServices@
removeAllServicesSelector :: Selector
removeAllServicesSelector = mkSelector "removeAllServices"

-- | @Selector@ for @respondToRequest:withResult:@
respondToRequest_withResultSelector :: Selector
respondToRequest_withResultSelector = mkSelector "respondToRequest:withResult:"

-- | @Selector@ for @updateValue:forCharacteristic:onSubscribedCentrals:@
updateValue_forCharacteristic_onSubscribedCentralsSelector :: Selector
updateValue_forCharacteristic_onSubscribedCentralsSelector = mkSelector "updateValue:forCharacteristic:onSubscribedCentrals:"

-- | @Selector@ for @publishL2CAPChannelWithEncryption:@
publishL2CAPChannelWithEncryptionSelector :: Selector
publishL2CAPChannelWithEncryptionSelector = mkSelector "publishL2CAPChannelWithEncryption:"

-- | @Selector@ for @unpublishL2CAPChannel:@
unpublishL2CAPChannelSelector :: Selector
unpublishL2CAPChannelSelector = mkSelector "unpublishL2CAPChannel:"

-- | @Selector@ for @isAdvertising@
isAdvertisingSelector :: Selector
isAdvertisingSelector = mkSelector "isAdvertising"

