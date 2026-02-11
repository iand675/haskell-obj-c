{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBCentralManager
--
-- Entry point to the central role. Commands should only be issued when its state is CBCentralManagerStatePoweredOn.
--
-- Generated bindings for @CBCentralManager@.
module ObjC.CoreBluetooth.CBCentralManager
  ( CBCentralManager
  , IsCBCentralManager(..)
  , supportsFeatures
  , init_
  , initWithDelegate_queue
  , initWithDelegate_queue_options
  , retrievePeripheralsWithIdentifiers
  , retrieveConnectedPeripheralsWithServices
  , scanForPeripheralsWithServices_options
  , stopScan
  , connectPeripheral_options
  , cancelPeripheralConnection
  , registerForConnectionEventsWithOptions
  , delegate
  , setDelegate
  , isScanning
  , supportsFeaturesSelector
  , initSelector
  , initWithDelegate_queueSelector
  , initWithDelegate_queue_optionsSelector
  , retrievePeripheralsWithIdentifiersSelector
  , retrieveConnectedPeripheralsWithServicesSelector
  , scanForPeripheralsWithServices_optionsSelector
  , stopScanSelector
  , connectPeripheral_optionsSelector
  , cancelPeripheralConnectionSelector
  , registerForConnectionEventsWithOptionsSelector
  , delegateSelector
  , setDelegateSelector
  , isScanningSelector

  -- * Enum types
  , CBCentralManagerFeature(CBCentralManagerFeature)
  , pattern CBCentralManagerFeatureExtendedScanAndConnect

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

-- | supportsFeatures
--
-- @features@ — One or more features you would like to check if supported.
--
-- Returns a boolean value representing the support for the provided features.
--
-- ObjC selector: @+ supportsFeatures:@
supportsFeatures :: CBCentralManagerFeature -> IO Bool
supportsFeatures features =
  do
    cls' <- getRequiredClass "CBCentralManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsFeatures:") retCULong [argCULong (coerce features)]

-- | @- init@
init_ :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO (Id CBCentralManager)
init_ cbCentralManager  =
    sendMsg cbCentralManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithDelegate:queue:
--
-- @delegate@ — The delegate that will receive central role events.
--
-- @queue@ — The dispatch queue on which the events will be dispatched.
--
-- The initialization call. The events of the central role will be dispatched on the provided queue.                  If nil, the main queue will be used.
--
-- ObjC selector: @- initWithDelegate:queue:@
initWithDelegate_queue :: (IsCBCentralManager cbCentralManager, IsNSObject queue) => cbCentralManager -> RawId -> queue -> IO (Id CBCentralManager)
initWithDelegate_queue cbCentralManager  delegate queue =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cbCentralManager (mkSelector "initWithDelegate:queue:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | initWithDelegate:queue:options:
--
-- @delegate@ — The delegate that will receive central role events.
--
-- @queue@ — The dispatch queue on which the events will be dispatched.
--
-- @options@ — An optional dictionary specifying options for the manager.
--
-- The initialization call. The events of the central role will be dispatched on the provided queue.                  If nil, the main queue will be used.
--
-- CBCentralManagerOptionShowPowerAlertKey
--
-- CBCentralManagerOptionRestoreIdentifierKey
--
-- ObjC selector: @- initWithDelegate:queue:options:@
initWithDelegate_queue_options :: (IsCBCentralManager cbCentralManager, IsNSObject queue, IsNSDictionary options) => cbCentralManager -> RawId -> queue -> options -> IO (Id CBCentralManager)
initWithDelegate_queue_options cbCentralManager  delegate queue options =
  withObjCPtr queue $ \raw_queue ->
    withObjCPtr options $ \raw_options ->
        sendMsg cbCentralManager (mkSelector "initWithDelegate:queue:options:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | retrievePeripheralsWithIdentifiers:
--
-- @identifiers@ — A list of NSUUID objects.
--
-- Attempts to retrieve the CBPeripheral object(s) with the corresponding identifiers.
--
-- Returns: A list of CBPeripheral objects.
--
-- ObjC selector: @- retrievePeripheralsWithIdentifiers:@
retrievePeripheralsWithIdentifiers :: (IsCBCentralManager cbCentralManager, IsNSArray identifiers) => cbCentralManager -> identifiers -> IO (Id NSArray)
retrievePeripheralsWithIdentifiers cbCentralManager  identifiers =
  withObjCPtr identifiers $ \raw_identifiers ->
      sendMsg cbCentralManager (mkSelector "retrievePeripheralsWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ())] >>= retainedObject . castPtr

-- | retrieveConnectedPeripheralsWithServices
--
-- Retrieves all peripherals that are connected to the system and implement any of the services listed in serviceUUIDs.				Note that this set can include peripherals which were connected by other applications, which will need to be connected locally				via {
--
-- connectPeripheral:options:} before they can be used.
--
-- @return		A list of <code>CBPeripheral</code> objects.
--
-- ObjC selector: @- retrieveConnectedPeripheralsWithServices:@
retrieveConnectedPeripheralsWithServices :: (IsCBCentralManager cbCentralManager, IsNSArray serviceUUIDs) => cbCentralManager -> serviceUUIDs -> IO (Id NSArray)
retrieveConnectedPeripheralsWithServices cbCentralManager  serviceUUIDs =
  withObjCPtr serviceUUIDs $ \raw_serviceUUIDs ->
      sendMsg cbCentralManager (mkSelector "retrieveConnectedPeripheralsWithServices:") (retPtr retVoid) [argPtr (castPtr raw_serviceUUIDs :: Ptr ())] >>= retainedObject . castPtr

-- | scanForPeripheralsWithServices:options:
--
-- @serviceUUIDs@ — A list of CBUUID objects representing the service(s) to scan for.
--
-- @options@ — An optional dictionary specifying options for the scan.
--
-- Starts scanning for peripherals that are advertising any of the services listed in serviceUUIDs. Although strongly discouraged,                      if serviceUUIDs is nil all discovered peripherals will be returned. If the central is already scanning with different                      serviceUUIDs or options, the provided parameters will replace them.                      Applications that have specified the bluetooth-central background mode are allowed to scan while backgrounded, with two                      caveats: the scan must specify one or more service types in serviceUUIDs, and the CBCentralManagerScanOptionAllowDuplicatesKey                      scan option will be ignored.
--
-- See: centralManager:didDiscoverPeripheral:advertisementData:RSSI:
--
-- CBCentralManagerScanOptionAllowDuplicatesKey
--
-- CBCentralManagerScanOptionSolicitedServiceUUIDsKey
--
-- ObjC selector: @- scanForPeripheralsWithServices:options:@
scanForPeripheralsWithServices_options :: (IsCBCentralManager cbCentralManager, IsNSArray serviceUUIDs, IsNSDictionary options) => cbCentralManager -> serviceUUIDs -> options -> IO ()
scanForPeripheralsWithServices_options cbCentralManager  serviceUUIDs options =
  withObjCPtr serviceUUIDs $ \raw_serviceUUIDs ->
    withObjCPtr options $ \raw_options ->
        sendMsg cbCentralManager (mkSelector "scanForPeripheralsWithServices:options:") retVoid [argPtr (castPtr raw_serviceUUIDs :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | stopScan:
--
-- Stops scanning for peripherals.
--
-- ObjC selector: @- stopScan@
stopScan :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO ()
stopScan cbCentralManager  =
    sendMsg cbCentralManager (mkSelector "stopScan") retVoid []

-- | connectPeripheral:options:
--
-- @peripheral@ — The CBPeripheral to be connected.
--
-- @options@ — An optional dictionary specifying connection behavior options.
--
-- Initiates a connection to peripheral. Connection attempts never time out and, depending on the outcome, will result                      in a call to either {
--
-- centralManager:didConnectPeripheral:} or {@link centralManager:didFailToConnectPeripheral:error:}.
-- Pending attempts are cancelled automatically upon deallocation of <i>peripheral</i>, and explicitly via {@link cancelPeripheralConnection}.
--
-- @see                centralManager:didConnectPeripheral:
-- @see                centralManager:didFailToConnectPeripheral:error:
-- @seealso            CBConnectPeripheralOptionNotifyOnConnectionKey
-- @seealso            CBConnectPeripheralOptionNotifyOnDisconnectionKey
-- @seealso            CBConnectPeripheralOptionNotifyOnNotificationKey
-- @seealso            CBConnectPeripheralOptionEnableTransportBridgingKey
-- @seealso			CBConnectPeripheralOptionRequiresANCS
-- @seealso            CBConnectPeripheralOptionEnableAutoReconnect
--
-- ObjC selector: @- connectPeripheral:options:@
connectPeripheral_options :: (IsCBCentralManager cbCentralManager, IsCBPeripheral peripheral, IsNSDictionary options) => cbCentralManager -> peripheral -> options -> IO ()
connectPeripheral_options cbCentralManager  peripheral options =
  withObjCPtr peripheral $ \raw_peripheral ->
    withObjCPtr options $ \raw_options ->
        sendMsg cbCentralManager (mkSelector "connectPeripheral:options:") retVoid [argPtr (castPtr raw_peripheral :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | cancelPeripheralConnection:
--
-- @peripheral@ — A CBPeripheral.
--
-- Cancels an active or pending connection to peripheral. Note that this is non-blocking, and any CBPeripheral                      commands that are still pending to peripheral may or may not complete.
--
-- See: centralManager:didDisconnectPeripheral:error:
--
-- ObjC selector: @- cancelPeripheralConnection:@
cancelPeripheralConnection :: (IsCBCentralManager cbCentralManager, IsCBPeripheral peripheral) => cbCentralManager -> peripheral -> IO ()
cancelPeripheralConnection cbCentralManager  peripheral =
  withObjCPtr peripheral $ \raw_peripheral ->
      sendMsg cbCentralManager (mkSelector "cancelPeripheralConnection:") retVoid [argPtr (castPtr raw_peripheral :: Ptr ())]

-- | registerForConnectionEventsWithOptions:
--
-- @options@ — A dictionary specifying connection event options.
--
-- Calls {
--
-- centralManager:connectionEventDidOccur:forPeripheral:} when a connection event occurs matching any of the given options.
-- Passing nil in the option parameter clears any prior registered matching options.
--
-- @see				centralManager:connectionEventDidOccur:forPeripheral:
-- @seealso        	CBConnectionEventMatchingOptionServiceUUIDs
-- @seealso            CBConnectionEventMatchingOptionPeripheralUUIDs
--
-- ObjC selector: @- registerForConnectionEventsWithOptions:@
registerForConnectionEventsWithOptions :: (IsCBCentralManager cbCentralManager, IsNSDictionary options) => cbCentralManager -> options -> IO ()
registerForConnectionEventsWithOptions cbCentralManager  options =
  withObjCPtr options $ \raw_options ->
      sendMsg cbCentralManager (mkSelector "registerForConnectionEventsWithOptions:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- | delegate
--
-- The delegate object that will receive central events.
--
-- ObjC selector: @- delegate@
delegate :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO RawId
delegate cbCentralManager  =
    fmap (RawId . castPtr) $ sendMsg cbCentralManager (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The delegate object that will receive central events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCBCentralManager cbCentralManager => cbCentralManager -> RawId -> IO ()
setDelegate cbCentralManager  value =
    sendMsg cbCentralManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | isScanning
--
-- Whether or not the central is currently scanning.
--
-- ObjC selector: @- isScanning@
isScanning :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO Bool
isScanning cbCentralManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbCentralManager (mkSelector "isScanning") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsFeatures:@
supportsFeaturesSelector :: Selector
supportsFeaturesSelector = mkSelector "supportsFeatures:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDelegate:queue:@
initWithDelegate_queueSelector :: Selector
initWithDelegate_queueSelector = mkSelector "initWithDelegate:queue:"

-- | @Selector@ for @initWithDelegate:queue:options:@
initWithDelegate_queue_optionsSelector :: Selector
initWithDelegate_queue_optionsSelector = mkSelector "initWithDelegate:queue:options:"

-- | @Selector@ for @retrievePeripheralsWithIdentifiers:@
retrievePeripheralsWithIdentifiersSelector :: Selector
retrievePeripheralsWithIdentifiersSelector = mkSelector "retrievePeripheralsWithIdentifiers:"

-- | @Selector@ for @retrieveConnectedPeripheralsWithServices:@
retrieveConnectedPeripheralsWithServicesSelector :: Selector
retrieveConnectedPeripheralsWithServicesSelector = mkSelector "retrieveConnectedPeripheralsWithServices:"

-- | @Selector@ for @scanForPeripheralsWithServices:options:@
scanForPeripheralsWithServices_optionsSelector :: Selector
scanForPeripheralsWithServices_optionsSelector = mkSelector "scanForPeripheralsWithServices:options:"

-- | @Selector@ for @stopScan@
stopScanSelector :: Selector
stopScanSelector = mkSelector "stopScan"

-- | @Selector@ for @connectPeripheral:options:@
connectPeripheral_optionsSelector :: Selector
connectPeripheral_optionsSelector = mkSelector "connectPeripheral:options:"

-- | @Selector@ for @cancelPeripheralConnection:@
cancelPeripheralConnectionSelector :: Selector
cancelPeripheralConnectionSelector = mkSelector "cancelPeripheralConnection:"

-- | @Selector@ for @registerForConnectionEventsWithOptions:@
registerForConnectionEventsWithOptionsSelector :: Selector
registerForConnectionEventsWithOptionsSelector = mkSelector "registerForConnectionEventsWithOptions:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @isScanning@
isScanningSelector :: Selector
isScanningSelector = mkSelector "isScanning"

