{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cancelPeripheralConnectionSelector
  , connectPeripheral_optionsSelector
  , delegateSelector
  , initSelector
  , initWithDelegate_queueSelector
  , initWithDelegate_queue_optionsSelector
  , isScanningSelector
  , registerForConnectionEventsWithOptionsSelector
  , retrieveConnectedPeripheralsWithServicesSelector
  , retrievePeripheralsWithIdentifiersSelector
  , scanForPeripheralsWithServices_optionsSelector
  , setDelegateSelector
  , stopScanSelector
  , supportsFeaturesSelector

  -- * Enum types
  , CBCentralManagerFeature(CBCentralManagerFeature)
  , pattern CBCentralManagerFeatureExtendedScanAndConnect

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' supportsFeaturesSelector features

-- | @- init@
init_ :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO (Id CBCentralManager)
init_ cbCentralManager =
  sendOwnedMessage cbCentralManager initSelector

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
initWithDelegate_queue cbCentralManager delegate queue =
  sendOwnedMessage cbCentralManager initWithDelegate_queueSelector delegate (toNSObject queue)

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
initWithDelegate_queue_options cbCentralManager delegate queue options =
  sendOwnedMessage cbCentralManager initWithDelegate_queue_optionsSelector delegate (toNSObject queue) (toNSDictionary options)

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
retrievePeripheralsWithIdentifiers cbCentralManager identifiers =
  sendMessage cbCentralManager retrievePeripheralsWithIdentifiersSelector (toNSArray identifiers)

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
retrieveConnectedPeripheralsWithServices cbCentralManager serviceUUIDs =
  sendMessage cbCentralManager retrieveConnectedPeripheralsWithServicesSelector (toNSArray serviceUUIDs)

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
scanForPeripheralsWithServices_options cbCentralManager serviceUUIDs options =
  sendMessage cbCentralManager scanForPeripheralsWithServices_optionsSelector (toNSArray serviceUUIDs) (toNSDictionary options)

-- | stopScan:
--
-- Stops scanning for peripherals.
--
-- ObjC selector: @- stopScan@
stopScan :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO ()
stopScan cbCentralManager =
  sendMessage cbCentralManager stopScanSelector

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
connectPeripheral_options cbCentralManager peripheral options =
  sendMessage cbCentralManager connectPeripheral_optionsSelector (toCBPeripheral peripheral) (toNSDictionary options)

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
cancelPeripheralConnection cbCentralManager peripheral =
  sendMessage cbCentralManager cancelPeripheralConnectionSelector (toCBPeripheral peripheral)

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
registerForConnectionEventsWithOptions cbCentralManager options =
  sendMessage cbCentralManager registerForConnectionEventsWithOptionsSelector (toNSDictionary options)

-- | delegate
--
-- The delegate object that will receive central events.
--
-- ObjC selector: @- delegate@
delegate :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO RawId
delegate cbCentralManager =
  sendMessage cbCentralManager delegateSelector

-- | delegate
--
-- The delegate object that will receive central events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCBCentralManager cbCentralManager => cbCentralManager -> RawId -> IO ()
setDelegate cbCentralManager value =
  sendMessage cbCentralManager setDelegateSelector value

-- | isScanning
--
-- Whether or not the central is currently scanning.
--
-- ObjC selector: @- isScanning@
isScanning :: IsCBCentralManager cbCentralManager => cbCentralManager -> IO Bool
isScanning cbCentralManager =
  sendMessage cbCentralManager isScanningSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsFeatures:@
supportsFeaturesSelector :: Selector '[CBCentralManagerFeature] Bool
supportsFeaturesSelector = mkSelector "supportsFeatures:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CBCentralManager)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDelegate:queue:@
initWithDelegate_queueSelector :: Selector '[RawId, Id NSObject] (Id CBCentralManager)
initWithDelegate_queueSelector = mkSelector "initWithDelegate:queue:"

-- | @Selector@ for @initWithDelegate:queue:options:@
initWithDelegate_queue_optionsSelector :: Selector '[RawId, Id NSObject, Id NSDictionary] (Id CBCentralManager)
initWithDelegate_queue_optionsSelector = mkSelector "initWithDelegate:queue:options:"

-- | @Selector@ for @retrievePeripheralsWithIdentifiers:@
retrievePeripheralsWithIdentifiersSelector :: Selector '[Id NSArray] (Id NSArray)
retrievePeripheralsWithIdentifiersSelector = mkSelector "retrievePeripheralsWithIdentifiers:"

-- | @Selector@ for @retrieveConnectedPeripheralsWithServices:@
retrieveConnectedPeripheralsWithServicesSelector :: Selector '[Id NSArray] (Id NSArray)
retrieveConnectedPeripheralsWithServicesSelector = mkSelector "retrieveConnectedPeripheralsWithServices:"

-- | @Selector@ for @scanForPeripheralsWithServices:options:@
scanForPeripheralsWithServices_optionsSelector :: Selector '[Id NSArray, Id NSDictionary] ()
scanForPeripheralsWithServices_optionsSelector = mkSelector "scanForPeripheralsWithServices:options:"

-- | @Selector@ for @stopScan@
stopScanSelector :: Selector '[] ()
stopScanSelector = mkSelector "stopScan"

-- | @Selector@ for @connectPeripheral:options:@
connectPeripheral_optionsSelector :: Selector '[Id CBPeripheral, Id NSDictionary] ()
connectPeripheral_optionsSelector = mkSelector "connectPeripheral:options:"

-- | @Selector@ for @cancelPeripheralConnection:@
cancelPeripheralConnectionSelector :: Selector '[Id CBPeripheral] ()
cancelPeripheralConnectionSelector = mkSelector "cancelPeripheralConnection:"

-- | @Selector@ for @registerForConnectionEventsWithOptions:@
registerForConnectionEventsWithOptionsSelector :: Selector '[Id NSDictionary] ()
registerForConnectionEventsWithOptionsSelector = mkSelector "registerForConnectionEventsWithOptions:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @isScanning@
isScanningSelector :: Selector '[] Bool
isScanningSelector = mkSelector "isScanning"

