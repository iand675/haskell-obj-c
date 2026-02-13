{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothDevice
--
-- An instance of IOBluetoothDevice represents a single remote Bluetooth device.
--
-- An IOBluetoothDevice object may exist independent of the existence of a baseband connection                with the target device.  Using this object, a client can request creation and destruction of baseband                connections, and request the opening of L2CAP and RFCOMM channels on the remote device.  Many of the other                APIs in the IOBluetooth framework will return this object, or it's C counterpart (IOBluetoothDeviceRef).
--
-- Generated bindings for @IOBluetoothDevice@.
module ObjC.IOBluetooth.IOBluetoothDevice
  ( IOBluetoothDevice
  , IsIOBluetoothDevice(..)
  , registerForConnectNotifications_selector
  , registerForDisconnectNotification_selector
  , deviceWithAddress
  , withAddress
  , deviceWithAddressString
  , withDeviceRef
  , getDeviceRef
  , openL2CAPChannelSync_withPSM_delegate
  , openL2CAPChannelAsync_withPSM_delegate
  , openL2CAPChannel_findExisting_newChannel
  , sendL2CAPEchoRequest_length
  , openRFCOMMChannel_channel
  , openRFCOMMChannelSync_withChannelID_delegate
  , openRFCOMMChannelAsync_withChannelID_delegate
  , getClassOfDevice
  , getServiceClassMajor
  , getDeviceClassMajor
  , getDeviceClassMinor
  , getName
  , getNameOrAddress
  , getLastNameUpdate
  , getAddress
  , getAddressString
  , getPageScanRepetitionMode
  , getPageScanPeriodMode
  , getPageScanMode
  , getClockOffset
  , getLastInquiryUpdate
  , rssi
  , rawRSSI
  , isConnected
  , openConnection
  , openConnection_withPageTimeout_authenticationRequired
  , closeConnection
  , remoteNameRequest
  , remoteNameRequest_withPageTimeout
  , requestAuthentication
  , getConnectionHandle
  , isIncoming
  , getLinkType
  , getEncryptionMode
  , performSDPQuery
  , performSDPQuery_uuids
  , getServices
  , getLastServicesUpdate
  , getServiceRecordForUUID
  , favoriteDevices
  , isFavorite
  , addToFavorites
  , removeFromFavorites
  , recentDevices
  , recentAccessDate
  , pairedDevices
  , isPaired
  , setSupervisionTimeout
  , openL2CAPChannelSync_withPSM_withConfiguration_delegate
  , openL2CAPChannelAsync_withPSM_withConfiguration_delegate
  , awakeAfterUsingCoder
  , handsFreeAudioGatewayDriverID
  , handsFreeAudioGatewayServiceRecord
  , handsFreeDeviceDriverID
  , handsFreeDeviceServiceRecord
  , classOfDevice
  , serviceClassMajor
  , deviceClassMajor
  , deviceClassMinor
  , name
  , nameOrAddress
  , lastNameUpdate
  , addressString
  , connectionHandle
  , services
  , handsFreeAudioGateway
  , handsFreeDevice
  , addToFavoritesSelector
  , addressStringSelector
  , awakeAfterUsingCoderSelector
  , classOfDeviceSelector
  , closeConnectionSelector
  , connectionHandleSelector
  , deviceClassMajorSelector
  , deviceClassMinorSelector
  , deviceWithAddressSelector
  , deviceWithAddressStringSelector
  , favoriteDevicesSelector
  , getAddressSelector
  , getAddressStringSelector
  , getClassOfDeviceSelector
  , getClockOffsetSelector
  , getConnectionHandleSelector
  , getDeviceClassMajorSelector
  , getDeviceClassMinorSelector
  , getDeviceRefSelector
  , getEncryptionModeSelector
  , getLastInquiryUpdateSelector
  , getLastNameUpdateSelector
  , getLastServicesUpdateSelector
  , getLinkTypeSelector
  , getNameOrAddressSelector
  , getNameSelector
  , getPageScanModeSelector
  , getPageScanPeriodModeSelector
  , getPageScanRepetitionModeSelector
  , getServiceClassMajorSelector
  , getServiceRecordForUUIDSelector
  , getServicesSelector
  , handsFreeAudioGatewayDriverIDSelector
  , handsFreeAudioGatewaySelector
  , handsFreeAudioGatewayServiceRecordSelector
  , handsFreeDeviceDriverIDSelector
  , handsFreeDeviceSelector
  , handsFreeDeviceServiceRecordSelector
  , isConnectedSelector
  , isFavoriteSelector
  , isIncomingSelector
  , isPairedSelector
  , lastNameUpdateSelector
  , nameOrAddressSelector
  , nameSelector
  , openConnectionSelector
  , openConnection_withPageTimeout_authenticationRequiredSelector
  , openL2CAPChannelAsync_withPSM_delegateSelector
  , openL2CAPChannelAsync_withPSM_withConfiguration_delegateSelector
  , openL2CAPChannelSync_withPSM_delegateSelector
  , openL2CAPChannelSync_withPSM_withConfiguration_delegateSelector
  , openL2CAPChannel_findExisting_newChannelSelector
  , openRFCOMMChannelAsync_withChannelID_delegateSelector
  , openRFCOMMChannelSync_withChannelID_delegateSelector
  , openRFCOMMChannel_channelSelector
  , pairedDevicesSelector
  , performSDPQuerySelector
  , performSDPQuery_uuidsSelector
  , rawRSSISelector
  , recentAccessDateSelector
  , recentDevicesSelector
  , registerForConnectNotifications_selectorSelector
  , registerForDisconnectNotification_selectorSelector
  , remoteNameRequestSelector
  , remoteNameRequest_withPageTimeoutSelector
  , removeFromFavoritesSelector
  , requestAuthenticationSelector
  , rssiSelector
  , sendL2CAPEchoRequest_lengthSelector
  , serviceClassMajorSelector
  , servicesSelector
  , setSupervisionTimeoutSelector
  , withAddressSelector
  , withDeviceRefSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | registerForConnectNotifications:selector:
--
-- Allows a client to register for device connect notifications for any connection.
--
-- The given selector will be called on the target observer whenever any device connection is made.                                The selector should accept two arguments.  The first is the user notification object.  The second                                is the device that was connected.
--
-- @observer@ — Target observer object
--
-- @inSelector@ — Selector to be sent to the observer when a new connection is made
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding device connect notification.                                To unregister the notification, call -unregister on the returned IOBluetoothUserNotification                                object.  If an error is encountered creating the notification, nil is returned.  The returned                                IOBluetoothUserNotification object will be valid for as long as the notification is registered.                                It is not necessary to retain the result.  Once -unregister is called on it, it will no longer                                be valid.
--
-- ObjC selector: @+ registerForConnectNotifications:selector:@
registerForConnectNotifications_selector :: RawId -> Sel -> IO (Id IOBluetoothUserNotification)
registerForConnectNotifications_selector observer inSelector =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' registerForConnectNotifications_selectorSelector observer inSelector

-- | registerForDisconnectNotification:selector:
--
-- Allows a client to register for device disconnect notification.
--
-- The given selector will be called on the target observer when the target device's connection is                                closed.  The selector should contain two arguments.  The first is the user notification object.  The second                                is the IOBluetoothDevice that was disconnected.
--
-- @observer@ — Target observer object
--
-- @inSelector@ — Selector to be sent to the observer when the connection is destroyed
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding device disconnect notification.                                To unregister the notification, call -unregister of the returned IOBluetoothUserNotification                                object.  If an error is encountered creating the notification, nil is returned.
--
-- ObjC selector: @- registerForDisconnectNotification:selector:@
registerForDisconnectNotification_selector :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> RawId -> Sel -> IO (Id IOBluetoothUserNotification)
registerForDisconnectNotification_selector ioBluetoothDevice observer inSelector =
  sendMessage ioBluetoothDevice registerForDisconnectNotification_selectorSelector observer inSelector

-- | deviceWithAddress:
--
-- Returns the IOBluetoothDevice object for the given BluetoothDeviceAddress
--
-- Within a single application, there will be only one instance of IOBluetoothDevice for a                given remote device address.
--
-- @address@ — Pointer to a BluetoothDeviceAddress for which an IOBluetoothDevice instance is desired
--
-- Returns: Returns the IOBluetoothDevice object for the given BluetoothDeviceAddress
--
-- ObjC selector: @+ deviceWithAddress:@
deviceWithAddress :: Const RawId -> IO (Id IOBluetoothDevice)
deviceWithAddress address =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' deviceWithAddressSelector address

-- | @+ withAddress:@
withAddress :: Const RawId -> IO (Id IOBluetoothDevice)
withAddress address =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' withAddressSelector address

-- | deviceWithAddressString:
--
-- Returns the IOBluetoothDevice object for the given BluetoothDeviceAddress
--
-- Within a single application, there will be only one instance of IOBluetoothDevice for a given remote device address.
--
-- @address@ — Pointer to an NSString containing the BD_ADDR for which an IOBluetoothDevice instance is desired.  The string should be of the form xx:xx:xx:xx:xx:xx
--
-- Returns: Returns the IOBluetoothDevice object for the given BluetoothDeviceAddress
--
-- ObjC selector: @+ deviceWithAddressString:@
deviceWithAddressString :: IsNSString address => address -> IO (Id IOBluetoothDevice)
deviceWithAddressString address =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' deviceWithAddressStringSelector (toNSString address)

-- | withDeviceRef:
--
-- Method call to convert an IOBluetoothDeviceRef into an IOBluetoothDevice *.
--
-- IOBluetoothDeviceRef and it's API are deprecated.  An IOBluetoothDeviceRef can be cast to a IOBluetoothDevice *
--
-- @deviceRef@ — IOBluetoothDeviceRef for which an IOBluetoothDevice * is desired.
--
-- Returns: Returns the IOBluetoothDevice * for the given IOBluetoothDeviceRef.
--
-- ObjC selector: @+ withDeviceRef:@
withDeviceRef :: Ptr () -> IO (Id IOBluetoothDevice)
withDeviceRef deviceRef =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' withDeviceRefSelector deviceRef

-- | getDeviceRef
--
-- Returns an IOBluetoothDeviceRef representation of the target IOBluetoothDevice object.
--
-- IOBluetoothDeviceRef and it's API are deprecated.  An IOBluetoothDeviceRef can be cast to a IOBluetoothDevice *
--
-- Returns: Returns an IOBluetoothDeviceRef representation of the target IOBluetoothDevice object.
--
-- ObjC selector: @- getDeviceRef@
getDeviceRef :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Ptr ())
getDeviceRef ioBluetoothDevice =
  sendMessage ioBluetoothDevice getDeviceRefSelector

-- | openL2CAPChannelSync:withPSM:delegate:
--
-- Opens a new L2CAP channel to the target device. Returns only after the channel is opened.
--
-- This method will begin the process of opening a new L2CAP channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The L2CAP                channel open process will not complete until the client has registered an incoming data                listener on the new channel.  This prevents a situation where the channel succeeds                in being configured and opened and receives data before the client is listening and                is ready for it.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @newChannel@ — A pointer to an IOBluetoothL2CAPChannel object to receive the L2CAP channel                                requested to be opened.  The newChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @withPSM@ — The L2CAP PSM value for the new channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the l2cap uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothL2CAPChannel.h" in the definition                                of the protocol IOBluetoothL2CAPChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                L2CAP channel was found).
--
-- ObjC selector: @- openL2CAPChannelSync:withPSM:delegate:@
openL2CAPChannelSync_withPSM_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothL2CAPChannel newChannel) => ioBluetoothDevice -> newChannel -> CUShort -> RawId -> IO CInt
openL2CAPChannelSync_withPSM_delegate ioBluetoothDevice newChannel psm channelDelegate =
  sendMessage ioBluetoothDevice openL2CAPChannelSync_withPSM_delegateSelector (toIOBluetoothL2CAPChannel newChannel) psm channelDelegate

-- | openL2CAPChannelAsync:withPSM:delegate:
--
-- Opens a new L2CAP channel to the target device. Returns immediately after starting the opening process.
--
-- This method will begin the process of opening a new L2CAP channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The L2CAP                channel open process will not complete until the client has registered an incoming data                listener on the new channel.  This prevents a situation where the channel succeeds                in being configured and opened and receives data before the client is listening and                is ready for it.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @newChannel@ — A pointer to an IOBluetoothL2CAPChannel object to receive the L2CAP channel                                requested to be opened.  The newChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @psm@ — The L2CAP PSM value for the new channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the l2cap uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothL2CAPChannel.h" in the definition                                of the protocol IOBluetoothL2CAPChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                L2CAP channel was found).
--
-- ObjC selector: @- openL2CAPChannelAsync:withPSM:delegate:@
openL2CAPChannelAsync_withPSM_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothL2CAPChannel newChannel) => ioBluetoothDevice -> newChannel -> CUShort -> RawId -> IO CInt
openL2CAPChannelAsync_withPSM_delegate ioBluetoothDevice newChannel psm channelDelegate =
  sendMessage ioBluetoothDevice openL2CAPChannelAsync_withPSM_delegateSelector (toIOBluetoothL2CAPChannel newChannel) psm channelDelegate

-- | openL2CAPChannel:findExisting:newChannel:
--
-- Opens a new L2CAP channel to the target device. Returns immedialty after starting the opening process.
--
-- This method will begin the process of opening a new L2CAP channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The L2CAP                channel open process will not complete until the client has registered an incoming data                listener on the new channel.  This prevents a situation where the channel succeeds                in being configured and opened and receives data before the client is listening and                is ready for it.
--
-- @psm@ — The L2CAP PSM value for the new channel.
--
-- @findExisting@ — This value should be set to TRUE if it should look for an existing channel                                with the PSM.  Typically this value will be FALSE.  It should be TRUE only                                in the case where a single channel is allowed by the spec for the given PSM.
--
-- @newChannel@ — A pointer to an IOBluetoothL2CAPChannel object to receive the L2CAP channel                                requested to be opened.  The newChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                L2CAP channel was found).
--
-- ObjC selector: @- openL2CAPChannel:findExisting:newChannel:@
openL2CAPChannel_findExisting_newChannel :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothL2CAPChannel newChannel) => ioBluetoothDevice -> CUShort -> Bool -> newChannel -> IO CInt
openL2CAPChannel_findExisting_newChannel ioBluetoothDevice psm findExisting newChannel =
  sendMessage ioBluetoothDevice openL2CAPChannel_findExisting_newChannelSelector psm findExisting (toIOBluetoothL2CAPChannel newChannel)

-- | sendL2CAPEchoRequest:length:
--
-- Send an echo request over the L2CAP connection to a remote device.
--
-- The current implementation returns when the request has been sent, but does not indicate when                a response is received.  Also, the baseband connection must be up for the echo request to be sent.                In the future, this method will also open the connection if necessary.  The API will be updated                to allow the client to be informed when the echo response has been received (both synchronously                and asynchronously).
--
-- @data@ — (void *) - Pointer to buffer to send.
--
-- @length@ — (UInt16) - Length of the buffer to send
--
-- Returns: Returns kIOReturnSuccess if the echo request was able to be sent.
--
-- ObjC selector: @- sendL2CAPEchoRequest:length:@
sendL2CAPEchoRequest_length :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> Ptr () -> CUShort -> IO CInt
sendL2CAPEchoRequest_length ioBluetoothDevice data_ length_ =
  sendMessage ioBluetoothDevice sendL2CAPEchoRequest_lengthSelector data_ length_

-- | openRFCOMMChannel:channel:
--
-- Opens a new RFCOMM channel to the target device. Returns only once the channel is open or failed to open.
--
-- This method will begin the process of opening a new RFCOMM channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The RFCOMM                channel open process will not complete until the client has registered an incoming data                listener on the new channel.
--
-- @channelID@ — The RFCOMM channel ID for the new channel.
--
-- @rfcommChannel@ — A pointer to an IOBluetoothRFCOMMChannel object to receive the RFCOMM channel                                requested to be opened.  The rfcommChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                RFCOMM channel was found).
--
-- ObjC selector: @- openRFCOMMChannel:channel:@
openRFCOMMChannel_channel :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothRFCOMMChannel rfcommChannel) => ioBluetoothDevice -> CUChar -> rfcommChannel -> IO CInt
openRFCOMMChannel_channel ioBluetoothDevice channelID rfcommChannel =
  sendMessage ioBluetoothDevice openRFCOMMChannel_channelSelector channelID (toIOBluetoothRFCOMMChannel rfcommChannel)

-- | openRFCOMMChannelSync:withChannelID:delegate:
--
-- Opens a new RFCOMM channel to the target device.  Returns only once the channel is open or failed to open.
--
-- This method will begin the process of opening a new RFCOMM channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The RFCOMM                channel open process will not complete until the client has registered an incoming data                listener on the new channel. The RFCOMM channel object is already retained when this function returns success;                                the channel must be released when the caller is done with it.
--
-- You should verify that the channel you wish to open exists on the remote device before attempting to open it,                                by performing an SDP query. This is recommended because the service might have been removed from the,                                remote device or the channel assignments for the service could have changed (this is rare, but it does happen                                frequently on some devices). This also works around a bug that existed in early Leopard versions in certain                                situations where the method would return an error; in these instances, the desired RFCOMM channel could not                                be opened again until the calling app was restarted.
--
-- NOTE:	This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @rfcommChannel@ — A pointer to an IOBluetoothRFCOMMChannel object to receive the RFCOMM channel                                requested to be opened.  The rfcommChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @channelID@ — The RFCOMM channel ID for the new channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the rfcomm uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothRFCOMMChannel.h" in the definition                                of the protocol IOBluetoothRFCOMMChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                RFCOMM channel was found). The channel must be released when the caller is done with it.
--
-- ObjC selector: @- openRFCOMMChannelSync:withChannelID:delegate:@
openRFCOMMChannelSync_withChannelID_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothRFCOMMChannel rfcommChannel) => ioBluetoothDevice -> rfcommChannel -> CUChar -> RawId -> IO CInt
openRFCOMMChannelSync_withChannelID_delegate ioBluetoothDevice rfcommChannel channelID channelDelegate =
  sendMessage ioBluetoothDevice openRFCOMMChannelSync_withChannelID_delegateSelector (toIOBluetoothRFCOMMChannel rfcommChannel) channelID channelDelegate

-- | openRFCOMMChannelAsync:withChannelID:delegate:
--
-- Opens a new RFCOMM channel to the target device. Returns immediately.
--
-- This method will begin the process of opening a new RFCOMM channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The RFCOMM                channel open process will not complete until the client has registered an incoming data                listener on the new channel. The RFCOMM channel object is already retained when this function returns success;                                the channel must be released when the caller is done with it.
--
-- You should verify that the channel you wish to open exists on the remote device before attempting to open it,                                by performing an SDP query. This is recommended because the service might have been removed from the,                                remote device or the channel assignments for the service could have changed (this is rare, but it does happen                                frequently on some devices). This also works around a bug that existed in early Leopard versions in certain                                situations where the method would return an error; in these instances, the desired RFCOMM channel could not                                be opened again until the calling app was restarted.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @rfcommChannel@ — A pointer to an IOBluetoothRFCOMMChannel object to receive the RFCOMM channel                                requested to be opened.  The rfcommChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @channelID@ — The RFCOMM channel ID for the new channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the rfcomm uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothRFCOMMChannel.h" in the definition                                of the protocol IOBluetoothRFCOMMChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                RFCOMM channel was found). The channel must be released when the caller is done with it.
--
-- ObjC selector: @- openRFCOMMChannelAsync:withChannelID:delegate:@
openRFCOMMChannelAsync_withChannelID_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothRFCOMMChannel rfcommChannel) => ioBluetoothDevice -> rfcommChannel -> CUChar -> RawId -> IO CInt
openRFCOMMChannelAsync_withChannelID_delegate ioBluetoothDevice rfcommChannel channelID channelDelegate =
  sendMessage ioBluetoothDevice openRFCOMMChannelAsync_withChannelID_delegateSelector (toIOBluetoothRFCOMMChannel rfcommChannel) channelID channelDelegate

-- | @- getClassOfDevice@
getClassOfDevice :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
getClassOfDevice ioBluetoothDevice =
  sendMessage ioBluetoothDevice getClassOfDeviceSelector

-- | @- getServiceClassMajor@
getServiceClassMajor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
getServiceClassMajor ioBluetoothDevice =
  sendMessage ioBluetoothDevice getServiceClassMajorSelector

-- | @- getDeviceClassMajor@
getDeviceClassMajor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
getDeviceClassMajor ioBluetoothDevice =
  sendMessage ioBluetoothDevice getDeviceClassMajorSelector

-- | @- getDeviceClassMinor@
getDeviceClassMinor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
getDeviceClassMinor ioBluetoothDevice =
  sendMessage ioBluetoothDevice getDeviceClassMinorSelector

-- | @- getName@
getName :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
getName ioBluetoothDevice =
  sendMessage ioBluetoothDevice getNameSelector

-- | @- getNameOrAddress@
getNameOrAddress :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
getNameOrAddress ioBluetoothDevice =
  sendMessage ioBluetoothDevice getNameOrAddressSelector

-- | @- getLastNameUpdate@
getLastNameUpdate :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSDate)
getLastNameUpdate ioBluetoothDevice =
  sendMessage ioBluetoothDevice getLastNameUpdateSelector

-- | getAddress
--
-- Get the Bluetooth device address for the target device.
--
-- Returns: Returns a pointer to the Bluetooth device address of the target device.
--
-- ObjC selector: @- getAddress@
getAddress :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Const RawId)
getAddress ioBluetoothDevice =
  sendMessage ioBluetoothDevice getAddressSelector

-- | @- getAddressString@
getAddressString :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
getAddressString ioBluetoothDevice =
  sendMessage ioBluetoothDevice getAddressStringSelector

-- | getPageScanRepetitionMode
--
-- Get the value of the page scan repetition mode for the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the page scan repetition mode value for this device.
--
-- ObjC selector: @- getPageScanRepetitionMode@
getPageScanRepetitionMode :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUChar
getPageScanRepetitionMode ioBluetoothDevice =
  sendMessage ioBluetoothDevice getPageScanRepetitionModeSelector

-- | getPageScanPeriodMode
--
-- Get the value of the page scan period mode for the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns page scan period mode value for the device.
--
-- ObjC selector: @- getPageScanPeriodMode@
getPageScanPeriodMode :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUChar
getPageScanPeriodMode ioBluetoothDevice =
  sendMessage ioBluetoothDevice getPageScanPeriodModeSelector

-- | getPageScanMode
--
-- Get the page scan mode for the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the value for the page scan mode for the device.
--
-- ObjC selector: @- getPageScanMode@
getPageScanMode :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUChar
getPageScanMode ioBluetoothDevice =
  sendMessage ioBluetoothDevice getPageScanModeSelector

-- | getClockOffset
--
-- Get the clock offset value of the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the clock offset value for the device.
--
-- ObjC selector: @- getClockOffset@
getClockOffset :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUShort
getClockOffset ioBluetoothDevice =
  sendMessage ioBluetoothDevice getClockOffsetSelector

-- | getLastInquiryUpdate
--
-- Get the date/time of the last time the device was returned during an inquiry.
--
-- Returns: Returns the date/time of the last time the device was seen during an inquiry.                If the device has never been seen during an inquiry, nil is returned.
--
-- ObjC selector: @- getLastInquiryUpdate@
getLastInquiryUpdate :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSDate)
getLastInquiryUpdate ioBluetoothDevice =
  sendMessage ioBluetoothDevice getLastInquiryUpdateSelector

-- | RSSI
--
-- Get the RSSI device (if connected), above or below the golden range. If the RSSI is within the golden                                range, a value of 0 is returned. For the actual RSSI value, use getRawRSSI. For more information, see                                the Bluetooth 4.0 Core Specification.
--
-- Returns: Returns the RSSI of the device. If the value cannot be read (e.g. the device is disconnected), a value                                of +127 will be returned.
--
-- ObjC selector: @- RSSI@
rssi :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CSChar
rssi ioBluetoothDevice =
  sendMessage ioBluetoothDevice rssiSelector

-- | rawRSSI
--
-- Get the raw RSSI device (if connected).
--
-- Returns: Returns the raw RSSI of the device.
--
-- This value is the perceived RSSI value, not relative the the golden range (see getRSSI for that value).                                This value will not available on all Bluetooth modules. If the value cannot be read (e.g. the device                                is disconnected) or is not available on a module, a value of +127 will be returned.
--
-- ObjC selector: @- rawRSSI@
rawRSSI :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CSChar
rawRSSI ioBluetoothDevice =
  sendMessage ioBluetoothDevice rawRSSISelector

-- | isConnected
--
-- Indicates whether a baseband connection to the device exists.
--
-- Returns: Returns YES if a baseband connection to the device exists.
--
-- ObjC selector: @- isConnected@
isConnected :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
isConnected ioBluetoothDevice =
  sendMessage ioBluetoothDevice isConnectedSelector

-- | openConnection
--
-- Create a baseband connection to the device.
--
-- This method is synchronous and will not return until either a connection has been established                or the create connection has failed (perhaps timed out).  This method does the same thing as                                calling -openConnection: with a nil target.	 This call with proceed without authentication required, and                                using the default page timeout value.  If authentication or a non-default page timeout is required the method                                -openConnection:withPageTimeout:authenticationRequired: should be used instead.
--
-- As of Mac OS X 10.7, this method will no longer mask out "Connection Exists" 'errors' with a success result code;                                your code must account for the cases where the baseband connection is already open.
--
-- Returns: Returns kIOReturnSuccess if the connection was successfully created.
--
-- ObjC selector: @- openConnection@
openConnection :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CInt
openConnection ioBluetoothDevice =
  sendMessage ioBluetoothDevice openConnectionSelector

-- | openConnection:withPageTimeout:authenticationRequired:
--
-- Create a baseband connection to the device.
--
-- If a target is specified, the open connection call is asynchronous and on completion of the                                CREATE_CONNECTION command, the method -connectionComplete:status: will be called on the specified target.                                If no target is specified, the call is synchronous and will not return until the connection is open                                or the CREATE_CONNECTION call has failed.
--
-- NOTE: This method is only available in Mac OS X 10.2.7 (Bluetooth v1.3) or later.
--
-- As of Mac OS X 10.7, this method will no longer mask out "Connection Exists" 'errors' with a success result code;                                your code must account for the cases where the baseband connection is already open.
--
-- @target@ — The target to message when the create connection call is complete
--
-- @pageTimeoutValue@ — The page timeout value to use for this call
--
-- @authenticationRequired@ — BOOL value to indicate whether authentication should be required for the connection
--
-- Returns: Returns kIOReturnSuccess if the connection was successfully created (or if asynchronous, if the                                CREATE_CONNECTION command was successfully issued).
--
-- ObjC selector: @- openConnection:withPageTimeout:authenticationRequired:@
openConnection_withPageTimeout_authenticationRequired :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> RawId -> CUShort -> Bool -> IO CInt
openConnection_withPageTimeout_authenticationRequired ioBluetoothDevice target pageTimeoutValue authenticationRequired =
  sendMessage ioBluetoothDevice openConnection_withPageTimeout_authenticationRequiredSelector target pageTimeoutValue authenticationRequired

-- | closeConnection
--
-- Close down the baseband connection to the device.
--
-- This method is synchronous and will not return until the connection has been closed (or the                command failed).  In the future this API will be changed to allow asynchronous operation.
--
-- Returns: Returns kIOReturnSuccess if the connection has successfully been closed.
--
-- ObjC selector: @- closeConnection@
closeConnection :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CInt
closeConnection ioBluetoothDevice =
  sendMessage ioBluetoothDevice closeConnectionSelector

-- | remoteNameRequest:
--
-- Issues a remote name request to the target device.
--
-- If a target is specified, the request is asynchronous and on completion of the request, the method
--
-- - (void)remoteNameRequestComplete:(IOBluetoothDevice *)device status:(IOReturn)status;
--
-- will be called on the specified target. If no target is specified, the request is made synchronously                                and won't return until the request is complete.  This call with operate with the default page                                timeout value. If a different page timeout value is desired, the method -remoteNameRequest:withPageTimeout:                                should be used instead.
--
-- @target@ — The target to message when the remote name request is complete
--
-- Returns: Returns kIOReturnSuccess if the remote name request was successfully issued (and if synchronous, if                the request completed successfully).
--
-- ObjC selector: @- remoteNameRequest:@
remoteNameRequest :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> RawId -> IO CInt
remoteNameRequest ioBluetoothDevice target =
  sendMessage ioBluetoothDevice remoteNameRequestSelector target

-- | remoteNameRequest:withPageTimeout:
--
-- Issues a remote name request to the target device.
--
-- If a target is specified, the request is asynchronous and on completion of the REMOTE_NAME_REQUEST                command, the method -remoteNameRequestComplete:status:name: will be called on the specified target.                If no target is specified, the request is made synchronously and won't return until the request is                complete.
--
-- NOTE: This method is only available in Mac OS X 10.2.7 (Bluetooth v1.3) or later.
--
-- @target@ — The target to message when the remote name request is complete
--
-- @pageTimeoutValue@ — The page timeout value to use for this call
--
-- Returns: Returns kIOReturnSuccess if the remote name request was successfully issued (and if synchronous, if                the request completed successfully).
--
-- ObjC selector: @- remoteNameRequest:withPageTimeout:@
remoteNameRequest_withPageTimeout :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> RawId -> CUShort -> IO CInt
remoteNameRequest_withPageTimeout ioBluetoothDevice target pageTimeoutValue =
  sendMessage ioBluetoothDevice remoteNameRequest_withPageTimeoutSelector target pageTimeoutValue

-- | requestAuthentication
--
-- Requests that the existing baseband connection be authenticated.
--
-- In order to authenticate a baseband connection, a link key needs to be generated as a result of                the pairing process.  This call will synchronously initiate the pairing process with the target device                and not return until the authentication process is complete.  This API will be updated to allow                for asynchronous operation.
--
-- Returns: Returns kIOReturnSuccess if the connection has been successfully been authenticated.  Returns an error                if authentication fails or no baseband connection exists.
--
-- ObjC selector: @- requestAuthentication@
requestAuthentication :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CInt
requestAuthentication ioBluetoothDevice =
  sendMessage ioBluetoothDevice requestAuthenticationSelector

-- | @- getConnectionHandle@
getConnectionHandle :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUShort
getConnectionHandle ioBluetoothDevice =
  sendMessage ioBluetoothDevice getConnectionHandleSelector

-- | isIncoming
--
-- Returns TRUE if the device connection was generated by the remote host.
--
-- Returns TRUE if the device connection was generated by the remote host. False if the connection was generated by some other device that connected to the local host.
--
-- NOTE: This method is only available in Mac OS X 10.2.7 (Bluetooth v1.3) or later.
--
-- Returns: Returns TRUE if the device connection was generated by the remote host.
--
-- ObjC selector: @- isIncoming@
isIncoming :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
isIncoming ioBluetoothDevice =
  sendMessage ioBluetoothDevice isIncomingSelector

-- | getLinkType
--
-- Get the link type for the baseband connection.
--
-- This method only returns a valid result if a baseband connection is present (-isConnected returns TRUE).
--
-- Returns: Returns the link type for the baseband connection.  If no baseband connection is present,                kBluetoothLinkTypeNone is returned.
--
-- ObjC selector: @- getLinkType@
getLinkType :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUChar
getLinkType ioBluetoothDevice =
  sendMessage ioBluetoothDevice getLinkTypeSelector

-- | getEncryptionMode
--
-- Get the encryption mode for the baseband connection.
--
-- This method only returns a valid result if a baseband connection is present (-isConnected returns TRUE).
--
-- Returns: Returns the encryption mode for the baseband connection.  If no baseband connection is present,                kEncryptionDisabled is returned.
--
-- ObjC selector: @- getEncryptionMode@
getEncryptionMode :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUChar
getEncryptionMode ioBluetoothDevice =
  sendMessage ioBluetoothDevice getEncryptionModeSelector

-- | performSDPQuery:
--
-- Performs an SDP query on the target device.
--
-- As a result of this call, a baseband connection will be built to the device (if not already connected).                                Then, an L2CAP channel will be opened to the SDP server on the device.  At that point, a Service                                Search Attribute request will be issued with a UUID of 0x0100 (L2CAP) and an attribute range of                                0x0000 - 0xffff specified.  This will cause the SDP server to return all attributes of all L2CAP-derived                                services on the device.  The results essentially encompass all services on the device.                                This function is always asynchronous.  If a target is specified, when the SDP query is complete (or                                an error is encountered), the method -sdpQueryComplete:status: will be called on the given target.  If no target                                is specified, the request is still asynchronous, but no callback will be made.  That can be useful if the client                                has	registered for SDP service changed notifications.
--
-- @target@ — The target to message when the SDP query is complete
--
-- Returns: Returns kIOReturnSuccess if the SDP query was successfully started.
--
-- ObjC selector: @- performSDPQuery:@
performSDPQuery :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> RawId -> IO CInt
performSDPQuery ioBluetoothDevice target =
  sendMessage ioBluetoothDevice performSDPQuerySelector target

-- | performSDPQuery:uuids:
--
-- Performs an SDP query on the target device with the specified service UUIDs.
--
-- As a result of this call, a baseband connection will be built to the device (if not already connected). Then, an L2CAP channel will be opened to the SDP server on the device.  At that point, a Service Search Attribute request will be issued for each service UUID specified in the UUID array.
--
-- This function is always asynchronous.  If a target is specified, when the SDP query is complete (or an error is encountered), the method -sdpQueryComplete:status: will be called on the given target.  If no target is specified, the request is still asynchronous, but no callback will be made.  That can be useful if the client has	registered for SDP service changed notifications.
--
-- @target@ — The target to message when the SDP query is complete
--
-- @uuidArray@ — An array of IOBluetoothSDPUUID objects for each service the caller is interested in
--
-- Returns: Returns kIOReturnSuccess if the SDP query was successfully started.
--
-- ObjC selector: @- performSDPQuery:uuids:@
performSDPQuery_uuids :: (IsIOBluetoothDevice ioBluetoothDevice, IsNSArray uuidArray) => ioBluetoothDevice -> RawId -> uuidArray -> IO CInt
performSDPQuery_uuids ioBluetoothDevice target uuidArray =
  sendMessage ioBluetoothDevice performSDPQuery_uuidsSelector target (toNSArray uuidArray)

-- | @- getServices@
getServices :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSArray)
getServices ioBluetoothDevice =
  sendMessage ioBluetoothDevice getServicesSelector

-- | getLastServicesUpdate
--
-- Get the date/time of the last SDP query.
--
-- Returns: Returns the date/time of the last SDP query.  If an SDP query has never been performed on the                device, nil is returned.
--
-- ObjC selector: @- getLastServicesUpdate@
getLastServicesUpdate :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSDate)
getLastServicesUpdate ioBluetoothDevice =
  sendMessage ioBluetoothDevice getLastServicesUpdateSelector

-- | getServiceRecordForUUID
--
-- Search for a service record containing the given UUID.
--
-- This method searches through the device's services to find a service that contains the given                UUID.  Only the first service record will be returned.  This method only operates on services                that have already been queried.  It will not initiate a new query.  This method should probably                be updated to return an array of service records if more than one contains the UUID.
--
-- @sdpUUID@ — UUID value to search for.
--
-- Returns: Returns the first service record that contains the given uuid.  If no service record is found,                nil is returned.
--
-- ObjC selector: @- getServiceRecordForUUID:@
getServiceRecordForUUID :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothSDPUUID sdpUUID) => ioBluetoothDevice -> sdpUUID -> IO (Id IOBluetoothSDPServiceRecord)
getServiceRecordForUUID ioBluetoothDevice sdpUUID =
  sendMessage ioBluetoothDevice getServiceRecordForUUIDSelector (toIOBluetoothSDPUUID sdpUUID)

-- | favoriteDevices
--
-- Gets an array of the user's favorite devices.
--
-- The resulting array contains IOBluetoothDevice objects.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns an array of device objects representing the user's favorite devices.  If the                                user has no favorites, nil is returned.
--
-- ObjC selector: @+ favoriteDevices@
favoriteDevices :: IO (Id NSArray)
favoriteDevices  =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' favoriteDevicesSelector

-- | isFavorite
--
-- Reports whether the target device is a favorite for the user.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns TRUE if the target device is a favorite for the user, FALSE if not.
--
-- ObjC selector: @- isFavorite@
isFavorite :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
isFavorite ioBluetoothDevice =
  sendMessage ioBluetoothDevice isFavoriteSelector

-- | addToFavorites
--
-- Adds the target device to the user's favorite devices list.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns kIOReturnSuccess if the device was successfully added to the user's                                list of favorite devices.
--
-- ObjC selector: @- addToFavorites@
addToFavorites :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CInt
addToFavorites ioBluetoothDevice =
  sendMessage ioBluetoothDevice addToFavoritesSelector

-- | removeFromFavorites
--
-- Removes the target device from the user's favorite devices list.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns kIOReturnSuccess if the device was successfully removed from the user's                                list of favorite devices.
--
-- ObjC selector: @- removeFromFavorites@
removeFromFavorites :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CInt
removeFromFavorites ioBluetoothDevice =
  sendMessage ioBluetoothDevice removeFromFavoritesSelector

-- | recentDevices
--
-- Gets an array of recently used Bluetooth devices.
--
-- The resulting array contains IOBluetoothDevice objects sorted in reverse chronological order.                                The most recently accessed devices are first.  If the numDevices parameter is 0, all devices                                accessed by the system are returned.  If numDevices is non-zero, only the most recent devices                                are returned.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @numDevices@ — The number of devices to return.
--
-- Returns: Returns an array of device objects recently used by the system.  If no devices have been accessed,                                nil is returned.
--
-- ObjC selector: @+ recentDevices:@
recentDevices :: CULong -> IO (Id NSArray)
recentDevices numDevices =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' recentDevicesSelector numDevices

-- | recentAccessDate
--
-- Returns the date/time of the most recent access of the target device.
--
-- This is the date that -recentDevices uses to sort its list of the most recently accessed                                devices.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the date/time of the most recent access of the target device.  If the device                                has not been accessed, nil is returned.
--
-- ObjC selector: @- recentAccessDate@
recentAccessDate :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSDate)
recentAccessDate ioBluetoothDevice =
  sendMessage ioBluetoothDevice recentAccessDateSelector

-- | pairedDevices
--
-- Gets an array of all of the paired devices on the system.
--
-- The resulting array contains IOBluetoothDevice objects.  The paired devices are currently NOT stored                                per user, so this is all devices paired by any user.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- Returns: Returns an array of device objects for all of the paired devices on the system.  If there are                                no paired devices, nil is returned.
--
-- ObjC selector: @+ pairedDevices@
pairedDevices :: IO (Id NSArray)
pairedDevices  =
  do
    cls' <- getRequiredClass "IOBluetoothDevice"
    sendClassMessage cls' pairedDevicesSelector

-- | isPaired
--
-- Returns whether the target device is paired.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- Returns: Returns TRUE if the target device is paired, FALSE if not.
--
-- ObjC selector: @- isPaired@
isPaired :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
isPaired ioBluetoothDevice =
  sendMessage ioBluetoothDevice isPairedSelector

-- | setSupervisionTimeout
--
-- Sets the connection supervision timeout.
--
-- NOTE: This method is only available in Mac OS X 10.5 (Bluetooth v2.0) or later.
--
-- @timeout@ — A client-supplied link supervision timeout value to use to monitor the connection. The timeout                                value should be specified in slots, so you can use the BluetoothGetSlotsFromSeconds macro to get the proper                                value. e.g. BluetoothGetSlotsFromSeconds( 5.0 ) will give yield the proper number of slots (8000) for 5 seconds.
--
-- Returns: Returns kIOReturnSuccess if it was possible to set the connection supervision timeout.
--
-- ObjC selector: @- setSupervisionTimeout:@
setSupervisionTimeout :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> CUShort -> IO CInt
setSupervisionTimeout ioBluetoothDevice timeout =
  sendMessage ioBluetoothDevice setSupervisionTimeoutSelector timeout

-- | openL2CAPChannelSync:withPSM:withConfiguration:delegate:
--
-- Opens a new L2CAP channel to the target device. Returns only after the channel is opened.
--
-- This method will begin the process of opening a new L2CAP channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The L2CAP                channel open process will not complete until the client has registered an incoming data                listener on the new channel.  This prevents a situation where the channel succeeds                in being configured and opened and receives data before the client is listening and                is ready for it. The L2CAP channel object is already retained when this function returns success;                                the channel must be released when the caller is done with it.
--
-- NOTE: This method is only available in Mac OS X 10.5 (Bluetooth v2.0) or later.
--
-- @newChannel@ — A pointer to an IOBluetoothL2CAPChannel object to receive the L2CAP channel                                requested to be opened.  The newChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @withPSM@ — The L2CAP PSM value for the new channel.
--
-- @channelConfiguration@ — the dictionary that describes the initial configuration for                                the channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the l2cap uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothL2CAPChannel.h" in the definition                                of the protocol IOBluetoothL2CAPChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                L2CAP channel was found). The channel must be released when the caller is done with it.
--
-- ObjC selector: @- openL2CAPChannelSync:withPSM:withConfiguration:delegate:@
openL2CAPChannelSync_withPSM_withConfiguration_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothL2CAPChannel newChannel, IsNSDictionary channelConfiguration) => ioBluetoothDevice -> newChannel -> CUShort -> channelConfiguration -> RawId -> IO CInt
openL2CAPChannelSync_withPSM_withConfiguration_delegate ioBluetoothDevice newChannel psm channelConfiguration channelDelegate =
  sendMessage ioBluetoothDevice openL2CAPChannelSync_withPSM_withConfiguration_delegateSelector (toIOBluetoothL2CAPChannel newChannel) psm (toNSDictionary channelConfiguration) channelDelegate

-- | openL2CAPChannelAsync:withPSM:withConfiguration:delegate:
--
-- Opens a new L2CAP channel to the target device. Returns immediately after starting the opening process.
--
-- This method will begin the process of opening a new L2CAP channel to the target device.                The baseband connection to the device will be opened if it is not open already.  The L2CAP                channel open process will not complete until the client has registered an incoming data                listener on the new channel.  This prevents a situation where the channel succeeds                in being configured and opened and receives data before the client is listening and                is ready for it. The L2CAP channel object is already retained when this function returns success;                                the channel must be released when the caller is done with it.
--
-- NOTE: This method is only available in Mac OS X 10.5 (Bluetooth v2.0) or later.
--
-- @newChannel@ — A pointer to an IOBluetoothL2CAPChannel object to receive the L2CAP channel                                requested to be opened.  The newChannel pointer will only be set if                                kIOReturnSuccess is returned.
--
-- @psm@ — The L2CAP PSM value for the new channel.
--
-- @channelConfiguration@ — the dictionary that describes the initial configuration for                                the channel.
--
-- @channelDelegate@ — the object that will play the role of delegate for the channel.                                A channel delegate is the object the l2cap uses as target for  data and events. The                                developer will implement only the the methods he/she is interested in. A list of the                                possible methods is at the end of the file "IOBluetoothL2CAPChannel.h" in the definition                                of the protocol IOBluetoothL2CAPChannelDelegate.
--
-- Returns: Returns kIOReturnSuccess if the open process was successfully started (or if an existing                L2CAP channel was found). The channel must be released when the caller is done with it.
--
-- ObjC selector: @- openL2CAPChannelAsync:withPSM:withConfiguration:delegate:@
openL2CAPChannelAsync_withPSM_withConfiguration_delegate :: (IsIOBluetoothDevice ioBluetoothDevice, IsIOBluetoothL2CAPChannel newChannel, IsNSDictionary channelConfiguration) => ioBluetoothDevice -> newChannel -> CUShort -> channelConfiguration -> RawId -> IO CInt
openL2CAPChannelAsync_withPSM_withConfiguration_delegate ioBluetoothDevice newChannel psm channelConfiguration channelDelegate =
  sendMessage ioBluetoothDevice openL2CAPChannelAsync_withPSM_withConfiguration_delegateSelector (toIOBluetoothL2CAPChannel newChannel) psm (toNSDictionary channelConfiguration) channelDelegate

-- | @- awakeAfterUsingCoder:@
awakeAfterUsingCoder :: (IsIOBluetoothDevice ioBluetoothDevice, IsNSCoder coder) => ioBluetoothDevice -> coder -> IO RawId
awakeAfterUsingCoder ioBluetoothDevice coder =
  sendMessage ioBluetoothDevice awakeAfterUsingCoderSelector (toNSCoder coder)

-- | handsFreeAudioGatewayDriverID
--
-- Return the hands free gateway driver ID
--
-- Returns the hands free gateway driver ID which is unique based on BT Address.
--
-- Returns: The hands free gateway driver ID
--
-- ObjC selector: @- handsFreeAudioGatewayDriverID@
handsFreeAudioGatewayDriverID :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
handsFreeAudioGatewayDriverID ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeAudioGatewayDriverIDSelector

-- | handsFreeAudioGatewayServiceRecord
--
-- Return the hands free gateway SDP record
--
-- Returns the hands free gateway SDP record.
--
-- Returns: The hands free gateway SDP record
--
-- ObjC selector: @- handsFreeAudioGatewayServiceRecord@
handsFreeAudioGatewayServiceRecord :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id IOBluetoothSDPServiceRecord)
handsFreeAudioGatewayServiceRecord ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeAudioGatewayServiceRecordSelector

-- | handsFreeDeviceDriverID
--
-- Return the hands free device driver ID
--
-- Returns the hands free device driver ID which is unique based on BT Address.
--
-- Returns: The hands free device driver ID
--
-- ObjC selector: @- handsFreeDeviceDriverID@
handsFreeDeviceDriverID :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
handsFreeDeviceDriverID ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeDeviceDriverIDSelector

-- | handsFreeDeviceServiceRecord
--
-- Return the hands free device SDP record
--
-- Returns the hands free device SDP record.
--
-- Returns: The hands free device SDP record
--
-- ObjC selector: @- handsFreeDeviceServiceRecord@
handsFreeDeviceServiceRecord :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id IOBluetoothSDPServiceRecord)
handsFreeDeviceServiceRecord ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeDeviceServiceRecordSelector

-- | getClassOfDevice
--
-- Gets the full class of device value for the remote device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the class of device for the remote device.
--
-- ObjC selector: @- classOfDevice@
classOfDevice :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
classOfDevice ioBluetoothDevice =
  sendMessage ioBluetoothDevice classOfDeviceSelector

-- | getServiceClassMajor
--
-- Get the major service class of the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the major service class of the device.
--
-- ObjC selector: @- serviceClassMajor@
serviceClassMajor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
serviceClassMajor ioBluetoothDevice =
  sendMessage ioBluetoothDevice serviceClassMajorSelector

-- | getDeviceClassMajor
--
-- Get the major device class of the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the major device class of the remote device.
--
-- ObjC selector: @- deviceClassMajor@
deviceClassMajor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
deviceClassMajor ioBluetoothDevice =
  sendMessage ioBluetoothDevice deviceClassMajorSelector

-- | getDeviceClassMinor
--
-- Get the minor service class of the device.
--
-- This value is only meaningful if the target device has been seen during an inquiry.  This can be                by checking the result of -getLastInquiryUpdate.  If nil is returned, then the device hasn't been                seen.
--
-- Returns: Returns the minor device class of the remote device.
--
-- ObjC selector: @- deviceClassMinor@
deviceClassMinor :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUInt
deviceClassMinor ioBluetoothDevice =
  sendMessage ioBluetoothDevice deviceClassMinorSelector

-- | getName
--
-- Get the human readable name of the remote device.
--
-- This only returns a value if a remote name request has been performed on the target device.  If a                successful remote name request has not been completed, nil is returned.  To perform a remote                name request, call -remoteNameRequest.  If a remote name request has been successfully completed,                the method -getLastNameUpdate will return the date/time of the last successful request.
--
-- Returns: Returns the name of the remote device name.  This value is an NSString generated from the UTF-8                format of the most recent remote name request.
--
-- ObjC selector: @- name@
name :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
name ioBluetoothDevice =
  sendMessage ioBluetoothDevice nameSelector

-- | getNameOrAddress
--
-- Get the human readable name of the remote device.  If the name is not present, it will return a string                containing the device's address.
--
-- If a remote name request has been successfully completed, the device name will be returned.  If not,                a string containg the device address in the format of "XX-XX-XX-XX-XX-XX" will be returned.
--
-- Returns: Returns the device's name or a string containing the device's address.
--
-- ObjC selector: @- nameOrAddress@
nameOrAddress :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
nameOrAddress ioBluetoothDevice =
  sendMessage ioBluetoothDevice nameOrAddressSelector

-- | getLastNameUpdate
--
-- Get the date/time of the last successful remote name request.
--
-- Returns: Returns the date/time of the last successful remote name request.  If no remote name request has been                completed on the target device, nil is returned.
--
-- ObjC selector: @- lastNameUpdate@
lastNameUpdate :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSDate)
lastNameUpdate ioBluetoothDevice =
  sendMessage ioBluetoothDevice lastNameUpdateSelector

-- | getAddressString
--
-- Get a string representation of the Bluetooth device address for the target device.  The                                format of the string is the same as returned by IOBluetoothNSStringFromDeviceAddress(void).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns an NSString containing the Bluetooth device address of the target device.
--
-- ObjC selector: @- addressString@
addressString :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSString)
addressString ioBluetoothDevice =
  sendMessage ioBluetoothDevice addressStringSelector

-- | getConnectionHandle
--
-- Get the connection handle for the baseband connection.
--
-- This method only returns a valid result if a baseband connection is present (-isConnected returns TRUE).
--
-- Returns: Returns the connection handle for the baseband connection.  If no baseband connection is present,                kBluetoothConnectionHandleNone is returned.
--
-- ObjC selector: @- connectionHandle@
connectionHandle :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO CUShort
connectionHandle ioBluetoothDevice =
  sendMessage ioBluetoothDevice connectionHandleSelector

-- | services
--
-- Gets an array of service records for the device.
--
-- The resulting array contains IOBluetoothSDPServiceRecord objects.  The service records are only                present if an SDP query has been done on the target object.  This can be determined by calling                -getLastServicesUpdate.  It will return the last date/time of the SDP query. To initiate an                                SDP query on a device, use -performSDPQuery: as defined above.
--
-- Instead of allowing individual clients to query for different services and service attributes,                the system request all of the device's services and service attributes.
--
-- Returns: Returns an array of service records for the device if an SDP query has been performed.  If no                SDP query has been performed, nil is returned.
--
-- ObjC selector: @- services@
services :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO (Id NSArray)
services ioBluetoothDevice =
  sendMessage ioBluetoothDevice servicesSelector

-- | isHandsFreeAudioGateway
--
-- Return the devices support for hands free gateway
--
-- Returns the devices support for hands free gateway (obtained from the devices SDP record).
--
-- Returns: YES if the device supports hands free gateway; otherwise, NO.
--
-- ObjC selector: @- handsFreeAudioGateway@
handsFreeAudioGateway :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
handsFreeAudioGateway ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeAudioGatewaySelector

-- | isHandsFreeDevice
--
-- Return the devices support for hands free device
--
-- Returns the devices support for hands free device (obtained from the devices SDP record).
--
-- Returns: YES if the device supports hands free device; otherwise, NO.
--
-- ObjC selector: @- handsFreeDevice@
handsFreeDevice :: IsIOBluetoothDevice ioBluetoothDevice => ioBluetoothDevice -> IO Bool
handsFreeDevice ioBluetoothDevice =
  sendMessage ioBluetoothDevice handsFreeDeviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerForConnectNotifications:selector:@
registerForConnectNotifications_selectorSelector :: Selector '[RawId, Sel] (Id IOBluetoothUserNotification)
registerForConnectNotifications_selectorSelector = mkSelector "registerForConnectNotifications:selector:"

-- | @Selector@ for @registerForDisconnectNotification:selector:@
registerForDisconnectNotification_selectorSelector :: Selector '[RawId, Sel] (Id IOBluetoothUserNotification)
registerForDisconnectNotification_selectorSelector = mkSelector "registerForDisconnectNotification:selector:"

-- | @Selector@ for @deviceWithAddress:@
deviceWithAddressSelector :: Selector '[Const RawId] (Id IOBluetoothDevice)
deviceWithAddressSelector = mkSelector "deviceWithAddress:"

-- | @Selector@ for @withAddress:@
withAddressSelector :: Selector '[Const RawId] (Id IOBluetoothDevice)
withAddressSelector = mkSelector "withAddress:"

-- | @Selector@ for @deviceWithAddressString:@
deviceWithAddressStringSelector :: Selector '[Id NSString] (Id IOBluetoothDevice)
deviceWithAddressStringSelector = mkSelector "deviceWithAddressString:"

-- | @Selector@ for @withDeviceRef:@
withDeviceRefSelector :: Selector '[Ptr ()] (Id IOBluetoothDevice)
withDeviceRefSelector = mkSelector "withDeviceRef:"

-- | @Selector@ for @getDeviceRef@
getDeviceRefSelector :: Selector '[] (Ptr ())
getDeviceRefSelector = mkSelector "getDeviceRef"

-- | @Selector@ for @openL2CAPChannelSync:withPSM:delegate:@
openL2CAPChannelSync_withPSM_delegateSelector :: Selector '[Id IOBluetoothL2CAPChannel, CUShort, RawId] CInt
openL2CAPChannelSync_withPSM_delegateSelector = mkSelector "openL2CAPChannelSync:withPSM:delegate:"

-- | @Selector@ for @openL2CAPChannelAsync:withPSM:delegate:@
openL2CAPChannelAsync_withPSM_delegateSelector :: Selector '[Id IOBluetoothL2CAPChannel, CUShort, RawId] CInt
openL2CAPChannelAsync_withPSM_delegateSelector = mkSelector "openL2CAPChannelAsync:withPSM:delegate:"

-- | @Selector@ for @openL2CAPChannel:findExisting:newChannel:@
openL2CAPChannel_findExisting_newChannelSelector :: Selector '[CUShort, Bool, Id IOBluetoothL2CAPChannel] CInt
openL2CAPChannel_findExisting_newChannelSelector = mkSelector "openL2CAPChannel:findExisting:newChannel:"

-- | @Selector@ for @sendL2CAPEchoRequest:length:@
sendL2CAPEchoRequest_lengthSelector :: Selector '[Ptr (), CUShort] CInt
sendL2CAPEchoRequest_lengthSelector = mkSelector "sendL2CAPEchoRequest:length:"

-- | @Selector@ for @openRFCOMMChannel:channel:@
openRFCOMMChannel_channelSelector :: Selector '[CUChar, Id IOBluetoothRFCOMMChannel] CInt
openRFCOMMChannel_channelSelector = mkSelector "openRFCOMMChannel:channel:"

-- | @Selector@ for @openRFCOMMChannelSync:withChannelID:delegate:@
openRFCOMMChannelSync_withChannelID_delegateSelector :: Selector '[Id IOBluetoothRFCOMMChannel, CUChar, RawId] CInt
openRFCOMMChannelSync_withChannelID_delegateSelector = mkSelector "openRFCOMMChannelSync:withChannelID:delegate:"

-- | @Selector@ for @openRFCOMMChannelAsync:withChannelID:delegate:@
openRFCOMMChannelAsync_withChannelID_delegateSelector :: Selector '[Id IOBluetoothRFCOMMChannel, CUChar, RawId] CInt
openRFCOMMChannelAsync_withChannelID_delegateSelector = mkSelector "openRFCOMMChannelAsync:withChannelID:delegate:"

-- | @Selector@ for @getClassOfDevice@
getClassOfDeviceSelector :: Selector '[] CUInt
getClassOfDeviceSelector = mkSelector "getClassOfDevice"

-- | @Selector@ for @getServiceClassMajor@
getServiceClassMajorSelector :: Selector '[] CUInt
getServiceClassMajorSelector = mkSelector "getServiceClassMajor"

-- | @Selector@ for @getDeviceClassMajor@
getDeviceClassMajorSelector :: Selector '[] CUInt
getDeviceClassMajorSelector = mkSelector "getDeviceClassMajor"

-- | @Selector@ for @getDeviceClassMinor@
getDeviceClassMinorSelector :: Selector '[] CUInt
getDeviceClassMinorSelector = mkSelector "getDeviceClassMinor"

-- | @Selector@ for @getName@
getNameSelector :: Selector '[] (Id NSString)
getNameSelector = mkSelector "getName"

-- | @Selector@ for @getNameOrAddress@
getNameOrAddressSelector :: Selector '[] (Id NSString)
getNameOrAddressSelector = mkSelector "getNameOrAddress"

-- | @Selector@ for @getLastNameUpdate@
getLastNameUpdateSelector :: Selector '[] (Id NSDate)
getLastNameUpdateSelector = mkSelector "getLastNameUpdate"

-- | @Selector@ for @getAddress@
getAddressSelector :: Selector '[] (Const RawId)
getAddressSelector = mkSelector "getAddress"

-- | @Selector@ for @getAddressString@
getAddressStringSelector :: Selector '[] (Id NSString)
getAddressStringSelector = mkSelector "getAddressString"

-- | @Selector@ for @getPageScanRepetitionMode@
getPageScanRepetitionModeSelector :: Selector '[] CUChar
getPageScanRepetitionModeSelector = mkSelector "getPageScanRepetitionMode"

-- | @Selector@ for @getPageScanPeriodMode@
getPageScanPeriodModeSelector :: Selector '[] CUChar
getPageScanPeriodModeSelector = mkSelector "getPageScanPeriodMode"

-- | @Selector@ for @getPageScanMode@
getPageScanModeSelector :: Selector '[] CUChar
getPageScanModeSelector = mkSelector "getPageScanMode"

-- | @Selector@ for @getClockOffset@
getClockOffsetSelector :: Selector '[] CUShort
getClockOffsetSelector = mkSelector "getClockOffset"

-- | @Selector@ for @getLastInquiryUpdate@
getLastInquiryUpdateSelector :: Selector '[] (Id NSDate)
getLastInquiryUpdateSelector = mkSelector "getLastInquiryUpdate"

-- | @Selector@ for @RSSI@
rssiSelector :: Selector '[] CSChar
rssiSelector = mkSelector "RSSI"

-- | @Selector@ for @rawRSSI@
rawRSSISelector :: Selector '[] CSChar
rawRSSISelector = mkSelector "rawRSSI"

-- | @Selector@ for @isConnected@
isConnectedSelector :: Selector '[] Bool
isConnectedSelector = mkSelector "isConnected"

-- | @Selector@ for @openConnection@
openConnectionSelector :: Selector '[] CInt
openConnectionSelector = mkSelector "openConnection"

-- | @Selector@ for @openConnection:withPageTimeout:authenticationRequired:@
openConnection_withPageTimeout_authenticationRequiredSelector :: Selector '[RawId, CUShort, Bool] CInt
openConnection_withPageTimeout_authenticationRequiredSelector = mkSelector "openConnection:withPageTimeout:authenticationRequired:"

-- | @Selector@ for @closeConnection@
closeConnectionSelector :: Selector '[] CInt
closeConnectionSelector = mkSelector "closeConnection"

-- | @Selector@ for @remoteNameRequest:@
remoteNameRequestSelector :: Selector '[RawId] CInt
remoteNameRequestSelector = mkSelector "remoteNameRequest:"

-- | @Selector@ for @remoteNameRequest:withPageTimeout:@
remoteNameRequest_withPageTimeoutSelector :: Selector '[RawId, CUShort] CInt
remoteNameRequest_withPageTimeoutSelector = mkSelector "remoteNameRequest:withPageTimeout:"

-- | @Selector@ for @requestAuthentication@
requestAuthenticationSelector :: Selector '[] CInt
requestAuthenticationSelector = mkSelector "requestAuthentication"

-- | @Selector@ for @getConnectionHandle@
getConnectionHandleSelector :: Selector '[] CUShort
getConnectionHandleSelector = mkSelector "getConnectionHandle"

-- | @Selector@ for @isIncoming@
isIncomingSelector :: Selector '[] Bool
isIncomingSelector = mkSelector "isIncoming"

-- | @Selector@ for @getLinkType@
getLinkTypeSelector :: Selector '[] CUChar
getLinkTypeSelector = mkSelector "getLinkType"

-- | @Selector@ for @getEncryptionMode@
getEncryptionModeSelector :: Selector '[] CUChar
getEncryptionModeSelector = mkSelector "getEncryptionMode"

-- | @Selector@ for @performSDPQuery:@
performSDPQuerySelector :: Selector '[RawId] CInt
performSDPQuerySelector = mkSelector "performSDPQuery:"

-- | @Selector@ for @performSDPQuery:uuids:@
performSDPQuery_uuidsSelector :: Selector '[RawId, Id NSArray] CInt
performSDPQuery_uuidsSelector = mkSelector "performSDPQuery:uuids:"

-- | @Selector@ for @getServices@
getServicesSelector :: Selector '[] (Id NSArray)
getServicesSelector = mkSelector "getServices"

-- | @Selector@ for @getLastServicesUpdate@
getLastServicesUpdateSelector :: Selector '[] (Id NSDate)
getLastServicesUpdateSelector = mkSelector "getLastServicesUpdate"

-- | @Selector@ for @getServiceRecordForUUID:@
getServiceRecordForUUIDSelector :: Selector '[Id IOBluetoothSDPUUID] (Id IOBluetoothSDPServiceRecord)
getServiceRecordForUUIDSelector = mkSelector "getServiceRecordForUUID:"

-- | @Selector@ for @favoriteDevices@
favoriteDevicesSelector :: Selector '[] (Id NSArray)
favoriteDevicesSelector = mkSelector "favoriteDevices"

-- | @Selector@ for @isFavorite@
isFavoriteSelector :: Selector '[] Bool
isFavoriteSelector = mkSelector "isFavorite"

-- | @Selector@ for @addToFavorites@
addToFavoritesSelector :: Selector '[] CInt
addToFavoritesSelector = mkSelector "addToFavorites"

-- | @Selector@ for @removeFromFavorites@
removeFromFavoritesSelector :: Selector '[] CInt
removeFromFavoritesSelector = mkSelector "removeFromFavorites"

-- | @Selector@ for @recentDevices:@
recentDevicesSelector :: Selector '[CULong] (Id NSArray)
recentDevicesSelector = mkSelector "recentDevices:"

-- | @Selector@ for @recentAccessDate@
recentAccessDateSelector :: Selector '[] (Id NSDate)
recentAccessDateSelector = mkSelector "recentAccessDate"

-- | @Selector@ for @pairedDevices@
pairedDevicesSelector :: Selector '[] (Id NSArray)
pairedDevicesSelector = mkSelector "pairedDevices"

-- | @Selector@ for @isPaired@
isPairedSelector :: Selector '[] Bool
isPairedSelector = mkSelector "isPaired"

-- | @Selector@ for @setSupervisionTimeout:@
setSupervisionTimeoutSelector :: Selector '[CUShort] CInt
setSupervisionTimeoutSelector = mkSelector "setSupervisionTimeout:"

-- | @Selector@ for @openL2CAPChannelSync:withPSM:withConfiguration:delegate:@
openL2CAPChannelSync_withPSM_withConfiguration_delegateSelector :: Selector '[Id IOBluetoothL2CAPChannel, CUShort, Id NSDictionary, RawId] CInt
openL2CAPChannelSync_withPSM_withConfiguration_delegateSelector = mkSelector "openL2CAPChannelSync:withPSM:withConfiguration:delegate:"

-- | @Selector@ for @openL2CAPChannelAsync:withPSM:withConfiguration:delegate:@
openL2CAPChannelAsync_withPSM_withConfiguration_delegateSelector :: Selector '[Id IOBluetoothL2CAPChannel, CUShort, Id NSDictionary, RawId] CInt
openL2CAPChannelAsync_withPSM_withConfiguration_delegateSelector = mkSelector "openL2CAPChannelAsync:withPSM:withConfiguration:delegate:"

-- | @Selector@ for @awakeAfterUsingCoder:@
awakeAfterUsingCoderSelector :: Selector '[Id NSCoder] RawId
awakeAfterUsingCoderSelector = mkSelector "awakeAfterUsingCoder:"

-- | @Selector@ for @handsFreeAudioGatewayDriverID@
handsFreeAudioGatewayDriverIDSelector :: Selector '[] (Id NSString)
handsFreeAudioGatewayDriverIDSelector = mkSelector "handsFreeAudioGatewayDriverID"

-- | @Selector@ for @handsFreeAudioGatewayServiceRecord@
handsFreeAudioGatewayServiceRecordSelector :: Selector '[] (Id IOBluetoothSDPServiceRecord)
handsFreeAudioGatewayServiceRecordSelector = mkSelector "handsFreeAudioGatewayServiceRecord"

-- | @Selector@ for @handsFreeDeviceDriverID@
handsFreeDeviceDriverIDSelector :: Selector '[] (Id NSString)
handsFreeDeviceDriverIDSelector = mkSelector "handsFreeDeviceDriverID"

-- | @Selector@ for @handsFreeDeviceServiceRecord@
handsFreeDeviceServiceRecordSelector :: Selector '[] (Id IOBluetoothSDPServiceRecord)
handsFreeDeviceServiceRecordSelector = mkSelector "handsFreeDeviceServiceRecord"

-- | @Selector@ for @classOfDevice@
classOfDeviceSelector :: Selector '[] CUInt
classOfDeviceSelector = mkSelector "classOfDevice"

-- | @Selector@ for @serviceClassMajor@
serviceClassMajorSelector :: Selector '[] CUInt
serviceClassMajorSelector = mkSelector "serviceClassMajor"

-- | @Selector@ for @deviceClassMajor@
deviceClassMajorSelector :: Selector '[] CUInt
deviceClassMajorSelector = mkSelector "deviceClassMajor"

-- | @Selector@ for @deviceClassMinor@
deviceClassMinorSelector :: Selector '[] CUInt
deviceClassMinorSelector = mkSelector "deviceClassMinor"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @nameOrAddress@
nameOrAddressSelector :: Selector '[] (Id NSString)
nameOrAddressSelector = mkSelector "nameOrAddress"

-- | @Selector@ for @lastNameUpdate@
lastNameUpdateSelector :: Selector '[] (Id NSDate)
lastNameUpdateSelector = mkSelector "lastNameUpdate"

-- | @Selector@ for @addressString@
addressStringSelector :: Selector '[] (Id NSString)
addressStringSelector = mkSelector "addressString"

-- | @Selector@ for @connectionHandle@
connectionHandleSelector :: Selector '[] CUShort
connectionHandleSelector = mkSelector "connectionHandle"

-- | @Selector@ for @services@
servicesSelector :: Selector '[] (Id NSArray)
servicesSelector = mkSelector "services"

-- | @Selector@ for @handsFreeAudioGateway@
handsFreeAudioGatewaySelector :: Selector '[] Bool
handsFreeAudioGatewaySelector = mkSelector "handsFreeAudioGateway"

-- | @Selector@ for @handsFreeDevice@
handsFreeDeviceSelector :: Selector '[] Bool
handsFreeDeviceSelector = mkSelector "handsFreeDevice"

