{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IOBluetoothOBEXSession@.
module ObjC.IOBluetooth.IOBluetoothOBEXSession
  ( IOBluetoothOBEXSession
  , IsIOBluetoothOBEXSession(..)
  , withSDPServiceRecord
  , withDevice_channelID
  , withIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon
  , initWithSDPServiceRecord
  , initWithDevice_channelID
  , initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon
  , getRFCOMMChannel
  , getDevice
  , sendBufferTroughChannel
  , restartTransmission
  , isSessionTargetAMac
  , openTransportConnection_selectorTarget_refCon
  , hasOpenTransportConnection
  , closeTransportConnection
  , sendDataToTransport_dataLength
  , setOpenTransportConnectionAsyncSelector_target_refCon
  , setOBEXSessionOpenConnectionCallback_refCon
  , closeTransportConnectionSelector
  , getDeviceSelector
  , getRFCOMMChannelSelector
  , hasOpenTransportConnectionSelector
  , initWithDevice_channelIDSelector
  , initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector
  , initWithSDPServiceRecordSelector
  , isSessionTargetAMacSelector
  , openTransportConnection_selectorTarget_refConSelector
  , restartTransmissionSelector
  , sendBufferTroughChannelSelector
  , sendDataToTransport_dataLengthSelector
  , setOBEXSessionOpenConnectionCallback_refConSelector
  , setOpenTransportConnectionAsyncSelector_target_refConSelector
  , withDevice_channelIDSelector
  , withIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector
  , withSDPServiceRecordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | withSDPServiceRecord
--
-- Creates a Bluetooth-based OBEX Session using an SDP service record, typically obtained from a device/service				browser window controller.
--
-- @inSDPServiceRecord@ — A valid SDP service record describing the service (and RFCOMM channel) you want to										connect to with Bluetooth/OBEX.
--
-- Returns: An OBEX session representing the device/rfcomm channel found in the service record. nil if we failed.
--
-- Note that this does NOT mean the transport connection was open. It will be opened when OBEXConnect is				invoked on the session object.
--
-- IMPORTANT NOTE*	In Bluetooth framework version 1.0.0, the session returned will NOT be autoreleased as it									should be according to objc convention. This has been changed starting in Bluetooth version									1.0.1 and later, so it WILL be autoreleased upon return, so you will need to retain									it if you want to reference it later.
--
-- ObjC selector: @+ withSDPServiceRecord:@
withSDPServiceRecord :: IsIOBluetoothSDPServiceRecord inSDPServiceRecord => inSDPServiceRecord -> IO (Id IOBluetoothOBEXSession)
withSDPServiceRecord inSDPServiceRecord =
  do
    cls' <- getRequiredClass "IOBluetoothOBEXSession"
    sendClassMessage cls' withSDPServiceRecordSelector (toIOBluetoothSDPServiceRecord inSDPServiceRecord)

-- | withDevice
--
-- Creates a Bluetooth-based OBEX Session using a Bluetooth device and a Bluetooth RFCOMM channel ID.
--
-- @inDevice@ — A valid Bluetooth device describing which device you want to connect to										with Bluetooth/OBEX.
--
-- @inRFCOMMChannelID@ — An RFCOMM Channel ID numbe that is available on the remote device. This channel will										be used when the transport connection is attempted.
--
-- Returns: An OBEX session representing the device/rfcomm channel found in the service record. nil if we failed.
--
-- Note that this does NOT mean the transport connection was open. It will be opened when OBEXConnect is				invoked on the session object.
--
-- IMPORTANT NOTE*	In Bluetooth framework version 1.0.0, the session returned will NOT be autoreleased as it									should be according to objc convention. This has been changed starting in Bluetooth version									1.0.1 and later, so it WILL be autoreleased upon return, so you will need to retain									it if you want to reference it later.
--
-- ObjC selector: @+ withDevice:channelID:@
withDevice_channelID :: IsIOBluetoothDevice inDevice => inDevice -> CUChar -> IO (Id IOBluetoothOBEXSession)
withDevice_channelID inDevice inRFCOMMChannelID =
  do
    cls' <- getRequiredClass "IOBluetoothOBEXSession"
    sendClassMessage cls' withDevice_channelIDSelector (toIOBluetoothDevice inDevice) inRFCOMMChannelID

-- | withIncomingRFCOMMChannel
--
-- Creates a Bluetooth-based OBEX Session using an incoming RFCOMM channel.
--
-- @inChannel@ — The channel to use to create a connection to a device.
--
-- @inEventSelector@ — The selector that gets called when an event occurs on the OBEX Session.
--
-- @inEventSelectorTarget@ — The object that is used to call the above selector.
--
-- @inUserRefCon@ — The reference constant. Pass whatever you wish - it will be returned to you in the selector.
--
-- Returns:
--
-- IMPORTANT NOTE*	In Bluetooth framework version 1.0.0, the session returned will NOT be autoreleased as it									should be according to objc convention. This has been changed starting in Bluetooth version									1.0.1 and later, so it WILL be autoreleased upon return, so you will need to retain									it if you want to reference it later.
--
-- ObjC selector: @+ withIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:@
withIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon :: IsIOBluetoothRFCOMMChannel inChannel => inChannel -> Sel -> RawId -> Ptr () -> IO (Id IOBluetoothOBEXSession)
withIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon inChannel inEventSelector inEventSelectorTarget inUserRefCon =
  do
    cls' <- getRequiredClass "IOBluetoothOBEXSession"
    sendClassMessage cls' withIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector (toIOBluetoothRFCOMMChannel inChannel) inEventSelector inEventSelectorTarget inUserRefCon

-- | initWithSDPServiceRecord
--
-- Initializes a Bluetooth-based OBEX Session using an SDP service record.
--
-- @inSDPServiceRecord@ —
--
-- Returns:
--
-- ObjC selector: @- initWithSDPServiceRecord:@
initWithSDPServiceRecord :: (IsIOBluetoothOBEXSession ioBluetoothOBEXSession, IsIOBluetoothSDPServiceRecord inSDPServiceRecord) => ioBluetoothOBEXSession -> inSDPServiceRecord -> IO (Id IOBluetoothOBEXSession)
initWithSDPServiceRecord ioBluetoothOBEXSession inSDPServiceRecord =
  sendOwnedMessage ioBluetoothOBEXSession initWithSDPServiceRecordSelector (toIOBluetoothSDPServiceRecord inSDPServiceRecord)

-- | initWithDevice
--
-- Initializes a Bluetooth-based OBEX Session using a Bluetooth device.
--
-- @inDevice@ — The bluetooth device on which to open the OBEXSession.
--
-- @inChannelID@ — The RFCOMM channel ID to use when opening the connection.
--
-- Returns:
--
-- ObjC selector: @- initWithDevice:channelID:@
initWithDevice_channelID :: (IsIOBluetoothOBEXSession ioBluetoothOBEXSession, IsIOBluetoothDevice inDevice) => ioBluetoothOBEXSession -> inDevice -> CUChar -> IO (Id IOBluetoothOBEXSession)
initWithDevice_channelID ioBluetoothOBEXSession inDevice inChannelID =
  sendOwnedMessage ioBluetoothOBEXSession initWithDevice_channelIDSelector (toIOBluetoothDevice inDevice) inChannelID

-- | initWithIncomingRFCOMMChannel
--
-- Initializes a Bluetooth-based OBEX Session using an incoming RFCOMM channel.
--
-- @inChannelID@ — RFCOMM channel ID of the desired channel to be used.
--
-- @inEventSelector@ — The selector to be called when an event is received.
--
-- @inEventSelectorTarget@ — The target object that get the selector message.
--
-- @refCon@ — caller reference constant, pass whatever you want, it will be returned to you in the selector.
--
-- Returns:
--
-- ObjC selector: @- initWithIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:@
initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon :: (IsIOBluetoothOBEXSession ioBluetoothOBEXSession, IsIOBluetoothRFCOMMChannel inChannel) => ioBluetoothOBEXSession -> inChannel -> Sel -> RawId -> Ptr () -> IO (Id IOBluetoothOBEXSession)
initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refCon ioBluetoothOBEXSession inChannel inEventSelector inEventSelectorTarget inUserRefCon =
  sendOwnedMessage ioBluetoothOBEXSession initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector (toIOBluetoothRFCOMMChannel inChannel) inEventSelector inEventSelectorTarget inUserRefCon

-- | getRFCOMMChannel
--
-- Get the Bluetooth RFCOMM channel being used by the session object.
--
-- Returns: A IOBluetoothRFCOMMChannel object.
--
-- This could potentially be nil even though you have a valid OBEX session, because the RFCOMM channel is				only valid when the session is connected.
--
-- ObjC selector: @- getRFCOMMChannel@
getRFCOMMChannel :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO (Id IOBluetoothRFCOMMChannel)
getRFCOMMChannel ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession getRFCOMMChannelSelector

-- | getDevice
--
-- Get the Bluetooth Device being used by the session object.
--
-- Returns: An IOBluetoothDevice object.
--
-- ObjC selector: @- getDevice@
getDevice :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO (Id IOBluetoothDevice)
getDevice ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession getDeviceSelector

-- | sendBufferTroughChannel
--
-- Sends the next block of data trough the rfcomm channel.
--
-- Returns:
--
-- Since a send in the rfcomm channel is broken in multiple write calls (this actually is true only if the size is grater	than the rfcomm MTU). Each write call is performed by sendBufferTroughChannel. This should never need to be overwritten.
--
-- ObjC selector: @- sendBufferTroughChannel@
sendBufferTroughChannel :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO CInt
sendBufferTroughChannel ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession sendBufferTroughChannelSelector

-- | restartTransmission
--
-- If the transmission was stopeed due to the lack of buffers this call restarts it.
--
-- Returns:
--
-- If the transmission was stopeed due to the lack of buffers this call restarts it.
--
-- ObjC selector: @- restartTransmission@
restartTransmission :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO ()
restartTransmission ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession restartTransmissionSelector

-- | isSessionTargetAMac
--
-- Tells whether the target device is a Mac by checking its service record.
--
-- Returns: TRUE only if device service record has Mac entry, FALSE for all else.
--
-- Tells whether the target device is a Mac by checking its service record.
--
-- ObjC selector: @- isSessionTargetAMac@
isSessionTargetAMac :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO Bool
isSessionTargetAMac ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession isSessionTargetAMacSelector

-- | openTransportConnection
--
-- An OBEXSession override. When this is called by the session baseclass, we will attempt to open the				transport connection. In our case, this would be an RFCOMM channel to another Bluetooth device.
--
-- Returns: Success or failure code.
--
-- Your selector should have the following signature:
--
-- -(void)transportConnectionSelector:(id)refcon		status:(OBEXError)status;
--
-- Thus you could use it with openTransportConnection like this:
--
-- OBEXError	error = [anOBEXSession	openTransportConnection:( transportConnectionSelector:status: )														selectorTarget:self														refCon:anOBEXSession];	// or whatever you want to pass as a refCon...
--
-- Be sure to check the status code! Assume the connection was not opened unless status is kOBEXSuccess.
--
-- ObjC selector: @- openTransportConnection:selectorTarget:refCon:@
openTransportConnection_selectorTarget_refCon :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> Sel -> RawId -> Ptr () -> IO CInt
openTransportConnection_selectorTarget_refCon ioBluetoothOBEXSession inSelector inTarget inUserRefCon =
  sendMessage ioBluetoothOBEXSession openTransportConnection_selectorTarget_refConSelector inSelector inTarget inUserRefCon

-- | hasOpenTransportConnection
--
-- An OBEXSession override. When this is called by the session baseclass, we will return whether or not we				have a transport connection established to another OBEX server/client. In our case we will tell whether				or not the RFCOMM channel to a remote device is still open.
--
-- Returns: True or false, whether there is already an open transport connection for this OBEX session.
--
-- ObjC selector: @- hasOpenTransportConnection@
hasOpenTransportConnection :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO CUChar
hasOpenTransportConnection ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession hasOpenTransportConnectionSelector

-- | closeTransportConnection
--
-- An OBEXSession override. When this is called by the session baseclass, we will close the transport				connection if it is opened. In our case, it will be the RFCOMM channel that needs closing.
--
-- Returns: Success or failure code, describing whether the call succeeded in closing the transport connection successfully.
--
-- ObjC selector: @- closeTransportConnection@
closeTransportConnection :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> IO CInt
closeTransportConnection ioBluetoothOBEXSession =
  sendMessage ioBluetoothOBEXSession closeTransportConnectionSelector

-- | sendDataToTransport
--
-- An OBEXSession override. When this is called by the session baseclass, we will send the data we are given				over our transport connection. If none is open, we could try to open it, or just return an error. In our				case, it will be sent over the RFCOMM channel.
--
-- Returns: Success or failure code, describing whether the call succeeded in writing the data to the transport.
--
-- ObjC selector: @- sendDataToTransport:dataLength:@
sendDataToTransport_dataLength :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> Ptr () -> CULong -> IO CInt
sendDataToTransport_dataLength ioBluetoothOBEXSession inDataToSend inDataLength =
  sendMessage ioBluetoothOBEXSession sendDataToTransport_dataLengthSelector inDataToSend inDataLength

-- | setOpenTransportConnectionAsyncSelector
--
-- Allows you to set the selector to be used when a transport connection is opened, or fails to open.
--
-- @inEventSelector@ — Selector to call on the target.
--
-- @inEventSelectorTarget@ — Target to be called with the selector.
--
-- @inUserRefCon@ — User's refCon that will get passed to them when their selector is invoked.
--
-- You do not need to call this on the session typically, unless you have subclassed the OBEXSession to				implement a new transport and that transport supports async opening of connections. If it does not support				async open, then using this is pointless.
--
-- ObjC selector: @- setOpenTransportConnectionAsyncSelector:target:refCon:@
setOpenTransportConnectionAsyncSelector_target_refCon :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> Sel -> RawId -> Ptr () -> IO ()
setOpenTransportConnectionAsyncSelector_target_refCon ioBluetoothOBEXSession inSelector inSelectorTarget inUserRefCon =
  sendMessage ioBluetoothOBEXSession setOpenTransportConnectionAsyncSelector_target_refConSelector inSelector inSelectorTarget inUserRefCon

-- | setOBEXSessionOpenConnectionCallback
--
-- For C API support. Allows you to set the callback to be invoked when the OBEX connection is actually opened.
--
-- @inCallback@ — function to call on the target.
--
-- @inUserRefCon@ — user's reference constant, will be returned on the callback.
--
-- ObjC selector: @- setOBEXSessionOpenConnectionCallback:refCon:@
setOBEXSessionOpenConnectionCallback_refCon :: IsIOBluetoothOBEXSession ioBluetoothOBEXSession => ioBluetoothOBEXSession -> Ptr () -> Ptr () -> IO ()
setOBEXSessionOpenConnectionCallback_refCon ioBluetoothOBEXSession inCallback inUserRefCon =
  sendMessage ioBluetoothOBEXSession setOBEXSessionOpenConnectionCallback_refConSelector inCallback inUserRefCon

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @withSDPServiceRecord:@
withSDPServiceRecordSelector :: Selector '[Id IOBluetoothSDPServiceRecord] (Id IOBluetoothOBEXSession)
withSDPServiceRecordSelector = mkSelector "withSDPServiceRecord:"

-- | @Selector@ for @withDevice:channelID:@
withDevice_channelIDSelector :: Selector '[Id IOBluetoothDevice, CUChar] (Id IOBluetoothOBEXSession)
withDevice_channelIDSelector = mkSelector "withDevice:channelID:"

-- | @Selector@ for @withIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:@
withIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector :: Selector '[Id IOBluetoothRFCOMMChannel, Sel, RawId, Ptr ()] (Id IOBluetoothOBEXSession)
withIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector = mkSelector "withIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @initWithSDPServiceRecord:@
initWithSDPServiceRecordSelector :: Selector '[Id IOBluetoothSDPServiceRecord] (Id IOBluetoothOBEXSession)
initWithSDPServiceRecordSelector = mkSelector "initWithSDPServiceRecord:"

-- | @Selector@ for @initWithDevice:channelID:@
initWithDevice_channelIDSelector :: Selector '[Id IOBluetoothDevice, CUChar] (Id IOBluetoothOBEXSession)
initWithDevice_channelIDSelector = mkSelector "initWithDevice:channelID:"

-- | @Selector@ for @initWithIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:@
initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector :: Selector '[Id IOBluetoothRFCOMMChannel, Sel, RawId, Ptr ()] (Id IOBluetoothOBEXSession)
initWithIncomingRFCOMMChannel_eventSelector_selectorTarget_refConSelector = mkSelector "initWithIncomingRFCOMMChannel:eventSelector:selectorTarget:refCon:"

-- | @Selector@ for @getRFCOMMChannel@
getRFCOMMChannelSelector :: Selector '[] (Id IOBluetoothRFCOMMChannel)
getRFCOMMChannelSelector = mkSelector "getRFCOMMChannel"

-- | @Selector@ for @getDevice@
getDeviceSelector :: Selector '[] (Id IOBluetoothDevice)
getDeviceSelector = mkSelector "getDevice"

-- | @Selector@ for @sendBufferTroughChannel@
sendBufferTroughChannelSelector :: Selector '[] CInt
sendBufferTroughChannelSelector = mkSelector "sendBufferTroughChannel"

-- | @Selector@ for @restartTransmission@
restartTransmissionSelector :: Selector '[] ()
restartTransmissionSelector = mkSelector "restartTransmission"

-- | @Selector@ for @isSessionTargetAMac@
isSessionTargetAMacSelector :: Selector '[] Bool
isSessionTargetAMacSelector = mkSelector "isSessionTargetAMac"

-- | @Selector@ for @openTransportConnection:selectorTarget:refCon:@
openTransportConnection_selectorTarget_refConSelector :: Selector '[Sel, RawId, Ptr ()] CInt
openTransportConnection_selectorTarget_refConSelector = mkSelector "openTransportConnection:selectorTarget:refCon:"

-- | @Selector@ for @hasOpenTransportConnection@
hasOpenTransportConnectionSelector :: Selector '[] CUChar
hasOpenTransportConnectionSelector = mkSelector "hasOpenTransportConnection"

-- | @Selector@ for @closeTransportConnection@
closeTransportConnectionSelector :: Selector '[] CInt
closeTransportConnectionSelector = mkSelector "closeTransportConnection"

-- | @Selector@ for @sendDataToTransport:dataLength:@
sendDataToTransport_dataLengthSelector :: Selector '[Ptr (), CULong] CInt
sendDataToTransport_dataLengthSelector = mkSelector "sendDataToTransport:dataLength:"

-- | @Selector@ for @setOpenTransportConnectionAsyncSelector:target:refCon:@
setOpenTransportConnectionAsyncSelector_target_refConSelector :: Selector '[Sel, RawId, Ptr ()] ()
setOpenTransportConnectionAsyncSelector_target_refConSelector = mkSelector "setOpenTransportConnectionAsyncSelector:target:refCon:"

-- | @Selector@ for @setOBEXSessionOpenConnectionCallback:refCon:@
setOBEXSessionOpenConnectionCallback_refConSelector :: Selector '[Ptr (), Ptr ()] ()
setOBEXSessionOpenConnectionCallback_refConSelector = mkSelector "setOBEXSessionOpenConnectionCallback:refCon:"

