{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothL2CAPChannel
--
-- An instance of IOBluetoothL2CAPChannel represents a single open L2CAP channel.
--
-- A client won't create IOBluetoothL2CAPChannel objects directly.  Instead, the IOBluetoothDevice's                 L2CAP channel open API is responsible for opening a new L2CAP channel and returning an                IOBluetoothL2CAPChannel instance representing that newly opened channel.  Additionally, the IOBluetooth                notification system will send notifications when new L2CAP channels are open (if requested).
--
-- After a new L2CAP channel is opened, the L2CAP configuration process will not be completed until an                incoming data listener is registered with the IOBluetoothL2CAPChannel object.  The reason for this is                to due to the limited buffering done of incoming L2CAP data.  This way, we avoid the situation where                 incoming data is received before the client is ready for it.  Once a client is done with an                IOBluetoothL2CAPChannel that it opened, it should call -closeChannel.  Additionally, if the client                does not intend to use the connection to the remote device any further, it should call -closeConnection                on the IOBluetoothDevice object.
--
-- Generated bindings for @IOBluetoothL2CAPChannel@.
module ObjC.IOBluetooth.IOBluetoothL2CAPChannel
  ( IOBluetoothL2CAPChannel
  , IsIOBluetoothL2CAPChannel(..)
  , registerForChannelOpenNotifications_selector
  , registerForChannelOpenNotifications_selector_withPSM_direction
  , withObjectID
  , closeChannel
  , getOutgoingMTU
  , getIncomingMTU
  , requestRemoteMTU
  , writeAsyncTrap_length_refcon
  , writeAsync_length_refcon
  , writeSync_length
  , setDelegate
  , setDelegate_withConfiguration
  , delegate
  , getDevice
  , getObjectID
  , getPSM
  , getLocalChannelID
  , getRemoteChannelID
  , isIncoming
  , registerForChannelCloseNotification_selector
  , outgoingMTU
  , incomingMTU
  , device
  , objectID
  , psm
  , localChannelID
  , remoteChannelID
  , registerForChannelOpenNotifications_selectorSelector
  , registerForChannelOpenNotifications_selector_withPSM_directionSelector
  , withObjectIDSelector
  , closeChannelSelector
  , getOutgoingMTUSelector
  , getIncomingMTUSelector
  , requestRemoteMTUSelector
  , writeAsyncTrap_length_refconSelector
  , writeAsync_length_refconSelector
  , writeSync_lengthSelector
  , setDelegateSelector
  , setDelegate_withConfigurationSelector
  , delegateSelector
  , getDeviceSelector
  , getObjectIDSelector
  , getPSMSelector
  , getLocalChannelIDSelector
  , getRemoteChannelIDSelector
  , isIncomingSelector
  , registerForChannelCloseNotification_selectorSelector
  , outgoingMTUSelector
  , incomingMTUSelector
  , deviceSelector
  , objectIDSelector
  , psmSelector
  , localChannelIDSelector
  , remoteChannelIDSelector

  -- * Enum types
  , IOBluetoothUserNotificationChannelDirection(IOBluetoothUserNotificationChannelDirection)
  , pattern KIOBluetoothUserNotificationChannelDirectionAny
  , pattern KIOBluetoothUserNotificationChannelDirectionIncoming
  , pattern KIOBluetoothUserNotificationChannelDirectionOutgoing

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

import ObjC.IOBluetooth.Internal.Classes
import ObjC.IOBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | registerForChannelOpenNotifications:selector:
--
-- Allows a client to register for L2CAP channel open notifications for any L2CAP channel.
--
-- The given selector will be called on the target object whenever any L2CAP channel is opened.				The selector should accept two arguments.  The first is the user notification object.  The second				is the IOBluetoothL2CAPChannel that was opened.
--
-- @object@ — Target object
--
-- @selector@ — Selector to be called on the target object when a new L2CAP channel is opened.
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding L2CAP channel notification.				To unregister the notification, call -unregister on the resulting IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.  The returned				IOBluetoothUserNotification will be valid for as long as the notification is registered.  It is				not necessary to retain the result.  Once -unregister is called on it, it will no longer be valid.
--
-- ObjC selector: @+ registerForChannelOpenNotifications:selector:@
registerForChannelOpenNotifications_selector :: RawId -> Selector -> IO (Id IOBluetoothUserNotification)
registerForChannelOpenNotifications_selector object selector =
  do
    cls' <- getRequiredClass "IOBluetoothL2CAPChannel"
    sendClassMsg cls' (mkSelector "registerForChannelOpenNotifications:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | registerForChannelOpenNotifications:selector:withPSM:direction:
--
-- Allows a client to register for L2CAP channel open notifications for certain types of				L2CAP channels.
--
-- The given selector will be called on the target object whenever an L2CAP channel with the given				attributes is opened.  The selector should accept two arguments.  The first is the user 				notification object.  The second is the IOBluetoothL2CAPChannel that was opened.
--
-- @object@ — Target object
--
-- @selector@ — Selector to be called on the target object when a new L2CAP channel is opened.
--
-- @psm@ — PSM to match a new L2CAP channel.  If the PSM doesn't matter, 0 may be passed in.
--
-- @inDirection@ — The desired direction of the L2CAP channel - kIOBluetoothUserNotificationChannelDirectionAny				if the direction doesn't matter.
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding L2CAP channel notification.				To unregister the notification, call -unregister on the resulting IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.  The returned				IOBluetoothUserNotification will be valid for as long as the notification is registered.  It is				not necessary to retain the result.  Once -unregister is called on it, it will no longer be valid.
--
-- ObjC selector: @+ registerForChannelOpenNotifications:selector:withPSM:direction:@
registerForChannelOpenNotifications_selector_withPSM_direction :: RawId -> Selector -> CUShort -> IOBluetoothUserNotificationChannelDirection -> IO (Id IOBluetoothUserNotification)
registerForChannelOpenNotifications_selector_withPSM_direction object selector psm inDirection =
  do
    cls' <- getRequiredClass "IOBluetoothL2CAPChannel"
    sendClassMsg cls' (mkSelector "registerForChannelOpenNotifications:selector:withPSM:direction:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (unSelector selector), argCUInt (fromIntegral psm), argCInt (coerce inDirection)] >>= retainedObject . castPtr

-- | withObjectID:
--
-- Returns the IObluetoothL2CAPChannel with the given IOBluetoothObjectID.
--
-- The IOBluetoothObjectID can be used as a global reference for a given IOBluetoothL2CAPChannel.  It allows				two separate applications to refer to the same IOBluetoothL2CAPChannel object.
--
-- @objectID@ — IOBluetoothObjectID of the desired IOBluetoothL2CAPChannel.
--
-- Returns: Returns the IOBluetoothL2CAPChannel that matches the given IOBluetoothObjectID if one exists.				If no matching L2CAP channel exists, nil is returned.
--
-- ObjC selector: @+ withObjectID:@
withObjectID :: CULong -> IO (Id IOBluetoothL2CAPChannel)
withObjectID objectID =
  do
    cls' <- getRequiredClass "IOBluetoothL2CAPChannel"
    sendClassMsg cls' (mkSelector "withObjectID:") (retPtr retVoid) [argCULong (fromIntegral objectID)] >>= retainedObject . castPtr

-- | closeChannel
--
-- Initiates the close process on an open L2CAP channel.
--
-- This method may only be called by the client that opened the channel in the first place.  In the future                asynchronous and synchronous versions will be provided that let the client know when the close process                has been finished.
--
-- Returns: Returns kIOReturnSuccess on success.
--
-- ObjC selector: @- closeChannel@
closeChannel :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CInt
closeChannel ioBluetoothL2CAPChannel  =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "closeChannel") retCInt []

-- | @- getOutgoingMTU@
getOutgoingMTU :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
getOutgoingMTU ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "getOutgoingMTU") retCUInt []

-- | @- getIncomingMTU@
getIncomingMTU :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
getIncomingMTU ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "getIncomingMTU") retCUInt []

-- | requestRemoteMTU:
--
-- Initiates the process to reconfigure the L2CAP channel with a new outgoing MTU.
--
-- Currently, this API does not give an indication that the re-config process has completed.  In                the future additional API will be available to provide that information both synchronously and                asynchronously.
--
-- @remoteMTU@ — The desired outgoing MTU.
--
-- Returns: Returns kIOReturnSuccess if the channel re-configure process was successfully initiated.
--
-- ObjC selector: @- requestRemoteMTU:@
requestRemoteMTU :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> CUShort -> IO CInt
requestRemoteMTU ioBluetoothL2CAPChannel  remoteMTU =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "requestRemoteMTU:") retCInt [argCUInt (fromIntegral remoteMTU)]

-- | writeAsyncTrap:length:refcon:
--
-- Writes the given data over the target L2CAP channel asynchronously to the remote device using 				IOConnectTrap4() call.
--
-- The length of the data may not exceed the L2CAP channel's ougoing MTU.  When the data has 				been successfully passed to the hardware to be transmitted, the delegate method 				-l2capChannelWriteComplete:refcon:status: will be called with the refcon passed 				into this method.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @data@ — Pointer to the buffer containing the data to send.
--
-- @length@ — The length of the given data buffer.
--
-- @refcon@ — User supplied value that gets passed to the write callback.
--
-- Returns: Returns kIOReturnSuccess if the data was buffered successfully.
--
-- ObjC selector: @- writeAsyncTrap:length:refcon:@
writeAsyncTrap_length_refcon :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> Ptr () -> CUShort -> Ptr () -> IO CInt
writeAsyncTrap_length_refcon ioBluetoothL2CAPChannel  data_ length_ refcon =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "writeAsyncTrap:length:refcon:") retCInt [argPtr data_, argCUInt (fromIntegral length_), argPtr refcon]

-- | writeAsync:length:refcon:
--
-- Writes the given data over the target L2CAP channel asynchronously to the remote device.
--
-- The length of the data may not exceed the L2CAP channel's ougoing MTU.  When the data has 				been successfully passed to the hardware to be transmitted, the delegate method 				-l2capChannelWriteComplete:refcon:status: will be called with the refcon passed 				into this method.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @data@ — Pointer to the buffer containing the data to send.
--
-- @length@ — The length of the given data buffer.
--
-- @refcon@ — User supplied value that gets passed to the write callback.
--
-- Returns: Returns kIOReturnSuccess if the data was buffered successfully.
--
-- ObjC selector: @- writeAsync:length:refcon:@
writeAsync_length_refcon :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> Ptr () -> CUShort -> Ptr () -> IO CInt
writeAsync_length_refcon ioBluetoothL2CAPChannel  data_ length_ refcon =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "writeAsync:length:refcon:") retCInt [argPtr data_, argCUInt (fromIntegral length_), argPtr refcon]

-- | writeSync:length:
--
-- Writes the given data synchronously over the target L2CAP channel to the remote device.
--
-- The length of the data may not exceed the L2CAP channel's ougoing MTU.  This method will				block until the data has been successfully sent to the hardware for transmission (or an error				occurs).
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @data@ — Pointer to the buffer containing the data to send.
--
-- @length@ — The length of the given data buffer.
--
-- Returns: Returns kIOReturnSuccess if the data was written successfully.
--
-- ObjC selector: @- writeSync:length:@
writeSync_length :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> Ptr () -> CUShort -> IO CInt
writeSync_length ioBluetoothL2CAPChannel  data_ length_ =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "writeSync:length:") retCInt [argPtr data_, argCUInt (fromIntegral length_)]

-- | setDelegate:
--
-- Allows an object to register itself as client of the L2CAP channel.
--
-- A channel delegate is the object the L2CAP channel uses as target for data and events. The				developer will implement only the the methods he/she is interested in. A list of the				possible methods is at the end of this file in the definition of the informal protocol				IOBluetoothL2CAPChannelDelegate.                A newly opened L2CAP channel will not complete its configuration process until the client                that opened it registers a connectionHandler.  This prevents that case where incoming                data is received before the client is ready.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @channelDelegate@ — the object that will play the role of channel delegate [NOTE the l2cap channel will retain the delegate].
--
-- Returns: Returns kIOReturnSuccess if the delegate is successfully registered.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> RawId -> IO CInt
setDelegate ioBluetoothL2CAPChannel  channelDelegate =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "setDelegate:") retCInt [argPtr (castPtr (unRawId channelDelegate) :: Ptr ())]

-- | setDelegate:withConfiguration:
--
-- Allows an object to register itself as client of the L2CAP channel.
--
-- A channel delegate is the object the L2CAP channel uses as target for data and events. The				developer will implement only the the methods he/she is interested in. A list of the				possible methods is at the end of this file in the definition of the informal protocol				IOBluetoothL2CAPChannelDelegate.                A newly opened L2CAP channel will not complete its configuration process until the client                that opened it registers a connectionHandler.  This prevents that case where incoming                data is received before the client is ready.
--
-- NOTE: This method is only available in Mac OS X 10.5 (Bluetooth v2.0) or later.
--
-- @channelDelegate@ — the object that will play the role of channel delegate.
--
-- @channelConfiguration@ — the dictionary that describes the initial configuration for				the channel.
--
-- Returns: Returns kIOReturnSuccess if the delegate is successfully registered.
--
-- ObjC selector: @- setDelegate:withConfiguration:@
setDelegate_withConfiguration :: (IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel, IsNSDictionary channelConfiguration) => ioBluetoothL2CAPChannel -> RawId -> channelConfiguration -> IO CInt
setDelegate_withConfiguration ioBluetoothL2CAPChannel  channelDelegate channelConfiguration =
withObjCPtr channelConfiguration $ \raw_channelConfiguration ->
    sendMsg ioBluetoothL2CAPChannel (mkSelector "setDelegate:withConfiguration:") retCInt [argPtr (castPtr (unRawId channelDelegate) :: Ptr ()), argPtr (castPtr raw_channelConfiguration :: Ptr ())]

-- | delegate
--
-- Returns the currently assigned delegate
--
-- An incoming channel is one that was initiated by a remote device.
--
-- Returns: Returns the current delegate, or nil if one is not set.
--
-- ObjC selector: @- delegate@
delegate :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO RawId
delegate ioBluetoothL2CAPChannel  =
  fmap (RawId . castPtr) $ sendMsg ioBluetoothL2CAPChannel (mkSelector "delegate") (retPtr retVoid) []

-- | @- getDevice@
getDevice :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO (Id IOBluetoothDevice)
getDevice ioBluetoothL2CAPChannel  =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "getDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getObjectID@
getObjectID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CULong
getObjectID ioBluetoothL2CAPChannel  =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "getObjectID") retCULong []

-- | @- getPSM@
getPSM :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
getPSM ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "getPSM") retCUInt []

-- | @- getLocalChannelID@
getLocalChannelID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
getLocalChannelID ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "getLocalChannelID") retCUInt []

-- | @- getRemoteChannelID@
getRemoteChannelID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
getRemoteChannelID ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "getRemoteChannelID") retCUInt []

-- | isIncoming
--
-- Returns TRUE if the channel is an incoming channel.
--
-- An incoming channel is one that was initiated by a remote device.
--
-- Returns: Returns TRUE if the channel is an incoming channel.
--
-- ObjC selector: @- isIncoming@
isIncoming :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO Bool
isIncoming ioBluetoothL2CAPChannel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothL2CAPChannel (mkSelector "isIncoming") retCULong []

-- | registerForChannelCloseNotification:selector:
--
-- Allows a client to register for a channel close notification.
--
-- The given selector will be called on the target observer when the L2CAP channel is closed.				The selector should contain two arguments.  The first is the user notification object. The second				is the IOBluetoothL2CAPChannel that was closed.
--
-- @observer@ — Target observer object
--
-- @inSelector@ — Selector to be sent to the observer when the L2CAP channel is closed.
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding L2CAP channel close notification.				To unregister the notification, call -unregister of the returned IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.
--
-- ObjC selector: @- registerForChannelCloseNotification:selector:@
registerForChannelCloseNotification_selector :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> RawId -> Selector -> IO (Id IOBluetoothUserNotification)
registerForChannelCloseNotification_selector ioBluetoothL2CAPChannel  observer inSelector =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "registerForChannelCloseNotification:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector inSelector)] >>= retainedObject . castPtr

-- | getOutgoingMTU
--
-- Returns the current outgoing MTU for the L2CAP channel.
--
-- The outgoing MTU represents the maximum L2CAP packet size for packets being sent to the remote device.
--
-- Returns: Returns the current outgoing MTU for the L2CAP channel.
--
-- ObjC selector: @- outgoingMTU@
outgoingMTU :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
outgoingMTU ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "outgoingMTU") retCUInt []

-- | getIncomingMTU
--
-- Returns the current incoming MTU for the L2CAP channel.
--
-- The incoming MTU represents the maximum L2CAP packet size for packets being sent by the remote device.
--
-- Returns: Returns the current incoming MTU for the L2CAP channel.
--
-- ObjC selector: @- incomingMTU@
incomingMTU :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
incomingMTU ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "incomingMTU") retCUInt []

-- | getDevice
--
-- Returns the IOBluetoothDevice to which the target L2CAP channel is open.
--
-- Returns: Returns the IOBluetoothDevice to which the target L2CAP channel is open.
--
-- ObjC selector: @- device@
device :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO (Id IOBluetoothDevice)
device ioBluetoothL2CAPChannel  =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | getObjectID
--
-- Returns the IOBluetoothObjectID of the given IOBluetoothL2CAPChannel.
--
-- The IOBluetoothObjectID can be used as a global reference for a given IOBluetoothL2CAPChannel.  It allows				two separate applications to refer to the same IOBluetoothL2CAPChannel.
--
-- Returns: Returns the IOBluetoothObjectID of the given IOBluetoothL2CAPChannel.
--
-- ObjC selector: @- objectID@
objectID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CULong
objectID ioBluetoothL2CAPChannel  =
  sendMsg ioBluetoothL2CAPChannel (mkSelector "objectID") retCULong []

-- | getPSM
--
-- Returns the PSM for the target L2CAP channel.
--
-- Returns: Returns the PSM for the target L2CAP channel.
--
-- ObjC selector: @- PSM@
psm :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
psm ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "PSM") retCUInt []

-- | getLocalChannelID
--
-- Returns the local L2CAP channel ID for the target L2CAP channel.
--
-- Returns: Returns the local L2CAP channel ID for the target L2CAP channel.
--
-- ObjC selector: @- localChannelID@
localChannelID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
localChannelID ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "localChannelID") retCUInt []

-- | getRemoteChannelID
--
-- Returns the remote L2CAP channel ID for the target L2CAP channel.
--
-- Returns: Returns the remote L2CAP channel ID for the target L2CAP channel.
--
-- ObjC selector: @- remoteChannelID@
remoteChannelID :: IsIOBluetoothL2CAPChannel ioBluetoothL2CAPChannel => ioBluetoothL2CAPChannel -> IO CUShort
remoteChannelID ioBluetoothL2CAPChannel  =
  fmap fromIntegral $ sendMsg ioBluetoothL2CAPChannel (mkSelector "remoteChannelID") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerForChannelOpenNotifications:selector:@
registerForChannelOpenNotifications_selectorSelector :: Selector
registerForChannelOpenNotifications_selectorSelector = mkSelector "registerForChannelOpenNotifications:selector:"

-- | @Selector@ for @registerForChannelOpenNotifications:selector:withPSM:direction:@
registerForChannelOpenNotifications_selector_withPSM_directionSelector :: Selector
registerForChannelOpenNotifications_selector_withPSM_directionSelector = mkSelector "registerForChannelOpenNotifications:selector:withPSM:direction:"

-- | @Selector@ for @withObjectID:@
withObjectIDSelector :: Selector
withObjectIDSelector = mkSelector "withObjectID:"

-- | @Selector@ for @closeChannel@
closeChannelSelector :: Selector
closeChannelSelector = mkSelector "closeChannel"

-- | @Selector@ for @getOutgoingMTU@
getOutgoingMTUSelector :: Selector
getOutgoingMTUSelector = mkSelector "getOutgoingMTU"

-- | @Selector@ for @getIncomingMTU@
getIncomingMTUSelector :: Selector
getIncomingMTUSelector = mkSelector "getIncomingMTU"

-- | @Selector@ for @requestRemoteMTU:@
requestRemoteMTUSelector :: Selector
requestRemoteMTUSelector = mkSelector "requestRemoteMTU:"

-- | @Selector@ for @writeAsyncTrap:length:refcon:@
writeAsyncTrap_length_refconSelector :: Selector
writeAsyncTrap_length_refconSelector = mkSelector "writeAsyncTrap:length:refcon:"

-- | @Selector@ for @writeAsync:length:refcon:@
writeAsync_length_refconSelector :: Selector
writeAsync_length_refconSelector = mkSelector "writeAsync:length:refcon:"

-- | @Selector@ for @writeSync:length:@
writeSync_lengthSelector :: Selector
writeSync_lengthSelector = mkSelector "writeSync:length:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @setDelegate:withConfiguration:@
setDelegate_withConfigurationSelector :: Selector
setDelegate_withConfigurationSelector = mkSelector "setDelegate:withConfiguration:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @getDevice@
getDeviceSelector :: Selector
getDeviceSelector = mkSelector "getDevice"

-- | @Selector@ for @getObjectID@
getObjectIDSelector :: Selector
getObjectIDSelector = mkSelector "getObjectID"

-- | @Selector@ for @getPSM@
getPSMSelector :: Selector
getPSMSelector = mkSelector "getPSM"

-- | @Selector@ for @getLocalChannelID@
getLocalChannelIDSelector :: Selector
getLocalChannelIDSelector = mkSelector "getLocalChannelID"

-- | @Selector@ for @getRemoteChannelID@
getRemoteChannelIDSelector :: Selector
getRemoteChannelIDSelector = mkSelector "getRemoteChannelID"

-- | @Selector@ for @isIncoming@
isIncomingSelector :: Selector
isIncomingSelector = mkSelector "isIncoming"

-- | @Selector@ for @registerForChannelCloseNotification:selector:@
registerForChannelCloseNotification_selectorSelector :: Selector
registerForChannelCloseNotification_selectorSelector = mkSelector "registerForChannelCloseNotification:selector:"

-- | @Selector@ for @outgoingMTU@
outgoingMTUSelector :: Selector
outgoingMTUSelector = mkSelector "outgoingMTU"

-- | @Selector@ for @incomingMTU@
incomingMTUSelector :: Selector
incomingMTUSelector = mkSelector "incomingMTU"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @PSM@
psmSelector :: Selector
psmSelector = mkSelector "PSM"

-- | @Selector@ for @localChannelID@
localChannelIDSelector :: Selector
localChannelIDSelector = mkSelector "localChannelID"

-- | @Selector@ for @remoteChannelID@
remoteChannelIDSelector :: Selector
remoteChannelIDSelector = mkSelector "remoteChannelID"

