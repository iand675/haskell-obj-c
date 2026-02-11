{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothRFCOMMChannel
--
-- An instance of this class represents an rfcomm channel as defined by the Bluetooth SDP spec..
--
-- An RFCOMM channel object can be obtained by opening an rfcomm channel in a device, or    by requesting a notification when a channel is created (this is commonly used to provide services).
--
-- Generated bindings for @IOBluetoothRFCOMMChannel@.
module ObjC.IOBluetooth.IOBluetoothRFCOMMChannel
  ( IOBluetoothRFCOMMChannel
  , IsIOBluetoothRFCOMMChannel(..)
  , registerForChannelOpenNotifications_selector
  , registerForChannelOpenNotifications_selector_withChannelID_direction
  , withRFCOMMChannelRef
  , withObjectID
  , getRFCOMMChannelRef
  , closeChannel
  , isOpen
  , getMTU
  , isTransmissionPaused
  , write_length_sleep
  , writeAsync_length_refcon
  , writeSync_length
  , writeSimple_length_sleep_bytesSent
  , setSerialParameters_dataBits_parity_stopBits
  , sendRemoteLineStatus
  , setDelegate
  , delegate
  , getChannelID
  , isIncoming
  , getDevice
  , getObjectID
  , registerForChannelCloseNotification_selector
  , registerForChannelOpenNotifications_selectorSelector
  , registerForChannelOpenNotifications_selector_withChannelID_directionSelector
  , withRFCOMMChannelRefSelector
  , withObjectIDSelector
  , getRFCOMMChannelRefSelector
  , closeChannelSelector
  , isOpenSelector
  , getMTUSelector
  , isTransmissionPausedSelector
  , write_length_sleepSelector
  , writeAsync_length_refconSelector
  , writeSync_lengthSelector
  , writeSimple_length_sleep_bytesSentSelector
  , setSerialParameters_dataBits_parity_stopBitsSelector
  , sendRemoteLineStatusSelector
  , setDelegateSelector
  , delegateSelector
  , getChannelIDSelector
  , isIncomingSelector
  , getDeviceSelector
  , getObjectIDSelector
  , registerForChannelCloseNotification_selectorSelector

  -- * Enum types
  , BluetoothRFCOMMLineStatus(BluetoothRFCOMMLineStatus)
  , pattern BluetoothRFCOMMLineStatusNoError
  , pattern BluetoothRFCOMMLineStatusOverrunError
  , pattern BluetoothRFCOMMLineStatusParityError
  , pattern BluetoothRFCOMMLineStatusFramingError
  , BluetoothRFCOMMParityType(BluetoothRFCOMMParityType)
  , pattern KBluetoothRFCOMMParityTypeNoParity
  , pattern KBluetoothRFCOMMParityTypeOddParity
  , pattern KBluetoothRFCOMMParityTypeEvenParity
  , pattern KBluetoothRFCOMMParityTypeMaxParity
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
-- Allows a client to register for RFCOMM channel open notifications for any RFCOMM channel.
--
-- The given selector will be called on the target object whenever any RFCOMM channel is opened.				The selector should accept two arguments.  The first is the user notification object.  The second				is the IOBluetoothRFCOMMChannel that was opened.
--
-- @rfcommChannelRef@ — IOBluetoothRFCOMMChannelRef for which an IOBluetoothRFCOMMChannel * is desired.
--
-- @object@ — Target object
--
-- @selector@ — Selector to be called on the target object when a new RFCOMM channel is opened.				the format for the selector is: 				-(void) selectorName:(IOBluetoothUserNotification *)inNotification channel:(IOBluetoothRFCOMMChannel *)newChannel
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding RFCOMM channel notification.				To unregister the notification, call -unregister on the resulting IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.  The returned				IOBluetoothUserNotification will be valid for as long as the notification is registered.  It is				not necessary to retain the result.  Once -unregister is called on it, it will no longer be valid.
--
-- ObjC selector: @+ registerForChannelOpenNotifications:selector:@
registerForChannelOpenNotifications_selector :: RawId -> Selector -> IO (Id IOBluetoothUserNotification)
registerForChannelOpenNotifications_selector object selector =
  do
    cls' <- getRequiredClass "IOBluetoothRFCOMMChannel"
    sendClassMsg cls' (mkSelector "registerForChannelOpenNotifications:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | registerForChannelOpenNotifications:selector:
--
-- Allows a client to register for RFCOMM channel open notifications for certain types of				RFCOMM channels.
--
-- The given selector will be called on the target object whenever an RFCOMM channel with the given				attributes is opened.  The selector should accept two arguments.  The first is the user 				notification object.  The second is the IOBluetoothRFCOMMChannel that was opened.
--
-- @object@ — Target object
--
-- @selector@ — Selector to be called on the target object when a new RFCOMM channel is opened.				the format for the selector is: 				-(void) selectorName:(IOBluetoothUserNotification *)inNotification channel:(IOBluetoothRFCOMMChannel *)newChannel
--
-- @channeLID@ — RFCOMM channel ID to match a new RFCOMM channel.  If the channel ID doesn't matter, 0 may be passed in.
--
-- @inDirection@ — The desired direction of the RFCOMM channel - kIOBluetoothUserNotificationChannelDirectionAny				if the direction doesn't matter.
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding RFCOMM channel notification.				To unregister the notification, call -unregister on the resulting IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.  The returned				IOBluetoothUserNotification will be valid for as long as the notification is registered.  It is				not necessary to retain the result.  Once -unregister is called on it, it will no longer be valid.
--
-- ObjC selector: @+ registerForChannelOpenNotifications:selector:withChannelID:direction:@
registerForChannelOpenNotifications_selector_withChannelID_direction :: RawId -> Selector -> CUChar -> IOBluetoothUserNotificationChannelDirection -> IO (Id IOBluetoothUserNotification)
registerForChannelOpenNotifications_selector_withChannelID_direction object selector channelID inDirection =
  do
    cls' <- getRequiredClass "IOBluetoothRFCOMMChannel"
    sendClassMsg cls' (mkSelector "registerForChannelOpenNotifications:selector:withChannelID:direction:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (unSelector selector), argCUChar channelID, argCInt (coerce inDirection)] >>= retainedObject . castPtr

-- | withRFCOMMChannelRef:
--
-- Method call to convert an IOBluetoothRFCOMMChannelRef into an IOBluetoothRFCOMMChannel *.
--
-- @rfcommChannelRef@ — IOBluetoothRFCOMMChannelRef for which an IOBluetoothRFCOMMChannel * is desired.
--
-- Returns: Returns the IOBluetoothRFCOMMChannel * for the given IOBluetoothRFCOMMChannelRef.
--
-- ObjC selector: @+ withRFCOMMChannelRef:@
withRFCOMMChannelRef :: Ptr () -> IO (Id IOBluetoothRFCOMMChannel)
withRFCOMMChannelRef rfcommChannelRef =
  do
    cls' <- getRequiredClass "IOBluetoothRFCOMMChannel"
    sendClassMsg cls' (mkSelector "withRFCOMMChannelRef:") (retPtr retVoid) [argPtr rfcommChannelRef] >>= retainedObject . castPtr

-- | withObjectID:
--
-- Returns the IObluetoothRFCOMMChannel with the given IOBluetoothObjectID.
--
-- The IOBluetoothObjectID can be used as a global reference for a given IObluetoothRFCOMMChannel.  It allows				two separate applications to refer to the same IObluetoothRFCOMMChannel object.
--
-- @objectID@ — IOBluetoothObjectID of the desired IObluetoothRFCOMMChannel.
--
-- Returns: Returns the IObluetoothRFCOMMChannel that matches the given IOBluetoothObjectID if one exists.				If no matching RFCOMM channel exists, nil is returned.
--
-- ObjC selector: @+ withObjectID:@
withObjectID :: CULong -> IO (Id IOBluetoothRFCOMMChannel)
withObjectID objectID =
  do
    cls' <- getRequiredClass "IOBluetoothRFCOMMChannel"
    sendClassMsg cls' (mkSelector "withObjectID:") (retPtr retVoid) [argCULong objectID] >>= retainedObject . castPtr

-- | getRFCOMMChannelRef
--
-- Returns an IOBluetoothRFCOMMChannelRef representation of the target IOBluetoothRFCOMMChannel object.
--
-- Returns: Returns an IOBluetoothRFCOMMChannelRef representation of the target IOBluetoothRFCOMMChannel object.
--
-- ObjC selector: @- getRFCOMMChannelRef@
getRFCOMMChannelRef :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO (Ptr ())
getRFCOMMChannelRef ioBluetoothRFCOMMChannel  =
    fmap castPtr $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "getRFCOMMChannelRef") (retPtr retVoid) []

-- | closeChannel
--
-- Close the channel.
--
-- Returns: An error code value. 0 if successful.
--
-- ObjC selector: @- closeChannel@
closeChannel :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO CInt
closeChannel ioBluetoothRFCOMMChannel  =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "closeChannel") retCInt []

-- | isOpen
--
-- Returns the state of the channel.
--
-- note that "not open" means closed, opening and closing.
--
-- Returns: TRUE if the channel state is open,  FALSE otherwise.
--
-- ObjC selector: @- isOpen@
isOpen :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO Bool
isOpen ioBluetoothRFCOMMChannel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "isOpen") retCULong []

-- | getMTU
--
-- Returns the channel maximum transfer unit.
--
-- Returns the length of the largest chunk of data that this channel can carry. If the    caller wishes to use the write:length:sleep: api the length of the data can not be bigger than    the channel MTU (maximum transfer unit).
--
-- Returns: Channel MTU size .
--
-- ObjC selector: @- getMTU@
getMTU :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO CUShort
getMTU ioBluetoothRFCOMMChannel  =
    fmap fromIntegral $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "getMTU") retCUInt []

-- | isTransmissionPaused
--
-- Returns TRUE if flow control is off.
--
-- Returns true if the remote device flow control is stopping out transmission. This is            useful because we do not buffer data, we stop the transmitting actor. With this method            the transmitter can check if sending data is going to be successful or is going to block.
--
-- Returns: TRUE if the action of sending data will block the current thread, FALSE otherwise.
--
-- ObjC selector: @- isTransmissionPaused@
isTransmissionPaused :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO Bool
isTransmissionPaused ioBluetoothRFCOMMChannel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "isTransmissionPaused") retCULong []

-- | write:length:sleep:
--
-- Sends a block of data in the channel syncronously.
--
-- ***WARNING*** This method is being deprecated in favor of -writeSync:... and -writeAsync:...				Sends data through the channel. The number of bytes to be sent must not exceed the channel MTU. 				If the return value is an error condition none of the data was sent.
--
-- @data@ — is a pointer to the data buffer to be sent.
--
-- @length@ — the length of the buffer to be sent (in bytes).
--
-- @sleep@ — is a boolean if set to TRUE the call will wait until it is possible to send data.    If set to FALSE and it is not possible to send data the method will return immediately with an    error.
--
-- Returns: An error code value. 0 if successful.
--
-- ObjC selector: @- write:length:sleep:@
write_length_sleep :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> Ptr () -> CUShort -> Bool -> IO CInt
write_length_sleep ioBluetoothRFCOMMChannel  data_ length_ sleep =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "write:length:sleep:") retCInt [argPtr data_, argCUInt (fromIntegral length_), argCULong (if sleep then 1 else 0)]

-- | writeAsync:length:refcon:
--
-- Sends a block of data in the channel asynchronously.
--
-- The number of bytes to be sent must not exceed the channel MTU. 				If the return value is an error condition none of the data was sent.  Once the data				has been successfully passed to the hardware to be transmitted, the delegate method				-rfcommChannelWriteComplete:refcon:status: will be called with the refcon that was passed				to this method.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @data@ — A pointer to the data buffer to be sent.
--
-- @length@ — The length of the buffer to be sent (in bytes).
--
-- @refcon@ — User supplied value that gets passed to the write callback.
--
-- Returns: Returns kIOReturnSuccess if the data was buffered successfully.
--
-- ObjC selector: @- writeAsync:length:refcon:@
writeAsync_length_refcon :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> Ptr () -> CUShort -> Ptr () -> IO CInt
writeAsync_length_refcon ioBluetoothRFCOMMChannel  data_ length_ refcon =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "writeAsync:length:refcon:") retCInt [argPtr data_, argCUInt (fromIntegral length_), argPtr refcon]

-- | writeSync:length:
--
-- Sends a block of data in the channel synchronously.
--
-- Sends data through the channel. The number of bytes to be sent must not exceed the channel MTU. 				If the return value is an error condition none of the data was sent.  This method will				block until the data has been successfully sent to the hardware for transmission (or until				an error occurs).
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.
--
-- @data@ — A pointer to the data buffer to be sent.
--
-- @length@ — The length of the buffer to be sent (in bytes).
--
-- Returns: Returns kIOReturnSuccess if the data was written successfully.
--
-- ObjC selector: @- writeSync:length:@
writeSync_length :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> Ptr () -> CUShort -> IO CInt
writeSync_length ioBluetoothRFCOMMChannel  data_ length_ =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "writeSync:length:") retCInt [argPtr data_, argCUInt (fromIntegral length_)]

-- | writeSimple:length:sleep:
--
-- Sends a block of data in the channel.
--
-- ***WARNING*** This method is being deprecated in favor of -writeSync:... and -writeAsync:...				Sends data through the channel. The number of bytes to be sent is arbitrary. The caller				does not have to worry about the MTU.
--
-- @data@ — a pointer to the data buffer to be sent.
--
-- @length@ — the length of the buffer to be sent (in bytes).
--
-- @sleep@ — a boolean if set to TRUE the call will wait until it is possible to send all the data.
--
-- @a@ — UInt32 pointer in which the caller received the nuber of bytes sent.    If set to FALSE and it is not possible to send part of the data the method will return immediately.
--
-- Returns: An error code value. 0 if successful.
--
-- ObjC selector: @- writeSimple:length:sleep:bytesSent:@
writeSimple_length_sleep_bytesSent :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> Ptr () -> CUShort -> Bool -> RawId -> IO CInt
writeSimple_length_sleep_bytesSent ioBluetoothRFCOMMChannel  data_ length_ sleep numBytesSent =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "writeSimple:length:sleep:bytesSent:") retCInt [argPtr data_, argCUInt (fromIntegral length_), argCULong (if sleep then 1 else 0), argPtr (castPtr (unRawId numBytesSent) :: Ptr ())]

-- | setSerialParameters:dataBits:parity:stopBits:
--
-- Changes the parameters of the serial connection.
--
-- @speed@ — the baudrate.
--
-- @nBits@ — number of data bits.
--
-- @parity@ — the type of parity can be NoParity, OddParity, EvenParity or MaxParity.
--
-- @bitStop@ — number of stop bits.
--
-- Returns: An error code value. 0 if successful.
--
-- ObjC selector: @- setSerialParameters:dataBits:parity:stopBits:@
setSerialParameters_dataBits_parity_stopBits :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> CUInt -> CUChar -> BluetoothRFCOMMParityType -> CUChar -> IO CInt
setSerialParameters_dataBits_parity_stopBits ioBluetoothRFCOMMChannel  speed nBits parity bitStop =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "setSerialParameters:dataBits:parity:stopBits:") retCInt [argCUInt speed, argCUChar nBits, argCInt (coerce parity), argCUChar bitStop]

-- | sendRemoteLineStatus:
--
-- Sends an error to the remote side.
--
-- @lineStatus@ — the error type. The error code can be NoError, OverrunError, ParityError or FramingError.
--
-- Returns: An error code value. 0 if successful.
--
-- ObjC selector: @- sendRemoteLineStatus:@
sendRemoteLineStatus :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> BluetoothRFCOMMLineStatus -> IO CInt
sendRemoteLineStatus ioBluetoothRFCOMMChannel  lineStatus =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "sendRemoteLineStatus:") retCInt [argCInt (coerce lineStatus)]

-- | setDelegate:
--
-- Allows an object to register itself as a client of the RFCOMM channel.
--
-- A channel delegate is the object the RFCOMM channel uses as target for data and events. The				developer will implement only the the methods he/she is interested in. A list of the				possible methods is at the end of this file in the definition of the informal protocol				IOBluetoothRFCOMMChannelDelegate.
--
-- NOTE: This method is only available in Mac OS X 10.2.5 (Bluetooth v1.2) or later.				NOTE: Before Mac OS X 10.6, the delegate was retained.  On 10.6 and later, it is not.
--
-- @delegate@ — The object that will play the role of channel delegate [NOTE the rfcomm channel will reatin the delegate].
--
-- Returns: Returns kIOReturnSuccess if the delegate is successfully registered.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> RawId -> IO CInt
setDelegate ioBluetoothRFCOMMChannel  delegate =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "setDelegate:") retCInt [argPtr (castPtr (unRawId delegate) :: Ptr ())]

-- | delegate
--
-- Returns the object delegate
--
-- Returns: the current delegate, or nil
--
-- ObjC selector: @- delegate@
delegate :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO RawId
delegate ioBluetoothRFCOMMChannel  =
    fmap (RawId . castPtr) $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "delegate") (retPtr retVoid) []

-- | channelNumber
--
-- Returns the object rfcomm channel ID.
--
-- Returns: the RFCOMM channel number .
--
-- ObjC selector: @- getChannelID@
getChannelID :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO CUChar
getChannelID ioBluetoothRFCOMMChannel  =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "getChannelID") retCUChar []

-- | isIncoming
--
-- Returns the direction of the channel.  An incoming channel is one that was opened by the remote                device.
--
-- Returns: Returns TRUE if the channel was opened by the remote device, FALSE if the channel was opened by this object.
--
-- ObjC selector: @- isIncoming@
isIncoming :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO Bool
isIncoming ioBluetoothRFCOMMChannel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothRFCOMMChannel (mkSelector "isIncoming") retCULong []

-- | getDevice
--
-- Returns the Bluetooth Device that carries the rfcomm data.
--
-- Returns: the IOBluetoothDevice object .
--
-- ObjC selector: @- getDevice@
getDevice :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO (Id IOBluetoothDevice)
getDevice ioBluetoothRFCOMMChannel  =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "getDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | getObjectID
--
-- Returns the IOBluetoothObjectID of the given IOBluetoothRFCOMMChannel.
--
-- The IOBluetoothObjectID can be used as a global reference for a given IOBluetoothRFCOMMChannel.  It allows				two separate applications to refer to the same IOBluetoothRFCOMMChannel.
--
-- Returns: Returns the IOBluetoothObjectID of the given IOBluetoothRFCOMMChannel.
--
-- ObjC selector: @- getObjectID@
getObjectID :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> IO CULong
getObjectID ioBluetoothRFCOMMChannel  =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "getObjectID") retCULong []

-- | registerForChannelCloseNotification:selector:
--
-- Allows a client to register for a channel close notification.
--
-- The given selector will be called on the target observer when the RFCOMM channel is closed.				The selector should contain two arguments.  The first is the user notification object. The second				is the IOBluetoothRFCOMMChannel that was closed.
--
-- @observer@ — Target observer object
--
-- @inSelector@ — Selector to be sent to the observer when the RFCOMM channel is closed.
--
-- Returns: Returns an IOBluetoothUserNotification representing the outstanding RFCOMM channel close notification.				To unregister the notification, call -unregister of the returned IOBluetoothUserNotification 				object.  If an error is encountered creating the notification, nil is returned.
--
-- ObjC selector: @- registerForChannelCloseNotification:selector:@
registerForChannelCloseNotification_selector :: IsIOBluetoothRFCOMMChannel ioBluetoothRFCOMMChannel => ioBluetoothRFCOMMChannel -> RawId -> Selector -> IO (Id IOBluetoothUserNotification)
registerForChannelCloseNotification_selector ioBluetoothRFCOMMChannel  observer inSelector =
    sendMsg ioBluetoothRFCOMMChannel (mkSelector "registerForChannelCloseNotification:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (unSelector inSelector)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerForChannelOpenNotifications:selector:@
registerForChannelOpenNotifications_selectorSelector :: Selector
registerForChannelOpenNotifications_selectorSelector = mkSelector "registerForChannelOpenNotifications:selector:"

-- | @Selector@ for @registerForChannelOpenNotifications:selector:withChannelID:direction:@
registerForChannelOpenNotifications_selector_withChannelID_directionSelector :: Selector
registerForChannelOpenNotifications_selector_withChannelID_directionSelector = mkSelector "registerForChannelOpenNotifications:selector:withChannelID:direction:"

-- | @Selector@ for @withRFCOMMChannelRef:@
withRFCOMMChannelRefSelector :: Selector
withRFCOMMChannelRefSelector = mkSelector "withRFCOMMChannelRef:"

-- | @Selector@ for @withObjectID:@
withObjectIDSelector :: Selector
withObjectIDSelector = mkSelector "withObjectID:"

-- | @Selector@ for @getRFCOMMChannelRef@
getRFCOMMChannelRefSelector :: Selector
getRFCOMMChannelRefSelector = mkSelector "getRFCOMMChannelRef"

-- | @Selector@ for @closeChannel@
closeChannelSelector :: Selector
closeChannelSelector = mkSelector "closeChannel"

-- | @Selector@ for @isOpen@
isOpenSelector :: Selector
isOpenSelector = mkSelector "isOpen"

-- | @Selector@ for @getMTU@
getMTUSelector :: Selector
getMTUSelector = mkSelector "getMTU"

-- | @Selector@ for @isTransmissionPaused@
isTransmissionPausedSelector :: Selector
isTransmissionPausedSelector = mkSelector "isTransmissionPaused"

-- | @Selector@ for @write:length:sleep:@
write_length_sleepSelector :: Selector
write_length_sleepSelector = mkSelector "write:length:sleep:"

-- | @Selector@ for @writeAsync:length:refcon:@
writeAsync_length_refconSelector :: Selector
writeAsync_length_refconSelector = mkSelector "writeAsync:length:refcon:"

-- | @Selector@ for @writeSync:length:@
writeSync_lengthSelector :: Selector
writeSync_lengthSelector = mkSelector "writeSync:length:"

-- | @Selector@ for @writeSimple:length:sleep:bytesSent:@
writeSimple_length_sleep_bytesSentSelector :: Selector
writeSimple_length_sleep_bytesSentSelector = mkSelector "writeSimple:length:sleep:bytesSent:"

-- | @Selector@ for @setSerialParameters:dataBits:parity:stopBits:@
setSerialParameters_dataBits_parity_stopBitsSelector :: Selector
setSerialParameters_dataBits_parity_stopBitsSelector = mkSelector "setSerialParameters:dataBits:parity:stopBits:"

-- | @Selector@ for @sendRemoteLineStatus:@
sendRemoteLineStatusSelector :: Selector
sendRemoteLineStatusSelector = mkSelector "sendRemoteLineStatus:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @getChannelID@
getChannelIDSelector :: Selector
getChannelIDSelector = mkSelector "getChannelID"

-- | @Selector@ for @isIncoming@
isIncomingSelector :: Selector
isIncomingSelector = mkSelector "isIncoming"

-- | @Selector@ for @getDevice@
getDeviceSelector :: Selector
getDeviceSelector = mkSelector "getDevice"

-- | @Selector@ for @getObjectID@
getObjectIDSelector :: Selector
getObjectIDSelector = mkSelector "getObjectID"

-- | @Selector@ for @registerForChannelCloseNotification:selector:@
registerForChannelCloseNotification_selectorSelector :: Selector
registerForChannelCloseNotification_selectorSelector = mkSelector "registerForChannelCloseNotification:selector:"

