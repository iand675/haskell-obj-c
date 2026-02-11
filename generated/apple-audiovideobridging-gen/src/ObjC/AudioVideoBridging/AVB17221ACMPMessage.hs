{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVB17221ACMPMessage
--
-- AVB17221ACMPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol message.
--
-- AVB17221ACMPMessage encapsulates an IEEE Std 1722.1™-2013 AVDECC Connection Management Protocol (ACMP) message.
--
-- Generated bindings for @AVB17221ACMPMessage@.
module ObjC.AudioVideoBridging.AVB17221ACMPMessage
  ( AVB17221ACMPMessage
  , IsAVB17221ACMPMessage(..)
  , avB17221ACMPMessageErrorForStatusCode
  , errorForStatusCode
  , messageType
  , setMessageType
  , status
  , setStatus
  , streamID
  , setStreamID
  , controllerEntityID
  , setControllerEntityID
  , talkerEntityID
  , setTalkerEntityID
  , listenerEntityID
  , setListenerEntityID
  , talkerUniqueID
  , setTalkerUniqueID
  , listenerUniqueID
  , setListenerUniqueID
  , destinationMAC
  , setDestinationMAC
  , connectionCount
  , setConnectionCount
  , sequenceID
  , setSequenceID
  , flags
  , setFlags
  , vlanID
  , setVlanID
  , connectedListenersEntries
  , setConnectedListenersEntries
  , ipFlags
  , setIpFlags
  , sourcePort
  , setSourcePort
  , destinationPort
  , setDestinationPort
  , sourceIPAddress
  , setSourceIPAddress
  , destinationIPAddress
  , setDestinationIPAddress
  , sourceMAC
  , setSourceMAC
  , errorForStatusCodeSelector
  , messageTypeSelector
  , setMessageTypeSelector
  , statusSelector
  , setStatusSelector
  , streamIDSelector
  , setStreamIDSelector
  , controllerEntityIDSelector
  , setControllerEntityIDSelector
  , talkerEntityIDSelector
  , setTalkerEntityIDSelector
  , listenerEntityIDSelector
  , setListenerEntityIDSelector
  , talkerUniqueIDSelector
  , setTalkerUniqueIDSelector
  , listenerUniqueIDSelector
  , setListenerUniqueIDSelector
  , destinationMACSelector
  , setDestinationMACSelector
  , connectionCountSelector
  , setConnectionCountSelector
  , sequenceIDSelector
  , setSequenceIDSelector
  , flagsSelector
  , setFlagsSelector
  , vlanIDSelector
  , setVlanIDSelector
  , connectedListenersEntriesSelector
  , setConnectedListenersEntriesSelector
  , ipFlagsSelector
  , setIpFlagsSelector
  , sourcePortSelector
  , setSourcePortSelector
  , destinationPortSelector
  , setDestinationPortSelector
  , sourceIPAddressSelector
  , setSourceIPAddressSelector
  , destinationIPAddressSelector
  , setDestinationIPAddressSelector
  , sourceMACSelector
  , setSourceMACSelector

  -- * Enum types
  , AVB17221ACMPFlags(AVB17221ACMPFlags)
  , pattern AVB17221ACMPFlagsNone
  , pattern AVB17221ACMPFlagsClassB
  , pattern AVB17221ACMPFlagsFastConnect
  , pattern AVB17221ACMPFlagsSavedState
  , pattern AVB17221ACMPFlagsStreamingWait
  , pattern AVB17221ACMPFlagsSupportsEncrypted
  , pattern AVB17221ACMPFlagsEncryptedPDU
  , pattern AVB17221ACMPFlagsStreamingTalkerFailed
  , pattern AVB17221ACMPFlagsStreamingConnectedListenersValid
  , pattern AVB17221ACMPFlagsStreamingNoStreamReservationProtocol
  , pattern AVB17221ACMPFlagsStreamingUsingUDP
  , AVB17221ACMPIPFlag(AVB17221ACMPIPFlag)
  , pattern AVB17221ACMPIPFlagNone
  , AVB17221ACMPMessageType(AVB17221ACMPMessageType)
  , pattern AVB17221ACMPMessageTypeConnectTXCommand
  , pattern AVB17221ACMPMessageTypeConnectTXResponse
  , pattern AVB17221ACMPMessageTypeDisconnectTXCommand
  , pattern AVB17221ACMPMessageTypeDisconnectTXResponse
  , pattern AVB17221ACMPMessageTypeGetTXStateCommand
  , pattern AVB17221ACMPMessageTypeGetTXStateResponse
  , pattern AVB17221ACMPMessageTypeConnectRXCommand
  , pattern AVB17221ACMPMessageTypeConnectRXResponse
  , pattern AVB17221ACMPMessageTypeDisconnectRXCommand
  , pattern AVB17221ACMPMessageTypeDisconnectRXResponse
  , pattern AVB17221ACMPMessageTypeGetRXStateCommand
  , pattern AVB17221ACMPMessageTypeGetRXStateResponse
  , pattern AVB17221ACMPMessageTypeGetTXConnectionCommand
  , pattern AVB17221ACMPMessageTypeGetTXConnectionResponse
  , AVB17221ACMPStatusCode(AVB17221ACMPStatusCode)
  , pattern AVB17221ACMPStatusSuccess
  , pattern AVB17221ACMPStatusListenerUnknownID
  , pattern AVB17221ACMPStatusTalkerUnknownID
  , pattern AVB17221ACMPStatusTalkerDestMACFail
  , pattern AVB17221ACMPStatusTalkerNoStreamIndex
  , pattern AVB17221ACMPStatusTalkerNoBandwidth
  , pattern AVB17221ACMPStatusTalkerExclusive
  , pattern AVB17221ACMPStatusListenerTalkerTimeout
  , pattern AVB17221ACMPStatusListenerExclusive
  , pattern AVB17221ACMPStatusStateUnavailable
  , pattern AVB17221ACMPStatusNotConnected
  , pattern AVB17221ACMPStatusNoSuchConnection
  , pattern AVB17221ACMPStatusUnableToSendMessage
  , pattern AVB17221ACMPStatusTalkerMisbehaving
  , pattern AVB17221ACMPStatusListenerMisbehaving
  , pattern AVB17221ACMPStatusSRPFace
  , pattern AVB17221ACMPStatusControllerNotAuthorized
  , pattern AVB17221ACMPStatusIncompatibleRequest
  , pattern AVB17221ACMPStatusListenerInvalidConnection
  , pattern AVB17221ACMPStatusListenerCanOnlyListenOnce
  , pattern AVB17221ACMPStatusNotSupported

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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.AudioVideoBridging.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | errorForStatusCode:
--
-- This method returns an NSError filled out with an appropriate description for the passed in status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @+ errorForStatusCode:@
avB17221ACMPMessageErrorForStatusCode :: AVB17221ACMPStatusCode -> IO (Id NSError)
avB17221ACMPMessageErrorForStatusCode statusCode =
  do
    cls' <- getRequiredClass "AVB17221ACMPMessage"
    sendClassMsg cls' (mkSelector "errorForStatusCode:") (retPtr retVoid) [argCUChar (coerce statusCode)] >>= retainedObject . castPtr

-- | errorForStatusCode
--
-- This method returns an NSError filled out with an appropriate description for the message's status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @- errorForStatusCode@
errorForStatusCode :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id NSError)
errorForStatusCode avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "errorForStatusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | messageType
--
-- The message_type field of the ACMP message.
--
-- ObjC selector: @- messageType@
messageType :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPMessageType
messageType avB17221ACMPMessage  =
    fmap (coerce :: CUChar -> AVB17221ACMPMessageType) $ sendMsg avB17221ACMPMessage (mkSelector "messageType") retCUChar []

-- | messageType
--
-- The message_type field of the ACMP message.
--
-- ObjC selector: @- setMessageType:@
setMessageType :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPMessageType -> IO ()
setMessageType avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setMessageType:") retVoid [argCUChar (coerce value)]

-- | status
--
-- The status field of the ACMP message.
--
-- ObjC selector: @- status@
status :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPStatusCode
status avB17221ACMPMessage  =
    fmap (coerce :: CUChar -> AVB17221ACMPStatusCode) $ sendMsg avB17221ACMPMessage (mkSelector "status") retCUChar []

-- | status
--
-- The status field of the ACMP message.
--
-- ObjC selector: @- setStatus:@
setStatus :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPStatusCode -> IO ()
setStatus avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setStatus:") retVoid [argCUChar (coerce value)]

-- | streamID
--
-- The stream_id field of the ACMP message.
--
-- ObjC selector: @- streamID@
streamID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
streamID avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "streamID") retCULong []

-- | streamID
--
-- The stream_id field of the ACMP message.
--
-- ObjC selector: @- setStreamID:@
setStreamID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setStreamID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setStreamID:") retVoid [argCULong value]

-- | controllerGUID
--
-- The controller_entity_id field of the ACMP message.
--
-- ObjC selector: @- controllerEntityID@
controllerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
controllerEntityID avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "controllerEntityID") retCULong []

-- | controllerGUID
--
-- The controller_entity_id field of the ACMP message.
--
-- ObjC selector: @- setControllerEntityID:@
setControllerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setControllerEntityID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setControllerEntityID:") retVoid [argCULong value]

-- | talkerEntityID
--
-- The talker_entity_id field of the ACMP message.
--
-- ObjC selector: @- talkerEntityID@
talkerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
talkerEntityID avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "talkerEntityID") retCULong []

-- | talkerEntityID
--
-- The talker_entity_id field of the ACMP message.
--
-- ObjC selector: @- setTalkerEntityID:@
setTalkerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setTalkerEntityID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setTalkerEntityID:") retVoid [argCULong value]

-- | listenerEntityID
--
-- The listener_entity_id field of the ACMP message.
--
-- ObjC selector: @- listenerEntityID@
listenerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
listenerEntityID avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "listenerEntityID") retCULong []

-- | listenerEntityID
--
-- The listener_entity_id field of the ACMP message.
--
-- ObjC selector: @- setListenerEntityID:@
setListenerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setListenerEntityID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setListenerEntityID:") retVoid [argCULong value]

-- | talkerUniqueID
--
-- The talker_unique_id field of the ACMP message.
--
-- ObjC selector: @- talkerUniqueID@
talkerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
talkerUniqueID avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "talkerUniqueID") retCUInt []

-- | talkerUniqueID
--
-- The talker_unique_id field of the ACMP message.
--
-- ObjC selector: @- setTalkerUniqueID:@
setTalkerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setTalkerUniqueID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setTalkerUniqueID:") retVoid [argCUInt (fromIntegral value)]

-- | listenerUniqueID
--
-- The listener_unique_id field of the ACMP message.
--
-- ObjC selector: @- listenerUniqueID@
listenerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
listenerUniqueID avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "listenerUniqueID") retCUInt []

-- | listenerUniqueID
--
-- The listener_unique_id field of the ACMP message.
--
-- ObjC selector: @- setListenerUniqueID:@
setListenerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setListenerUniqueID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setListenerUniqueID:") retVoid [argCUInt (fromIntegral value)]

-- | destinationMAC
--
-- The dest_mac field of the ACMP message.
--
-- ObjC selector: @- destinationMAC@
destinationMAC :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBMACAddress)
destinationMAC avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "destinationMAC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | destinationMAC
--
-- The dest_mac field of the ACMP message.
--
-- ObjC selector: @- setDestinationMAC:@
setDestinationMAC :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBMACAddress value) => avB17221ACMPMessage -> value -> IO ()
setDestinationMAC avB17221ACMPMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avB17221ACMPMessage (mkSelector "setDestinationMAC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | connectionCount
--
-- The connection_count field of the ACMP message.
--
-- ObjC selector: @- connectionCount@
connectionCount :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
connectionCount avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "connectionCount") retCUInt []

-- | connectionCount
--
-- The connection_count field of the ACMP message.
--
-- ObjC selector: @- setConnectionCount:@
setConnectionCount :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setConnectionCount avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setConnectionCount:") retVoid [argCUInt (fromIntegral value)]

-- | sequenceID
--
-- The sequence_id field of the ACMP message.
--
-- ObjC selector: @- sequenceID@
sequenceID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
sequenceID avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "sequenceID") retCUInt []

-- | sequenceID
--
-- The sequence_id field of the ACMP message.
--
-- ObjC selector: @- setSequenceID:@
setSequenceID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setSequenceID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setSequenceID:") retVoid [argCUInt (fromIntegral value)]

-- | flags
--
-- The flags field of the ACMP message.
--
-- ObjC selector: @- flags@
flags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPFlags
flags avB17221ACMPMessage  =
    fmap (AVB17221ACMPFlags . fromIntegral :: CUInt -> AVB17221ACMPFlags) $ sendMsg avB17221ACMPMessage (mkSelector "flags") retCUInt []

-- | flags
--
-- The flags field of the ACMP message.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPFlags -> IO ()
setFlags avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setFlags:") retVoid [argCUInt (fromIntegral (coerce value :: CUShort))]

-- | vlanID
--
-- The stream_vlan_id field of the ACMP message.
--
-- ObjC selector: @- vlanID@
vlanID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
vlanID avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "vlanID") retCUInt []

-- | vlanID
--
-- The stream_vlan_id field of the ACMP message.
--
-- ObjC selector: @- setVlanID:@
setVlanID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setVlanID avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setVlanID:") retVoid [argCUInt (fromIntegral value)]

-- | connectedListenersEntries
--
-- The connected_listeners_entries field of the ACMP message.
--
-- ObjC selector: @- connectedListenersEntries@
connectedListenersEntries :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
connectedListenersEntries avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "connectedListenersEntries") retCUInt []

-- | connectedListenersEntries
--
-- The connected_listeners_entries field of the ACMP message.
--
-- ObjC selector: @- setConnectedListenersEntries:@
setConnectedListenersEntries :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setConnectedListenersEntries avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setConnectedListenersEntries:") retVoid [argCUInt (fromIntegral value)]

-- | ipFlags
--
-- The ip_flags field of the ACMP message.
--
-- ObjC selector: @- ipFlags@
ipFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPIPFlag
ipFlags avB17221ACMPMessage  =
    fmap (AVB17221ACMPIPFlag . fromIntegral :: CUInt -> AVB17221ACMPIPFlag) $ sendMsg avB17221ACMPMessage (mkSelector "ipFlags") retCUInt []

-- | ipFlags
--
-- The ip_flags field of the ACMP message.
--
-- ObjC selector: @- setIpFlags:@
setIpFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPIPFlag -> IO ()
setIpFlags avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setIpFlags:") retVoid [argCUInt (fromIntegral (coerce value :: CUShort))]

-- | sourcePort
--
-- The source_port field of the ACMP message.
--
-- ObjC selector: @- sourcePort@
sourcePort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
sourcePort avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "sourcePort") retCUInt []

-- | sourcePort
--
-- The source_port field of the ACMP message.
--
-- ObjC selector: @- setSourcePort:@
setSourcePort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setSourcePort avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setSourcePort:") retVoid [argCUInt (fromIntegral value)]

-- | destinationPort
--
-- The destination_port field of the ACMP message.
--
-- ObjC selector: @- destinationPort@
destinationPort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
destinationPort avB17221ACMPMessage  =
    fmap fromIntegral $ sendMsg avB17221ACMPMessage (mkSelector "destinationPort") retCUInt []

-- | destinationPort
--
-- The destination_port field of the ACMP message.
--
-- ObjC selector: @- setDestinationPort:@
setDestinationPort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setDestinationPort avB17221ACMPMessage  value =
    sendMsg avB17221ACMPMessage (mkSelector "setDestinationPort:") retVoid [argCUInt (fromIntegral value)]

-- | sourceAddress
--
-- The source_ip_address field of the ACMP message.
--
-- ObjC selector: @- sourceIPAddress@
sourceIPAddress :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBIPAddress)
sourceIPAddress avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "sourceIPAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceAddress
--
-- The source_ip_address field of the ACMP message.
--
-- ObjC selector: @- setSourceIPAddress:@
setSourceIPAddress :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBIPAddress value) => avB17221ACMPMessage -> value -> IO ()
setSourceIPAddress avB17221ACMPMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avB17221ACMPMessage (mkSelector "setSourceIPAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | destinationAddress
--
-- The destination_ip_address field of the ACMP message.
--
-- ObjC selector: @- destinationIPAddress@
destinationIPAddress :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBIPAddress)
destinationIPAddress avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "destinationIPAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | destinationAddress
--
-- The destination_ip_address field of the ACMP message.
--
-- ObjC selector: @- setDestinationIPAddress:@
setDestinationIPAddress :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBIPAddress value) => avB17221ACMPMessage -> value -> IO ()
setDestinationIPAddress avB17221ACMPMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avB17221ACMPMessage (mkSelector "setDestinationIPAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sourceMAC
--
-- The source_mac field of the ACMP message.
--
-- ObjC selector: @- sourceMAC@
sourceMAC :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBMACAddress)
sourceMAC avB17221ACMPMessage  =
    sendMsg avB17221ACMPMessage (mkSelector "sourceMAC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sourceMAC
--
-- The source_mac field of the ACMP message.
--
-- ObjC selector: @- setSourceMAC:@
setSourceMAC :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBMACAddress value) => avB17221ACMPMessage -> value -> IO ()
setSourceMAC avB17221ACMPMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avB17221ACMPMessage (mkSelector "setSourceMAC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorForStatusCode:@
errorForStatusCodeSelector :: Selector
errorForStatusCodeSelector = mkSelector "errorForStatusCode:"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @setMessageType:@
setMessageTypeSelector :: Selector
setMessageTypeSelector = mkSelector "setMessageType:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @streamID@
streamIDSelector :: Selector
streamIDSelector = mkSelector "streamID"

-- | @Selector@ for @setStreamID:@
setStreamIDSelector :: Selector
setStreamIDSelector = mkSelector "setStreamID:"

-- | @Selector@ for @controllerEntityID@
controllerEntityIDSelector :: Selector
controllerEntityIDSelector = mkSelector "controllerEntityID"

-- | @Selector@ for @setControllerEntityID:@
setControllerEntityIDSelector :: Selector
setControllerEntityIDSelector = mkSelector "setControllerEntityID:"

-- | @Selector@ for @talkerEntityID@
talkerEntityIDSelector :: Selector
talkerEntityIDSelector = mkSelector "talkerEntityID"

-- | @Selector@ for @setTalkerEntityID:@
setTalkerEntityIDSelector :: Selector
setTalkerEntityIDSelector = mkSelector "setTalkerEntityID:"

-- | @Selector@ for @listenerEntityID@
listenerEntityIDSelector :: Selector
listenerEntityIDSelector = mkSelector "listenerEntityID"

-- | @Selector@ for @setListenerEntityID:@
setListenerEntityIDSelector :: Selector
setListenerEntityIDSelector = mkSelector "setListenerEntityID:"

-- | @Selector@ for @talkerUniqueID@
talkerUniqueIDSelector :: Selector
talkerUniqueIDSelector = mkSelector "talkerUniqueID"

-- | @Selector@ for @setTalkerUniqueID:@
setTalkerUniqueIDSelector :: Selector
setTalkerUniqueIDSelector = mkSelector "setTalkerUniqueID:"

-- | @Selector@ for @listenerUniqueID@
listenerUniqueIDSelector :: Selector
listenerUniqueIDSelector = mkSelector "listenerUniqueID"

-- | @Selector@ for @setListenerUniqueID:@
setListenerUniqueIDSelector :: Selector
setListenerUniqueIDSelector = mkSelector "setListenerUniqueID:"

-- | @Selector@ for @destinationMAC@
destinationMACSelector :: Selector
destinationMACSelector = mkSelector "destinationMAC"

-- | @Selector@ for @setDestinationMAC:@
setDestinationMACSelector :: Selector
setDestinationMACSelector = mkSelector "setDestinationMAC:"

-- | @Selector@ for @connectionCount@
connectionCountSelector :: Selector
connectionCountSelector = mkSelector "connectionCount"

-- | @Selector@ for @setConnectionCount:@
setConnectionCountSelector :: Selector
setConnectionCountSelector = mkSelector "setConnectionCount:"

-- | @Selector@ for @sequenceID@
sequenceIDSelector :: Selector
sequenceIDSelector = mkSelector "sequenceID"

-- | @Selector@ for @setSequenceID:@
setSequenceIDSelector :: Selector
setSequenceIDSelector = mkSelector "setSequenceID:"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @vlanID@
vlanIDSelector :: Selector
vlanIDSelector = mkSelector "vlanID"

-- | @Selector@ for @setVlanID:@
setVlanIDSelector :: Selector
setVlanIDSelector = mkSelector "setVlanID:"

-- | @Selector@ for @connectedListenersEntries@
connectedListenersEntriesSelector :: Selector
connectedListenersEntriesSelector = mkSelector "connectedListenersEntries"

-- | @Selector@ for @setConnectedListenersEntries:@
setConnectedListenersEntriesSelector :: Selector
setConnectedListenersEntriesSelector = mkSelector "setConnectedListenersEntries:"

-- | @Selector@ for @ipFlags@
ipFlagsSelector :: Selector
ipFlagsSelector = mkSelector "ipFlags"

-- | @Selector@ for @setIpFlags:@
setIpFlagsSelector :: Selector
setIpFlagsSelector = mkSelector "setIpFlags:"

-- | @Selector@ for @sourcePort@
sourcePortSelector :: Selector
sourcePortSelector = mkSelector "sourcePort"

-- | @Selector@ for @setSourcePort:@
setSourcePortSelector :: Selector
setSourcePortSelector = mkSelector "setSourcePort:"

-- | @Selector@ for @destinationPort@
destinationPortSelector :: Selector
destinationPortSelector = mkSelector "destinationPort"

-- | @Selector@ for @setDestinationPort:@
setDestinationPortSelector :: Selector
setDestinationPortSelector = mkSelector "setDestinationPort:"

-- | @Selector@ for @sourceIPAddress@
sourceIPAddressSelector :: Selector
sourceIPAddressSelector = mkSelector "sourceIPAddress"

-- | @Selector@ for @setSourceIPAddress:@
setSourceIPAddressSelector :: Selector
setSourceIPAddressSelector = mkSelector "setSourceIPAddress:"

-- | @Selector@ for @destinationIPAddress@
destinationIPAddressSelector :: Selector
destinationIPAddressSelector = mkSelector "destinationIPAddress"

-- | @Selector@ for @setDestinationIPAddress:@
setDestinationIPAddressSelector :: Selector
setDestinationIPAddressSelector = mkSelector "setDestinationIPAddress:"

-- | @Selector@ for @sourceMAC@
sourceMACSelector :: Selector
sourceMACSelector = mkSelector "sourceMAC"

-- | @Selector@ for @setSourceMAC:@
setSourceMACSelector :: Selector
setSourceMACSelector = mkSelector "setSourceMAC:"

