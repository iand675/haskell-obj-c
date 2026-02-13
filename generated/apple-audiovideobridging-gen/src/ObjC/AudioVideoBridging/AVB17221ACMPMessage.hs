{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , avB17221ACMPMessageErrorForStatusCodeSelector
  , connectedListenersEntriesSelector
  , connectionCountSelector
  , controllerEntityIDSelector
  , destinationIPAddressSelector
  , destinationMACSelector
  , destinationPortSelector
  , errorForStatusCodeSelector
  , flagsSelector
  , ipFlagsSelector
  , listenerEntityIDSelector
  , listenerUniqueIDSelector
  , messageTypeSelector
  , sequenceIDSelector
  , setConnectedListenersEntriesSelector
  , setConnectionCountSelector
  , setControllerEntityIDSelector
  , setDestinationIPAddressSelector
  , setDestinationMACSelector
  , setDestinationPortSelector
  , setFlagsSelector
  , setIpFlagsSelector
  , setListenerEntityIDSelector
  , setListenerUniqueIDSelector
  , setMessageTypeSelector
  , setSequenceIDSelector
  , setSourceIPAddressSelector
  , setSourceMACSelector
  , setSourcePortSelector
  , setStatusSelector
  , setStreamIDSelector
  , setTalkerEntityIDSelector
  , setTalkerUniqueIDSelector
  , setVlanIDSelector
  , sourceIPAddressSelector
  , sourceMACSelector
  , sourcePortSelector
  , statusSelector
  , streamIDSelector
  , talkerEntityIDSelector
  , talkerUniqueIDSelector
  , vlanIDSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' avB17221ACMPMessageErrorForStatusCodeSelector statusCode

-- | errorForStatusCode
--
-- This method returns an NSError filled out with an appropriate description for the message's status code.
--
-- Returns: An NSError instance within the AVBErrorDomain with the status code and an appropriate description.				Will return nil if status code is success or in progress.
--
-- ObjC selector: @- errorForStatusCode@
errorForStatusCode :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id NSError)
errorForStatusCode avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage errorForStatusCodeSelector

-- | messageType
--
-- The message_type field of the ACMP message.
--
-- ObjC selector: @- messageType@
messageType :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPMessageType
messageType avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage messageTypeSelector

-- | messageType
--
-- The message_type field of the ACMP message.
--
-- ObjC selector: @- setMessageType:@
setMessageType :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPMessageType -> IO ()
setMessageType avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setMessageTypeSelector value

-- | status
--
-- The status field of the ACMP message.
--
-- ObjC selector: @- status@
status :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPStatusCode
status avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage statusSelector

-- | status
--
-- The status field of the ACMP message.
--
-- ObjC selector: @- setStatus:@
setStatus :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPStatusCode -> IO ()
setStatus avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setStatusSelector value

-- | streamID
--
-- The stream_id field of the ACMP message.
--
-- ObjC selector: @- streamID@
streamID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
streamID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage streamIDSelector

-- | streamID
--
-- The stream_id field of the ACMP message.
--
-- ObjC selector: @- setStreamID:@
setStreamID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setStreamID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setStreamIDSelector value

-- | controllerGUID
--
-- The controller_entity_id field of the ACMP message.
--
-- ObjC selector: @- controllerEntityID@
controllerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
controllerEntityID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage controllerEntityIDSelector

-- | controllerGUID
--
-- The controller_entity_id field of the ACMP message.
--
-- ObjC selector: @- setControllerEntityID:@
setControllerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setControllerEntityID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setControllerEntityIDSelector value

-- | talkerEntityID
--
-- The talker_entity_id field of the ACMP message.
--
-- ObjC selector: @- talkerEntityID@
talkerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
talkerEntityID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage talkerEntityIDSelector

-- | talkerEntityID
--
-- The talker_entity_id field of the ACMP message.
--
-- ObjC selector: @- setTalkerEntityID:@
setTalkerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setTalkerEntityID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setTalkerEntityIDSelector value

-- | listenerEntityID
--
-- The listener_entity_id field of the ACMP message.
--
-- ObjC selector: @- listenerEntityID@
listenerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CULong
listenerEntityID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage listenerEntityIDSelector

-- | listenerEntityID
--
-- The listener_entity_id field of the ACMP message.
--
-- ObjC selector: @- setListenerEntityID:@
setListenerEntityID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CULong -> IO ()
setListenerEntityID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setListenerEntityIDSelector value

-- | talkerUniqueID
--
-- The talker_unique_id field of the ACMP message.
--
-- ObjC selector: @- talkerUniqueID@
talkerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
talkerUniqueID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage talkerUniqueIDSelector

-- | talkerUniqueID
--
-- The talker_unique_id field of the ACMP message.
--
-- ObjC selector: @- setTalkerUniqueID:@
setTalkerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setTalkerUniqueID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setTalkerUniqueIDSelector value

-- | listenerUniqueID
--
-- The listener_unique_id field of the ACMP message.
--
-- ObjC selector: @- listenerUniqueID@
listenerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
listenerUniqueID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage listenerUniqueIDSelector

-- | listenerUniqueID
--
-- The listener_unique_id field of the ACMP message.
--
-- ObjC selector: @- setListenerUniqueID:@
setListenerUniqueID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setListenerUniqueID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setListenerUniqueIDSelector value

-- | destinationMAC
--
-- The dest_mac field of the ACMP message.
--
-- ObjC selector: @- destinationMAC@
destinationMAC :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBMACAddress)
destinationMAC avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage destinationMACSelector

-- | destinationMAC
--
-- The dest_mac field of the ACMP message.
--
-- ObjC selector: @- setDestinationMAC:@
setDestinationMAC :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBMACAddress value) => avB17221ACMPMessage -> value -> IO ()
setDestinationMAC avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setDestinationMACSelector (toAVBMACAddress value)

-- | connectionCount
--
-- The connection_count field of the ACMP message.
--
-- ObjC selector: @- connectionCount@
connectionCount :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
connectionCount avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage connectionCountSelector

-- | connectionCount
--
-- The connection_count field of the ACMP message.
--
-- ObjC selector: @- setConnectionCount:@
setConnectionCount :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setConnectionCount avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setConnectionCountSelector value

-- | sequenceID
--
-- The sequence_id field of the ACMP message.
--
-- ObjC selector: @- sequenceID@
sequenceID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
sequenceID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage sequenceIDSelector

-- | sequenceID
--
-- The sequence_id field of the ACMP message.
--
-- ObjC selector: @- setSequenceID:@
setSequenceID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setSequenceID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setSequenceIDSelector value

-- | flags
--
-- The flags field of the ACMP message.
--
-- ObjC selector: @- flags@
flags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPFlags
flags avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage flagsSelector

-- | flags
--
-- The flags field of the ACMP message.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPFlags -> IO ()
setFlags avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setFlagsSelector value

-- | vlanID
--
-- The stream_vlan_id field of the ACMP message.
--
-- ObjC selector: @- vlanID@
vlanID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
vlanID avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage vlanIDSelector

-- | vlanID
--
-- The stream_vlan_id field of the ACMP message.
--
-- ObjC selector: @- setVlanID:@
setVlanID :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setVlanID avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setVlanIDSelector value

-- | connectedListenersEntries
--
-- The connected_listeners_entries field of the ACMP message.
--
-- ObjC selector: @- connectedListenersEntries@
connectedListenersEntries :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
connectedListenersEntries avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage connectedListenersEntriesSelector

-- | connectedListenersEntries
--
-- The connected_listeners_entries field of the ACMP message.
--
-- ObjC selector: @- setConnectedListenersEntries:@
setConnectedListenersEntries :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setConnectedListenersEntries avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setConnectedListenersEntriesSelector value

-- | ipFlags
--
-- The ip_flags field of the ACMP message.
--
-- ObjC selector: @- ipFlags@
ipFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO AVB17221ACMPIPFlag
ipFlags avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage ipFlagsSelector

-- | ipFlags
--
-- The ip_flags field of the ACMP message.
--
-- ObjC selector: @- setIpFlags:@
setIpFlags :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> AVB17221ACMPIPFlag -> IO ()
setIpFlags avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setIpFlagsSelector value

-- | sourcePort
--
-- The source_port field of the ACMP message.
--
-- ObjC selector: @- sourcePort@
sourcePort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
sourcePort avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage sourcePortSelector

-- | sourcePort
--
-- The source_port field of the ACMP message.
--
-- ObjC selector: @- setSourcePort:@
setSourcePort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setSourcePort avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setSourcePortSelector value

-- | destinationPort
--
-- The destination_port field of the ACMP message.
--
-- ObjC selector: @- destinationPort@
destinationPort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO CUShort
destinationPort avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage destinationPortSelector

-- | destinationPort
--
-- The destination_port field of the ACMP message.
--
-- ObjC selector: @- setDestinationPort:@
setDestinationPort :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> CUShort -> IO ()
setDestinationPort avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setDestinationPortSelector value

-- | sourceAddress
--
-- The source_ip_address field of the ACMP message.
--
-- ObjC selector: @- sourceIPAddress@
sourceIPAddress :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBIPAddress)
sourceIPAddress avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage sourceIPAddressSelector

-- | sourceAddress
--
-- The source_ip_address field of the ACMP message.
--
-- ObjC selector: @- setSourceIPAddress:@
setSourceIPAddress :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBIPAddress value) => avB17221ACMPMessage -> value -> IO ()
setSourceIPAddress avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setSourceIPAddressSelector (toAVBIPAddress value)

-- | destinationAddress
--
-- The destination_ip_address field of the ACMP message.
--
-- ObjC selector: @- destinationIPAddress@
destinationIPAddress :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBIPAddress)
destinationIPAddress avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage destinationIPAddressSelector

-- | destinationAddress
--
-- The destination_ip_address field of the ACMP message.
--
-- ObjC selector: @- setDestinationIPAddress:@
setDestinationIPAddress :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBIPAddress value) => avB17221ACMPMessage -> value -> IO ()
setDestinationIPAddress avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setDestinationIPAddressSelector (toAVBIPAddress value)

-- | sourceMAC
--
-- The source_mac field of the ACMP message.
--
-- ObjC selector: @- sourceMAC@
sourceMAC :: IsAVB17221ACMPMessage avB17221ACMPMessage => avB17221ACMPMessage -> IO (Id AVBMACAddress)
sourceMAC avB17221ACMPMessage =
  sendMessage avB17221ACMPMessage sourceMACSelector

-- | sourceMAC
--
-- The source_mac field of the ACMP message.
--
-- ObjC selector: @- setSourceMAC:@
setSourceMAC :: (IsAVB17221ACMPMessage avB17221ACMPMessage, IsAVBMACAddress value) => avB17221ACMPMessage -> value -> IO ()
setSourceMAC avB17221ACMPMessage value =
  sendMessage avB17221ACMPMessage setSourceMACSelector (toAVBMACAddress value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorForStatusCode:@
avB17221ACMPMessageErrorForStatusCodeSelector :: Selector '[AVB17221ACMPStatusCode] (Id NSError)
avB17221ACMPMessageErrorForStatusCodeSelector = mkSelector "errorForStatusCode:"

-- | @Selector@ for @errorForStatusCode@
errorForStatusCodeSelector :: Selector '[] (Id NSError)
errorForStatusCodeSelector = mkSelector "errorForStatusCode"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector '[] AVB17221ACMPMessageType
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @setMessageType:@
setMessageTypeSelector :: Selector '[AVB17221ACMPMessageType] ()
setMessageTypeSelector = mkSelector "setMessageType:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] AVB17221ACMPStatusCode
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[AVB17221ACMPStatusCode] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @streamID@
streamIDSelector :: Selector '[] CULong
streamIDSelector = mkSelector "streamID"

-- | @Selector@ for @setStreamID:@
setStreamIDSelector :: Selector '[CULong] ()
setStreamIDSelector = mkSelector "setStreamID:"

-- | @Selector@ for @controllerEntityID@
controllerEntityIDSelector :: Selector '[] CULong
controllerEntityIDSelector = mkSelector "controllerEntityID"

-- | @Selector@ for @setControllerEntityID:@
setControllerEntityIDSelector :: Selector '[CULong] ()
setControllerEntityIDSelector = mkSelector "setControllerEntityID:"

-- | @Selector@ for @talkerEntityID@
talkerEntityIDSelector :: Selector '[] CULong
talkerEntityIDSelector = mkSelector "talkerEntityID"

-- | @Selector@ for @setTalkerEntityID:@
setTalkerEntityIDSelector :: Selector '[CULong] ()
setTalkerEntityIDSelector = mkSelector "setTalkerEntityID:"

-- | @Selector@ for @listenerEntityID@
listenerEntityIDSelector :: Selector '[] CULong
listenerEntityIDSelector = mkSelector "listenerEntityID"

-- | @Selector@ for @setListenerEntityID:@
setListenerEntityIDSelector :: Selector '[CULong] ()
setListenerEntityIDSelector = mkSelector "setListenerEntityID:"

-- | @Selector@ for @talkerUniqueID@
talkerUniqueIDSelector :: Selector '[] CUShort
talkerUniqueIDSelector = mkSelector "talkerUniqueID"

-- | @Selector@ for @setTalkerUniqueID:@
setTalkerUniqueIDSelector :: Selector '[CUShort] ()
setTalkerUniqueIDSelector = mkSelector "setTalkerUniqueID:"

-- | @Selector@ for @listenerUniqueID@
listenerUniqueIDSelector :: Selector '[] CUShort
listenerUniqueIDSelector = mkSelector "listenerUniqueID"

-- | @Selector@ for @setListenerUniqueID:@
setListenerUniqueIDSelector :: Selector '[CUShort] ()
setListenerUniqueIDSelector = mkSelector "setListenerUniqueID:"

-- | @Selector@ for @destinationMAC@
destinationMACSelector :: Selector '[] (Id AVBMACAddress)
destinationMACSelector = mkSelector "destinationMAC"

-- | @Selector@ for @setDestinationMAC:@
setDestinationMACSelector :: Selector '[Id AVBMACAddress] ()
setDestinationMACSelector = mkSelector "setDestinationMAC:"

-- | @Selector@ for @connectionCount@
connectionCountSelector :: Selector '[] CUShort
connectionCountSelector = mkSelector "connectionCount"

-- | @Selector@ for @setConnectionCount:@
setConnectionCountSelector :: Selector '[CUShort] ()
setConnectionCountSelector = mkSelector "setConnectionCount:"

-- | @Selector@ for @sequenceID@
sequenceIDSelector :: Selector '[] CUShort
sequenceIDSelector = mkSelector "sequenceID"

-- | @Selector@ for @setSequenceID:@
setSequenceIDSelector :: Selector '[CUShort] ()
setSequenceIDSelector = mkSelector "setSequenceID:"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] AVB17221ACMPFlags
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector '[AVB17221ACMPFlags] ()
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @vlanID@
vlanIDSelector :: Selector '[] CUShort
vlanIDSelector = mkSelector "vlanID"

-- | @Selector@ for @setVlanID:@
setVlanIDSelector :: Selector '[CUShort] ()
setVlanIDSelector = mkSelector "setVlanID:"

-- | @Selector@ for @connectedListenersEntries@
connectedListenersEntriesSelector :: Selector '[] CUShort
connectedListenersEntriesSelector = mkSelector "connectedListenersEntries"

-- | @Selector@ for @setConnectedListenersEntries:@
setConnectedListenersEntriesSelector :: Selector '[CUShort] ()
setConnectedListenersEntriesSelector = mkSelector "setConnectedListenersEntries:"

-- | @Selector@ for @ipFlags@
ipFlagsSelector :: Selector '[] AVB17221ACMPIPFlag
ipFlagsSelector = mkSelector "ipFlags"

-- | @Selector@ for @setIpFlags:@
setIpFlagsSelector :: Selector '[AVB17221ACMPIPFlag] ()
setIpFlagsSelector = mkSelector "setIpFlags:"

-- | @Selector@ for @sourcePort@
sourcePortSelector :: Selector '[] CUShort
sourcePortSelector = mkSelector "sourcePort"

-- | @Selector@ for @setSourcePort:@
setSourcePortSelector :: Selector '[CUShort] ()
setSourcePortSelector = mkSelector "setSourcePort:"

-- | @Selector@ for @destinationPort@
destinationPortSelector :: Selector '[] CUShort
destinationPortSelector = mkSelector "destinationPort"

-- | @Selector@ for @setDestinationPort:@
setDestinationPortSelector :: Selector '[CUShort] ()
setDestinationPortSelector = mkSelector "setDestinationPort:"

-- | @Selector@ for @sourceIPAddress@
sourceIPAddressSelector :: Selector '[] (Id AVBIPAddress)
sourceIPAddressSelector = mkSelector "sourceIPAddress"

-- | @Selector@ for @setSourceIPAddress:@
setSourceIPAddressSelector :: Selector '[Id AVBIPAddress] ()
setSourceIPAddressSelector = mkSelector "setSourceIPAddress:"

-- | @Selector@ for @destinationIPAddress@
destinationIPAddressSelector :: Selector '[] (Id AVBIPAddress)
destinationIPAddressSelector = mkSelector "destinationIPAddress"

-- | @Selector@ for @setDestinationIPAddress:@
setDestinationIPAddressSelector :: Selector '[Id AVBIPAddress] ()
setDestinationIPAddressSelector = mkSelector "setDestinationIPAddress:"

-- | @Selector@ for @sourceMAC@
sourceMACSelector :: Selector '[] (Id AVBMACAddress)
sourceMACSelector = mkSelector "sourceMAC"

-- | @Selector@ for @setSourceMAC:@
setSourceMACSelector :: Selector '[Id AVBMACAddress] ()
setSourceMACSelector = mkSelector "setSourceMAC:"

