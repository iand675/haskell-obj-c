{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSocketPort@.
module ObjC.Foundation.NSSocketPort
  ( NSSocketPort
  , IsNSSocketPort(..)
  , init_
  , initWithTCPPort
  , initWithProtocolFamily_socketType_protocol_address
  , initWithProtocolFamily_socketType_protocol_socket
  , initRemoteWithTCPPort_host
  , initRemoteWithProtocolFamily_socketType_protocol_address
  , protocolFamily
  , socketType
  , protocol
  , address
  , socket
  , addressSelector
  , initRemoteWithProtocolFamily_socketType_protocol_addressSelector
  , initRemoteWithTCPPort_hostSelector
  , initSelector
  , initWithProtocolFamily_socketType_protocol_addressSelector
  , initWithProtocolFamily_socketType_protocol_socketSelector
  , initWithTCPPortSelector
  , protocolFamilySelector
  , protocolSelector
  , socketSelector
  , socketTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO (Id NSSocketPort)
init_ nsSocketPort =
  sendOwnedMessage nsSocketPort initSelector

-- | @- initWithTCPPort:@
initWithTCPPort :: IsNSSocketPort nsSocketPort => nsSocketPort -> CUShort -> IO (Id NSSocketPort)
initWithTCPPort nsSocketPort port =
  sendOwnedMessage nsSocketPort initWithTCPPortSelector port

-- | @- initWithProtocolFamily:socketType:protocol:address:@
initWithProtocolFamily_socketType_protocol_address :: (IsNSSocketPort nsSocketPort, IsNSData address) => nsSocketPort -> CInt -> CInt -> CInt -> address -> IO (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_address nsSocketPort family_ type_ protocol address =
  sendOwnedMessage nsSocketPort initWithProtocolFamily_socketType_protocol_addressSelector family_ type_ protocol (toNSData address)

-- | @- initWithProtocolFamily:socketType:protocol:socket:@
initWithProtocolFamily_socketType_protocol_socket :: IsNSSocketPort nsSocketPort => nsSocketPort -> CInt -> CInt -> CInt -> CInt -> IO (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_socket nsSocketPort family_ type_ protocol sock =
  sendOwnedMessage nsSocketPort initWithProtocolFamily_socketType_protocol_socketSelector family_ type_ protocol sock

-- | @- initRemoteWithTCPPort:host:@
initRemoteWithTCPPort_host :: (IsNSSocketPort nsSocketPort, IsNSString hostName) => nsSocketPort -> CUShort -> hostName -> IO (Id NSSocketPort)
initRemoteWithTCPPort_host nsSocketPort port hostName =
  sendOwnedMessage nsSocketPort initRemoteWithTCPPort_hostSelector port (toNSString hostName)

-- | @- initRemoteWithProtocolFamily:socketType:protocol:address:@
initRemoteWithProtocolFamily_socketType_protocol_address :: (IsNSSocketPort nsSocketPort, IsNSData address) => nsSocketPort -> CInt -> CInt -> CInt -> address -> IO (Id NSSocketPort)
initRemoteWithProtocolFamily_socketType_protocol_address nsSocketPort family_ type_ protocol address =
  sendOwnedMessage nsSocketPort initRemoteWithProtocolFamily_socketType_protocol_addressSelector family_ type_ protocol (toNSData address)

-- | @- protocolFamily@
protocolFamily :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
protocolFamily nsSocketPort =
  sendMessage nsSocketPort protocolFamilySelector

-- | @- socketType@
socketType :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
socketType nsSocketPort =
  sendMessage nsSocketPort socketTypeSelector

-- | @- protocol@
protocol :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
protocol nsSocketPort =
  sendMessage nsSocketPort protocolSelector

-- | @- address@
address :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO (Id NSData)
address nsSocketPort =
  sendMessage nsSocketPort addressSelector

-- | @- socket@
socket :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
socket nsSocketPort =
  sendMessage nsSocketPort socketSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSocketPort)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTCPPort:@
initWithTCPPortSelector :: Selector '[CUShort] (Id NSSocketPort)
initWithTCPPortSelector = mkSelector "initWithTCPPort:"

-- | @Selector@ for @initWithProtocolFamily:socketType:protocol:address:@
initWithProtocolFamily_socketType_protocol_addressSelector :: Selector '[CInt, CInt, CInt, Id NSData] (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_addressSelector = mkSelector "initWithProtocolFamily:socketType:protocol:address:"

-- | @Selector@ for @initWithProtocolFamily:socketType:protocol:socket:@
initWithProtocolFamily_socketType_protocol_socketSelector :: Selector '[CInt, CInt, CInt, CInt] (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_socketSelector = mkSelector "initWithProtocolFamily:socketType:protocol:socket:"

-- | @Selector@ for @initRemoteWithTCPPort:host:@
initRemoteWithTCPPort_hostSelector :: Selector '[CUShort, Id NSString] (Id NSSocketPort)
initRemoteWithTCPPort_hostSelector = mkSelector "initRemoteWithTCPPort:host:"

-- | @Selector@ for @initRemoteWithProtocolFamily:socketType:protocol:address:@
initRemoteWithProtocolFamily_socketType_protocol_addressSelector :: Selector '[CInt, CInt, CInt, Id NSData] (Id NSSocketPort)
initRemoteWithProtocolFamily_socketType_protocol_addressSelector = mkSelector "initRemoteWithProtocolFamily:socketType:protocol:address:"

-- | @Selector@ for @protocolFamily@
protocolFamilySelector :: Selector '[] CInt
protocolFamilySelector = mkSelector "protocolFamily"

-- | @Selector@ for @socketType@
socketTypeSelector :: Selector '[] CInt
socketTypeSelector = mkSelector "socketType"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] CInt
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id NSData)
addressSelector = mkSelector "address"

-- | @Selector@ for @socket@
socketSelector :: Selector '[] CInt
socketSelector = mkSelector "socket"

