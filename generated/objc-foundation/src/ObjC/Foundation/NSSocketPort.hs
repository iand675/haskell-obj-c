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
  , initSelector
  , initWithTCPPortSelector
  , initWithProtocolFamily_socketType_protocol_addressSelector
  , initWithProtocolFamily_socketType_protocol_socketSelector
  , initRemoteWithTCPPort_hostSelector
  , initRemoteWithProtocolFamily_socketType_protocol_addressSelector
  , protocolFamilySelector
  , socketTypeSelector
  , protocolSelector
  , addressSelector
  , socketSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO (Id NSSocketPort)
init_ nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTCPPort:@
initWithTCPPort :: IsNSSocketPort nsSocketPort => nsSocketPort -> CUShort -> IO (Id NSSocketPort)
initWithTCPPort nsSocketPort  port =
  sendMsg nsSocketPort (mkSelector "initWithTCPPort:") (retPtr retVoid) [argCUInt (fromIntegral port)] >>= ownedObject . castPtr

-- | @- initWithProtocolFamily:socketType:protocol:address:@
initWithProtocolFamily_socketType_protocol_address :: (IsNSSocketPort nsSocketPort, IsNSData address) => nsSocketPort -> CInt -> CInt -> CInt -> address -> IO (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_address nsSocketPort  family_ type_ protocol address =
withObjCPtr address $ \raw_address ->
    sendMsg nsSocketPort (mkSelector "initWithProtocolFamily:socketType:protocol:address:") (retPtr retVoid) [argCInt (fromIntegral family_), argCInt (fromIntegral type_), argCInt (fromIntegral protocol), argPtr (castPtr raw_address :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProtocolFamily:socketType:protocol:socket:@
initWithProtocolFamily_socketType_protocol_socket :: IsNSSocketPort nsSocketPort => nsSocketPort -> CInt -> CInt -> CInt -> CInt -> IO (Id NSSocketPort)
initWithProtocolFamily_socketType_protocol_socket nsSocketPort  family_ type_ protocol sock =
  sendMsg nsSocketPort (mkSelector "initWithProtocolFamily:socketType:protocol:socket:") (retPtr retVoid) [argCInt (fromIntegral family_), argCInt (fromIntegral type_), argCInt (fromIntegral protocol), argCInt (fromIntegral sock)] >>= ownedObject . castPtr

-- | @- initRemoteWithTCPPort:host:@
initRemoteWithTCPPort_host :: (IsNSSocketPort nsSocketPort, IsNSString hostName) => nsSocketPort -> CUShort -> hostName -> IO (Id NSSocketPort)
initRemoteWithTCPPort_host nsSocketPort  port hostName =
withObjCPtr hostName $ \raw_hostName ->
    sendMsg nsSocketPort (mkSelector "initRemoteWithTCPPort:host:") (retPtr retVoid) [argCUInt (fromIntegral port), argPtr (castPtr raw_hostName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initRemoteWithProtocolFamily:socketType:protocol:address:@
initRemoteWithProtocolFamily_socketType_protocol_address :: (IsNSSocketPort nsSocketPort, IsNSData address) => nsSocketPort -> CInt -> CInt -> CInt -> address -> IO (Id NSSocketPort)
initRemoteWithProtocolFamily_socketType_protocol_address nsSocketPort  family_ type_ protocol address =
withObjCPtr address $ \raw_address ->
    sendMsg nsSocketPort (mkSelector "initRemoteWithProtocolFamily:socketType:protocol:address:") (retPtr retVoid) [argCInt (fromIntegral family_), argCInt (fromIntegral type_), argCInt (fromIntegral protocol), argPtr (castPtr raw_address :: Ptr ())] >>= ownedObject . castPtr

-- | @- protocolFamily@
protocolFamily :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
protocolFamily nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "protocolFamily") retCInt []

-- | @- socketType@
socketType :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
socketType nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "socketType") retCInt []

-- | @- protocol@
protocol :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
protocol nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "protocol") retCInt []

-- | @- address@
address :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO (Id NSData)
address nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- socket@
socket :: IsNSSocketPort nsSocketPort => nsSocketPort -> IO CInt
socket nsSocketPort  =
  sendMsg nsSocketPort (mkSelector "socket") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTCPPort:@
initWithTCPPortSelector :: Selector
initWithTCPPortSelector = mkSelector "initWithTCPPort:"

-- | @Selector@ for @initWithProtocolFamily:socketType:protocol:address:@
initWithProtocolFamily_socketType_protocol_addressSelector :: Selector
initWithProtocolFamily_socketType_protocol_addressSelector = mkSelector "initWithProtocolFamily:socketType:protocol:address:"

-- | @Selector@ for @initWithProtocolFamily:socketType:protocol:socket:@
initWithProtocolFamily_socketType_protocol_socketSelector :: Selector
initWithProtocolFamily_socketType_protocol_socketSelector = mkSelector "initWithProtocolFamily:socketType:protocol:socket:"

-- | @Selector@ for @initRemoteWithTCPPort:host:@
initRemoteWithTCPPort_hostSelector :: Selector
initRemoteWithTCPPort_hostSelector = mkSelector "initRemoteWithTCPPort:host:"

-- | @Selector@ for @initRemoteWithProtocolFamily:socketType:protocol:address:@
initRemoteWithProtocolFamily_socketType_protocol_addressSelector :: Selector
initRemoteWithProtocolFamily_socketType_protocol_addressSelector = mkSelector "initRemoteWithProtocolFamily:socketType:protocol:address:"

-- | @Selector@ for @protocolFamily@
protocolFamilySelector :: Selector
protocolFamilySelector = mkSelector "protocolFamily"

-- | @Selector@ for @socketType@
socketTypeSelector :: Selector
socketTypeSelector = mkSelector "socketType"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @socket@
socketSelector :: Selector
socketSelector = mkSelector "socket"

