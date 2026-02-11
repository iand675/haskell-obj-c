{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterSocketFlow
--
-- The NEFilterSocketFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered, which is originated from the socket.
--
-- NEFilterSocketFlow is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterSocketFlow@.
module ObjC.NetworkExtension.NEFilterSocketFlow
  ( NEFilterSocketFlow
  , IsNEFilterSocketFlow(..)
  , remoteFlowEndpoint
  , remoteEndpoint
  , remoteHostname
  , localFlowEndpoint
  , localEndpoint
  , socketFamily
  , socketType
  , socketProtocol
  , remoteFlowEndpointSelector
  , remoteEndpointSelector
  , remoteHostnameSelector
  , localFlowEndpointSelector
  , localEndpointSelector
  , socketFamilySelector
  , socketTypeSelector
  , socketProtocolSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | remoteFlowEndpoint
--
-- The flow's remote endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- remoteFlowEndpoint@
remoteFlowEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NSObject)
remoteFlowEndpoint neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "remoteFlowEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | remoteEndpoint
--
-- The flow's remote endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- remoteEndpoint@
remoteEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NWEndpoint)
remoteEndpoint neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "remoteEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | remoteHostname
--
-- The flow's remote hostname. This property is only non-nil if the flow was created using Network.framework or NSURLSession.
--
-- ObjC selector: @- remoteHostname@
remoteHostname :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NSString)
remoteHostname neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "remoteHostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localFlowEndpoint
--
-- The flow's local endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- localFlowEndpoint@
localFlowEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NSObject)
localFlowEndpoint neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "localFlowEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localEndpoint
--
-- The flow's local endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- localEndpoint@
localEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NWEndpoint)
localEndpoint neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "localEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | socketFamily
--
-- Socket family of the socket flow, such as PF_INET.
--
-- ObjC selector: @- socketFamily@
socketFamily :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketFamily neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "socketFamily") retCInt []

-- | socketType
--
-- Socket type of the socket flow, such as SOCK_STREAM.
--
-- ObjC selector: @- socketType@
socketType :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketType neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "socketType") retCInt []

-- | socketProtocol
--
-- Socket protocol of the socket flow, such as IPPROTO_TCP.
--
-- ObjC selector: @- socketProtocol@
socketProtocol :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketProtocol neFilterSocketFlow  =
  sendMsg neFilterSocketFlow (mkSelector "socketProtocol") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remoteFlowEndpoint@
remoteFlowEndpointSelector :: Selector
remoteFlowEndpointSelector = mkSelector "remoteFlowEndpoint"

-- | @Selector@ for @remoteEndpoint@
remoteEndpointSelector :: Selector
remoteEndpointSelector = mkSelector "remoteEndpoint"

-- | @Selector@ for @remoteHostname@
remoteHostnameSelector :: Selector
remoteHostnameSelector = mkSelector "remoteHostname"

-- | @Selector@ for @localFlowEndpoint@
localFlowEndpointSelector :: Selector
localFlowEndpointSelector = mkSelector "localFlowEndpoint"

-- | @Selector@ for @localEndpoint@
localEndpointSelector :: Selector
localEndpointSelector = mkSelector "localEndpoint"

-- | @Selector@ for @socketFamily@
socketFamilySelector :: Selector
socketFamilySelector = mkSelector "socketFamily"

-- | @Selector@ for @socketType@
socketTypeSelector :: Selector
socketTypeSelector = mkSelector "socketType"

-- | @Selector@ for @socketProtocol@
socketProtocolSelector :: Selector
socketProtocolSelector = mkSelector "socketProtocol"

