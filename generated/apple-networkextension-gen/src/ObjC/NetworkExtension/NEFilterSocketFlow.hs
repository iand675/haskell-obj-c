{-# LANGUAGE DataKinds #-}
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
  , localEndpointSelector
  , localFlowEndpointSelector
  , remoteEndpointSelector
  , remoteFlowEndpointSelector
  , remoteHostnameSelector
  , socketFamilySelector
  , socketProtocolSelector
  , socketTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
remoteFlowEndpoint neFilterSocketFlow =
  sendMessage neFilterSocketFlow remoteFlowEndpointSelector

-- | remoteEndpoint
--
-- The flow's remote endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- remoteEndpoint@
remoteEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NWEndpoint)
remoteEndpoint neFilterSocketFlow =
  sendMessage neFilterSocketFlow remoteEndpointSelector

-- | remoteHostname
--
-- The flow's remote hostname. This property is only non-nil if the flow was created using Network.framework or NSURLSession.
--
-- ObjC selector: @- remoteHostname@
remoteHostname :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NSString)
remoteHostname neFilterSocketFlow =
  sendMessage neFilterSocketFlow remoteHostnameSelector

-- | localFlowEndpoint
--
-- The flow's local endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- localFlowEndpoint@
localFlowEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NSObject)
localFlowEndpoint neFilterSocketFlow =
  sendMessage neFilterSocketFlow localFlowEndpointSelector

-- | localEndpoint
--
-- The flow's local endpoint. This endpoint object may be nil when [NEFilterDataProvider handleNewFlow:] is invoked and if so will be populated upon receiving network data.		In such a case, filtering on the flow may still be performed based on its socket type, socket family or socket protocol.
--
-- ObjC selector: @- localEndpoint@
localEndpoint :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO (Id NWEndpoint)
localEndpoint neFilterSocketFlow =
  sendMessage neFilterSocketFlow localEndpointSelector

-- | socketFamily
--
-- Socket family of the socket flow, such as PF_INET.
--
-- ObjC selector: @- socketFamily@
socketFamily :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketFamily neFilterSocketFlow =
  sendMessage neFilterSocketFlow socketFamilySelector

-- | socketType
--
-- Socket type of the socket flow, such as SOCK_STREAM.
--
-- ObjC selector: @- socketType@
socketType :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketType neFilterSocketFlow =
  sendMessage neFilterSocketFlow socketTypeSelector

-- | socketProtocol
--
-- Socket protocol of the socket flow, such as IPPROTO_TCP.
--
-- ObjC selector: @- socketProtocol@
socketProtocol :: IsNEFilterSocketFlow neFilterSocketFlow => neFilterSocketFlow -> IO CInt
socketProtocol neFilterSocketFlow =
  sendMessage neFilterSocketFlow socketProtocolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remoteFlowEndpoint@
remoteFlowEndpointSelector :: Selector '[] (Id NSObject)
remoteFlowEndpointSelector = mkSelector "remoteFlowEndpoint"

-- | @Selector@ for @remoteEndpoint@
remoteEndpointSelector :: Selector '[] (Id NWEndpoint)
remoteEndpointSelector = mkSelector "remoteEndpoint"

-- | @Selector@ for @remoteHostname@
remoteHostnameSelector :: Selector '[] (Id NSString)
remoteHostnameSelector = mkSelector "remoteHostname"

-- | @Selector@ for @localFlowEndpoint@
localFlowEndpointSelector :: Selector '[] (Id NSObject)
localFlowEndpointSelector = mkSelector "localFlowEndpoint"

-- | @Selector@ for @localEndpoint@
localEndpointSelector :: Selector '[] (Id NWEndpoint)
localEndpointSelector = mkSelector "localEndpoint"

-- | @Selector@ for @socketFamily@
socketFamilySelector :: Selector '[] CInt
socketFamilySelector = mkSelector "socketFamily"

-- | @Selector@ for @socketType@
socketTypeSelector :: Selector '[] CInt
socketTypeSelector = mkSelector "socketType"

-- | @Selector@ for @socketProtocol@
socketProtocolSelector :: Selector '[] CInt
socketProtocolSelector = mkSelector "socketProtocol"

