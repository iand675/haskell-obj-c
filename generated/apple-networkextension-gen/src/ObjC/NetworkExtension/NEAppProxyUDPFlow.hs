{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppProxyUDPFlow
--
-- The NEAppProxyUDPFlow class declares the programmatic interface of an object that is used by NEAppProxyProvider implementations to proxy the payload of UDP datagrams.
--
-- NEAppProxyUDPFlow is part of NetworkExtension.framework.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppProxyUDPFlow@.
module ObjC.NetworkExtension.NEAppProxyUDPFlow
  ( NEAppProxyUDPFlow
  , IsNEAppProxyUDPFlow(..)
  , writeDatagrams_sentByFlowEndpoints_completionHandler
  , writeDatagrams_sentByEndpoints_completionHandler
  , localFlowEndpoint
  , localEndpoint
  , localEndpointSelector
  , localFlowEndpointSelector
  , writeDatagrams_sentByEndpoints_completionHandlerSelector
  , writeDatagrams_sentByFlowEndpoints_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | writeDatagrams:sentByFlowEndpoints:completionHandler:
--
-- Write datagrams to the flow.
--
-- @datagrams@ — An array of NSData objects containing the data to be written.
--
-- @remoteEndpoints@ — The source endpoints of the datagrams.
--
-- @completionHandler@ — A block that will be executed when the datagrams have been written to the corresponding socket's receive buffer.
--
-- ObjC selector: @- writeDatagrams:sentByFlowEndpoints:completionHandler:@
writeDatagrams_sentByFlowEndpoints_completionHandler :: (IsNEAppProxyUDPFlow neAppProxyUDPFlow, IsNSArray datagrams) => neAppProxyUDPFlow -> datagrams -> RawId -> Ptr () -> IO ()
writeDatagrams_sentByFlowEndpoints_completionHandler neAppProxyUDPFlow datagrams remoteEndpoints completionHandler =
  sendMessage neAppProxyUDPFlow writeDatagrams_sentByFlowEndpoints_completionHandlerSelector (toNSArray datagrams) remoteEndpoints completionHandler

-- | writeDatagrams:sentByEndpoint:completionHandler:
--
-- Write datagrams to the flow.
--
-- @datagrams@ — An array of NSData objects containing the data to be written.
--
-- @remoteEndpoints@ — The source endpoints of the datagrams.
--
-- @completionHandler@ — A block that will be executed when the datagrams have been written to the corresponding socket's receive buffer.
--
-- ObjC selector: @- writeDatagrams:sentByEndpoints:completionHandler:@
writeDatagrams_sentByEndpoints_completionHandler :: (IsNEAppProxyUDPFlow neAppProxyUDPFlow, IsNSArray datagrams, IsNSArray remoteEndpoints) => neAppProxyUDPFlow -> datagrams -> remoteEndpoints -> Ptr () -> IO ()
writeDatagrams_sentByEndpoints_completionHandler neAppProxyUDPFlow datagrams remoteEndpoints completionHandler =
  sendMessage neAppProxyUDPFlow writeDatagrams_sentByEndpoints_completionHandlerSelector (toNSArray datagrams) (toNSArray remoteEndpoints) completionHandler

-- | localFlowEndpoint
--
-- An @nw_endpoint_t@ object containing the local endpoint of the flow's corresponding socket.
--
-- ObjC selector: @- localFlowEndpoint@
localFlowEndpoint :: IsNEAppProxyUDPFlow neAppProxyUDPFlow => neAppProxyUDPFlow -> IO (Id NSObject)
localFlowEndpoint neAppProxyUDPFlow =
  sendMessage neAppProxyUDPFlow localFlowEndpointSelector

-- | localEndpoint
--
-- An NWEndpoint object containing the local endpoint of the flow's corresponding socket.
--
-- ObjC selector: @- localEndpoint@
localEndpoint :: IsNEAppProxyUDPFlow neAppProxyUDPFlow => neAppProxyUDPFlow -> IO (Id NWEndpoint)
localEndpoint neAppProxyUDPFlow =
  sendMessage neAppProxyUDPFlow localEndpointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @writeDatagrams:sentByFlowEndpoints:completionHandler:@
writeDatagrams_sentByFlowEndpoints_completionHandlerSelector :: Selector '[Id NSArray, RawId, Ptr ()] ()
writeDatagrams_sentByFlowEndpoints_completionHandlerSelector = mkSelector "writeDatagrams:sentByFlowEndpoints:completionHandler:"

-- | @Selector@ for @writeDatagrams:sentByEndpoints:completionHandler:@
writeDatagrams_sentByEndpoints_completionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] ()
writeDatagrams_sentByEndpoints_completionHandlerSelector = mkSelector "writeDatagrams:sentByEndpoints:completionHandler:"

-- | @Selector@ for @localFlowEndpoint@
localFlowEndpointSelector :: Selector '[] (Id NSObject)
localFlowEndpointSelector = mkSelector "localFlowEndpoint"

-- | @Selector@ for @localEndpoint@
localEndpointSelector :: Selector '[] (Id NWEndpoint)
localEndpointSelector = mkSelector "localEndpoint"

