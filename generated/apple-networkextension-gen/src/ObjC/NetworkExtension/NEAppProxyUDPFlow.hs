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
  , writeDatagrams_sentByFlowEndpoints_completionHandlerSelector
  , writeDatagrams_sentByEndpoints_completionHandlerSelector
  , localFlowEndpointSelector
  , localEndpointSelector


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
writeDatagrams_sentByFlowEndpoints_completionHandler neAppProxyUDPFlow  datagrams remoteEndpoints completionHandler =
  withObjCPtr datagrams $ \raw_datagrams ->
      sendMsg neAppProxyUDPFlow (mkSelector "writeDatagrams:sentByFlowEndpoints:completionHandler:") retVoid [argPtr (castPtr raw_datagrams :: Ptr ()), argPtr (castPtr (unRawId remoteEndpoints) :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
writeDatagrams_sentByEndpoints_completionHandler neAppProxyUDPFlow  datagrams remoteEndpoints completionHandler =
  withObjCPtr datagrams $ \raw_datagrams ->
    withObjCPtr remoteEndpoints $ \raw_remoteEndpoints ->
        sendMsg neAppProxyUDPFlow (mkSelector "writeDatagrams:sentByEndpoints:completionHandler:") retVoid [argPtr (castPtr raw_datagrams :: Ptr ()), argPtr (castPtr raw_remoteEndpoints :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | localFlowEndpoint
--
-- An @nw_endpoint_t@ object containing the local endpoint of the flow's corresponding socket.
--
-- ObjC selector: @- localFlowEndpoint@
localFlowEndpoint :: IsNEAppProxyUDPFlow neAppProxyUDPFlow => neAppProxyUDPFlow -> IO (Id NSObject)
localFlowEndpoint neAppProxyUDPFlow  =
    sendMsg neAppProxyUDPFlow (mkSelector "localFlowEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localEndpoint
--
-- An NWEndpoint object containing the local endpoint of the flow's corresponding socket.
--
-- ObjC selector: @- localEndpoint@
localEndpoint :: IsNEAppProxyUDPFlow neAppProxyUDPFlow => neAppProxyUDPFlow -> IO (Id NWEndpoint)
localEndpoint neAppProxyUDPFlow  =
    sendMsg neAppProxyUDPFlow (mkSelector "localEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @writeDatagrams:sentByFlowEndpoints:completionHandler:@
writeDatagrams_sentByFlowEndpoints_completionHandlerSelector :: Selector
writeDatagrams_sentByFlowEndpoints_completionHandlerSelector = mkSelector "writeDatagrams:sentByFlowEndpoints:completionHandler:"

-- | @Selector@ for @writeDatagrams:sentByEndpoints:completionHandler:@
writeDatagrams_sentByEndpoints_completionHandlerSelector :: Selector
writeDatagrams_sentByEndpoints_completionHandlerSelector = mkSelector "writeDatagrams:sentByEndpoints:completionHandler:"

-- | @Selector@ for @localFlowEndpoint@
localFlowEndpointSelector :: Selector
localFlowEndpointSelector = mkSelector "localFlowEndpoint"

-- | @Selector@ for @localEndpoint@
localEndpointSelector :: Selector
localEndpointSelector = mkSelector "localEndpoint"

