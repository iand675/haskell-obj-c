{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppProxyTCPFlow
--
-- The NEAppProxyTCPFlow class declares the programmatic interface of an object that is used by NEAppProxyProvider implementations to proxy the payload of TCP connections.
--
-- NEAppProxyTCPFlow is part of NetworkExtension.framework
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppProxyTCPFlow@.
module ObjC.NetworkExtension.NEAppProxyTCPFlow
  ( NEAppProxyTCPFlow
  , IsNEAppProxyTCPFlow(..)
  , readDataWithCompletionHandler
  , writeData_withCompletionHandler
  , remoteFlowEndpoint
  , remoteEndpoint
  , readDataWithCompletionHandlerSelector
  , remoteEndpointSelector
  , remoteFlowEndpointSelector
  , writeData_withCompletionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | readDataWithCompletionHandler:
--
-- Read data from the flow.
--
-- @completionHandler@ — A block that will be executed when some data is read from the flow. The block is passed either the data that was read or a non-nil error if an error occurred. If data has a length of 0 then no data can be subsequently read from the flow. The completion handler is only called for the single read operation that was initiated by calling this method. If the caller wants to read more data then it should call this method again to schedule another read operation and another execution of the completion handler block.
--
-- ObjC selector: @- readDataWithCompletionHandler:@
readDataWithCompletionHandler :: IsNEAppProxyTCPFlow neAppProxyTCPFlow => neAppProxyTCPFlow -> Ptr () -> IO ()
readDataWithCompletionHandler neAppProxyTCPFlow completionHandler =
  sendMessage neAppProxyTCPFlow readDataWithCompletionHandlerSelector completionHandler

-- | writeData:completionHandler
--
-- Write data to the flow.
--
-- @data@ — The data to write.
--
-- @completionHandler@ — A block that will be executed when the data is written into the associated socket's receive buffer. The caller should use this callback as an indication that it is possible to write more data to the flow without using up excessive buffer memory. If an error occurs while writing the data then a non-nil NSError object is passed to the block.
--
-- ObjC selector: @- writeData:withCompletionHandler:@
writeData_withCompletionHandler :: (IsNEAppProxyTCPFlow neAppProxyTCPFlow, IsNSData data_) => neAppProxyTCPFlow -> data_ -> Ptr () -> IO ()
writeData_withCompletionHandler neAppProxyTCPFlow data_ completionHandler =
  sendMessage neAppProxyTCPFlow writeData_withCompletionHandlerSelector (toNSData data_) completionHandler

-- | remoteFlowEndpoint
--
-- An @nw_endpoint_t@ object containing information about the intended remote endpoint of the flow.
--
-- ObjC selector: @- remoteFlowEndpoint@
remoteFlowEndpoint :: IsNEAppProxyTCPFlow neAppProxyTCPFlow => neAppProxyTCPFlow -> IO (Id NSObject)
remoteFlowEndpoint neAppProxyTCPFlow =
  sendMessage neAppProxyTCPFlow remoteFlowEndpointSelector

-- | remoteEndpoint
--
-- An NWEndpoint object containing information about the intended remote endpoint of the flow.
--
-- ObjC selector: @- remoteEndpoint@
remoteEndpoint :: IsNEAppProxyTCPFlow neAppProxyTCPFlow => neAppProxyTCPFlow -> IO (Id NWEndpoint)
remoteEndpoint neAppProxyTCPFlow =
  sendMessage neAppProxyTCPFlow remoteEndpointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readDataWithCompletionHandler:@
readDataWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readDataWithCompletionHandlerSelector = mkSelector "readDataWithCompletionHandler:"

-- | @Selector@ for @writeData:withCompletionHandler:@
writeData_withCompletionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
writeData_withCompletionHandlerSelector = mkSelector "writeData:withCompletionHandler:"

-- | @Selector@ for @remoteFlowEndpoint@
remoteFlowEndpointSelector :: Selector '[] (Id NSObject)
remoteFlowEndpointSelector = mkSelector "remoteFlowEndpoint"

-- | @Selector@ for @remoteEndpoint@
remoteEndpointSelector :: Selector '[] (Id NWEndpoint)
remoteEndpointSelector = mkSelector "remoteEndpoint"

