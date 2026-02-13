{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppProxyFlow
--
-- The NEAppProxyFlow class is an abstract base class that declares the programmatic interface for a flow of network data.
--
-- NEAppProxyFlow is part of NetworkExtension.framework.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppProxyFlow@.
module ObjC.NetworkExtension.NEAppProxyFlow
  ( NEAppProxyFlow
  , IsNEAppProxyFlow(..)
  , openWithLocalFlowEndpoint_completionHandler
  , openWithLocalEndpoint_completionHandler
  , closeReadWithError
  , closeWriteWithError
  , setMetadata
  , metaData
  , networkInterface
  , setNetworkInterface
  , remoteHostname
  , isBound
  , closeReadWithErrorSelector
  , closeWriteWithErrorSelector
  , isBoundSelector
  , metaDataSelector
  , networkInterfaceSelector
  , openWithLocalEndpoint_completionHandlerSelector
  , openWithLocalFlowEndpoint_completionHandlerSelector
  , remoteHostnameSelector
  , setMetadataSelector
  , setNetworkInterfaceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | openWithLocalFlowEndpoint:completionHandler:
--
-- This function is used by an NEProvider implementation to indicate that it is ready to handle flow data.
--
-- @localEndpoint@ — The address and port that should be used as the local endpoint of the socket associated with this flow. If the source application already specified a local endpoint by binding the socket then this parameter is ignored.
--
-- @completionHandler@ — A block that is called when the process of opening flow is complete. A nil value passed to this block indicates that the flow was opened successfully. A non-nil NSError value indicates that the flow failed to open successfully.
--
-- ObjC selector: @- openWithLocalFlowEndpoint:completionHandler:@
openWithLocalFlowEndpoint_completionHandler :: (IsNEAppProxyFlow neAppProxyFlow, IsNSObject localEndpoint) => neAppProxyFlow -> localEndpoint -> Ptr () -> IO ()
openWithLocalFlowEndpoint_completionHandler neAppProxyFlow localEndpoint completionHandler =
  sendMessage neAppProxyFlow openWithLocalFlowEndpoint_completionHandlerSelector (toNSObject localEndpoint) completionHandler

-- | openWithLocalEndpoint:completionHandler:
--
-- This function is used by an NEProvider implementation to indicate that it is ready to handle flow data.
--
-- @localEndpoint@ — The address and port that should be used as the local endpoint of the socket associated with this flow. If the source application already specified a local endpoint by binding the socket then this parameter is ignored.
--
-- @completionHandler@ — A block that is called when the process of opening flow is complete. A nil value passed to this block indicates that the flow was opened successfully. A non-nil NSError value indicates that the flow failed to open successfully.
--
-- ObjC selector: @- openWithLocalEndpoint:completionHandler:@
openWithLocalEndpoint_completionHandler :: (IsNEAppProxyFlow neAppProxyFlow, IsNWHostEndpoint localEndpoint) => neAppProxyFlow -> localEndpoint -> Ptr () -> IO ()
openWithLocalEndpoint_completionHandler neAppProxyFlow localEndpoint completionHandler =
  sendMessage neAppProxyFlow openWithLocalEndpoint_completionHandlerSelector (toNWHostEndpoint localEndpoint) completionHandler

-- | closeReadWithError:
--
-- This function is used by an NEProvider implementation to indicate that it does not want to receive any more data from the flow.
--
-- @error@ — An error in NEAppProxyErrorDomain that should be passed to the flow's source application.
--
-- ObjC selector: @- closeReadWithError:@
closeReadWithError :: (IsNEAppProxyFlow neAppProxyFlow, IsNSError error_) => neAppProxyFlow -> error_ -> IO ()
closeReadWithError neAppProxyFlow error_ =
  sendMessage neAppProxyFlow closeReadWithErrorSelector (toNSError error_)

-- | closeWriteWithError:
--
-- This functions is used by an NEProvider implementation to indicate that it does not have any more data to write to the flow.
--
-- @error@ — An error in NEAppProxyErrorDomain that should be passed to the flow's source application.
--
-- ObjC selector: @- closeWriteWithError:@
closeWriteWithError :: (IsNEAppProxyFlow neAppProxyFlow, IsNSError error_) => neAppProxyFlow -> error_ -> IO ()
closeWriteWithError neAppProxyFlow error_ =
  sendMessage neAppProxyFlow closeWriteWithErrorSelector (toNSError error_)

-- | setMetadata:
--
-- Set the flow's NEFlowMetaData object in an nw_parameters_t object. The nw_parameters_t object can then be used to create a connection that transparently proxies the flow's     data, and provides accurate source app information to any subsequent NEAppProxyProvider instances that transparently proxy the flow.
--
-- @parameters@ — An nw_parameters_t object.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsNEAppProxyFlow neAppProxyFlow, IsNSObject parameters) => neAppProxyFlow -> parameters -> IO ()
setMetadata neAppProxyFlow parameters =
  sendMessage neAppProxyFlow setMetadataSelector (toNSObject parameters)

-- | metaData
--
-- An NEFlowMetaData object containing meta data for the flow.
--
-- ObjC selector: @- metaData@
metaData :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NEFlowMetaData)
metaData neAppProxyFlow =
  sendMessage neAppProxyFlow metaDataSelector

-- | networkInterface
--
-- An nw_interface_t containing information about the network interface used by the flow. If the flow's data is transported using a different interface, this property    should be set to that interface.
--
-- ObjC selector: @- networkInterface@
networkInterface :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NSObject)
networkInterface neAppProxyFlow =
  sendMessage neAppProxyFlow networkInterfaceSelector

-- | networkInterface
--
-- An nw_interface_t containing information about the network interface used by the flow. If the flow's data is transported using a different interface, this property    should be set to that interface.
--
-- ObjC selector: @- setNetworkInterface:@
setNetworkInterface :: (IsNEAppProxyFlow neAppProxyFlow, IsNSObject value) => neAppProxyFlow -> value -> IO ()
setNetworkInterface neAppProxyFlow value =
  sendMessage neAppProxyFlow setNetworkInterfaceSelector (toNSObject value)

-- | remoteHostname
--
-- If the flow was created by passing a hostname to a "connect by name" API such as NSURLSession or Network.framework, this property is set to the     remote hostname.
--
-- ObjC selector: @- remoteHostname@
remoteHostname :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NSString)
remoteHostname neAppProxyFlow =
  sendMessage neAppProxyFlow remoteHostnameSelector

-- | isBound
--
-- YES if the flow was bound by the application to a specific interface (contained in the networkInterface property), NO otherwise.
--
-- ObjC selector: @- isBound@
isBound :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO Bool
isBound neAppProxyFlow =
  sendMessage neAppProxyFlow isBoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithLocalFlowEndpoint:completionHandler:@
openWithLocalFlowEndpoint_completionHandlerSelector :: Selector '[Id NSObject, Ptr ()] ()
openWithLocalFlowEndpoint_completionHandlerSelector = mkSelector "openWithLocalFlowEndpoint:completionHandler:"

-- | @Selector@ for @openWithLocalEndpoint:completionHandler:@
openWithLocalEndpoint_completionHandlerSelector :: Selector '[Id NWHostEndpoint, Ptr ()] ()
openWithLocalEndpoint_completionHandlerSelector = mkSelector "openWithLocalEndpoint:completionHandler:"

-- | @Selector@ for @closeReadWithError:@
closeReadWithErrorSelector :: Selector '[Id NSError] ()
closeReadWithErrorSelector = mkSelector "closeReadWithError:"

-- | @Selector@ for @closeWriteWithError:@
closeWriteWithErrorSelector :: Selector '[Id NSError] ()
closeWriteWithErrorSelector = mkSelector "closeWriteWithError:"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector '[Id NSObject] ()
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector '[] (Id NEFlowMetaData)
metaDataSelector = mkSelector "metaData"

-- | @Selector@ for @networkInterface@
networkInterfaceSelector :: Selector '[] (Id NSObject)
networkInterfaceSelector = mkSelector "networkInterface"

-- | @Selector@ for @setNetworkInterface:@
setNetworkInterfaceSelector :: Selector '[Id NSObject] ()
setNetworkInterfaceSelector = mkSelector "setNetworkInterface:"

-- | @Selector@ for @remoteHostname@
remoteHostnameSelector :: Selector '[] (Id NSString)
remoteHostnameSelector = mkSelector "remoteHostname"

-- | @Selector@ for @isBound@
isBoundSelector :: Selector '[] Bool
isBoundSelector = mkSelector "isBound"

