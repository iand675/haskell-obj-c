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
  , openWithLocalFlowEndpoint_completionHandlerSelector
  , openWithLocalEndpoint_completionHandlerSelector
  , closeReadWithErrorSelector
  , closeWriteWithErrorSelector
  , setMetadataSelector
  , metaDataSelector
  , networkInterfaceSelector
  , setNetworkInterfaceSelector
  , remoteHostnameSelector
  , isBoundSelector


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
openWithLocalFlowEndpoint_completionHandler neAppProxyFlow  localEndpoint completionHandler =
withObjCPtr localEndpoint $ \raw_localEndpoint ->
    sendMsg neAppProxyFlow (mkSelector "openWithLocalFlowEndpoint:completionHandler:") retVoid [argPtr (castPtr raw_localEndpoint :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
openWithLocalEndpoint_completionHandler neAppProxyFlow  localEndpoint completionHandler =
withObjCPtr localEndpoint $ \raw_localEndpoint ->
    sendMsg neAppProxyFlow (mkSelector "openWithLocalEndpoint:completionHandler:") retVoid [argPtr (castPtr raw_localEndpoint :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | closeReadWithError:
--
-- This function is used by an NEProvider implementation to indicate that it does not want to receive any more data from the flow.
--
-- @error@ — An error in NEAppProxyErrorDomain that should be passed to the flow's source application.
--
-- ObjC selector: @- closeReadWithError:@
closeReadWithError :: (IsNEAppProxyFlow neAppProxyFlow, IsNSError error_) => neAppProxyFlow -> error_ -> IO ()
closeReadWithError neAppProxyFlow  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg neAppProxyFlow (mkSelector "closeReadWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | closeWriteWithError:
--
-- This functions is used by an NEProvider implementation to indicate that it does not have any more data to write to the flow.
--
-- @error@ — An error in NEAppProxyErrorDomain that should be passed to the flow's source application.
--
-- ObjC selector: @- closeWriteWithError:@
closeWriteWithError :: (IsNEAppProxyFlow neAppProxyFlow, IsNSError error_) => neAppProxyFlow -> error_ -> IO ()
closeWriteWithError neAppProxyFlow  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg neAppProxyFlow (mkSelector "closeWriteWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | setMetadata:
--
-- Set the flow's NEFlowMetaData object in an nw_parameters_t object. The nw_parameters_t object can then be used to create a connection that transparently proxies the flow's     data, and provides accurate source app information to any subsequent NEAppProxyProvider instances that transparently proxy the flow.
--
-- @parameters@ — An nw_parameters_t object.
--
-- ObjC selector: @- setMetadata:@
setMetadata :: (IsNEAppProxyFlow neAppProxyFlow, IsNSObject parameters) => neAppProxyFlow -> parameters -> IO ()
setMetadata neAppProxyFlow  parameters =
withObjCPtr parameters $ \raw_parameters ->
    sendMsg neAppProxyFlow (mkSelector "setMetadata:") retVoid [argPtr (castPtr raw_parameters :: Ptr ())]

-- | metaData
--
-- An NEFlowMetaData object containing meta data for the flow.
--
-- ObjC selector: @- metaData@
metaData :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NEFlowMetaData)
metaData neAppProxyFlow  =
  sendMsg neAppProxyFlow (mkSelector "metaData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | networkInterface
--
-- An nw_interface_t containing information about the network interface used by the flow. If the flow's data is transported using a different interface, this property    should be set to that interface.
--
-- ObjC selector: @- networkInterface@
networkInterface :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NSObject)
networkInterface neAppProxyFlow  =
  sendMsg neAppProxyFlow (mkSelector "networkInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | networkInterface
--
-- An nw_interface_t containing information about the network interface used by the flow. If the flow's data is transported using a different interface, this property    should be set to that interface.
--
-- ObjC selector: @- setNetworkInterface:@
setNetworkInterface :: (IsNEAppProxyFlow neAppProxyFlow, IsNSObject value) => neAppProxyFlow -> value -> IO ()
setNetworkInterface neAppProxyFlow  value =
withObjCPtr value $ \raw_value ->
    sendMsg neAppProxyFlow (mkSelector "setNetworkInterface:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | remoteHostname
--
-- If the flow was created by passing a hostname to a "connect by name" API such as NSURLSession or Network.framework, this property is set to the     remote hostname.
--
-- ObjC selector: @- remoteHostname@
remoteHostname :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO (Id NSString)
remoteHostname neAppProxyFlow  =
  sendMsg neAppProxyFlow (mkSelector "remoteHostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isBound
--
-- YES if the flow was bound by the application to a specific interface (contained in the networkInterface property), NO otherwise.
--
-- ObjC selector: @- isBound@
isBound :: IsNEAppProxyFlow neAppProxyFlow => neAppProxyFlow -> IO Bool
isBound neAppProxyFlow  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppProxyFlow (mkSelector "isBound") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithLocalFlowEndpoint:completionHandler:@
openWithLocalFlowEndpoint_completionHandlerSelector :: Selector
openWithLocalFlowEndpoint_completionHandlerSelector = mkSelector "openWithLocalFlowEndpoint:completionHandler:"

-- | @Selector@ for @openWithLocalEndpoint:completionHandler:@
openWithLocalEndpoint_completionHandlerSelector :: Selector
openWithLocalEndpoint_completionHandlerSelector = mkSelector "openWithLocalEndpoint:completionHandler:"

-- | @Selector@ for @closeReadWithError:@
closeReadWithErrorSelector :: Selector
closeReadWithErrorSelector = mkSelector "closeReadWithError:"

-- | @Selector@ for @closeWriteWithError:@
closeWriteWithErrorSelector :: Selector
closeWriteWithErrorSelector = mkSelector "closeWriteWithError:"

-- | @Selector@ for @setMetadata:@
setMetadataSelector :: Selector
setMetadataSelector = mkSelector "setMetadata:"

-- | @Selector@ for @metaData@
metaDataSelector :: Selector
metaDataSelector = mkSelector "metaData"

-- | @Selector@ for @networkInterface@
networkInterfaceSelector :: Selector
networkInterfaceSelector = mkSelector "networkInterface"

-- | @Selector@ for @setNetworkInterface:@
setNetworkInterfaceSelector :: Selector
setNetworkInterfaceSelector = mkSelector "setNetworkInterface:"

-- | @Selector@ for @remoteHostname@
remoteHostnameSelector :: Selector
remoteHostnameSelector = mkSelector "remoteHostname"

-- | @Selector@ for @isBound@
isBoundSelector :: Selector
isBoundSelector = mkSelector "isBound"

