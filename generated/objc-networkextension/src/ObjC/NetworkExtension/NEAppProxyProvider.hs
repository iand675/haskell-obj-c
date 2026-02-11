{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppProxyProvider
--
-- The NEAppProxyProvider class declares the programmatic interface for an object that implements the client side of a custom network proxy solution.
--
-- NEAppProxyProvider is part of NetworkExtension.framework
--
-- Generated bindings for @NEAppProxyProvider@.
module ObjC.NetworkExtension.NEAppProxyProvider
  ( NEAppProxyProvider
  , IsNEAppProxyProvider(..)
  , startProxyWithOptions_completionHandler
  , stopProxyWithReason_completionHandler
  , cancelProxyWithError
  , handleNewFlow
  , handleNewUDPFlow_initialRemoteFlowEndpoint
  , handleNewUDPFlow_initialRemoteEndpoint
  , startProxyWithOptions_completionHandlerSelector
  , stopProxyWithReason_completionHandlerSelector
  , cancelProxyWithErrorSelector
  , handleNewFlowSelector
  , handleNewUDPFlow_initialRemoteFlowEndpointSelector
  , handleNewUDPFlow_initialRemoteEndpointSelector

  -- * Enum types
  , NEProviderStopReason(NEProviderStopReason)
  , pattern NEProviderStopReasonNone
  , pattern NEProviderStopReasonUserInitiated
  , pattern NEProviderStopReasonProviderFailed
  , pattern NEProviderStopReasonNoNetworkAvailable
  , pattern NEProviderStopReasonUnrecoverableNetworkChange
  , pattern NEProviderStopReasonProviderDisabled
  , pattern NEProviderStopReasonAuthenticationCanceled
  , pattern NEProviderStopReasonConfigurationFailed
  , pattern NEProviderStopReasonIdleTimeout
  , pattern NEProviderStopReasonConfigurationDisabled
  , pattern NEProviderStopReasonConfigurationRemoved
  , pattern NEProviderStopReasonSuperceded
  , pattern NEProviderStopReasonUserLogout
  , pattern NEProviderStopReasonUserSwitch
  , pattern NEProviderStopReasonConnectionFailed
  , pattern NEProviderStopReasonSleep
  , pattern NEProviderStopReasonAppUpdate
  , pattern NEProviderStopReasonInternalError

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
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | startProxyWithOptions:completionHandler:
--
-- This function is called by the framework when a new proxy instance is being created. Subclasses must override this method to perform whatever steps are necessary to ready the proxy for handling flows of network data.
--
-- @options@ — A dictionary containing keys and values passed by the provider's containing app. If the containing app did not start the proxy then this parameter will be nil.
--
-- @completionHandler@ — A block that must be called when the process of starting the proxy is complete. If the proxy cannot be started then the subclass' implementation of this method must pass a non-nil NSError object to this block. A value of nil passed to the completion handler indicates that the proxy was successfully started.
--
-- ObjC selector: @- startProxyWithOptions:completionHandler:@
startProxyWithOptions_completionHandler :: (IsNEAppProxyProvider neAppProxyProvider, IsNSDictionary options) => neAppProxyProvider -> options -> Ptr () -> IO ()
startProxyWithOptions_completionHandler neAppProxyProvider  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg neAppProxyProvider (mkSelector "startProxyWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | stopProxyWithReason:completionHandler:
--
-- This function is called by the framework when the proxy is being stopped. Subclasses must override this method to perform whatever steps are necessary to stop the proxy.
--
-- @reason@ — An NEProviderStopReason indicating why the proxy is being stopped.
--
-- @completionHandler@ — A block that must be called when the proxy is completely stopped.
--
-- ObjC selector: @- stopProxyWithReason:completionHandler:@
stopProxyWithReason_completionHandler :: IsNEAppProxyProvider neAppProxyProvider => neAppProxyProvider -> NEProviderStopReason -> Ptr () -> IO ()
stopProxyWithReason_completionHandler neAppProxyProvider  reason completionHandler =
  sendMsg neAppProxyProvider (mkSelector "stopProxyWithReason:completionHandler:") retVoid [argCLong (coerce reason), argPtr (castPtr completionHandler :: Ptr ())]

-- | cancelProxyWithError:
--
-- This function is called by proxy provider implementations to stop the proxy when a network error is encountered that renders the proxy no longer viable. Subclasses should not override this method.
--
-- @error@ — An NSError object containing details about the error that the proxy provider implementation encountered.
--
-- ObjC selector: @- cancelProxyWithError:@
cancelProxyWithError :: (IsNEAppProxyProvider neAppProxyProvider, IsNSError error_) => neAppProxyProvider -> error_ -> IO ()
cancelProxyWithError neAppProxyProvider  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg neAppProxyProvider (mkSelector "cancelProxyWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | handleNewFlow:
--
-- This function is called by the framework to deliver a new network data flow to the proxy provider implementation. Subclasses must override this method to perform whatever steps are necessary to ready the proxy to receive data from the flow. The proxy provider implementation indicates that the proxy is ready to handle flow data by calling -[NEAppProxyFlow openWithLocalFlowEndpoint:completionHandler:] on the flow. If the proxy implementation decides to not handle the flow and instead terminate it, the subclass implementation of this method should return NO. If the proxy implementation decides to handle the flow, the subclass implementation of this method should return YES. In this case the proxy implementation is responsible for retaining the NEAppProxyFlow object.
--
-- @flow@ — The new flow
--
-- Returns: YES if the proxy implementation has retained the flow and intends to handle the flow data. NO if the proxy implementation has not retained the flow and will not handle the flow data. In NETransparentProxyProvider sub-classes returning NO causes the flow to be handled by the networking stack without any proxy. In all other cases the flow is terminated when NO is returned.
--
-- ObjC selector: @- handleNewFlow:@
handleNewFlow :: (IsNEAppProxyProvider neAppProxyProvider, IsNEAppProxyFlow flow) => neAppProxyProvider -> flow -> IO Bool
handleNewFlow neAppProxyProvider  flow =
withObjCPtr flow $ \raw_flow ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppProxyProvider (mkSelector "handleNewFlow:") retCULong [argPtr (castPtr raw_flow :: Ptr ())]

-- | handleNewUDPFlow:initialRemoteFlowEndpoint:
--
-- This function is called by the framework to deliver a new UDP data flow to the proxy provider implementation. Subclasses can override this method to perform whatever steps are necessary to ready the proxy to receive     data from the flow. The proxy provider implementation indicates that the proxy is ready to handle flow data by calling -[NEAppProxyFlow openWithLocalFlowEndpoint:completionHandler:] on the flow. If the proxy implementation decides     to not handle the flow and instead terminate it, the subclass implementation of this method should return NO. If the proxy implementation decides to handle the flow, the subclass implementation of this method should return YES.     In this case the proxy implementation is responsible for retaining the NEAppProxyUDPFlow object.     The default implementation of this method calls -[NEAppProxyProvider handleNewFlow:] and returns its result.
--
-- NEAppProxyUDPFlowHandling for Swift subclasses.
--
-- @flow@ — The new UDP flow
--
-- @remoteEndpoint@ — The initial remote endpoint provided by the proxied app when the flow was opened.
--
-- Returns: YES if the proxy implementation has retained the flow and intends to handle the flow data. NO if the proxy implementation has not retained the flow and will not handle the flow data. In this case the flow is terminated.
--
-- ObjC selector: @- handleNewUDPFlow:initialRemoteFlowEndpoint:@
handleNewUDPFlow_initialRemoteFlowEndpoint :: (IsNEAppProxyProvider neAppProxyProvider, IsNEAppProxyUDPFlow flow, IsNSObject remoteEndpoint) => neAppProxyProvider -> flow -> remoteEndpoint -> IO Bool
handleNewUDPFlow_initialRemoteFlowEndpoint neAppProxyProvider  flow remoteEndpoint =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr remoteEndpoint $ \raw_remoteEndpoint ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppProxyProvider (mkSelector "handleNewUDPFlow:initialRemoteFlowEndpoint:") retCULong [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr raw_remoteEndpoint :: Ptr ())]

-- | handleNewUDPFlow:initialRemoteEndpoint:
--
-- This function is called by the framework to deliver a new UDP data flow to the proxy provider implementation. Subclasses can override this method to perform whatever steps are necessary to ready the proxy to receive     data from the flow. The proxy provider implementation indicates that the proxy is ready to handle flow data by calling -[NEAppProxyFlow openWithLocalEndpoint:completionHandler:] on the flow. If the proxy implementation decides     to not handle the flow and instead terminate it, the subclass implementation of this method should return NO. If the proxy implementation decides to handle the flow, the subclass implementation of this method should return YES.     In this case the proxy implementation is responsible for retaining the NEAppProxyUDPFlow object.     The default implementation of this method calls -[NEAppProxyProvider handleNewFlow:] and returns its result.
--
-- @flow@ — The new UDP flow
--
-- @remoteEndpoint@ — The initial remote endpoint provided by the proxied app when the flow was opened.
--
-- Returns: YES if the proxy implementation has retained the flow and intends to handle the flow data. NO if the proxy implementation has not retained the flow and will not handle the flow data. In NETransparentProxyProvider sub-classes returning NO causes the flow to be handled by the networking stack without any proxy. In all other cases the flow is terminated when NO is returned.
--
-- ObjC selector: @- handleNewUDPFlow:initialRemoteEndpoint:@
handleNewUDPFlow_initialRemoteEndpoint :: (IsNEAppProxyProvider neAppProxyProvider, IsNEAppProxyUDPFlow flow, IsNWEndpoint remoteEndpoint) => neAppProxyProvider -> flow -> remoteEndpoint -> IO Bool
handleNewUDPFlow_initialRemoteEndpoint neAppProxyProvider  flow remoteEndpoint =
withObjCPtr flow $ \raw_flow ->
  withObjCPtr remoteEndpoint $ \raw_remoteEndpoint ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg neAppProxyProvider (mkSelector "handleNewUDPFlow:initialRemoteEndpoint:") retCULong [argPtr (castPtr raw_flow :: Ptr ()), argPtr (castPtr raw_remoteEndpoint :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startProxyWithOptions:completionHandler:@
startProxyWithOptions_completionHandlerSelector :: Selector
startProxyWithOptions_completionHandlerSelector = mkSelector "startProxyWithOptions:completionHandler:"

-- | @Selector@ for @stopProxyWithReason:completionHandler:@
stopProxyWithReason_completionHandlerSelector :: Selector
stopProxyWithReason_completionHandlerSelector = mkSelector "stopProxyWithReason:completionHandler:"

-- | @Selector@ for @cancelProxyWithError:@
cancelProxyWithErrorSelector :: Selector
cancelProxyWithErrorSelector = mkSelector "cancelProxyWithError:"

-- | @Selector@ for @handleNewFlow:@
handleNewFlowSelector :: Selector
handleNewFlowSelector = mkSelector "handleNewFlow:"

-- | @Selector@ for @handleNewUDPFlow:initialRemoteFlowEndpoint:@
handleNewUDPFlow_initialRemoteFlowEndpointSelector :: Selector
handleNewUDPFlow_initialRemoteFlowEndpointSelector = mkSelector "handleNewUDPFlow:initialRemoteFlowEndpoint:"

-- | @Selector@ for @handleNewUDPFlow:initialRemoteEndpoint:@
handleNewUDPFlow_initialRemoteEndpointSelector :: Selector
handleNewUDPFlow_initialRemoteEndpointSelector = mkSelector "handleNewUDPFlow:initialRemoteEndpoint:"

