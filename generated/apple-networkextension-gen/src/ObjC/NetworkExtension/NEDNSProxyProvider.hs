{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEDNSProxyProvider
--
-- The NEDNSProxyProvider class declares the programmatic interface for an object that implements the client side of a custom DNS proxy solution.
--
-- NEDNSProxyProvider is part of NetworkExtension.framework
--
-- Generated bindings for @NEDNSProxyProvider@.
module ObjC.NetworkExtension.NEDNSProxyProvider
  ( NEDNSProxyProvider
  , IsNEDNSProxyProvider(..)
  , startProxyWithOptions_completionHandler
  , stopProxyWithReason_completionHandler
  , cancelProxyWithError
  , handleNewFlow
  , handleNewUDPFlow_initialRemoteFlowEndpoint
  , handleNewUDPFlow_initialRemoteEndpoint
  , systemDNSSettings
  , cancelProxyWithErrorSelector
  , handleNewFlowSelector
  , handleNewUDPFlow_initialRemoteEndpointSelector
  , handleNewUDPFlow_initialRemoteFlowEndpointSelector
  , startProxyWithOptions_completionHandlerSelector
  , stopProxyWithReason_completionHandlerSelector
  , systemDNSSettingsSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
startProxyWithOptions_completionHandler :: (IsNEDNSProxyProvider nednsProxyProvider, IsNSDictionary options) => nednsProxyProvider -> options -> Ptr () -> IO ()
startProxyWithOptions_completionHandler nednsProxyProvider options completionHandler =
  sendMessage nednsProxyProvider startProxyWithOptions_completionHandlerSelector (toNSDictionary options) completionHandler

-- | stopProxyWithReason:completionHandler:
--
-- This function is called by the framework when the proxy is being stopped. Subclasses must override this method to perform whatever steps are necessary to stop the proxy.
--
-- @reason@ — An NEProviderStopReason indicating why the proxy is being stopped.
--
-- @completionHandler@ — A block that must be called when the proxy is completely stopped.
--
-- ObjC selector: @- stopProxyWithReason:completionHandler:@
stopProxyWithReason_completionHandler :: IsNEDNSProxyProvider nednsProxyProvider => nednsProxyProvider -> NEProviderStopReason -> Ptr () -> IO ()
stopProxyWithReason_completionHandler nednsProxyProvider reason completionHandler =
  sendMessage nednsProxyProvider stopProxyWithReason_completionHandlerSelector reason completionHandler

-- | cancelProxyWithError:
--
-- This function is called by proxy provider implementations to stop the proxy when a network error is encountered that renders the proxy no longer viable. Subclasses should not override this method.
--
-- @error@ — An NSError object containing details about the error that the proxy provider implementation encountered.
--
-- ObjC selector: @- cancelProxyWithError:@
cancelProxyWithError :: (IsNEDNSProxyProvider nednsProxyProvider, IsNSError error_) => nednsProxyProvider -> error_ -> IO ()
cancelProxyWithError nednsProxyProvider error_ =
  sendMessage nednsProxyProvider cancelProxyWithErrorSelector (toNSError error_)

-- | handleNewFlow:
--
-- This function is called by the framework to deliver a new network data flow to the proxy provider implementation. Subclasses must override this method to perform whatever steps are necessary to ready the proxy to receive data from the flow. The proxy provider implementation indicates that the proxy is ready to handle flow data by calling -[NEAppProxyFlow openWithLocalFlowEndpoint:completionHandler:] on the flow. If the proxy implementation decides to not handle the flow and instead terminate it, the subclass implementation of this method should return NO. If the proxy implementation decides to handle the flow, the subclass implementation of this method should return YES. In this case the proxy implementation is responsible for retaining the NEAppProxyFlow object.
--
-- @flow@ — The new flow
--
-- Returns: YES if the proxy implementation has retained the flow and intends to handle the flow data. NO if the proxy implementation has not retained the flow and will not handle the flow data. In this case the flow is terminated.
--
-- ObjC selector: @- handleNewFlow:@
handleNewFlow :: (IsNEDNSProxyProvider nednsProxyProvider, IsNEAppProxyFlow flow) => nednsProxyProvider -> flow -> IO Bool
handleNewFlow nednsProxyProvider flow =
  sendMessage nednsProxyProvider handleNewFlowSelector (toNEAppProxyFlow flow)

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
handleNewUDPFlow_initialRemoteFlowEndpoint :: (IsNEDNSProxyProvider nednsProxyProvider, IsNEAppProxyUDPFlow flow, IsNSObject remoteEndpoint) => nednsProxyProvider -> flow -> remoteEndpoint -> IO Bool
handleNewUDPFlow_initialRemoteFlowEndpoint nednsProxyProvider flow remoteEndpoint =
  sendMessage nednsProxyProvider handleNewUDPFlow_initialRemoteFlowEndpointSelector (toNEAppProxyUDPFlow flow) (toNSObject remoteEndpoint)

-- | handleNewUDPFlow:initialRemoteEndpoint:
--
-- This function is called by the framework to deliver a new UDP data flow to the proxy provider implementation. Subclasses can override this method to perform whatever steps are necessary to ready the proxy to receive     data from the flow. The proxy provider implementation indicates that the proxy is ready to handle flow data by calling -[NEAppProxyFlow openWithLocalEndpoint:completionHandler:] on the flow. If the proxy implementation decides     to not handle the flow and instead terminate it, the subclass implementation of this method should return NO. If the proxy implementation decides to handle the flow, the subclass implementation of this method should return YES.     In this case the proxy implementation is responsible for retaining the NEAppProxyUDPFlow object.     The default implementation of this method calls -[NEAppProxyProvider handleNewFlow:] and returns its result.
--
-- @flow@ — The new UDP flow
--
-- @remoteEndpoint@ — The initial remote endpoint provided by the proxied app when the flow was opened.
--
-- Returns: YES if the proxy implementation has retained the flow and intends to handle the flow data. NO if the proxy implementation has not retained the flow and will not handle the flow data. In this case the flow is terminated.
--
-- ObjC selector: @- handleNewUDPFlow:initialRemoteEndpoint:@
handleNewUDPFlow_initialRemoteEndpoint :: (IsNEDNSProxyProvider nednsProxyProvider, IsNEAppProxyUDPFlow flow, IsNWEndpoint remoteEndpoint) => nednsProxyProvider -> flow -> remoteEndpoint -> IO Bool
handleNewUDPFlow_initialRemoteEndpoint nednsProxyProvider flow remoteEndpoint =
  sendMessage nednsProxyProvider handleNewUDPFlow_initialRemoteEndpointSelector (toNEAppProxyUDPFlow flow) (toNWEndpoint remoteEndpoint)

-- | systemDNSSettings
--
-- The current system DNS settings. Use KVO to watch for changes.
--
-- ObjC selector: @- systemDNSSettings@
systemDNSSettings :: IsNEDNSProxyProvider nednsProxyProvider => nednsProxyProvider -> IO (Id NSArray)
systemDNSSettings nednsProxyProvider =
  sendMessage nednsProxyProvider systemDNSSettingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startProxyWithOptions:completionHandler:@
startProxyWithOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
startProxyWithOptions_completionHandlerSelector = mkSelector "startProxyWithOptions:completionHandler:"

-- | @Selector@ for @stopProxyWithReason:completionHandler:@
stopProxyWithReason_completionHandlerSelector :: Selector '[NEProviderStopReason, Ptr ()] ()
stopProxyWithReason_completionHandlerSelector = mkSelector "stopProxyWithReason:completionHandler:"

-- | @Selector@ for @cancelProxyWithError:@
cancelProxyWithErrorSelector :: Selector '[Id NSError] ()
cancelProxyWithErrorSelector = mkSelector "cancelProxyWithError:"

-- | @Selector@ for @handleNewFlow:@
handleNewFlowSelector :: Selector '[Id NEAppProxyFlow] Bool
handleNewFlowSelector = mkSelector "handleNewFlow:"

-- | @Selector@ for @handleNewUDPFlow:initialRemoteFlowEndpoint:@
handleNewUDPFlow_initialRemoteFlowEndpointSelector :: Selector '[Id NEAppProxyUDPFlow, Id NSObject] Bool
handleNewUDPFlow_initialRemoteFlowEndpointSelector = mkSelector "handleNewUDPFlow:initialRemoteFlowEndpoint:"

-- | @Selector@ for @handleNewUDPFlow:initialRemoteEndpoint:@
handleNewUDPFlow_initialRemoteEndpointSelector :: Selector '[Id NEAppProxyUDPFlow, Id NWEndpoint] Bool
handleNewUDPFlow_initialRemoteEndpointSelector = mkSelector "handleNewUDPFlow:initialRemoteEndpoint:"

-- | @Selector@ for @systemDNSSettings@
systemDNSSettingsSelector :: Selector '[] (Id NSArray)
systemDNSSettingsSelector = mkSelector "systemDNSSettings"

