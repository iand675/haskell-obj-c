{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEPacketTunnelProvider
--
-- The NEPacketTunnelProvider class declares the programmatic interface of an object that implements the client side of a custom IP packet tunneling protocol.
--
-- NEPacketTunnelProvider is part of NetworkExtension.framework.
--
-- Generated bindings for @NEPacketTunnelProvider@.
module ObjC.NetworkExtension.NEPacketTunnelProvider
  ( NEPacketTunnelProvider
  , IsNEPacketTunnelProvider(..)
  , startTunnelWithOptions_completionHandler
  , stopTunnelWithReason_completionHandler
  , cancelTunnelWithError
  , createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegate
  , createUDPSessionThroughTunnelToEndpoint_fromEndpoint
  , packetFlow
  , virtualInterface
  , cancelTunnelWithErrorSelector
  , createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegateSelector
  , createUDPSessionThroughTunnelToEndpoint_fromEndpointSelector
  , packetFlowSelector
  , startTunnelWithOptions_completionHandlerSelector
  , stopTunnelWithReason_completionHandlerSelector
  , virtualInterfaceSelector

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

-- | startTunnelWithOptions:completionHandler:
--
-- This function is called by the framework when a new tunnel is being created. Subclasses must override this method to perform whatever steps are necessary to establish the tunnel.
--
-- @options@ — A dictionary containing keys and values passed by the provider's containing app. If the containing app did not start the tunnel then this parameter will be nil.
--
-- @completionHandler@ — A block that must be called when the process of starting the tunnel is complete. If the tunnel cannot be established then the subclass' implementation of this method must pass a non-nil NSError object to this block. A value of nil passed to the completion handler indicates that the tunnel was successfully established.
--
-- ObjC selector: @- startTunnelWithOptions:completionHandler:@
startTunnelWithOptions_completionHandler :: (IsNEPacketTunnelProvider nePacketTunnelProvider, IsNSDictionary options) => nePacketTunnelProvider -> options -> Ptr () -> IO ()
startTunnelWithOptions_completionHandler nePacketTunnelProvider options completionHandler =
  sendMessage nePacketTunnelProvider startTunnelWithOptions_completionHandlerSelector (toNSDictionary options) completionHandler

-- | stopTunnelWithReason:completionHandler:
--
-- This function is called by the framework when the tunnel is being destroyed. Subclasses must override this method to perform whatever steps are necessary to tear down the tunnel.
--
-- @reason@ — An NEProviderStopReason indicating why the tunnel is being stopped.
--
-- @completionHandler@ — A block that must be called when the tunnel is completely torn down.
--
-- ObjC selector: @- stopTunnelWithReason:completionHandler:@
stopTunnelWithReason_completionHandler :: IsNEPacketTunnelProvider nePacketTunnelProvider => nePacketTunnelProvider -> NEProviderStopReason -> Ptr () -> IO ()
stopTunnelWithReason_completionHandler nePacketTunnelProvider reason completionHandler =
  sendMessage nePacketTunnelProvider stopTunnelWithReason_completionHandlerSelector reason completionHandler

-- | cancelTunnelWithError:
--
-- This function is called by tunnel provider implementations to initiate tunnel destruction when a network error is encountered that renders the tunnel no longer viable. Subclasses should not override this method.
--
-- @error@ — An NSError object containing details about the error that the tunnel provider implementation encountered.
--
-- ObjC selector: @- cancelTunnelWithError:@
cancelTunnelWithError :: (IsNEPacketTunnelProvider nePacketTunnelProvider, IsNSError error_) => nePacketTunnelProvider -> error_ -> IO ()
cancelTunnelWithError nePacketTunnelProvider error_ =
  sendMessage nePacketTunnelProvider cancelTunnelWithErrorSelector (toNSError error_)

-- | createTCPConnectionThroughTunnelToEndpoint:enableTLS:TLSParameters:delegate:
--
-- This function can be called by subclass implementations to create a TCP connection to a given network endpoint, through the tunnel established by the provider. This function should not be overridden by subclasses.
--
-- @remoteEndpoint@ — An NWEndpoint object that specifies the remote network endpoint to connect to.
--
-- @enableTLS@ — A flag indicating if a TLS session should be negotiated on the connection.
--
-- @TLSParameters@ — A set of optional TLS parameters. Only valid if enableTLS is YES. If TLSParameters is nil, the default system parameters will be used for TLS negotiation.
--
-- @delegate@ — An object to use as the connection delegate. This object should conform to the NWTCPConnectionAuthenticationDelegate protocol.
--
-- Returns: An NWTCPConnection object.
--
-- ObjC selector: @- createTCPConnectionThroughTunnelToEndpoint:enableTLS:TLSParameters:delegate:@
createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegate :: (IsNEPacketTunnelProvider nePacketTunnelProvider, IsNWEndpoint remoteEndpoint, IsNWTLSParameters tlsParameters) => nePacketTunnelProvider -> remoteEndpoint -> Bool -> tlsParameters -> RawId -> IO (Id NWTCPConnection)
createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegate nePacketTunnelProvider remoteEndpoint enableTLS tlsParameters delegate =
  sendMessage nePacketTunnelProvider createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegateSelector (toNWEndpoint remoteEndpoint) enableTLS (toNWTLSParameters tlsParameters) delegate

-- | createUDPSessionThroughTunnelToEndpoint:fromEndpoint:
--
-- This function can be called by subclass implementations to create a UDP session between a local network endpoint and a remote network endpoint, through the tunnel established by the provider. This function should not be overridden by subclasses.
--
-- @remoteEndpoint@ — An NWEndpoint object that specifies the remote endpoint to which UDP datagrams will be sent by the UDP session.
--
-- @localEndpoint@ — An NWHostEndpoint object that specifies the local IP address endpoint to use as the source endpoint of the UDP session.
--
-- Returns: An NWUDPSession object.
--
-- ObjC selector: @- createUDPSessionThroughTunnelToEndpoint:fromEndpoint:@
createUDPSessionThroughTunnelToEndpoint_fromEndpoint :: (IsNEPacketTunnelProvider nePacketTunnelProvider, IsNWEndpoint remoteEndpoint, IsNWHostEndpoint localEndpoint) => nePacketTunnelProvider -> remoteEndpoint -> localEndpoint -> IO (Id NWUDPSession)
createUDPSessionThroughTunnelToEndpoint_fromEndpoint nePacketTunnelProvider remoteEndpoint localEndpoint =
  sendMessage nePacketTunnelProvider createUDPSessionThroughTunnelToEndpoint_fromEndpointSelector (toNWEndpoint remoteEndpoint) (toNWHostEndpoint localEndpoint)

-- | packetFlow
--
-- An NEPacketFlow object that the tunnel provider implementation should use to receive packets from the network stack and inject packets into the network stack. Every time the tunnel is started the packet flow object is in an initialized state and must be explicitly opened before any packets can be received or injected.
--
-- ObjC selector: @- packetFlow@
packetFlow :: IsNEPacketTunnelProvider nePacketTunnelProvider => nePacketTunnelProvider -> IO (Id NEPacketTunnelFlow)
packetFlow nePacketTunnelProvider =
  sendMessage nePacketTunnelProvider packetFlowSelector

-- | virtualInterface
--
-- The virtual network interface used to route packets to the packet tunnel provider.
--
-- For NEPacketTunnelProvider sub-classes, this property will be non-nil when @-[NEPacketTunnelProvider startTunnelWithOptions:completionHandler:]@ is called. For NEEthernetTunnelProvider sub-classes, this property will be non-nil when the completion handler passed to @-[NETunnelProvider setTunnelNetworkSettings:completionHandler:]@ is executed. To create a connection through the tunnel, pass this interface to @nw_parameters_require_interface@.
--
-- ObjC selector: @- virtualInterface@
virtualInterface :: IsNEPacketTunnelProvider nePacketTunnelProvider => nePacketTunnelProvider -> IO (Id NSObject)
virtualInterface nePacketTunnelProvider =
  sendMessage nePacketTunnelProvider virtualInterfaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTunnelWithOptions:completionHandler:@
startTunnelWithOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
startTunnelWithOptions_completionHandlerSelector = mkSelector "startTunnelWithOptions:completionHandler:"

-- | @Selector@ for @stopTunnelWithReason:completionHandler:@
stopTunnelWithReason_completionHandlerSelector :: Selector '[NEProviderStopReason, Ptr ()] ()
stopTunnelWithReason_completionHandlerSelector = mkSelector "stopTunnelWithReason:completionHandler:"

-- | @Selector@ for @cancelTunnelWithError:@
cancelTunnelWithErrorSelector :: Selector '[Id NSError] ()
cancelTunnelWithErrorSelector = mkSelector "cancelTunnelWithError:"

-- | @Selector@ for @createTCPConnectionThroughTunnelToEndpoint:enableTLS:TLSParameters:delegate:@
createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegateSelector :: Selector '[Id NWEndpoint, Bool, Id NWTLSParameters, RawId] (Id NWTCPConnection)
createTCPConnectionThroughTunnelToEndpoint_enableTLS_TLSParameters_delegateSelector = mkSelector "createTCPConnectionThroughTunnelToEndpoint:enableTLS:TLSParameters:delegate:"

-- | @Selector@ for @createUDPSessionThroughTunnelToEndpoint:fromEndpoint:@
createUDPSessionThroughTunnelToEndpoint_fromEndpointSelector :: Selector '[Id NWEndpoint, Id NWHostEndpoint] (Id NWUDPSession)
createUDPSessionThroughTunnelToEndpoint_fromEndpointSelector = mkSelector "createUDPSessionThroughTunnelToEndpoint:fromEndpoint:"

-- | @Selector@ for @packetFlow@
packetFlowSelector :: Selector '[] (Id NEPacketTunnelFlow)
packetFlowSelector = mkSelector "packetFlow"

-- | @Selector@ for @virtualInterface@
virtualInterfaceSelector :: Selector '[] (Id NSObject)
virtualInterfaceSelector = mkSelector "virtualInterface"

