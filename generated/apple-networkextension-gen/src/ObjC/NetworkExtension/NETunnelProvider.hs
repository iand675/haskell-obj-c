{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProvider
--
-- The NETunnelProvider class declares the programmatic interface for an object that provides a network tunnel service.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETunnelProvider@.
module ObjC.NetworkExtension.NETunnelProvider
  ( NETunnelProvider
  , IsNETunnelProvider(..)
  , handleAppMessage_completionHandler
  , setTunnelNetworkSettings_completionHandler
  , protocolConfiguration
  , appRules
  , routingMethod
  , reasserting
  , setReasserting
  , appRulesSelector
  , handleAppMessage_completionHandlerSelector
  , protocolConfigurationSelector
  , reassertingSelector
  , routingMethodSelector
  , setReassertingSelector
  , setTunnelNetworkSettings_completionHandlerSelector

  -- * Enum types
  , NETunnelProviderRoutingMethod(NETunnelProviderRoutingMethod)
  , pattern NETunnelProviderRoutingMethodDestinationIP
  , pattern NETunnelProviderRoutingMethodSourceApplication
  , pattern NETunnelProviderRoutingMethodNetworkRule

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

-- | handleAppMessage:completionHandler:
--
-- This function is called by the framework when the container app sends a message to the provider. Subclasses should override this method to handle the message and optionally send a response.
--
-- @messageData@ — An NSData object containing the message sent by the container app.
--
-- @completionHandler@ — A block that the method can execute to send a response to the container app. If this parameter is non-nil then the method implementation should always execute the block. If this parameter is nil then the method implementation should treat this as an indication that the container app is not expecting a response.
--
-- ObjC selector: @- handleAppMessage:completionHandler:@
handleAppMessage_completionHandler :: (IsNETunnelProvider neTunnelProvider, IsNSData messageData) => neTunnelProvider -> messageData -> Ptr () -> IO ()
handleAppMessage_completionHandler neTunnelProvider messageData completionHandler =
  sendMessage neTunnelProvider handleAppMessage_completionHandlerSelector (toNSData messageData) completionHandler

-- | setTunnelNetworkSettings:completionHandler:
--
-- This function is called by tunnel provider implementations to set the network settings of the tunnel, including IP routes, DNS servers, and virtual interface addresses depending on the tunnel type. Subclasses should not override this method. This method can be called multiple times during the lifetime of a particular tunnel. It is not necessary to call this function with nil to clear out the existing settings before calling this function with a non-nil configuration.
--
-- @tunnelNetworkSettings@ — An NETunnelNetworkSettings object containing all of the desired network settings for the tunnel. Pass nil to clear out the current network settings.
--
-- @completionHandler@ — A block that will be called by the framework when the process of setting or clearing the network settings is complete. If an error occurred during the process of setting or clearing the IP network settings then a non-nill NSError object will be passed to this block containing error details.
--
-- ObjC selector: @- setTunnelNetworkSettings:completionHandler:@
setTunnelNetworkSettings_completionHandler :: (IsNETunnelProvider neTunnelProvider, IsNETunnelNetworkSettings tunnelNetworkSettings) => neTunnelProvider -> tunnelNetworkSettings -> Ptr () -> IO ()
setTunnelNetworkSettings_completionHandler neTunnelProvider tunnelNetworkSettings completionHandler =
  sendMessage neTunnelProvider setTunnelNetworkSettings_completionHandlerSelector (toNETunnelNetworkSettings tunnelNetworkSettings) completionHandler

-- | protocolConfiguration
--
-- An NEVPNProtocol object containing the provider's current configuration. The value of this property may change during the lifetime of the tunnel provided by this NETunnelProvider, KVO can be used to detect when changes occur.  For different protocol types, this property will contain the corresponding subclass.   For NEVPNProtocolTypePlugin protocol type, this property will contain the NETunnelProviderProtocol subclass.  For NEVPNProtocolTypeIKEv2 protocol type, this property will contain the NEVPNProtocolIKEv2 subclass.
--
-- ObjC selector: @- protocolConfiguration@
protocolConfiguration :: IsNETunnelProvider neTunnelProvider => neTunnelProvider -> IO (Id NEVPNProtocol)
protocolConfiguration neTunnelProvider =
  sendMessage neTunnelProvider protocolConfigurationSelector

-- | appRules
--
-- An array of NEAppRule objects specifying which applications are currently being routed through the tunnel provided by this NETunnelProvider. If application-based routing is not enabled for the tunnel, then this property is set to nil.
--
-- ObjC selector: @- appRules@
appRules :: IsNETunnelProvider neTunnelProvider => neTunnelProvider -> IO (Id NSArray)
appRules neTunnelProvider =
  sendMessage neTunnelProvider appRulesSelector

-- | routingMethod
--
-- The method by which network traffic is routed to the tunnel. The default is NETunnelProviderRoutingMethodDestinationIP.
--
-- ObjC selector: @- routingMethod@
routingMethod :: IsNETunnelProvider neTunnelProvider => neTunnelProvider -> IO NETunnelProviderRoutingMethod
routingMethod neTunnelProvider =
  sendMessage neTunnelProvider routingMethodSelector

-- | reasserting
--
-- A flag that indicates to the framework if this NETunnelProvider is currently re-establishing the tunnel. Setting this flag will cause the session status visible to the user to change to "Reasserting". Clearing this flag will change the user-visible status of the session back to "Connected". Setting and clearing this flag only has an effect if the session is in the "Connected" state.
--
-- ObjC selector: @- reasserting@
reasserting :: IsNETunnelProvider neTunnelProvider => neTunnelProvider -> IO Bool
reasserting neTunnelProvider =
  sendMessage neTunnelProvider reassertingSelector

-- | reasserting
--
-- A flag that indicates to the framework if this NETunnelProvider is currently re-establishing the tunnel. Setting this flag will cause the session status visible to the user to change to "Reasserting". Clearing this flag will change the user-visible status of the session back to "Connected". Setting and clearing this flag only has an effect if the session is in the "Connected" state.
--
-- ObjC selector: @- setReasserting:@
setReasserting :: IsNETunnelProvider neTunnelProvider => neTunnelProvider -> Bool -> IO ()
setReasserting neTunnelProvider value =
  sendMessage neTunnelProvider setReassertingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @handleAppMessage:completionHandler:@
handleAppMessage_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
handleAppMessage_completionHandlerSelector = mkSelector "handleAppMessage:completionHandler:"

-- | @Selector@ for @setTunnelNetworkSettings:completionHandler:@
setTunnelNetworkSettings_completionHandlerSelector :: Selector '[Id NETunnelNetworkSettings, Ptr ()] ()
setTunnelNetworkSettings_completionHandlerSelector = mkSelector "setTunnelNetworkSettings:completionHandler:"

-- | @Selector@ for @protocolConfiguration@
protocolConfigurationSelector :: Selector '[] (Id NEVPNProtocol)
protocolConfigurationSelector = mkSelector "protocolConfiguration"

-- | @Selector@ for @appRules@
appRulesSelector :: Selector '[] (Id NSArray)
appRulesSelector = mkSelector "appRules"

-- | @Selector@ for @routingMethod@
routingMethodSelector :: Selector '[] NETunnelProviderRoutingMethod
routingMethodSelector = mkSelector "routingMethod"

-- | @Selector@ for @reasserting@
reassertingSelector :: Selector '[] Bool
reassertingSelector = mkSelector "reasserting"

-- | @Selector@ for @setReasserting:@
setReassertingSelector :: Selector '[Bool] ()
setReassertingSelector = mkSelector "setReasserting:"

