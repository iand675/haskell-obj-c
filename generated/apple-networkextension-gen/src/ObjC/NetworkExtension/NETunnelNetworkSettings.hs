{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelNetworkSettings
--
-- The NETunnelNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NETunnelNetworkSettings is used by NETunnelProviders to communicate the desired network settings for the tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETunnelNetworkSettings@.
module ObjC.NetworkExtension.NETunnelNetworkSettings
  ( NETunnelNetworkSettings
  , IsNETunnelNetworkSettings(..)
  , initWithTunnelRemoteAddress
  , tunnelRemoteAddress
  , dnsSettings
  , setDNSSettings
  , proxySettings
  , setProxySettings
  , dnsSettingsSelector
  , initWithTunnelRemoteAddressSelector
  , proxySettingsSelector
  , setDNSSettingsSelector
  , setProxySettingsSelector
  , tunnelRemoteAddressSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithTunnelRemoteAddress:
--
-- This function initializes a newly-allocated NETunnelNetworkSettings object with a given tunnel remote address.
--
-- @address@ — The address of the remote endpoint that is providing the tunnel service.
--
-- ObjC selector: @- initWithTunnelRemoteAddress:@
initWithTunnelRemoteAddress :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNSString address) => neTunnelNetworkSettings -> address -> IO (Id NETunnelNetworkSettings)
initWithTunnelRemoteAddress neTunnelNetworkSettings address =
  sendOwnedMessage neTunnelNetworkSettings initWithTunnelRemoteAddressSelector (toNSString address)

-- | tunnelRemoteAddress
--
-- A string containing the IP address of the remote endpoint that is providing the tunnel service.
--
-- ObjC selector: @- tunnelRemoteAddress@
tunnelRemoteAddress :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NSString)
tunnelRemoteAddress neTunnelNetworkSettings =
  sendMessage neTunnelNetworkSettings tunnelRemoteAddressSelector

-- | DNSSettings
--
-- An NEDNSSettings object that contains the desired tunnel DNS settings.
--
-- ObjC selector: @- DNSSettings@
dnsSettings :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NEDNSSettings)
dnsSettings neTunnelNetworkSettings =
  sendMessage neTunnelNetworkSettings dnsSettingsSelector

-- | DNSSettings
--
-- An NEDNSSettings object that contains the desired tunnel DNS settings.
--
-- ObjC selector: @- setDNSSettings:@
setDNSSettings :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNEDNSSettings value) => neTunnelNetworkSettings -> value -> IO ()
setDNSSettings neTunnelNetworkSettings value =
  sendMessage neTunnelNetworkSettings setDNSSettingsSelector (toNEDNSSettings value)

-- | proxySettings
--
-- An NEProxySettings object that contains the desired tunnel proxy settings.
--
-- ObjC selector: @- proxySettings@
proxySettings :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NEProxySettings)
proxySettings neTunnelNetworkSettings =
  sendMessage neTunnelNetworkSettings proxySettingsSelector

-- | proxySettings
--
-- An NEProxySettings object that contains the desired tunnel proxy settings.
--
-- ObjC selector: @- setProxySettings:@
setProxySettings :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNEProxySettings value) => neTunnelNetworkSettings -> value -> IO ()
setProxySettings neTunnelNetworkSettings value =
  sendMessage neTunnelNetworkSettings setProxySettingsSelector (toNEProxySettings value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTunnelRemoteAddress:@
initWithTunnelRemoteAddressSelector :: Selector '[Id NSString] (Id NETunnelNetworkSettings)
initWithTunnelRemoteAddressSelector = mkSelector "initWithTunnelRemoteAddress:"

-- | @Selector@ for @tunnelRemoteAddress@
tunnelRemoteAddressSelector :: Selector '[] (Id NSString)
tunnelRemoteAddressSelector = mkSelector "tunnelRemoteAddress"

-- | @Selector@ for @DNSSettings@
dnsSettingsSelector :: Selector '[] (Id NEDNSSettings)
dnsSettingsSelector = mkSelector "DNSSettings"

-- | @Selector@ for @setDNSSettings:@
setDNSSettingsSelector :: Selector '[Id NEDNSSettings] ()
setDNSSettingsSelector = mkSelector "setDNSSettings:"

-- | @Selector@ for @proxySettings@
proxySettingsSelector :: Selector '[] (Id NEProxySettings)
proxySettingsSelector = mkSelector "proxySettings"

-- | @Selector@ for @setProxySettings:@
setProxySettingsSelector :: Selector '[Id NEProxySettings] ()
setProxySettingsSelector = mkSelector "setProxySettings:"

