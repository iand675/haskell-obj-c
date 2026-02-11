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
  , initWithTunnelRemoteAddressSelector
  , tunnelRemoteAddressSelector
  , dnsSettingsSelector
  , setDNSSettingsSelector
  , proxySettingsSelector
  , setProxySettingsSelector


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

-- | initWithTunnelRemoteAddress:
--
-- This function initializes a newly-allocated NETunnelNetworkSettings object with a given tunnel remote address.
--
-- @address@ — The address of the remote endpoint that is providing the tunnel service.
--
-- ObjC selector: @- initWithTunnelRemoteAddress:@
initWithTunnelRemoteAddress :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNSString address) => neTunnelNetworkSettings -> address -> IO (Id NETunnelNetworkSettings)
initWithTunnelRemoteAddress neTunnelNetworkSettings  address =
  withObjCPtr address $ \raw_address ->
      sendMsg neTunnelNetworkSettings (mkSelector "initWithTunnelRemoteAddress:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ())] >>= ownedObject . castPtr

-- | tunnelRemoteAddress
--
-- A string containing the IP address of the remote endpoint that is providing the tunnel service.
--
-- ObjC selector: @- tunnelRemoteAddress@
tunnelRemoteAddress :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NSString)
tunnelRemoteAddress neTunnelNetworkSettings  =
    sendMsg neTunnelNetworkSettings (mkSelector "tunnelRemoteAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DNSSettings
--
-- An NEDNSSettings object that contains the desired tunnel DNS settings.
--
-- ObjC selector: @- DNSSettings@
dnsSettings :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NEDNSSettings)
dnsSettings neTunnelNetworkSettings  =
    sendMsg neTunnelNetworkSettings (mkSelector "DNSSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DNSSettings
--
-- An NEDNSSettings object that contains the desired tunnel DNS settings.
--
-- ObjC selector: @- setDNSSettings:@
setDNSSettings :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNEDNSSettings value) => neTunnelNetworkSettings -> value -> IO ()
setDNSSettings neTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelNetworkSettings (mkSelector "setDNSSettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | proxySettings
--
-- An NEProxySettings object that contains the desired tunnel proxy settings.
--
-- ObjC selector: @- proxySettings@
proxySettings :: IsNETunnelNetworkSettings neTunnelNetworkSettings => neTunnelNetworkSettings -> IO (Id NEProxySettings)
proxySettings neTunnelNetworkSettings  =
    sendMsg neTunnelNetworkSettings (mkSelector "proxySettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | proxySettings
--
-- An NEProxySettings object that contains the desired tunnel proxy settings.
--
-- ObjC selector: @- setProxySettings:@
setProxySettings :: (IsNETunnelNetworkSettings neTunnelNetworkSettings, IsNEProxySettings value) => neTunnelNetworkSettings -> value -> IO ()
setProxySettings neTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelNetworkSettings (mkSelector "setProxySettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTunnelRemoteAddress:@
initWithTunnelRemoteAddressSelector :: Selector
initWithTunnelRemoteAddressSelector = mkSelector "initWithTunnelRemoteAddress:"

-- | @Selector@ for @tunnelRemoteAddress@
tunnelRemoteAddressSelector :: Selector
tunnelRemoteAddressSelector = mkSelector "tunnelRemoteAddress"

-- | @Selector@ for @DNSSettings@
dnsSettingsSelector :: Selector
dnsSettingsSelector = mkSelector "DNSSettings"

-- | @Selector@ for @setDNSSettings:@
setDNSSettingsSelector :: Selector
setDNSSettingsSelector = mkSelector "setDNSSettings:"

-- | @Selector@ for @proxySettings@
proxySettingsSelector :: Selector
proxySettingsSelector = mkSelector "proxySettings"

-- | @Selector@ for @setProxySettings:@
setProxySettingsSelector :: Selector
setProxySettingsSelector = mkSelector "setProxySettings:"

