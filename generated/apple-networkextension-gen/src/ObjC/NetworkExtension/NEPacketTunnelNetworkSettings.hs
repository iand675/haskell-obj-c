{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEPacketTunnelNetworkSettings
--
-- The NEPacketTunnelNetworkSettings class declares the programmatic interface for an object that contains IP network settings.
--
-- NEPacketTunnelNetworkSettings is used by NEPacketTunnelProviders to communicate the desired IP network settings for the packet tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEPacketTunnelNetworkSettings@.
module ObjC.NetworkExtension.NEPacketTunnelNetworkSettings
  ( NEPacketTunnelNetworkSettings
  , IsNEPacketTunnelNetworkSettings(..)
  , iPv4Settings
  , setIPv4Settings
  , iPv6Settings
  , setIPv6Settings
  , tunnelOverheadBytes
  , setTunnelOverheadBytes
  , mtu
  , setMTU
  , iPv4SettingsSelector
  , iPv6SettingsSelector
  , mtuSelector
  , setIPv4SettingsSelector
  , setIPv6SettingsSelector
  , setMTUSelector
  , setTunnelOverheadBytesSelector
  , tunnelOverheadBytesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | IPv4Settings
--
-- An NEIPv4Settings object that contains the desired tunnel IPv4 settings.
--
-- ObjC selector: @- IPv4Settings@
iPv4Settings :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NEIPv4Settings)
iPv4Settings nePacketTunnelNetworkSettings =
  sendMessage nePacketTunnelNetworkSettings iPv4SettingsSelector

-- | IPv4Settings
--
-- An NEIPv4Settings object that contains the desired tunnel IPv4 settings.
--
-- ObjC selector: @- setIPv4Settings:@
setIPv4Settings :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNEIPv4Settings value) => nePacketTunnelNetworkSettings -> value -> IO ()
setIPv4Settings nePacketTunnelNetworkSettings value =
  sendMessage nePacketTunnelNetworkSettings setIPv4SettingsSelector (toNEIPv4Settings value)

-- | IPv6Settings
--
-- An NEIPv6Settings object that contains the desired tunnel IPv6 settings.
--
-- ObjC selector: @- IPv6Settings@
iPv6Settings :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NEIPv6Settings)
iPv6Settings nePacketTunnelNetworkSettings =
  sendMessage nePacketTunnelNetworkSettings iPv6SettingsSelector

-- | IPv6Settings
--
-- An NEIPv6Settings object that contains the desired tunnel IPv6 settings.
--
-- ObjC selector: @- setIPv6Settings:@
setIPv6Settings :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNEIPv6Settings value) => nePacketTunnelNetworkSettings -> value -> IO ()
setIPv6Settings nePacketTunnelNetworkSettings value =
  sendMessage nePacketTunnelNetworkSettings setIPv6SettingsSelector (toNEIPv6Settings value)

-- | tunnelOverheadBytes
--
-- An NSNumber object containing the number of bytes of overhead appended to each outbound packet through the tunnel. The MTU for the TUN interface is computed by subtracting this value from the MTU of the primary physical interface.
--
-- ObjC selector: @- tunnelOverheadBytes@
tunnelOverheadBytes :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NSNumber)
tunnelOverheadBytes nePacketTunnelNetworkSettings =
  sendMessage nePacketTunnelNetworkSettings tunnelOverheadBytesSelector

-- | tunnelOverheadBytes
--
-- An NSNumber object containing the number of bytes of overhead appended to each outbound packet through the tunnel. The MTU for the TUN interface is computed by subtracting this value from the MTU of the primary physical interface.
--
-- ObjC selector: @- setTunnelOverheadBytes:@
setTunnelOverheadBytes :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNSNumber value) => nePacketTunnelNetworkSettings -> value -> IO ()
setTunnelOverheadBytes nePacketTunnelNetworkSettings value =
  sendMessage nePacketTunnelNetworkSettings setTunnelOverheadBytesSelector (toNSNumber value)

-- | MTU
--
-- An NSNumber object containing the Maximum Transmission Unit (MTU) size in bytes to assign to the TUN interface. If this property is set, the tunnelOverheadBytes property is ignored.
--
-- ObjC selector: @- MTU@
mtu :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NSNumber)
mtu nePacketTunnelNetworkSettings =
  sendMessage nePacketTunnelNetworkSettings mtuSelector

-- | MTU
--
-- An NSNumber object containing the Maximum Transmission Unit (MTU) size in bytes to assign to the TUN interface. If this property is set, the tunnelOverheadBytes property is ignored.
--
-- ObjC selector: @- setMTU:@
setMTU :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNSNumber value) => nePacketTunnelNetworkSettings -> value -> IO ()
setMTU nePacketTunnelNetworkSettings value =
  sendMessage nePacketTunnelNetworkSettings setMTUSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @IPv4Settings@
iPv4SettingsSelector :: Selector '[] (Id NEIPv4Settings)
iPv4SettingsSelector = mkSelector "IPv4Settings"

-- | @Selector@ for @setIPv4Settings:@
setIPv4SettingsSelector :: Selector '[Id NEIPv4Settings] ()
setIPv4SettingsSelector = mkSelector "setIPv4Settings:"

-- | @Selector@ for @IPv6Settings@
iPv6SettingsSelector :: Selector '[] (Id NEIPv6Settings)
iPv6SettingsSelector = mkSelector "IPv6Settings"

-- | @Selector@ for @setIPv6Settings:@
setIPv6SettingsSelector :: Selector '[Id NEIPv6Settings] ()
setIPv6SettingsSelector = mkSelector "setIPv6Settings:"

-- | @Selector@ for @tunnelOverheadBytes@
tunnelOverheadBytesSelector :: Selector '[] (Id NSNumber)
tunnelOverheadBytesSelector = mkSelector "tunnelOverheadBytes"

-- | @Selector@ for @setTunnelOverheadBytes:@
setTunnelOverheadBytesSelector :: Selector '[Id NSNumber] ()
setTunnelOverheadBytesSelector = mkSelector "setTunnelOverheadBytes:"

-- | @Selector@ for @MTU@
mtuSelector :: Selector '[] (Id NSNumber)
mtuSelector = mkSelector "MTU"

-- | @Selector@ for @setMTU:@
setMTUSelector :: Selector '[Id NSNumber] ()
setMTUSelector = mkSelector "setMTU:"

