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
  , setIPv4SettingsSelector
  , iPv6SettingsSelector
  , setIPv6SettingsSelector
  , tunnelOverheadBytesSelector
  , setTunnelOverheadBytesSelector
  , mtuSelector
  , setMTUSelector


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

-- | IPv4Settings
--
-- An NEIPv4Settings object that contains the desired tunnel IPv4 settings.
--
-- ObjC selector: @- IPv4Settings@
iPv4Settings :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NEIPv4Settings)
iPv4Settings nePacketTunnelNetworkSettings  =
    sendMsg nePacketTunnelNetworkSettings (mkSelector "IPv4Settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | IPv4Settings
--
-- An NEIPv4Settings object that contains the desired tunnel IPv4 settings.
--
-- ObjC selector: @- setIPv4Settings:@
setIPv4Settings :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNEIPv4Settings value) => nePacketTunnelNetworkSettings -> value -> IO ()
setIPv4Settings nePacketTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePacketTunnelNetworkSettings (mkSelector "setIPv4Settings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | IPv6Settings
--
-- An NEIPv6Settings object that contains the desired tunnel IPv6 settings.
--
-- ObjC selector: @- IPv6Settings@
iPv6Settings :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NEIPv6Settings)
iPv6Settings nePacketTunnelNetworkSettings  =
    sendMsg nePacketTunnelNetworkSettings (mkSelector "IPv6Settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | IPv6Settings
--
-- An NEIPv6Settings object that contains the desired tunnel IPv6 settings.
--
-- ObjC selector: @- setIPv6Settings:@
setIPv6Settings :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNEIPv6Settings value) => nePacketTunnelNetworkSettings -> value -> IO ()
setIPv6Settings nePacketTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePacketTunnelNetworkSettings (mkSelector "setIPv6Settings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | tunnelOverheadBytes
--
-- An NSNumber object containing the number of bytes of overhead appended to each outbound packet through the tunnel. The MTU for the TUN interface is computed by subtracting this value from the MTU of the primary physical interface.
--
-- ObjC selector: @- tunnelOverheadBytes@
tunnelOverheadBytes :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NSNumber)
tunnelOverheadBytes nePacketTunnelNetworkSettings  =
    sendMsg nePacketTunnelNetworkSettings (mkSelector "tunnelOverheadBytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tunnelOverheadBytes
--
-- An NSNumber object containing the number of bytes of overhead appended to each outbound packet through the tunnel. The MTU for the TUN interface is computed by subtracting this value from the MTU of the primary physical interface.
--
-- ObjC selector: @- setTunnelOverheadBytes:@
setTunnelOverheadBytes :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNSNumber value) => nePacketTunnelNetworkSettings -> value -> IO ()
setTunnelOverheadBytes nePacketTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePacketTunnelNetworkSettings (mkSelector "setTunnelOverheadBytes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | MTU
--
-- An NSNumber object containing the Maximum Transmission Unit (MTU) size in bytes to assign to the TUN interface. If this property is set, the tunnelOverheadBytes property is ignored.
--
-- ObjC selector: @- MTU@
mtu :: IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings => nePacketTunnelNetworkSettings -> IO (Id NSNumber)
mtu nePacketTunnelNetworkSettings  =
    sendMsg nePacketTunnelNetworkSettings (mkSelector "MTU") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | MTU
--
-- An NSNumber object containing the Maximum Transmission Unit (MTU) size in bytes to assign to the TUN interface. If this property is set, the tunnelOverheadBytes property is ignored.
--
-- ObjC selector: @- setMTU:@
setMTU :: (IsNEPacketTunnelNetworkSettings nePacketTunnelNetworkSettings, IsNSNumber value) => nePacketTunnelNetworkSettings -> value -> IO ()
setMTU nePacketTunnelNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nePacketTunnelNetworkSettings (mkSelector "setMTU:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @IPv4Settings@
iPv4SettingsSelector :: Selector
iPv4SettingsSelector = mkSelector "IPv4Settings"

-- | @Selector@ for @setIPv4Settings:@
setIPv4SettingsSelector :: Selector
setIPv4SettingsSelector = mkSelector "setIPv4Settings:"

-- | @Selector@ for @IPv6Settings@
iPv6SettingsSelector :: Selector
iPv6SettingsSelector = mkSelector "IPv6Settings"

-- | @Selector@ for @setIPv6Settings:@
setIPv6SettingsSelector :: Selector
setIPv6SettingsSelector = mkSelector "setIPv6Settings:"

-- | @Selector@ for @tunnelOverheadBytes@
tunnelOverheadBytesSelector :: Selector
tunnelOverheadBytesSelector = mkSelector "tunnelOverheadBytes"

-- | @Selector@ for @setTunnelOverheadBytes:@
setTunnelOverheadBytesSelector :: Selector
setTunnelOverheadBytesSelector = mkSelector "setTunnelOverheadBytes:"

-- | @Selector@ for @MTU@
mtuSelector :: Selector
mtuSelector = mkSelector "MTU"

-- | @Selector@ for @setMTU:@
setMTUSelector :: Selector
setMTUSelector = mkSelector "setMTU:"

