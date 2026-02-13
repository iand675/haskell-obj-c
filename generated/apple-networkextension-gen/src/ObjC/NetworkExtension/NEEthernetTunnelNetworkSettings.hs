{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEEthernetTunnelNetworkSettings
--
-- The NEEthernetTunnelNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NEEthernetTunnelNetworkSettings is used by NEEthernetTunnelProviders to communicate the desired network settings for the packet tunnel to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEEthernetTunnelNetworkSettings@.
module ObjC.NetworkExtension.NEEthernetTunnelNetworkSettings
  ( NEEthernetTunnelNetworkSettings
  , IsNEEthernetTunnelNetworkSettings(..)
  , initWithTunnelRemoteAddress_ethernetAddress_mtu
  , ethernetAddress
  , ethernetAddressSelector
  , initWithTunnelRemoteAddress_ethernetAddress_mtuSelector


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
-- This function initializes a newly-allocated NEEthernetTunnelNetworkSettings object with a given tunnel remote address and MAC address.
--
-- @address@ — The address of the remote endpoint that is providing the tunnel service.
--
-- @ethernetAddress@ — The ethernet address to be assigned to the tunnel interface. This string should be in the format "xx:xx:xx:xx:xx:xx", where each xx is a hexidecimal number between 0 and ff.
--
-- @mtu@ — The MTU (Maxium Transmission Unit) in bytes to be assigned to the tunnel interface.
--
-- ObjC selector: @- initWithTunnelRemoteAddress:ethernetAddress:mtu:@
initWithTunnelRemoteAddress_ethernetAddress_mtu :: (IsNEEthernetTunnelNetworkSettings neEthernetTunnelNetworkSettings, IsNSString address, IsNSString ethernetAddress) => neEthernetTunnelNetworkSettings -> address -> ethernetAddress -> CLong -> IO (Id NEEthernetTunnelNetworkSettings)
initWithTunnelRemoteAddress_ethernetAddress_mtu neEthernetTunnelNetworkSettings address ethernetAddress mtu =
  sendOwnedMessage neEthernetTunnelNetworkSettings initWithTunnelRemoteAddress_ethernetAddress_mtuSelector (toNSString address) (toNSString ethernetAddress) mtu

-- | ethernetAddress
--
-- An NSString object containing the ethernet address of the tunnel interface.
--
-- ObjC selector: @- ethernetAddress@
ethernetAddress :: IsNEEthernetTunnelNetworkSettings neEthernetTunnelNetworkSettings => neEthernetTunnelNetworkSettings -> IO (Id NSString)
ethernetAddress neEthernetTunnelNetworkSettings =
  sendMessage neEthernetTunnelNetworkSettings ethernetAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTunnelRemoteAddress:ethernetAddress:mtu:@
initWithTunnelRemoteAddress_ethernetAddress_mtuSelector :: Selector '[Id NSString, Id NSString, CLong] (Id NEEthernetTunnelNetworkSettings)
initWithTunnelRemoteAddress_ethernetAddress_mtuSelector = mkSelector "initWithTunnelRemoteAddress:ethernetAddress:mtu:"

-- | @Selector@ for @ethernetAddress@
ethernetAddressSelector :: Selector '[] (Id NSString)
ethernetAddressSelector = mkSelector "ethernetAddress"

