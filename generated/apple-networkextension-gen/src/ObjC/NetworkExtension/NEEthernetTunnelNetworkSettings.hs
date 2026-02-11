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
  , initWithTunnelRemoteAddress_ethernetAddress_mtuSelector
  , ethernetAddressSelector


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
initWithTunnelRemoteAddress_ethernetAddress_mtu neEthernetTunnelNetworkSettings  address ethernetAddress mtu =
  withObjCPtr address $ \raw_address ->
    withObjCPtr ethernetAddress $ \raw_ethernetAddress ->
        sendMsg neEthernetTunnelNetworkSettings (mkSelector "initWithTunnelRemoteAddress:ethernetAddress:mtu:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ()), argPtr (castPtr raw_ethernetAddress :: Ptr ()), argCLong mtu] >>= ownedObject . castPtr

-- | ethernetAddress
--
-- An NSString object containing the ethernet address of the tunnel interface.
--
-- ObjC selector: @- ethernetAddress@
ethernetAddress :: IsNEEthernetTunnelNetworkSettings neEthernetTunnelNetworkSettings => neEthernetTunnelNetworkSettings -> IO (Id NSString)
ethernetAddress neEthernetTunnelNetworkSettings  =
    sendMsg neEthernetTunnelNetworkSettings (mkSelector "ethernetAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTunnelRemoteAddress:ethernetAddress:mtu:@
initWithTunnelRemoteAddress_ethernetAddress_mtuSelector :: Selector
initWithTunnelRemoteAddress_ethernetAddress_mtuSelector = mkSelector "initWithTunnelRemoteAddress:ethernetAddress:mtu:"

-- | @Selector@ for @ethernetAddress@
ethernetAddressSelector :: Selector
ethernetAddressSelector = mkSelector "ethernetAddress"

