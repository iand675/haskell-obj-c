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
  , initWithTunnelRemoteAddressSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTunnelRemoteAddress:@
initWithTunnelRemoteAddressSelector :: Selector
initWithTunnelRemoteAddressSelector = mkSelector "initWithTunnelRemoteAddress:"

