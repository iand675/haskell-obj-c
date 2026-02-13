{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NWHostEndpoint
--
-- NWHostEndpoint is a subclass of NWEndpoint. It represents an endpoint backed by a		hostname and port. Note that a hostname string may be an IP or IPv6 address.
--
-- Generated bindings for @NWHostEndpoint@.
module ObjC.NetworkExtension.NWHostEndpoint
  ( NWHostEndpoint
  , IsNWHostEndpoint(..)
  , endpointWithHostname_port
  , hostname
  , port
  , endpointWithHostname_portSelector
  , hostnameSelector
  , portSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | endpointWithHostname:port:
--
-- @hostname@ — A string representation of the hostname or address, such as www.apple.com or 10.0.0.1.
--
-- @port@ — A string containing the port on the host, such as 80.
--
-- Returns: An initialized NWHostEndpoint object.
--
-- ObjC selector: @+ endpointWithHostname:port:@
endpointWithHostname_port :: (IsNSString hostname, IsNSString port) => hostname -> port -> IO (Id NWHostEndpoint)
endpointWithHostname_port hostname port =
  do
    cls' <- getRequiredClass "NWHostEndpoint"
    sendClassMessage cls' endpointWithHostname_portSelector (toNSString hostname) (toNSString port)

-- | hostname
--
-- The endpoint's hostname.
--
-- ObjC selector: @- hostname@
hostname :: IsNWHostEndpoint nwHostEndpoint => nwHostEndpoint -> IO (Id NSString)
hostname nwHostEndpoint =
  sendMessage nwHostEndpoint hostnameSelector

-- | port
--
-- The endpoint's port.
--
-- ObjC selector: @- port@
port :: IsNWHostEndpoint nwHostEndpoint => nwHostEndpoint -> IO (Id NSString)
port nwHostEndpoint =
  sendMessage nwHostEndpoint portSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointWithHostname:port:@
endpointWithHostname_portSelector :: Selector '[Id NSString, Id NSString] (Id NWHostEndpoint)
endpointWithHostname_portSelector = mkSelector "endpointWithHostname:port:"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector '[] (Id NSString)
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSString)
portSelector = mkSelector "port"

