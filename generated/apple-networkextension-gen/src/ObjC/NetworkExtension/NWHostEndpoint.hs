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
    withObjCPtr hostname $ \raw_hostname ->
      withObjCPtr port $ \raw_port ->
        sendClassMsg cls' (mkSelector "endpointWithHostname:port:") (retPtr retVoid) [argPtr (castPtr raw_hostname :: Ptr ()), argPtr (castPtr raw_port :: Ptr ())] >>= retainedObject . castPtr

-- | hostname
--
-- The endpoint's hostname.
--
-- ObjC selector: @- hostname@
hostname :: IsNWHostEndpoint nwHostEndpoint => nwHostEndpoint -> IO (Id NSString)
hostname nwHostEndpoint  =
    sendMsg nwHostEndpoint (mkSelector "hostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | port
--
-- The endpoint's port.
--
-- ObjC selector: @- port@
port :: IsNWHostEndpoint nwHostEndpoint => nwHostEndpoint -> IO (Id NSString)
port nwHostEndpoint  =
    sendMsg nwHostEndpoint (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointWithHostname:port:@
endpointWithHostname_portSelector :: Selector
endpointWithHostname_portSelector = mkSelector "endpointWithHostname:port:"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

