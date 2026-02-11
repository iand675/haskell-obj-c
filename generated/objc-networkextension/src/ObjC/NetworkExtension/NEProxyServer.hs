{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEProxyServer
--
-- The NEProxyServer class declares the programmatic interface for an object that contains settings for a proxy server.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEProxyServer@.
module ObjC.NetworkExtension.NEProxyServer
  ( NEProxyServer
  , IsNEProxyServer(..)
  , initWithAddress_port
  , port
  , authenticationRequired
  , setAuthenticationRequired
  , initWithAddress_portSelector
  , portSelector
  , authenticationRequiredSelector
  , setAuthenticationRequiredSelector


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

-- | initWithAddress:port:
--
-- This function initializes a newly-allocated NEProxyServer object
--
-- @address@ — The string representation of the proxy server IP address.
--
-- @port@ — The TCP port of the proxy server.
--
-- ObjC selector: @- initWithAddress:port:@
initWithAddress_port :: (IsNEProxyServer neProxyServer, IsNSString address) => neProxyServer -> address -> CLong -> IO (Id NEProxyServer)
initWithAddress_port neProxyServer  address port =
withObjCPtr address $ \raw_address ->
    sendMsg neProxyServer (mkSelector "initWithAddress:port:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ()), argCLong (fromIntegral port)] >>= ownedObject . castPtr

-- | port
--
-- The TCP port of the proxy server.
--
-- ObjC selector: @- port@
port :: IsNEProxyServer neProxyServer => neProxyServer -> IO CLong
port neProxyServer  =
  sendMsg neProxyServer (mkSelector "port") retCLong []

-- | authenticationRequired
--
-- A flag indicating if the server requires authentication credentials.
--
-- ObjC selector: @- authenticationRequired@
authenticationRequired :: IsNEProxyServer neProxyServer => neProxyServer -> IO Bool
authenticationRequired neProxyServer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxyServer (mkSelector "authenticationRequired") retCULong []

-- | authenticationRequired
--
-- A flag indicating if the server requires authentication credentials.
--
-- ObjC selector: @- setAuthenticationRequired:@
setAuthenticationRequired :: IsNEProxyServer neProxyServer => neProxyServer -> Bool -> IO ()
setAuthenticationRequired neProxyServer  value =
  sendMsg neProxyServer (mkSelector "setAuthenticationRequired:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddress:port:@
initWithAddress_portSelector :: Selector
initWithAddress_portSelector = mkSelector "initWithAddress:port:"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @authenticationRequired@
authenticationRequiredSelector :: Selector
authenticationRequiredSelector = mkSelector "authenticationRequired"

-- | @Selector@ for @setAuthenticationRequired:@
setAuthenticationRequiredSelector :: Selector
setAuthenticationRequiredSelector = mkSelector "setAuthenticationRequired:"

