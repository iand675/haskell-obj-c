{-# LANGUAGE DataKinds #-}
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
  , address
  , port
  , authenticationRequired
  , setAuthenticationRequired
  , username
  , setUsername
  , password
  , setPassword
  , addressSelector
  , authenticationRequiredSelector
  , initWithAddress_portSelector
  , passwordSelector
  , portSelector
  , setAuthenticationRequiredSelector
  , setPasswordSelector
  , setUsernameSelector
  , usernameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithAddress_port neProxyServer address port =
  sendOwnedMessage neProxyServer initWithAddress_portSelector (toNSString address) port

-- | address
--
-- The string representation of the proxy server IP address.
--
-- ObjC selector: @- address@
address :: IsNEProxyServer neProxyServer => neProxyServer -> IO (Id NSString)
address neProxyServer =
  sendMessage neProxyServer addressSelector

-- | port
--
-- The TCP port of the proxy server.
--
-- ObjC selector: @- port@
port :: IsNEProxyServer neProxyServer => neProxyServer -> IO CLong
port neProxyServer =
  sendMessage neProxyServer portSelector

-- | authenticationRequired
--
-- A flag indicating if the server requires authentication credentials.
--
-- ObjC selector: @- authenticationRequired@
authenticationRequired :: IsNEProxyServer neProxyServer => neProxyServer -> IO Bool
authenticationRequired neProxyServer =
  sendMessage neProxyServer authenticationRequiredSelector

-- | authenticationRequired
--
-- A flag indicating if the server requires authentication credentials.
--
-- ObjC selector: @- setAuthenticationRequired:@
setAuthenticationRequired :: IsNEProxyServer neProxyServer => neProxyServer -> Bool -> IO ()
setAuthenticationRequired neProxyServer value =
  sendMessage neProxyServer setAuthenticationRequiredSelector value

-- | username
--
-- The username portion of the authentication credential to use when communicating with the proxy server.
--
-- ObjC selector: @- username@
username :: IsNEProxyServer neProxyServer => neProxyServer -> IO (Id NSString)
username neProxyServer =
  sendMessage neProxyServer usernameSelector

-- | username
--
-- The username portion of the authentication credential to use when communicating with the proxy server.
--
-- ObjC selector: @- setUsername:@
setUsername :: (IsNEProxyServer neProxyServer, IsNSString value) => neProxyServer -> value -> IO ()
setUsername neProxyServer value =
  sendMessage neProxyServer setUsernameSelector (toNSString value)

-- | password
--
-- The password portion of the authentication credential to use when communicating with the proxy server. This property is only saved persistently if the username property is non-nil and non-empty and if the authenticationRequired flag is set.
--
-- ObjC selector: @- password@
password :: IsNEProxyServer neProxyServer => neProxyServer -> IO (Id NSString)
password neProxyServer =
  sendMessage neProxyServer passwordSelector

-- | password
--
-- The password portion of the authentication credential to use when communicating with the proxy server. This property is only saved persistently if the username property is non-nil and non-empty and if the authenticationRequired flag is set.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsNEProxyServer neProxyServer, IsNSString value) => neProxyServer -> value -> IO ()
setPassword neProxyServer value =
  sendMessage neProxyServer setPasswordSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddress:port:@
initWithAddress_portSelector :: Selector '[Id NSString, CLong] (Id NEProxyServer)
initWithAddress_portSelector = mkSelector "initWithAddress:port:"

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id NSString)
addressSelector = mkSelector "address"

-- | @Selector@ for @port@
portSelector :: Selector '[] CLong
portSelector = mkSelector "port"

-- | @Selector@ for @authenticationRequired@
authenticationRequiredSelector :: Selector '[] Bool
authenticationRequiredSelector = mkSelector "authenticationRequired"

-- | @Selector@ for @setAuthenticationRequired:@
setAuthenticationRequiredSelector :: Selector '[Bool] ()
setAuthenticationRequiredSelector = mkSelector "setAuthenticationRequired:"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector '[Id NSString] ()
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

