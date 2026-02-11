{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLProtectionSpace
--
-- This class represents a protection space requiring authentication.
--
-- Generated bindings for @NSURLProtectionSpace@.
module ObjC.Foundation.NSURLProtectionSpace
  ( NSURLProtectionSpace
  , IsNSURLProtectionSpace(..)
  , initWithHost_port_protocol_realm_authenticationMethod
  , initWithProxyHost_port_type_realm_authenticationMethod
  , realm
  , receivesCredentialSecurely
  , isProxy
  , host
  , port
  , proxyType
  , protocol
  , authenticationMethod
  , serverTrust
  , distinguishedNames
  , initWithHost_port_protocol_realm_authenticationMethodSelector
  , initWithProxyHost_port_type_realm_authenticationMethodSelector
  , realmSelector
  , receivesCredentialSecurelySelector
  , isProxySelector
  , hostSelector
  , portSelector
  , proxyTypeSelector
  , protocolSelector
  , authenticationMethodSelector
  , serverTrustSelector
  , distinguishedNamesSelector


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

import ObjC.Foundation.Internal.Classes

-- | initWithHost:port:protocol:realm:authenticationMethod:
--
-- Initialize a protection space representing an origin server, or a realm on one
--
-- @host@ — The hostname of the server
--
-- @port@ — The port for the server
--
-- @protocol@ — The protocol for this server - e.g. "http", "ftp", "https"
--
-- @realm@ — A string indicating a protocol-specific subdivision    of a single host. For http and https, this maps to the realm    string in http authentication challenges. For many other protocols    it is unused.
--
-- @authenticationMethod@ — The authentication method to use to access this protection space -    valid values include nil (default method), "digest" and \@"form".
--
-- Returns: The initialized object.
--
-- ObjC selector: @- initWithHost:port:protocol:realm:authenticationMethod:@
initWithHost_port_protocol_realm_authenticationMethod :: (IsNSURLProtectionSpace nsurlProtectionSpace, IsNSString host, IsNSString protocol, IsNSString realm, IsNSString authenticationMethod) => nsurlProtectionSpace -> host -> CLong -> protocol -> realm -> authenticationMethod -> IO (Id NSURLProtectionSpace)
initWithHost_port_protocol_realm_authenticationMethod nsurlProtectionSpace  host port protocol realm authenticationMethod =
withObjCPtr host $ \raw_host ->
  withObjCPtr protocol $ \raw_protocol ->
    withObjCPtr realm $ \raw_realm ->
      withObjCPtr authenticationMethod $ \raw_authenticationMethod ->
          sendMsg nsurlProtectionSpace (mkSelector "initWithHost:port:protocol:realm:authenticationMethod:") (retPtr retVoid) [argPtr (castPtr raw_host :: Ptr ()), argCLong (fromIntegral port), argPtr (castPtr raw_protocol :: Ptr ()), argPtr (castPtr raw_realm :: Ptr ()), argPtr (castPtr raw_authenticationMethod :: Ptr ())] >>= ownedObject . castPtr

-- | initWithProxyHost:port:type:realm:authenticationMethod:
--
-- Initialize a protection space representing a proxy server, or a realm on one
--
-- @host@ — The hostname of the proxy server
--
-- @port@ — The port for the proxy server
--
-- @type@ — The type of proxy - e.g. "http", "ftp", "SOCKS"
--
-- @realm@ — A string indicating a protocol-specific subdivision    of a single host. For http and https, this maps to the realm    string in http authentication challenges. For many other protocols    it is unused.
--
-- @authenticationMethod@ — The authentication method to use to access this protection space -    valid values include nil (default method) and "digest"
--
-- Returns: The initialized object.
--
-- ObjC selector: @- initWithProxyHost:port:type:realm:authenticationMethod:@
initWithProxyHost_port_type_realm_authenticationMethod :: (IsNSURLProtectionSpace nsurlProtectionSpace, IsNSString host, IsNSString type_, IsNSString realm, IsNSString authenticationMethod) => nsurlProtectionSpace -> host -> CLong -> type_ -> realm -> authenticationMethod -> IO (Id NSURLProtectionSpace)
initWithProxyHost_port_type_realm_authenticationMethod nsurlProtectionSpace  host port type_ realm authenticationMethod =
withObjCPtr host $ \raw_host ->
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr realm $ \raw_realm ->
      withObjCPtr authenticationMethod $ \raw_authenticationMethod ->
          sendMsg nsurlProtectionSpace (mkSelector "initWithProxyHost:port:type:realm:authenticationMethod:") (retPtr retVoid) [argPtr (castPtr raw_host :: Ptr ()), argCLong (fromIntegral port), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_realm :: Ptr ()), argPtr (castPtr raw_authenticationMethod :: Ptr ())] >>= ownedObject . castPtr

-- | Get the authentication realm for which the protection space that    needs authentication
--
-- This is generally only available for http    authentication, and may be nil otherwise.
--
-- Returns: The realm string
--
-- ObjC selector: @- realm@
realm :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSString)
realm nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "realm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Determine if the password for this protection space can be sent securely
--
-- Returns: YES if a secure authentication method or protocol will be used, NO otherwise
--
-- ObjC selector: @- receivesCredentialSecurely@
receivesCredentialSecurely :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO Bool
receivesCredentialSecurely nsurlProtectionSpace  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlProtectionSpace (mkSelector "receivesCredentialSecurely") retCULong []

-- | Determine if this authenticating protection space is a proxy server
--
-- Returns: YES if a proxy, NO otherwise
--
-- ObjC selector: @- isProxy@
isProxy :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO Bool
isProxy nsurlProtectionSpace  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlProtectionSpace (mkSelector "isProxy") retCULong []

-- | Get the proxy host if this is a proxy authentication, or the host from the URL.
--
-- Returns: The host for this protection space.
--
-- ObjC selector: @- host@
host :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSString)
host nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the proxy port if this is a proxy authentication, or the port from the URL.
--
-- Returns: The port for this protection space, or 0 if not set.
--
-- ObjC selector: @- port@
port :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO CLong
port nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "port") retCLong []

-- | Get the type of this protection space, if a proxy
--
-- Returns: The type string, or nil if not a proxy.
--
-- ObjC selector: @- proxyType@
proxyType :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSString)
proxyType nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "proxyType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the protocol of this protection space, if not a proxy
--
-- Returns: The type string, or nil if a proxy.
--
-- ObjC selector: @- protocol@
protocol :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSString)
protocol nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the authentication method to be used for this protection space
--
-- Returns: The authentication method
--
-- ObjC selector: @- authenticationMethod@
authenticationMethod :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSString)
authenticationMethod nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "authenticationMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a SecTrustRef which represents the state of the servers SSL transaction state
--
-- Returns: A SecTrustRef from Security.framework.  (Nil if the authenticationMethod is not NSURLAuthenticationMethodServerTrust)
--
-- ObjC selector: @- serverTrust@
serverTrust :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Ptr ())
serverTrust nsurlProtectionSpace  =
  fmap castPtr $ sendMsg nsurlProtectionSpace (mkSelector "serverTrust") (retPtr retVoid) []

-- | Returns an array of acceptable certificate issuing authorities for client certification authentication. Issuers are identified by their distinguished name and returned as a DER encoded data.
--
-- Returns: An array of NSData objects.  (Nil if the authenticationMethod is not NSURLAuthenticationMethodClientCertificate)
--
-- ObjC selector: @- distinguishedNames@
distinguishedNames :: IsNSURLProtectionSpace nsurlProtectionSpace => nsurlProtectionSpace -> IO (Id NSArray)
distinguishedNames nsurlProtectionSpace  =
  sendMsg nsurlProtectionSpace (mkSelector "distinguishedNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHost:port:protocol:realm:authenticationMethod:@
initWithHost_port_protocol_realm_authenticationMethodSelector :: Selector
initWithHost_port_protocol_realm_authenticationMethodSelector = mkSelector "initWithHost:port:protocol:realm:authenticationMethod:"

-- | @Selector@ for @initWithProxyHost:port:type:realm:authenticationMethod:@
initWithProxyHost_port_type_realm_authenticationMethodSelector :: Selector
initWithProxyHost_port_type_realm_authenticationMethodSelector = mkSelector "initWithProxyHost:port:type:realm:authenticationMethod:"

-- | @Selector@ for @realm@
realmSelector :: Selector
realmSelector = mkSelector "realm"

-- | @Selector@ for @receivesCredentialSecurely@
receivesCredentialSecurelySelector :: Selector
receivesCredentialSecurelySelector = mkSelector "receivesCredentialSecurely"

-- | @Selector@ for @isProxy@
isProxySelector :: Selector
isProxySelector = mkSelector "isProxy"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @proxyType@
proxyTypeSelector :: Selector
proxyTypeSelector = mkSelector "proxyType"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @authenticationMethod@
authenticationMethodSelector :: Selector
authenticationMethodSelector = mkSelector "authenticationMethod"

-- | @Selector@ for @serverTrust@
serverTrustSelector :: Selector
serverTrustSelector = mkSelector "serverTrust"

-- | @Selector@ for @distinguishedNames@
distinguishedNamesSelector :: Selector
distinguishedNamesSelector = mkSelector "distinguishedNames"

