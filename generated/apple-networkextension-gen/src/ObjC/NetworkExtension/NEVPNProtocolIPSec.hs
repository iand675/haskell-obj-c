{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEVPNProtocolIPSec
--
-- The NEVPNProtocolIPSec class declares the programmatic interface of an object that manages the IPSec-specific portion of a VPN configuration.
--
-- Instances of this class use IKE version 1 for key negotiation.
--
-- Generated bindings for @NEVPNProtocolIPSec@.
module ObjC.NetworkExtension.NEVPNProtocolIPSec
  ( NEVPNProtocolIPSec
  , IsNEVPNProtocolIPSec(..)
  , authenticationMethod
  , setAuthenticationMethod
  , useExtendedAuthentication
  , setUseExtendedAuthentication
  , sharedSecretReference
  , setSharedSecretReference
  , localIdentifier
  , setLocalIdentifier
  , remoteIdentifier
  , setRemoteIdentifier
  , authenticationMethodSelector
  , localIdentifierSelector
  , remoteIdentifierSelector
  , setAuthenticationMethodSelector
  , setLocalIdentifierSelector
  , setRemoteIdentifierSelector
  , setSharedSecretReferenceSelector
  , setUseExtendedAuthenticationSelector
  , sharedSecretReferenceSelector
  , useExtendedAuthenticationSelector

  -- * Enum types
  , NEVPNIKEAuthenticationMethod(NEVPNIKEAuthenticationMethod)
  , pattern NEVPNIKEAuthenticationMethodNone
  , pattern NEVPNIKEAuthenticationMethodCertificate
  , pattern NEVPNIKEAuthenticationMethodSharedSecret

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | authenticationMethod
--
-- The method used to authenticate with the IPSec server. Note that if this property is set to NEVPNIKEAuthenticationMethodNone, extended authentication will still be negotiated if useExtendedAuthentication is set to YES.
--
-- ObjC selector: @- authenticationMethod@
authenticationMethod :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO NEVPNIKEAuthenticationMethod
authenticationMethod nevpnProtocolIPSec =
  sendMessage nevpnProtocolIPSec authenticationMethodSelector

-- | authenticationMethod
--
-- The method used to authenticate with the IPSec server. Note that if this property is set to NEVPNIKEAuthenticationMethodNone, extended authentication will still be negotiated if useExtendedAuthentication is set to YES.
--
-- ObjC selector: @- setAuthenticationMethod:@
setAuthenticationMethod :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> NEVPNIKEAuthenticationMethod -> IO ()
setAuthenticationMethod nevpnProtocolIPSec value =
  sendMessage nevpnProtocolIPSec setAuthenticationMethodSelector value

-- | useExtendedAuthentication
--
-- A flag indicating if extended authentication will be negotiated. This authentication is in addition to the IKE authentication used to authenticate the endpoints of the IKE session.   For IKE version 1, when this flag is set X-Auth authentication will be negotiated as part of the IKE session, using the username and password properties as the credential.   For IKE version 2, when this flag is set EAP authentication will be negotiated as part of the IKE session, using the username, password, and/or identity properties as the credential depending on which EAP method the server requires.
--
-- ObjC selector: @- useExtendedAuthentication@
useExtendedAuthentication :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO Bool
useExtendedAuthentication nevpnProtocolIPSec =
  sendMessage nevpnProtocolIPSec useExtendedAuthenticationSelector

-- | useExtendedAuthentication
--
-- A flag indicating if extended authentication will be negotiated. This authentication is in addition to the IKE authentication used to authenticate the endpoints of the IKE session.   For IKE version 1, when this flag is set X-Auth authentication will be negotiated as part of the IKE session, using the username and password properties as the credential.   For IKE version 2, when this flag is set EAP authentication will be negotiated as part of the IKE session, using the username, password, and/or identity properties as the credential depending on which EAP method the server requires.
--
-- ObjC selector: @- setUseExtendedAuthentication:@
setUseExtendedAuthentication :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> Bool -> IO ()
setUseExtendedAuthentication nevpnProtocolIPSec value =
  sendMessage nevpnProtocolIPSec setUseExtendedAuthenticationSelector value

-- | sharedSecretReference
--
-- A persistent reference to a keychain item of class kSecClassGenericPassword containing the IKE shared secret.
--
-- ObjC selector: @- sharedSecretReference@
sharedSecretReference :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO (Id NSData)
sharedSecretReference nevpnProtocolIPSec =
  sendMessage nevpnProtocolIPSec sharedSecretReferenceSelector

-- | sharedSecretReference
--
-- A persistent reference to a keychain item of class kSecClassGenericPassword containing the IKE shared secret.
--
-- ObjC selector: @- setSharedSecretReference:@
setSharedSecretReference :: (IsNEVPNProtocolIPSec nevpnProtocolIPSec, IsNSData value) => nevpnProtocolIPSec -> value -> IO ()
setSharedSecretReference nevpnProtocolIPSec value =
  sendMessage nevpnProtocolIPSec setSharedSecretReferenceSelector (toNSData value)

-- | localIdentifier
--
-- A string identifying the local IPSec endpoint for authentication purposes.
--
-- ObjC selector: @- localIdentifier@
localIdentifier :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO (Id NSString)
localIdentifier nevpnProtocolIPSec =
  sendMessage nevpnProtocolIPSec localIdentifierSelector

-- | localIdentifier
--
-- A string identifying the local IPSec endpoint for authentication purposes.
--
-- ObjC selector: @- setLocalIdentifier:@
setLocalIdentifier :: (IsNEVPNProtocolIPSec nevpnProtocolIPSec, IsNSString value) => nevpnProtocolIPSec -> value -> IO ()
setLocalIdentifier nevpnProtocolIPSec value =
  sendMessage nevpnProtocolIPSec setLocalIdentifierSelector (toNSString value)

-- | remoteIdentifier
--
-- A string identifying the remote IPSec endpoint for authentication purposes.
--
-- ObjC selector: @- remoteIdentifier@
remoteIdentifier :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO (Id NSString)
remoteIdentifier nevpnProtocolIPSec =
  sendMessage nevpnProtocolIPSec remoteIdentifierSelector

-- | remoteIdentifier
--
-- A string identifying the remote IPSec endpoint for authentication purposes.
--
-- ObjC selector: @- setRemoteIdentifier:@
setRemoteIdentifier :: (IsNEVPNProtocolIPSec nevpnProtocolIPSec, IsNSString value) => nevpnProtocolIPSec -> value -> IO ()
setRemoteIdentifier nevpnProtocolIPSec value =
  sendMessage nevpnProtocolIPSec setRemoteIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authenticationMethod@
authenticationMethodSelector :: Selector '[] NEVPNIKEAuthenticationMethod
authenticationMethodSelector = mkSelector "authenticationMethod"

-- | @Selector@ for @setAuthenticationMethod:@
setAuthenticationMethodSelector :: Selector '[NEVPNIKEAuthenticationMethod] ()
setAuthenticationMethodSelector = mkSelector "setAuthenticationMethod:"

-- | @Selector@ for @useExtendedAuthentication@
useExtendedAuthenticationSelector :: Selector '[] Bool
useExtendedAuthenticationSelector = mkSelector "useExtendedAuthentication"

-- | @Selector@ for @setUseExtendedAuthentication:@
setUseExtendedAuthenticationSelector :: Selector '[Bool] ()
setUseExtendedAuthenticationSelector = mkSelector "setUseExtendedAuthentication:"

-- | @Selector@ for @sharedSecretReference@
sharedSecretReferenceSelector :: Selector '[] (Id NSData)
sharedSecretReferenceSelector = mkSelector "sharedSecretReference"

-- | @Selector@ for @setSharedSecretReference:@
setSharedSecretReferenceSelector :: Selector '[Id NSData] ()
setSharedSecretReferenceSelector = mkSelector "setSharedSecretReference:"

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector '[] (Id NSString)
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @setLocalIdentifier:@
setLocalIdentifierSelector :: Selector '[Id NSString] ()
setLocalIdentifierSelector = mkSelector "setLocalIdentifier:"

-- | @Selector@ for @remoteIdentifier@
remoteIdentifierSelector :: Selector '[] (Id NSString)
remoteIdentifierSelector = mkSelector "remoteIdentifier"

-- | @Selector@ for @setRemoteIdentifier:@
setRemoteIdentifierSelector :: Selector '[Id NSString] ()
setRemoteIdentifierSelector = mkSelector "setRemoteIdentifier:"

