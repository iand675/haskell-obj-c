{-# LANGUAGE PatternSynonyms #-}
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
  , authenticationMethodSelector
  , setAuthenticationMethodSelector
  , useExtendedAuthenticationSelector
  , setUseExtendedAuthenticationSelector

  -- * Enum types
  , NEVPNIKEAuthenticationMethod(NEVPNIKEAuthenticationMethod)
  , pattern NEVPNIKEAuthenticationMethodNone
  , pattern NEVPNIKEAuthenticationMethodCertificate
  , pattern NEVPNIKEAuthenticationMethodSharedSecret

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
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | authenticationMethod
--
-- The method used to authenticate with the IPSec server. Note that if this property is set to NEVPNIKEAuthenticationMethodNone, extended authentication will still be negotiated if useExtendedAuthentication is set to YES.
--
-- ObjC selector: @- authenticationMethod@
authenticationMethod :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO NEVPNIKEAuthenticationMethod
authenticationMethod nevpnProtocolIPSec  =
  fmap (coerce :: CLong -> NEVPNIKEAuthenticationMethod) $ sendMsg nevpnProtocolIPSec (mkSelector "authenticationMethod") retCLong []

-- | authenticationMethod
--
-- The method used to authenticate with the IPSec server. Note that if this property is set to NEVPNIKEAuthenticationMethodNone, extended authentication will still be negotiated if useExtendedAuthentication is set to YES.
--
-- ObjC selector: @- setAuthenticationMethod:@
setAuthenticationMethod :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> NEVPNIKEAuthenticationMethod -> IO ()
setAuthenticationMethod nevpnProtocolIPSec  value =
  sendMsg nevpnProtocolIPSec (mkSelector "setAuthenticationMethod:") retVoid [argCLong (coerce value)]

-- | useExtendedAuthentication
--
-- A flag indicating if extended authentication will be negotiated. This authentication is in addition to the IKE authentication used to authenticate the endpoints of the IKE session.   For IKE version 1, when this flag is set X-Auth authentication will be negotiated as part of the IKE session, using the username and password properties as the credential.   For IKE version 2, when this flag is set EAP authentication will be negotiated as part of the IKE session, using the username, password, and/or identity properties as the credential depending on which EAP method the server requires.
--
-- ObjC selector: @- useExtendedAuthentication@
useExtendedAuthentication :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> IO Bool
useExtendedAuthentication nevpnProtocolIPSec  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnProtocolIPSec (mkSelector "useExtendedAuthentication") retCULong []

-- | useExtendedAuthentication
--
-- A flag indicating if extended authentication will be negotiated. This authentication is in addition to the IKE authentication used to authenticate the endpoints of the IKE session.   For IKE version 1, when this flag is set X-Auth authentication will be negotiated as part of the IKE session, using the username and password properties as the credential.   For IKE version 2, when this flag is set EAP authentication will be negotiated as part of the IKE session, using the username, password, and/or identity properties as the credential depending on which EAP method the server requires.
--
-- ObjC selector: @- setUseExtendedAuthentication:@
setUseExtendedAuthentication :: IsNEVPNProtocolIPSec nevpnProtocolIPSec => nevpnProtocolIPSec -> Bool -> IO ()
setUseExtendedAuthentication nevpnProtocolIPSec  value =
  sendMsg nevpnProtocolIPSec (mkSelector "setUseExtendedAuthentication:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authenticationMethod@
authenticationMethodSelector :: Selector
authenticationMethodSelector = mkSelector "authenticationMethod"

-- | @Selector@ for @setAuthenticationMethod:@
setAuthenticationMethodSelector :: Selector
setAuthenticationMethodSelector = mkSelector "setAuthenticationMethod:"

-- | @Selector@ for @useExtendedAuthentication@
useExtendedAuthenticationSelector :: Selector
useExtendedAuthenticationSelector = mkSelector "useExtendedAuthentication"

-- | @Selector@ for @setUseExtendedAuthentication:@
setUseExtendedAuthenticationSelector :: Selector
setUseExtendedAuthenticationSelector = mkSelector "setUseExtendedAuthentication:"

