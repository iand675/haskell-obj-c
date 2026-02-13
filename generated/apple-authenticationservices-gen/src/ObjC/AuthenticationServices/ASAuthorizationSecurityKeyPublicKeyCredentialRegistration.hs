{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialRegistration@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialRegistration
  ( ASAuthorizationSecurityKeyPublicKeyCredentialRegistration
  , IsASAuthorizationSecurityKeyPublicKeyCredentialRegistration(..)
  , transports
  , transportsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A list of transports that the authenticator is believed to support, if this could be determined.
--
-- ObjC selector: @- transports@
transports :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistration asAuthorizationSecurityKeyPublicKeyCredentialRegistration => asAuthorizationSecurityKeyPublicKeyCredentialRegistration -> IO (Id NSArray)
transports asAuthorizationSecurityKeyPublicKeyCredentialRegistration =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistration transportsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transports@
transportsSelector :: Selector '[] (Id NSArray)
transportsSelector = mkSelector "transports"

