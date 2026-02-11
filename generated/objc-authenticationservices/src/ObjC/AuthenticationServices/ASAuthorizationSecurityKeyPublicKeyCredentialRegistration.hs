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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A list of transports that the authenticator is believed to support, if this could be determined.
--
-- ObjC selector: @- transports@
transports :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistration asAuthorizationSecurityKeyPublicKeyCredentialRegistration => asAuthorizationSecurityKeyPublicKeyCredentialRegistration -> IO (Id NSArray)
transports asAuthorizationSecurityKeyPublicKeyCredentialRegistration  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistration (mkSelector "transports") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transports@
transportsSelector :: Selector
transportsSelector = mkSelector "transports"

