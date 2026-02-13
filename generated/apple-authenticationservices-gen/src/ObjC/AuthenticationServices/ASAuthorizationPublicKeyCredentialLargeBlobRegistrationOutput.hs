{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput
  ( ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput
  , IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput(..)
  , isSupported
  , isSupportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isSupported@
isSupported :: IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput asAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput => asAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput -> IO Bool
isSupported asAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput isSupportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector '[] Bool
isSupportedSelector = mkSelector "isSupported"

