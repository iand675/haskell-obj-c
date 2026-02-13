{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionOutput
  ( ASAuthorizationPublicKeyCredentialPRFAssertionOutput
  , IsASAuthorizationPublicKeyCredentialPRFAssertionOutput(..)
  , first
  , second
  , firstSelector
  , secondSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- first@
first :: IsASAuthorizationPublicKeyCredentialPRFAssertionOutput asAuthorizationPublicKeyCredentialPRFAssertionOutput => asAuthorizationPublicKeyCredentialPRFAssertionOutput -> IO (Id NSData)
first asAuthorizationPublicKeyCredentialPRFAssertionOutput =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionOutput firstSelector

-- | @- second@
second :: IsASAuthorizationPublicKeyCredentialPRFAssertionOutput asAuthorizationPublicKeyCredentialPRFAssertionOutput => asAuthorizationPublicKeyCredentialPRFAssertionOutput -> IO (Id NSData)
second asAuthorizationPublicKeyCredentialPRFAssertionOutput =
  sendMessage asAuthorizationPublicKeyCredentialPRFAssertionOutput secondSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @first@
firstSelector :: Selector '[] (Id NSData)
firstSelector = mkSelector "first"

-- | @Selector@ for @second@
secondSelector :: Selector '[] (Id NSData)
secondSelector = mkSelector "second"

