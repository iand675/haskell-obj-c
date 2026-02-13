{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFRegistrationOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFRegistrationOutput
  ( ASAuthorizationPublicKeyCredentialPRFRegistrationOutput
  , IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput(..)
  , isSupported
  , first
  , second
  , firstSelector
  , isSupportedSelector
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

-- | @- isSupported@
isSupported :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO Bool
isSupported asAuthorizationPublicKeyCredentialPRFRegistrationOutput =
  sendMessage asAuthorizationPublicKeyCredentialPRFRegistrationOutput isSupportedSelector

-- | @- first@
first :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO (Id NSData)
first asAuthorizationPublicKeyCredentialPRFRegistrationOutput =
  sendMessage asAuthorizationPublicKeyCredentialPRFRegistrationOutput firstSelector

-- | @- second@
second :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO (Id NSData)
second asAuthorizationPublicKeyCredentialPRFRegistrationOutput =
  sendMessage asAuthorizationPublicKeyCredentialPRFRegistrationOutput secondSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector '[] Bool
isSupportedSelector = mkSelector "isSupported"

-- | @Selector@ for @first@
firstSelector :: Selector '[] (Id NSData)
firstSelector = mkSelector "first"

-- | @Selector@ for @second@
secondSelector :: Selector '[] (Id NSData)
secondSelector = mkSelector "second"

