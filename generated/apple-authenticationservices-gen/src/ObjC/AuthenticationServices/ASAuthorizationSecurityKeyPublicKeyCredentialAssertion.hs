{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialAssertion@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialAssertion
  ( ASAuthorizationSecurityKeyPublicKeyCredentialAssertion
  , IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion(..)
  , new
  , init_
  , appID
  , appIDSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialAssertion"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion asAuthorizationSecurityKeyPublicKeyCredentialAssertion => asAuthorizationSecurityKeyPublicKeyCredentialAssertion -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
init_ asAuthorizationSecurityKeyPublicKeyCredentialAssertion =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertion initSelector

-- | Indicates that this assertion used the appid WebAuthn extension. This can only happen if the requesting app is a web browser and requested to use this extension.
--
-- ObjC selector: @- appID@
appID :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion asAuthorizationSecurityKeyPublicKeyCredentialAssertion => asAuthorizationSecurityKeyPublicKeyCredentialAssertion -> IO Bool
appID asAuthorizationSecurityKeyPublicKeyCredentialAssertion =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertion appIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
initSelector = mkSelector "init"

-- | @Selector@ for @appID@
appIDSelector :: Selector '[] Bool
appIDSelector = mkSelector "appID"

