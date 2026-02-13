{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationWebBrowserPlatformPublicKeyCredential@.
module ObjC.AuthenticationServices.ASAuthorizationWebBrowserPlatformPublicKeyCredential
  ( ASAuthorizationWebBrowserPlatformPublicKeyCredential
  , IsASAuthorizationWebBrowserPlatformPublicKeyCredential(..)
  , new
  , init_
  , name
  , customTitle
  , relyingParty
  , credentialID
  , userHandle
  , providerName
  , credentialIDSelector
  , customTitleSelector
  , initSelector
  , nameSelector
  , newSelector
  , providerNameSelector
  , relyingPartySelector
  , userHandleSelector


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
new :: IO (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationWebBrowserPlatformPublicKeyCredential"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
init_ asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendOwnedMessage asAuthorizationWebBrowserPlatformPublicKeyCredential initSelector

-- | The user name of the saved credential.
--
-- ObjC selector: @- name@
name :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
name asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential nameSelector

-- | A user-specified title for the credential.
--
-- ObjC selector: @- customTitle@
customTitle :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
customTitle asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential customTitleSelector

-- | The "relying party" (generally website) the credential was saved for.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
relyingParty asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential relyingPartySelector

-- | A unique identifier for this credential.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSData)
credentialID asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential credentialIDSelector

-- | A unique identifier for the user account associated with this credential. One account may have multiple associated credentials.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSData)
userHandle asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential userHandleSelector

-- | The localized name of the credential provider that provided this passkey.
--
-- ObjC selector: @- providerName@
providerName :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
providerName asAuthorizationWebBrowserPlatformPublicKeyCredential =
  sendMessage asAuthorizationWebBrowserPlatformPublicKeyCredential providerNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @customTitle@
customTitleSelector :: Selector '[] (Id NSString)
customTitleSelector = mkSelector "customTitle"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector '[] (Id NSString)
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector '[] (Id NSData)
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector '[] (Id NSData)
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @providerName@
providerNameSelector :: Selector '[] (Id NSString)
providerNameSelector = mkSelector "providerName"

