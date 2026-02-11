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
  , newSelector
  , initSelector
  , nameSelector
  , customTitleSelector
  , relyingPartySelector
  , credentialIDSelector
  , userHandleSelector
  , providerNameSelector


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

-- | @+ new@
new :: IO (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationWebBrowserPlatformPublicKeyCredential"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id ASAuthorizationWebBrowserPlatformPublicKeyCredential)
init_ asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The user name of the saved credential.
--
-- ObjC selector: @- name@
name :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
name asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A user-specified title for the credential.
--
-- ObjC selector: @- customTitle@
customTitle :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
customTitle asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "customTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The "relying party" (generally website) the credential was saved for.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
relyingParty asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "relyingParty") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier for this credential.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSData)
credentialID asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "credentialID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier for the user account associated with this credential. One account may have multiple associated credentials.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSData)
userHandle asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "userHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized name of the credential provider that provided this passkey.
--
-- ObjC selector: @- providerName@
providerName :: IsASAuthorizationWebBrowserPlatformPublicKeyCredential asAuthorizationWebBrowserPlatformPublicKeyCredential => asAuthorizationWebBrowserPlatformPublicKeyCredential -> IO (Id NSString)
providerName asAuthorizationWebBrowserPlatformPublicKeyCredential  =
  sendMsg asAuthorizationWebBrowserPlatformPublicKeyCredential (mkSelector "providerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @customTitle@
customTitleSelector :: Selector
customTitleSelector = mkSelector "customTitle"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @providerName@
providerNameSelector :: Selector
providerNameSelector = mkSelector "providerName"

