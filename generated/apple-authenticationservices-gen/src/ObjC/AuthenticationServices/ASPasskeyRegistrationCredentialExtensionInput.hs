{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates input for various WebAuthn extensions during passkey registration.
--
-- Generated bindings for @ASPasskeyRegistrationCredentialExtensionInput@.
module ObjC.AuthenticationServices.ASPasskeyRegistrationCredentialExtensionInput
  ( ASPasskeyRegistrationCredentialExtensionInput
  , IsASPasskeyRegistrationCredentialExtensionInput(..)
  , init_
  , new
  , largeBlob
  , initSelector
  , largeBlobSelector
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

-- | @- init@
init_ :: IsASPasskeyRegistrationCredentialExtensionInput asPasskeyRegistrationCredentialExtensionInput => asPasskeyRegistrationCredentialExtensionInput -> IO (Id ASPasskeyRegistrationCredentialExtensionInput)
init_ asPasskeyRegistrationCredentialExtensionInput =
  sendOwnedMessage asPasskeyRegistrationCredentialExtensionInput initSelector

-- | @+ new@
new :: IO (Id ASPasskeyRegistrationCredentialExtensionInput)
new  =
  do
    cls' <- getRequiredClass "ASPasskeyRegistrationCredentialExtensionInput"
    sendOwnedClassMessage cls' newSelector

-- | Input for the @largeBlob@ extension in passkey registration requests.
--
-- ObjC selector: @- largeBlob@
largeBlob :: IsASPasskeyRegistrationCredentialExtensionInput asPasskeyRegistrationCredentialExtensionInput => asPasskeyRegistrationCredentialExtensionInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlob asPasskeyRegistrationCredentialExtensionInput =
  sendMessage asPasskeyRegistrationCredentialExtensionInput largeBlobSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasskeyRegistrationCredentialExtensionInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASPasskeyRegistrationCredentialExtensionInput)
newSelector = mkSelector "new"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlobSelector = mkSelector "largeBlob"

