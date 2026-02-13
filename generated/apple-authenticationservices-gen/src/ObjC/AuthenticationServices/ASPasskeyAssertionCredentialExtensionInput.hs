{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates input for various WebAuthn extensions during passkey assertion.
--
-- Generated bindings for @ASPasskeyAssertionCredentialExtensionInput@.
module ObjC.AuthenticationServices.ASPasskeyAssertionCredentialExtensionInput
  ( ASPasskeyAssertionCredentialExtensionInput
  , IsASPasskeyAssertionCredentialExtensionInput(..)
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
init_ :: IsASPasskeyAssertionCredentialExtensionInput asPasskeyAssertionCredentialExtensionInput => asPasskeyAssertionCredentialExtensionInput -> IO (Id ASPasskeyAssertionCredentialExtensionInput)
init_ asPasskeyAssertionCredentialExtensionInput =
  sendOwnedMessage asPasskeyAssertionCredentialExtensionInput initSelector

-- | @+ new@
new :: IO (Id ASPasskeyAssertionCredentialExtensionInput)
new  =
  do
    cls' <- getRequiredClass "ASPasskeyAssertionCredentialExtensionInput"
    sendOwnedClassMessage cls' newSelector

-- | Input for the @largeBlob@ extension in passkey assertion requests.
--
-- ObjC selector: @- largeBlob@
largeBlob :: IsASPasskeyAssertionCredentialExtensionInput asPasskeyAssertionCredentialExtensionInput => asPasskeyAssertionCredentialExtensionInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
largeBlob asPasskeyAssertionCredentialExtensionInput =
  sendMessage asPasskeyAssertionCredentialExtensionInput largeBlobSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasskeyAssertionCredentialExtensionInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASPasskeyAssertionCredentialExtensionInput)
newSelector = mkSelector "new"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
largeBlobSelector = mkSelector "largeBlob"

