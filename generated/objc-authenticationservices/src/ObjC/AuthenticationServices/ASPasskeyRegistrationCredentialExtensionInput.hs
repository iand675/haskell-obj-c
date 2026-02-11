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
  , newSelector
  , largeBlobSelector


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

-- | @- init@
init_ :: IsASPasskeyRegistrationCredentialExtensionInput asPasskeyRegistrationCredentialExtensionInput => asPasskeyRegistrationCredentialExtensionInput -> IO (Id ASPasskeyRegistrationCredentialExtensionInput)
init_ asPasskeyRegistrationCredentialExtensionInput  =
  sendMsg asPasskeyRegistrationCredentialExtensionInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASPasskeyRegistrationCredentialExtensionInput)
new  =
  do
    cls' <- getRequiredClass "ASPasskeyRegistrationCredentialExtensionInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Input for the @largeBlob@ extension in passkey registration requests.
--
-- ObjC selector: @- largeBlob@
largeBlob :: IsASPasskeyRegistrationCredentialExtensionInput asPasskeyRegistrationCredentialExtensionInput => asPasskeyRegistrationCredentialExtensionInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlob asPasskeyRegistrationCredentialExtensionInput  =
  sendMsg asPasskeyRegistrationCredentialExtensionInput (mkSelector "largeBlob") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector
largeBlobSelector = mkSelector "largeBlob"

