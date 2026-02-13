{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates output for various WebAuthn extensions used during passkey registration.
--
-- Generated bindings for @ASPasskeyRegistrationCredentialExtensionOutput@.
module ObjC.AuthenticationServices.ASPasskeyRegistrationCredentialExtensionOutput
  ( ASPasskeyRegistrationCredentialExtensionOutput
  , IsASPasskeyRegistrationCredentialExtensionOutput(..)
  , initWithLargeBlobOutput
  , largeBlobRegistrationOutput
  , initWithLargeBlobOutputSelector
  , largeBlobRegistrationOutputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLargeBlobOutput:@
initWithLargeBlobOutput :: (IsASPasskeyRegistrationCredentialExtensionOutput asPasskeyRegistrationCredentialExtensionOutput, IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput largeBlob) => asPasskeyRegistrationCredentialExtensionOutput -> largeBlob -> IO (Id ASPasskeyRegistrationCredentialExtensionOutput)
initWithLargeBlobOutput asPasskeyRegistrationCredentialExtensionOutput largeBlob =
  sendOwnedMessage asPasskeyRegistrationCredentialExtensionOutput initWithLargeBlobOutputSelector (toASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput largeBlob)

-- | Output for @largeBlob@ operation during passkey registration.
--
-- ObjC selector: @- largeBlobRegistrationOutput@
largeBlobRegistrationOutput :: IsASPasskeyRegistrationCredentialExtensionOutput asPasskeyRegistrationCredentialExtensionOutput => asPasskeyRegistrationCredentialExtensionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput)
largeBlobRegistrationOutput asPasskeyRegistrationCredentialExtensionOutput =
  sendMessage asPasskeyRegistrationCredentialExtensionOutput largeBlobRegistrationOutputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLargeBlobOutput:@
initWithLargeBlobOutputSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput] (Id ASPasskeyRegistrationCredentialExtensionOutput)
initWithLargeBlobOutputSelector = mkSelector "initWithLargeBlobOutput:"

-- | @Selector@ for @largeBlobRegistrationOutput@
largeBlobRegistrationOutputSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput)
largeBlobRegistrationOutputSelector = mkSelector "largeBlobRegistrationOutput"

