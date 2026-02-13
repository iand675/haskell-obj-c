{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates output for various WebAuthn extensions used during passkey assertion.
--
-- Generated bindings for @ASPasskeyAssertionCredentialExtensionOutput@.
module ObjC.AuthenticationServices.ASPasskeyAssertionCredentialExtensionOutput
  ( ASPasskeyAssertionCredentialExtensionOutput
  , IsASPasskeyAssertionCredentialExtensionOutput(..)
  , initWithLargeBlobOutput
  , largeBlobAssertionOutput
  , initWithLargeBlobOutputSelector
  , largeBlobAssertionOutputSelector


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
initWithLargeBlobOutput :: (IsASPasskeyAssertionCredentialExtensionOutput asPasskeyAssertionCredentialExtensionOutput, IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput largeBlob) => asPasskeyAssertionCredentialExtensionOutput -> largeBlob -> IO (Id ASPasskeyAssertionCredentialExtensionOutput)
initWithLargeBlobOutput asPasskeyAssertionCredentialExtensionOutput largeBlob =
  sendOwnedMessage asPasskeyAssertionCredentialExtensionOutput initWithLargeBlobOutputSelector (toASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput largeBlob)

-- | Output for @largeBlob@ operation during passkey assertion.
--
-- ObjC selector: @- largeBlobAssertionOutput@
largeBlobAssertionOutput :: IsASPasskeyAssertionCredentialExtensionOutput asPasskeyAssertionCredentialExtensionOutput => asPasskeyAssertionCredentialExtensionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
largeBlobAssertionOutput asPasskeyAssertionCredentialExtensionOutput =
  sendMessage asPasskeyAssertionCredentialExtensionOutput largeBlobAssertionOutputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLargeBlobOutput:@
initWithLargeBlobOutputSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput] (Id ASPasskeyAssertionCredentialExtensionOutput)
initWithLargeBlobOutputSelector = mkSelector "initWithLargeBlobOutput:"

-- | @Selector@ for @largeBlobAssertionOutput@
largeBlobAssertionOutputSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
largeBlobAssertionOutputSelector = mkSelector "largeBlobAssertionOutput"

