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

-- | @- initWithLargeBlobOutput:@
initWithLargeBlobOutput :: (IsASPasskeyRegistrationCredentialExtensionOutput asPasskeyRegistrationCredentialExtensionOutput, IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput largeBlob) => asPasskeyRegistrationCredentialExtensionOutput -> largeBlob -> IO (Id ASPasskeyRegistrationCredentialExtensionOutput)
initWithLargeBlobOutput asPasskeyRegistrationCredentialExtensionOutput  largeBlob =
withObjCPtr largeBlob $ \raw_largeBlob ->
    sendMsg asPasskeyRegistrationCredentialExtensionOutput (mkSelector "initWithLargeBlobOutput:") (retPtr retVoid) [argPtr (castPtr raw_largeBlob :: Ptr ())] >>= ownedObject . castPtr

-- | Output for @largeBlob@ operation during passkey registration.
--
-- ObjC selector: @- largeBlobRegistrationOutput@
largeBlobRegistrationOutput :: IsASPasskeyRegistrationCredentialExtensionOutput asPasskeyRegistrationCredentialExtensionOutput => asPasskeyRegistrationCredentialExtensionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput)
largeBlobRegistrationOutput asPasskeyRegistrationCredentialExtensionOutput  =
  sendMsg asPasskeyRegistrationCredentialExtensionOutput (mkSelector "largeBlobRegistrationOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLargeBlobOutput:@
initWithLargeBlobOutputSelector :: Selector
initWithLargeBlobOutputSelector = mkSelector "initWithLargeBlobOutput:"

-- | @Selector@ for @largeBlobRegistrationOutput@
largeBlobRegistrationOutputSelector :: Selector
largeBlobRegistrationOutputSelector = mkSelector "largeBlobRegistrationOutput"

