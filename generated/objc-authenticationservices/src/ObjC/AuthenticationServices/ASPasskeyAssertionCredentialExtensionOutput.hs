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
initWithLargeBlobOutput :: (IsASPasskeyAssertionCredentialExtensionOutput asPasskeyAssertionCredentialExtensionOutput, IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput largeBlob) => asPasskeyAssertionCredentialExtensionOutput -> largeBlob -> IO (Id ASPasskeyAssertionCredentialExtensionOutput)
initWithLargeBlobOutput asPasskeyAssertionCredentialExtensionOutput  largeBlob =
withObjCPtr largeBlob $ \raw_largeBlob ->
    sendMsg asPasskeyAssertionCredentialExtensionOutput (mkSelector "initWithLargeBlobOutput:") (retPtr retVoid) [argPtr (castPtr raw_largeBlob :: Ptr ())] >>= ownedObject . castPtr

-- | Output for @largeBlob@ operation during passkey assertion.
--
-- ObjC selector: @- largeBlobAssertionOutput@
largeBlobAssertionOutput :: IsASPasskeyAssertionCredentialExtensionOutput asPasskeyAssertionCredentialExtensionOutput => asPasskeyAssertionCredentialExtensionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
largeBlobAssertionOutput asPasskeyAssertionCredentialExtensionOutput  =
  sendMsg asPasskeyAssertionCredentialExtensionOutput (mkSelector "largeBlobAssertionOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLargeBlobOutput:@
initWithLargeBlobOutputSelector :: Selector
initWithLargeBlobOutputSelector = mkSelector "initWithLargeBlobOutput:"

-- | @Selector@ for @largeBlobAssertionOutput@
largeBlobAssertionOutputSelector :: Selector
largeBlobAssertionOutputSelector = mkSelector "largeBlobAssertionOutput"

