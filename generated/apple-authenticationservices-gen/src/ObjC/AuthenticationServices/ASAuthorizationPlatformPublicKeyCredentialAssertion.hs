{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialAssertion@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialAssertion
  ( ASAuthorizationPlatformPublicKeyCredentialAssertion
  , IsASAuthorizationPlatformPublicKeyCredentialAssertion(..)
  , new
  , init_
  , attachment
  , largeBlob
  , prf
  , attachmentSelector
  , initSelector
  , largeBlobSelector
  , newSelector
  , prfSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialAttachment(ASAuthorizationPublicKeyCredentialAttachment)
  , pattern ASAuthorizationPublicKeyCredentialAttachmentPlatform
  , pattern ASAuthorizationPublicKeyCredentialAttachmentCrossPlatform

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertion)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialAssertion"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialAssertion asAuthorizationPlatformPublicKeyCredentialAssertion => asAuthorizationPlatformPublicKeyCredentialAssertion -> IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertion)
init_ asAuthorizationPlatformPublicKeyCredentialAssertion =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialAssertion initSelector

-- | @- attachment@
attachment :: IsASAuthorizationPlatformPublicKeyCredentialAssertion asAuthorizationPlatformPublicKeyCredentialAssertion => asAuthorizationPlatformPublicKeyCredentialAssertion -> IO ASAuthorizationPublicKeyCredentialAttachment
attachment asAuthorizationPlatformPublicKeyCredentialAssertion =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertion attachmentSelector

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialAssertion asAuthorizationPlatformPublicKeyCredentialAssertion => asAuthorizationPlatformPublicKeyCredentialAssertion -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
largeBlob asAuthorizationPlatformPublicKeyCredentialAssertion =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertion largeBlobSelector

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialAssertion asAuthorizationPlatformPublicKeyCredentialAssertion => asAuthorizationPlatformPublicKeyCredentialAssertion -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput)
prf asAuthorizationPlatformPublicKeyCredentialAssertion =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertion prfSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialAssertion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialAssertion)
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] ASAuthorizationPublicKeyCredentialAttachment
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
largeBlobSelector = mkSelector "largeBlob"

-- | @Selector@ for @prf@
prfSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFAssertionOutput)
prfSelector = mkSelector "prf"

