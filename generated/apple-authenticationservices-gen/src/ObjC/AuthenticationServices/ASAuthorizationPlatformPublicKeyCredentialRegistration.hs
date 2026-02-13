{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialRegistration@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialRegistration
  ( ASAuthorizationPlatformPublicKeyCredentialRegistration
  , IsASAuthorizationPlatformPublicKeyCredentialRegistration(..)
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
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
init_ asAuthorizationPlatformPublicKeyCredentialRegistration =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialRegistration initSelector

-- | @- attachment@
attachment :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO ASAuthorizationPublicKeyCredentialAttachment
attachment asAuthorizationPlatformPublicKeyCredentialRegistration =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistration attachmentSelector

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput)
largeBlob asAuthorizationPlatformPublicKeyCredentialRegistration =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistration largeBlobSelector

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput)
prf asAuthorizationPlatformPublicKeyCredentialRegistration =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistration prfSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] ASAuthorizationPublicKeyCredentialAttachment
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput)
largeBlobSelector = mkSelector "largeBlob"

-- | @Selector@ for @prf@
prfSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFRegistrationOutput)
prfSelector = mkSelector "prf"

