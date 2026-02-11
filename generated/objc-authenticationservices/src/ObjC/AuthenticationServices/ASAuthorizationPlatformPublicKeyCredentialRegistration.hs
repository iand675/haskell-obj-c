{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , attachmentSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialAttachment(ASAuthorizationPublicKeyCredentialAttachment)
  , pattern ASAuthorizationPublicKeyCredentialAttachmentPlatform
  , pattern ASAuthorizationPublicKeyCredentialAttachmentCrossPlatform

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
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistration)
init_ asAuthorizationPlatformPublicKeyCredentialRegistration  =
  sendMsg asAuthorizationPlatformPublicKeyCredentialRegistration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- attachment@
attachment :: IsASAuthorizationPlatformPublicKeyCredentialRegistration asAuthorizationPlatformPublicKeyCredentialRegistration => asAuthorizationPlatformPublicKeyCredentialRegistration -> IO ASAuthorizationPublicKeyCredentialAttachment
attachment asAuthorizationPlatformPublicKeyCredentialRegistration  =
  fmap (coerce :: CLong -> ASAuthorizationPublicKeyCredentialAttachment) $ sendMsg asAuthorizationPlatformPublicKeyCredentialRegistration (mkSelector "attachment") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

