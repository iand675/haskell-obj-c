{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIdentityDocumentMetadata@.
module ObjC.PassKit.PKIdentityDocumentMetadata
  ( PKIdentityDocumentMetadata
  , IsPKIdentityDocumentMetadata(..)
  , init_
  , new
  , credentialIdentifier
  , sharingInstanceIdentifier
  , cardTemplateIdentifier
  , cardConfigurationIdentifier
  , serverEnvironmentIdentifier
  , setServerEnvironmentIdentifier
  , documentType
  , initSelector
  , newSelector
  , credentialIdentifierSelector
  , sharingInstanceIdentifierSelector
  , cardTemplateIdentifierSelector
  , cardConfigurationIdentifierSelector
  , serverEnvironmentIdentifierSelector
  , setServerEnvironmentIdentifierSelector
  , documentTypeSelector

  -- * Enum types
  , PKAddIdentityDocumentType(PKAddIdentityDocumentType)
  , pattern PKAddIdentityDocumentTypeIDCard
  , pattern PKAddIdentityDocumentTypeMDL
  , pattern PKAddIdentityDocumentTypePhotoID

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id PKIdentityDocumentMetadata)
init_ pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKIdentityDocumentMetadata)
new  =
  do
    cls' <- getRequiredClass "PKIdentityDocumentMetadata"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | credentialIdentifier: A unique identifier for provisioning credential data.
--
-- ObjC selector: @- credentialIdentifier@
credentialIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
credentialIdentifier pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "credentialIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sharingInstanceIdentifier: A unique identifier that refers to an instance of sharing of credentials to a user's device initiated from another user, device, or web.
--
-- ObjC selector: @- sharingInstanceIdentifier@
sharingInstanceIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
sharingInstanceIdentifier pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "sharingInstanceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cardTemplateIdentifier: Identifier referencing a card template registered by developers in web portal - identifies a combination of cardProfileIdentifier, cardConfigurationIdentifier, and cardArtBundleName. Returns empty string if no identifier is set.
--
-- ObjC selector: @- cardTemplateIdentifier@
cardTemplateIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
cardTemplateIdentifier pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "cardTemplateIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cardConfigurationIdentifier: Identifier referencing a card configuration registered by developers. Returns empty string if no identifier is set.
--
-- ObjC selector: @- cardConfigurationIdentifier@
cardConfigurationIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
cardConfigurationIdentifier pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "cardConfigurationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverEnvironmentIdentifier: Identifier referencing the target server environment Apple Pay servers should reach out to to provision this pass. If not present, the default Apply Pay server environment will be used and an empty string will be returned.
--
-- ObjC selector: @- serverEnvironmentIdentifier@
serverEnvironmentIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
serverEnvironmentIdentifier pkIdentityDocumentMetadata  =
  sendMsg pkIdentityDocumentMetadata (mkSelector "serverEnvironmentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverEnvironmentIdentifier: Identifier referencing the target server environment Apple Pay servers should reach out to to provision this pass. If not present, the default Apply Pay server environment will be used and an empty string will be returned.
--
-- ObjC selector: @- setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifier :: (IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata, IsNSString value) => pkIdentityDocumentMetadata -> value -> IO ()
setServerEnvironmentIdentifier pkIdentityDocumentMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkIdentityDocumentMetadata (mkSelector "setServerEnvironmentIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityDocumentType: identifies the type of the identity document
--
-- ObjC selector: @- documentType@
documentType :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO PKAddIdentityDocumentType
documentType pkIdentityDocumentMetadata  =
  fmap (coerce :: CLong -> PKAddIdentityDocumentType) $ sendMsg pkIdentityDocumentMetadata (mkSelector "documentType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @credentialIdentifier@
credentialIdentifierSelector :: Selector
credentialIdentifierSelector = mkSelector "credentialIdentifier"

-- | @Selector@ for @sharingInstanceIdentifier@
sharingInstanceIdentifierSelector :: Selector
sharingInstanceIdentifierSelector = mkSelector "sharingInstanceIdentifier"

-- | @Selector@ for @cardTemplateIdentifier@
cardTemplateIdentifierSelector :: Selector
cardTemplateIdentifierSelector = mkSelector "cardTemplateIdentifier"

-- | @Selector@ for @cardConfigurationIdentifier@
cardConfigurationIdentifierSelector :: Selector
cardConfigurationIdentifierSelector = mkSelector "cardConfigurationIdentifier"

-- | @Selector@ for @serverEnvironmentIdentifier@
serverEnvironmentIdentifierSelector :: Selector
serverEnvironmentIdentifierSelector = mkSelector "serverEnvironmentIdentifier"

-- | @Selector@ for @setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifierSelector :: Selector
setServerEnvironmentIdentifierSelector = mkSelector "setServerEnvironmentIdentifier:"

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector
documentTypeSelector = mkSelector "documentType"

