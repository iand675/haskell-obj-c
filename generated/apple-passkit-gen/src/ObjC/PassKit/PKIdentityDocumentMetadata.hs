{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , issuingCountryCode
  , documentType
  , cardConfigurationIdentifierSelector
  , cardTemplateIdentifierSelector
  , credentialIdentifierSelector
  , documentTypeSelector
  , initSelector
  , issuingCountryCodeSelector
  , newSelector
  , serverEnvironmentIdentifierSelector
  , setServerEnvironmentIdentifierSelector
  , sharingInstanceIdentifierSelector

  -- * Enum types
  , PKAddIdentityDocumentType(PKAddIdentityDocumentType)
  , pattern PKAddIdentityDocumentTypeIDCard
  , pattern PKAddIdentityDocumentTypeMDL
  , pattern PKAddIdentityDocumentTypePhotoID

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id PKIdentityDocumentMetadata)
init_ pkIdentityDocumentMetadata =
  sendOwnedMessage pkIdentityDocumentMetadata initSelector

-- | @+ new@
new :: IO (Id PKIdentityDocumentMetadata)
new  =
  do
    cls' <- getRequiredClass "PKIdentityDocumentMetadata"
    sendOwnedClassMessage cls' newSelector

-- | credentialIdentifier: A unique identifier for provisioning credential data.
--
-- ObjC selector: @- credentialIdentifier@
credentialIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
credentialIdentifier pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata credentialIdentifierSelector

-- | sharingInstanceIdentifier: A unique identifier that refers to an instance of sharing of credentials to a user's device initiated from another user, device, or web.
--
-- ObjC selector: @- sharingInstanceIdentifier@
sharingInstanceIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
sharingInstanceIdentifier pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata sharingInstanceIdentifierSelector

-- | cardTemplateIdentifier: Identifier referencing a card template registered by developers in web portal - identifies a combination of cardProfileIdentifier, cardConfigurationIdentifier, and cardArtBundleName. Returns empty string if no identifier is set.
--
-- ObjC selector: @- cardTemplateIdentifier@
cardTemplateIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
cardTemplateIdentifier pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata cardTemplateIdentifierSelector

-- | cardConfigurationIdentifier: Identifier referencing a card configuration registered by developers. Returns empty string if no identifier is set.
--
-- ObjC selector: @- cardConfigurationIdentifier@
cardConfigurationIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
cardConfigurationIdentifier pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata cardConfigurationIdentifierSelector

-- | serverEnvironmentIdentifier: Identifier referencing the target server environment Apple Pay servers should reach out to to provision this pass. If not present, the default Apply Pay server environment will be used and an empty string will be returned.
--
-- ObjC selector: @- serverEnvironmentIdentifier@
serverEnvironmentIdentifier :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
serverEnvironmentIdentifier pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata serverEnvironmentIdentifierSelector

-- | serverEnvironmentIdentifier: Identifier referencing the target server environment Apple Pay servers should reach out to to provision this pass. If not present, the default Apply Pay server environment will be used and an empty string will be returned.
--
-- ObjC selector: @- setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifier :: (IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata, IsNSString value) => pkIdentityDocumentMetadata -> value -> IO ()
setServerEnvironmentIdentifier pkIdentityDocumentMetadata value =
  sendMessage pkIdentityDocumentMetadata setServerEnvironmentIdentifierSelector (toNSString value)

-- | issuingCountryCode: identifies the issuing country of the identity document
--
-- ObjC selector: @- issuingCountryCode@
issuingCountryCode :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO (Id NSString)
issuingCountryCode pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata issuingCountryCodeSelector

-- | identityDocumentType: identifies the type of the identity document
--
-- ObjC selector: @- documentType@
documentType :: IsPKIdentityDocumentMetadata pkIdentityDocumentMetadata => pkIdentityDocumentMetadata -> IO PKAddIdentityDocumentType
documentType pkIdentityDocumentMetadata =
  sendMessage pkIdentityDocumentMetadata documentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIdentityDocumentMetadata)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKIdentityDocumentMetadata)
newSelector = mkSelector "new"

-- | @Selector@ for @credentialIdentifier@
credentialIdentifierSelector :: Selector '[] (Id NSString)
credentialIdentifierSelector = mkSelector "credentialIdentifier"

-- | @Selector@ for @sharingInstanceIdentifier@
sharingInstanceIdentifierSelector :: Selector '[] (Id NSString)
sharingInstanceIdentifierSelector = mkSelector "sharingInstanceIdentifier"

-- | @Selector@ for @cardTemplateIdentifier@
cardTemplateIdentifierSelector :: Selector '[] (Id NSString)
cardTemplateIdentifierSelector = mkSelector "cardTemplateIdentifier"

-- | @Selector@ for @cardConfigurationIdentifier@
cardConfigurationIdentifierSelector :: Selector '[] (Id NSString)
cardConfigurationIdentifierSelector = mkSelector "cardConfigurationIdentifier"

-- | @Selector@ for @serverEnvironmentIdentifier@
serverEnvironmentIdentifierSelector :: Selector '[] (Id NSString)
serverEnvironmentIdentifierSelector = mkSelector "serverEnvironmentIdentifier"

-- | @Selector@ for @setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifierSelector :: Selector '[Id NSString] ()
setServerEnvironmentIdentifierSelector = mkSelector "setServerEnvironmentIdentifier:"

-- | @Selector@ for @issuingCountryCode@
issuingCountryCodeSelector :: Selector '[] (Id NSString)
issuingCountryCodeSelector = mkSelector "issuingCountryCode"

-- | @Selector@ for @documentType@
documentTypeSelector :: Selector '[] PKAddIdentityDocumentType
documentTypeSelector = mkSelector "documentType"

