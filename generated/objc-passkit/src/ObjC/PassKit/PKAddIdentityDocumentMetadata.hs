{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddIdentityDocumentMetadata@.
module ObjC.PassKit.PKAddIdentityDocumentMetadata
  ( PKAddIdentityDocumentMetadata
  , IsPKAddIdentityDocumentMetadata(..)
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_preview
  , preview
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_previewSelector
  , previewSelector

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

-- | Initialize with parameters configured by issuer's server to indicate the specific product instance to provision. - Properties:   - provisioningCredentialIdentifier: Identifies this user's instance for provisioning.   - sharingInstanceIdentifier: A short lived token to prevent replay-ability.   - cardTemplateIdentifier: An identifier for a legacy product on our Apple Pay servers.   - preview: Object containing information to represent the pass to provision in our UI.   - issuingCountryCode:  identifies the issuing country of the identity document   - identityDocumentType: identifies the type of the identity document   - preview: Object containing information to represent the pass to provision in our UI.
--
-- ObjC selector: @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:issuingCountryCode:documentType:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_preview :: (IsPKAddIdentityDocumentMetadata pkAddIdentityDocumentMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsNSString issuingCountryCode, IsPKAddPassMetadataPreview preview) => pkAddIdentityDocumentMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> issuingCountryCode -> PKAddIdentityDocumentType -> preview -> IO (Id PKAddIdentityDocumentMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_preview pkAddIdentityDocumentMetadata  credentialIdentifier sharingInstanceIdentifier templateIdentifier issuingCountryCode documentType preview =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
      withObjCPtr issuingCountryCode $ \raw_issuingCountryCode ->
        withObjCPtr preview $ \raw_preview ->
            sendMsg pkAddIdentityDocumentMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:issuingCountryCode:documentType:preview:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr (castPtr raw_templateIdentifier :: Ptr ()), argPtr (castPtr raw_issuingCountryCode :: Ptr ()), argCLong (coerce documentType), argPtr (castPtr raw_preview :: Ptr ())] >>= ownedObject . castPtr

-- | preview: A preview object containing the necessary information to represent the pass during provisioning.
--
-- ObjC selector: @- preview@
preview :: IsPKAddIdentityDocumentMetadata pkAddIdentityDocumentMetadata => pkAddIdentityDocumentMetadata -> IO (Id PKAddPassMetadataPreview)
preview pkAddIdentityDocumentMetadata  =
  sendMsg pkAddIdentityDocumentMetadata (mkSelector "preview") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:issuingCountryCode:documentType:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_previewSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_issuingCountryCode_documentType_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:issuingCountryCode:documentType:preview:"

-- | @Selector@ for @preview@
previewSelector :: Selector
previewSelector = mkSelector "preview"

