{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKShareablePassMetadata@.
module ObjC.PassKit.PKShareablePassMetadata
  ( PKShareablePassMetadata
  , IsPKShareablePassMetadata(..)
  , initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDevice
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview
  , credentialIdentifier
  , sharingInstanceIdentifier
  , templateIdentifier
  , cardTemplateIdentifier
  , cardConfigurationIdentifier
  , requiresUnifiedAccessCapableDevice
  , setRequiresUnifiedAccessCapableDevice
  , serverEnvironmentIdentifier
  , setServerEnvironmentIdentifier
  , preview
  , passThumbnailImage
  , localizedDescription
  , ownerDisplayName
  , accountHash
  , setAccountHash
  , relyingPartyIdentifier
  , setRelyingPartyIdentifier
  , accountHashSelector
  , cardConfigurationIdentifierSelector
  , cardTemplateIdentifierSelector
  , credentialIdentifierSelector
  , initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector
  , localizedDescriptionSelector
  , ownerDisplayNameSelector
  , passThumbnailImageSelector
  , previewSelector
  , relyingPartyIdentifierSelector
  , requiresUnifiedAccessCapableDeviceSelector
  , serverEnvironmentIdentifierSelector
  , setAccountHashSelector
  , setRelyingPartyIdentifierSelector
  , setRequiresUnifiedAccessCapableDeviceSelector
  , setServerEnvironmentIdentifierSelector
  , sharingInstanceIdentifierSelector
  , templateIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:@
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString cardConfigurationIdentifier, IsNSString sharingInstanceIdentifier, IsNSString ownerDisplayName, IsNSString localizedDescription) => pkShareablePassMetadata -> credentialIdentifier -> cardConfigurationIdentifier -> sharingInstanceIdentifier -> Ptr () -> ownerDisplayName -> localizedDescription -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription pkShareablePassMetadata credentialIdentifier cardConfigurationIdentifier sharingInstanceIdentifier passThumbnailImage ownerDisplayName localizedDescription =
  sendOwnedMessage pkShareablePassMetadata initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector (toNSString credentialIdentifier) (toNSString cardConfigurationIdentifier) (toNSString sharingInstanceIdentifier) passThumbnailImage (toNSString ownerDisplayName) (toNSString localizedDescription)

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDevice :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString ownerDisplayName, IsNSString localizedDescription, IsNSString accountHash, IsNSString templateIdentifier, IsNSString relyingPartyIdentifier) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> Ptr () -> ownerDisplayName -> localizedDescription -> accountHash -> templateIdentifier -> relyingPartyIdentifier -> Bool -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDevice pkShareablePassMetadata credentialIdentifier sharingInstanceIdentifier passThumbnailImage ownerDisplayName localizedDescription accountHash templateIdentifier relyingPartyIdentifier requiresUnifiedAccessCapableDevice =
  sendOwnedMessage pkShareablePassMetadata initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector (toNSString credentialIdentifier) (toNSString sharingInstanceIdentifier) passThumbnailImage (toNSString ownerDisplayName) (toNSString localizedDescription) (toNSString accountHash) (toNSString templateIdentifier) (toNSString relyingPartyIdentifier) requiresUnifiedAccessCapableDevice

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKShareablePassMetadataPreview preview) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview pkShareablePassMetadata credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
  sendOwnedMessage pkShareablePassMetadata initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector (toNSString credentialIdentifier) (toNSString sharingInstanceIdentifier) (toNSString templateIdentifier) (toPKShareablePassMetadataPreview preview)

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKShareablePassMetadataPreview preview) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview pkShareablePassMetadata credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
  sendOwnedMessage pkShareablePassMetadata initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector (toNSString credentialIdentifier) (toNSString sharingInstanceIdentifier) (toNSString templateIdentifier) (toPKShareablePassMetadataPreview preview)

-- | @- credentialIdentifier@
credentialIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
credentialIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata credentialIdentifierSelector

-- | @- sharingInstanceIdentifier@
sharingInstanceIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
sharingInstanceIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata sharingInstanceIdentifierSelector

-- | @- templateIdentifier@
templateIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
templateIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata templateIdentifierSelector

-- | @- cardTemplateIdentifier@
cardTemplateIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
cardTemplateIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata cardTemplateIdentifierSelector

-- | @- cardConfigurationIdentifier@
cardConfigurationIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
cardConfigurationIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata cardConfigurationIdentifierSelector

-- | @- requiresUnifiedAccessCapableDevice@
requiresUnifiedAccessCapableDevice :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO Bool
requiresUnifiedAccessCapableDevice pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata requiresUnifiedAccessCapableDeviceSelector

-- | @- setRequiresUnifiedAccessCapableDevice:@
setRequiresUnifiedAccessCapableDevice :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> Bool -> IO ()
setRequiresUnifiedAccessCapableDevice pkShareablePassMetadata value =
  sendMessage pkShareablePassMetadata setRequiresUnifiedAccessCapableDeviceSelector value

-- | @- serverEnvironmentIdentifier@
serverEnvironmentIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
serverEnvironmentIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata serverEnvironmentIdentifierSelector

-- | @- setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifier :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString value) => pkShareablePassMetadata -> value -> IO ()
setServerEnvironmentIdentifier pkShareablePassMetadata value =
  sendMessage pkShareablePassMetadata setServerEnvironmentIdentifierSelector (toNSString value)

-- | @- preview@
preview :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id PKShareablePassMetadataPreview)
preview pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata previewSelector

-- | @- passThumbnailImage@
passThumbnailImage :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Ptr ())
passThumbnailImage pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata passThumbnailImageSelector

-- | @- localizedDescription@
localizedDescription :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
localizedDescription pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata localizedDescriptionSelector

-- | @- ownerDisplayName@
ownerDisplayName :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
ownerDisplayName pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata ownerDisplayNameSelector

-- | @- accountHash@
accountHash :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
accountHash pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata accountHashSelector

-- | @- setAccountHash:@
setAccountHash :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString value) => pkShareablePassMetadata -> value -> IO ()
setAccountHash pkShareablePassMetadata value =
  sendMessage pkShareablePassMetadata setAccountHashSelector (toNSString value)

-- | @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
relyingPartyIdentifier pkShareablePassMetadata =
  sendMessage pkShareablePassMetadata relyingPartyIdentifierSelector

-- | @- setRelyingPartyIdentifier:@
setRelyingPartyIdentifier :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString value) => pkShareablePassMetadata -> value -> IO ()
setRelyingPartyIdentifier pkShareablePassMetadata value =
  sendMessage pkShareablePassMetadata setRelyingPartyIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:@
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector :: Selector '[Id NSString, Id NSString, Id NSString, Ptr (), Id NSString, Id NSString] (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector = mkSelector "initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector :: Selector '[Id NSString, Id NSString, Ptr (), Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Bool] (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id PKShareablePassMetadataPreview] (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id PKShareablePassMetadataPreview] (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:"

-- | @Selector@ for @credentialIdentifier@
credentialIdentifierSelector :: Selector '[] (Id NSString)
credentialIdentifierSelector = mkSelector "credentialIdentifier"

-- | @Selector@ for @sharingInstanceIdentifier@
sharingInstanceIdentifierSelector :: Selector '[] (Id NSString)
sharingInstanceIdentifierSelector = mkSelector "sharingInstanceIdentifier"

-- | @Selector@ for @templateIdentifier@
templateIdentifierSelector :: Selector '[] (Id NSString)
templateIdentifierSelector = mkSelector "templateIdentifier"

-- | @Selector@ for @cardTemplateIdentifier@
cardTemplateIdentifierSelector :: Selector '[] (Id NSString)
cardTemplateIdentifierSelector = mkSelector "cardTemplateIdentifier"

-- | @Selector@ for @cardConfigurationIdentifier@
cardConfigurationIdentifierSelector :: Selector '[] (Id NSString)
cardConfigurationIdentifierSelector = mkSelector "cardConfigurationIdentifier"

-- | @Selector@ for @requiresUnifiedAccessCapableDevice@
requiresUnifiedAccessCapableDeviceSelector :: Selector '[] Bool
requiresUnifiedAccessCapableDeviceSelector = mkSelector "requiresUnifiedAccessCapableDevice"

-- | @Selector@ for @setRequiresUnifiedAccessCapableDevice:@
setRequiresUnifiedAccessCapableDeviceSelector :: Selector '[Bool] ()
setRequiresUnifiedAccessCapableDeviceSelector = mkSelector "setRequiresUnifiedAccessCapableDevice:"

-- | @Selector@ for @serverEnvironmentIdentifier@
serverEnvironmentIdentifierSelector :: Selector '[] (Id NSString)
serverEnvironmentIdentifierSelector = mkSelector "serverEnvironmentIdentifier"

-- | @Selector@ for @setServerEnvironmentIdentifier:@
setServerEnvironmentIdentifierSelector :: Selector '[Id NSString] ()
setServerEnvironmentIdentifierSelector = mkSelector "setServerEnvironmentIdentifier:"

-- | @Selector@ for @preview@
previewSelector :: Selector '[] (Id PKShareablePassMetadataPreview)
previewSelector = mkSelector "preview"

-- | @Selector@ for @passThumbnailImage@
passThumbnailImageSelector :: Selector '[] (Ptr ())
passThumbnailImageSelector = mkSelector "passThumbnailImage"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @ownerDisplayName@
ownerDisplayNameSelector :: Selector '[] (Id NSString)
ownerDisplayNameSelector = mkSelector "ownerDisplayName"

-- | @Selector@ for @accountHash@
accountHashSelector :: Selector '[] (Id NSString)
accountHashSelector = mkSelector "accountHash"

-- | @Selector@ for @setAccountHash:@
setAccountHashSelector :: Selector '[Id NSString] ()
setAccountHashSelector = mkSelector "setAccountHash:"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector '[] (Id NSString)
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

-- | @Selector@ for @setRelyingPartyIdentifier:@
setRelyingPartyIdentifierSelector :: Selector '[Id NSString] ()
setRelyingPartyIdentifierSelector = mkSelector "setRelyingPartyIdentifier:"

