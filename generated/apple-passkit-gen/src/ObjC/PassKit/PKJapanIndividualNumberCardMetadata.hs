{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKJapanIndividualNumberCardMetadata@.
module ObjC.PassKit.PKJapanIndividualNumberCardMetadata
  ( PKJapanIndividualNumberCardMetadata
  , IsPKJapanIndividualNumberCardMetadata(..)
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview
  , authenticationPassword
  , setAuthenticationPassword
  , signingPassword
  , setSigningPassword
  , preview
  , setPreview
  , authenticationPasswordSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector
  , previewSelector
  , setAuthenticationPasswordSelector
  , setPreviewSelector
  , setSigningPasswordSelector
  , signingPasswordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize with parameters configured by issuer's server to indicate the specific product instance to provision. - Properties:   - provisioningCredentialIdentifier: Identifies this user's instance for provisioning.   - sharingInstanceIdentifier: A short lived token to prevent replay-ability.   - cardTemplateIdentifier: An identifier for a legacy product on our Apple Pay servers.   - preview: Object containing information to represent the pass to provision in our UI.
--
-- ObjC selector: @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKAddPassMetadataPreview preview) => pkJapanIndividualNumberCardMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview pkJapanIndividualNumberCardMetadata credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
  sendOwnedMessage pkJapanIndividualNumberCardMetadata initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector (toNSString credentialIdentifier) (toNSString sharingInstanceIdentifier) (toNSString templateIdentifier) (toPKAddPassMetadataPreview preview)

-- | Initialize with parameters configured by issuer's server to indicate the specific product instance to provision. - Properties:   - provisioningCredentialIdentifier: Identifies this user's instance for provisioning.   - sharingInstanceIdentifier: A short lived token to prevent replay-ability.   - cardConfigurationIdentifier: An identifier for a product on our Apple Pay servers.   - preview: Object containing information to represent the pass to provision in our UI.
--
-- ObjC selector: @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString cardConfigurationIdentifier, IsPKAddPassMetadataPreview preview) => pkJapanIndividualNumberCardMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> cardConfigurationIdentifier -> preview -> IO (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview pkJapanIndividualNumberCardMetadata credentialIdentifier sharingInstanceIdentifier cardConfigurationIdentifier preview =
  sendOwnedMessage pkJapanIndividualNumberCardMetadata initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector (toNSString credentialIdentifier) (toNSString sharingInstanceIdentifier) (toNSString cardConfigurationIdentifier) (toPKAddPassMetadataPreview preview)

-- | Raw authentication password used to protect authentication functionality. If configured in the pass, this functionality allows users to present their identity credentials to external parties.
--
-- ObjC selector: @- authenticationPassword@
authenticationPassword :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id NSString)
authenticationPassword pkJapanIndividualNumberCardMetadata =
  sendMessage pkJapanIndividualNumberCardMetadata authenticationPasswordSelector

-- | Raw authentication password used to protect authentication functionality. If configured in the pass, this functionality allows users to present their identity credentials to external parties.
--
-- ObjC selector: @- setAuthenticationPassword:@
setAuthenticationPassword :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setAuthenticationPassword pkJapanIndividualNumberCardMetadata value =
  sendMessage pkJapanIndividualNumberCardMetadata setAuthenticationPasswordSelector (toNSString value)

-- | Raw signing password used to protect signing functionality. If configured in the pass, this functionality allows users to digitally sign with external parties or print officially signed documents.
--
-- ObjC selector: @- signingPassword@
signingPassword :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id NSString)
signingPassword pkJapanIndividualNumberCardMetadata =
  sendMessage pkJapanIndividualNumberCardMetadata signingPasswordSelector

-- | Raw signing password used to protect signing functionality. If configured in the pass, this functionality allows users to digitally sign with external parties or print officially signed documents.
--
-- ObjC selector: @- setSigningPassword:@
setSigningPassword :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setSigningPassword pkJapanIndividualNumberCardMetadata value =
  sendMessage pkJapanIndividualNumberCardMetadata setSigningPasswordSelector (toNSString value)

-- | preview: A preview object containing the necessary information to represent the pass during provisioning.
--
-- ObjC selector: @- preview@
preview :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id PKAddPassMetadataPreview)
preview pkJapanIndividualNumberCardMetadata =
  sendMessage pkJapanIndividualNumberCardMetadata previewSelector

-- | preview: A preview object containing the necessary information to represent the pass during provisioning.
--
-- ObjC selector: @- setPreview:@
setPreview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsPKAddPassMetadataPreview value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setPreview pkJapanIndividualNumberCardMetadata value =
  sendMessage pkJapanIndividualNumberCardMetadata setPreviewSelector (toPKAddPassMetadataPreview value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id PKAddPassMetadataPreview] (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id PKAddPassMetadataPreview] (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:"

-- | @Selector@ for @authenticationPassword@
authenticationPasswordSelector :: Selector '[] (Id NSString)
authenticationPasswordSelector = mkSelector "authenticationPassword"

-- | @Selector@ for @setAuthenticationPassword:@
setAuthenticationPasswordSelector :: Selector '[Id NSString] ()
setAuthenticationPasswordSelector = mkSelector "setAuthenticationPassword:"

-- | @Selector@ for @signingPassword@
signingPasswordSelector :: Selector '[] (Id NSString)
signingPasswordSelector = mkSelector "signingPassword"

-- | @Selector@ for @setSigningPassword:@
setSigningPasswordSelector :: Selector '[Id NSString] ()
setSigningPasswordSelector = mkSelector "setSigningPassword:"

-- | @Selector@ for @preview@
previewSelector :: Selector '[] (Id PKAddPassMetadataPreview)
previewSelector = mkSelector "preview"

-- | @Selector@ for @setPreview:@
setPreviewSelector :: Selector '[Id PKAddPassMetadataPreview] ()
setPreviewSelector = mkSelector "setPreview:"

