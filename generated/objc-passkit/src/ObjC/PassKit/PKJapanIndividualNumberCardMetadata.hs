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
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector
  , authenticationPasswordSelector
  , setAuthenticationPasswordSelector
  , signingPasswordSelector
  , setSigningPasswordSelector
  , previewSelector
  , setPreviewSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initialize with parameters configured by issuer's server to indicate the specific product instance to provision. - Properties:   - provisioningCredentialIdentifier: Identifies this user's instance for provisioning.   - sharingInstanceIdentifier: A short lived token to prevent replay-ability.   - cardTemplateIdentifier: An identifier for a legacy product on our Apple Pay servers.   - preview: Object containing information to represent the pass to provision in our UI.
--
-- ObjC selector: @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKAddPassMetadataPreview preview) => pkJapanIndividualNumberCardMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview pkJapanIndividualNumberCardMetadata  credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
      withObjCPtr preview $ \raw_preview ->
          sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr (castPtr raw_templateIdentifier :: Ptr ()), argPtr (castPtr raw_preview :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize with parameters configured by issuer's server to indicate the specific product instance to provision. - Properties:   - provisioningCredentialIdentifier: Identifies this user's instance for provisioning.   - sharingInstanceIdentifier: A short lived token to prevent replay-ability.   - cardConfigurationIdentifier: An identifier for a product on our Apple Pay servers.   - preview: Object containing information to represent the pass to provision in our UI.
--
-- ObjC selector: @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString cardConfigurationIdentifier, IsPKAddPassMetadataPreview preview) => pkJapanIndividualNumberCardMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> cardConfigurationIdentifier -> preview -> IO (Id PKJapanIndividualNumberCardMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview pkJapanIndividualNumberCardMetadata  credentialIdentifier sharingInstanceIdentifier cardConfigurationIdentifier preview =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr cardConfigurationIdentifier $ \raw_cardConfigurationIdentifier ->
      withObjCPtr preview $ \raw_preview ->
          sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr (castPtr raw_cardConfigurationIdentifier :: Ptr ()), argPtr (castPtr raw_preview :: Ptr ())] >>= ownedObject . castPtr

-- | Raw authentication password used to protect authentication functionality. If configured in the pass, this functionality allows users to present their identity credentials to external parties.
--
-- ObjC selector: @- authenticationPassword@
authenticationPassword :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id NSString)
authenticationPassword pkJapanIndividualNumberCardMetadata  =
  sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "authenticationPassword") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Raw authentication password used to protect authentication functionality. If configured in the pass, this functionality allows users to present their identity credentials to external parties.
--
-- ObjC selector: @- setAuthenticationPassword:@
setAuthenticationPassword :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setAuthenticationPassword pkJapanIndividualNumberCardMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "setAuthenticationPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Raw signing password used to protect signing functionality. If configured in the pass, this functionality allows users to digitally sign with external parties or print officially signed documents.
--
-- ObjC selector: @- signingPassword@
signingPassword :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id NSString)
signingPassword pkJapanIndividualNumberCardMetadata  =
  sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "signingPassword") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Raw signing password used to protect signing functionality. If configured in the pass, this functionality allows users to digitally sign with external parties or print officially signed documents.
--
-- ObjC selector: @- setSigningPassword:@
setSigningPassword :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsNSString value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setSigningPassword pkJapanIndividualNumberCardMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "setSigningPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | preview: A preview object containing the necessary information to represent the pass during provisioning.
--
-- ObjC selector: @- preview@
preview :: IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata => pkJapanIndividualNumberCardMetadata -> IO (Id PKAddPassMetadataPreview)
preview pkJapanIndividualNumberCardMetadata  =
  sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "preview") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preview: A preview object containing the necessary information to represent the pass during provisioning.
--
-- ObjC selector: @- setPreview:@
setPreview :: (IsPKJapanIndividualNumberCardMetadata pkJapanIndividualNumberCardMetadata, IsPKAddPassMetadataPreview value) => pkJapanIndividualNumberCardMetadata -> value -> IO ()
setPreview pkJapanIndividualNumberCardMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkJapanIndividualNumberCardMetadata (mkSelector "setPreview:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:"

-- | @Selector@ for @authenticationPassword@
authenticationPasswordSelector :: Selector
authenticationPasswordSelector = mkSelector "authenticationPassword"

-- | @Selector@ for @setAuthenticationPassword:@
setAuthenticationPasswordSelector :: Selector
setAuthenticationPasswordSelector = mkSelector "setAuthenticationPassword:"

-- | @Selector@ for @signingPassword@
signingPasswordSelector :: Selector
signingPasswordSelector = mkSelector "signingPassword"

-- | @Selector@ for @setSigningPassword:@
setSigningPasswordSelector :: Selector
setSigningPasswordSelector = mkSelector "setSigningPassword:"

-- | @Selector@ for @preview@
previewSelector :: Selector
previewSelector = mkSelector "preview"

-- | @Selector@ for @setPreview:@
setPreviewSelector :: Selector
setPreviewSelector = mkSelector "setPreview:"

