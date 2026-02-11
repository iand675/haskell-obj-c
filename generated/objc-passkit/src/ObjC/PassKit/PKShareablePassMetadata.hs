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
  , cardConfigurationIdentifier
  , requiresUnifiedAccessCapableDevice
  , setRequiresUnifiedAccessCapableDevice
  , passThumbnailImage
  , localizedDescription
  , ownerDisplayName
  , initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector
  , initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector
  , credentialIdentifierSelector
  , sharingInstanceIdentifierSelector
  , cardConfigurationIdentifierSelector
  , requiresUnifiedAccessCapableDeviceSelector
  , setRequiresUnifiedAccessCapableDeviceSelector
  , passThumbnailImageSelector
  , localizedDescriptionSelector
  , ownerDisplayNameSelector


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

-- | @- initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:@
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString cardConfigurationIdentifier, IsNSString sharingInstanceIdentifier, IsNSString ownerDisplayName, IsNSString localizedDescription) => pkShareablePassMetadata -> credentialIdentifier -> cardConfigurationIdentifier -> sharingInstanceIdentifier -> Ptr () -> ownerDisplayName -> localizedDescription -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription pkShareablePassMetadata  credentialIdentifier cardConfigurationIdentifier sharingInstanceIdentifier passThumbnailImage ownerDisplayName localizedDescription =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr cardConfigurationIdentifier $ \raw_cardConfigurationIdentifier ->
    withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
      withObjCPtr ownerDisplayName $ \raw_ownerDisplayName ->
        withObjCPtr localizedDescription $ \raw_localizedDescription ->
            sendMsg pkShareablePassMetadata (mkSelector "initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_cardConfigurationIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr passThumbnailImage, argPtr (castPtr raw_ownerDisplayName :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDevice :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString ownerDisplayName, IsNSString localizedDescription, IsNSString accountHash, IsNSString templateIdentifier, IsNSString relyingPartyIdentifier) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> Ptr () -> ownerDisplayName -> localizedDescription -> accountHash -> templateIdentifier -> relyingPartyIdentifier -> Bool -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDevice pkShareablePassMetadata  credentialIdentifier sharingInstanceIdentifier passThumbnailImage ownerDisplayName localizedDescription accountHash templateIdentifier relyingPartyIdentifier requiresUnifiedAccessCapableDevice =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr ownerDisplayName $ \raw_ownerDisplayName ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        withObjCPtr accountHash $ \raw_accountHash ->
          withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
            withObjCPtr relyingPartyIdentifier $ \raw_relyingPartyIdentifier ->
                sendMsg pkShareablePassMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr passThumbnailImage, argPtr (castPtr raw_ownerDisplayName :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ()), argPtr (castPtr raw_accountHash :: Ptr ()), argPtr (castPtr raw_templateIdentifier :: Ptr ()), argPtr (castPtr raw_relyingPartyIdentifier :: Ptr ()), argCULong (if requiresUnifiedAccessCapableDevice then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKShareablePassMetadataPreview preview) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_preview pkShareablePassMetadata  credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
      withObjCPtr preview $ \raw_preview ->
          sendMsg pkShareablePassMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr (castPtr raw_templateIdentifier :: Ptr ()), argPtr (castPtr raw_preview :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview :: (IsPKShareablePassMetadata pkShareablePassMetadata, IsNSString credentialIdentifier, IsNSString sharingInstanceIdentifier, IsNSString templateIdentifier, IsPKShareablePassMetadataPreview preview) => pkShareablePassMetadata -> credentialIdentifier -> sharingInstanceIdentifier -> templateIdentifier -> preview -> IO (Id PKShareablePassMetadata)
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_preview pkShareablePassMetadata  credentialIdentifier sharingInstanceIdentifier templateIdentifier preview =
withObjCPtr credentialIdentifier $ \raw_credentialIdentifier ->
  withObjCPtr sharingInstanceIdentifier $ \raw_sharingInstanceIdentifier ->
    withObjCPtr templateIdentifier $ \raw_templateIdentifier ->
      withObjCPtr preview $ \raw_preview ->
          sendMsg pkShareablePassMetadata (mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentifier :: Ptr ()), argPtr (castPtr raw_sharingInstanceIdentifier :: Ptr ()), argPtr (castPtr raw_templateIdentifier :: Ptr ()), argPtr (castPtr raw_preview :: Ptr ())] >>= ownedObject . castPtr

-- | @- credentialIdentifier@
credentialIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
credentialIdentifier pkShareablePassMetadata  =
  sendMsg pkShareablePassMetadata (mkSelector "credentialIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharingInstanceIdentifier@
sharingInstanceIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
sharingInstanceIdentifier pkShareablePassMetadata  =
  sendMsg pkShareablePassMetadata (mkSelector "sharingInstanceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cardConfigurationIdentifier@
cardConfigurationIdentifier :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
cardConfigurationIdentifier pkShareablePassMetadata  =
  sendMsg pkShareablePassMetadata (mkSelector "cardConfigurationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requiresUnifiedAccessCapableDevice@
requiresUnifiedAccessCapableDevice :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO Bool
requiresUnifiedAccessCapableDevice pkShareablePassMetadata  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkShareablePassMetadata (mkSelector "requiresUnifiedAccessCapableDevice") retCULong []

-- | @- setRequiresUnifiedAccessCapableDevice:@
setRequiresUnifiedAccessCapableDevice :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> Bool -> IO ()
setRequiresUnifiedAccessCapableDevice pkShareablePassMetadata  value =
  sendMsg pkShareablePassMetadata (mkSelector "setRequiresUnifiedAccessCapableDevice:") retVoid [argCULong (if value then 1 else 0)]

-- | @- passThumbnailImage@
passThumbnailImage :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Ptr ())
passThumbnailImage pkShareablePassMetadata  =
  fmap castPtr $ sendMsg pkShareablePassMetadata (mkSelector "passThumbnailImage") (retPtr retVoid) []

-- | @- localizedDescription@
localizedDescription :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
localizedDescription pkShareablePassMetadata  =
  sendMsg pkShareablePassMetadata (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ownerDisplayName@
ownerDisplayName :: IsPKShareablePassMetadata pkShareablePassMetadata => pkShareablePassMetadata -> IO (Id NSString)
ownerDisplayName pkShareablePassMetadata  =
  sendMsg pkShareablePassMetadata (mkSelector "ownerDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:@
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector :: Selector
initWithProvisioningCredentialIdentifier_cardConfigurationIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescriptionSelector = mkSelector "initWithProvisioningCredentialIdentifier:cardConfigurationIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_passThumbnailImage_ownerDisplayName_localizedDescription_accountHash_templateIdentifier_relyingPartyIdentifier_requiresUnifiedAccessCapableDeviceSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:passThumbnailImage:ownerDisplayName:localizedDescription:accountHash:templateIdentifier:relyingPartyIdentifier:requiresUnifiedAccessCapableDevice:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardTemplateIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardTemplateIdentifier:preview:"

-- | @Selector@ for @initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:@
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector :: Selector
initWithProvisioningCredentialIdentifier_sharingInstanceIdentifier_cardConfigurationIdentifier_previewSelector = mkSelector "initWithProvisioningCredentialIdentifier:sharingInstanceIdentifier:cardConfigurationIdentifier:preview:"

-- | @Selector@ for @credentialIdentifier@
credentialIdentifierSelector :: Selector
credentialIdentifierSelector = mkSelector "credentialIdentifier"

-- | @Selector@ for @sharingInstanceIdentifier@
sharingInstanceIdentifierSelector :: Selector
sharingInstanceIdentifierSelector = mkSelector "sharingInstanceIdentifier"

-- | @Selector@ for @cardConfigurationIdentifier@
cardConfigurationIdentifierSelector :: Selector
cardConfigurationIdentifierSelector = mkSelector "cardConfigurationIdentifier"

-- | @Selector@ for @requiresUnifiedAccessCapableDevice@
requiresUnifiedAccessCapableDeviceSelector :: Selector
requiresUnifiedAccessCapableDeviceSelector = mkSelector "requiresUnifiedAccessCapableDevice"

-- | @Selector@ for @setRequiresUnifiedAccessCapableDevice:@
setRequiresUnifiedAccessCapableDeviceSelector :: Selector
setRequiresUnifiedAccessCapableDeviceSelector = mkSelector "setRequiresUnifiedAccessCapableDevice:"

-- | @Selector@ for @passThumbnailImage@
passThumbnailImageSelector :: Selector
passThumbnailImageSelector = mkSelector "passThumbnailImage"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @ownerDisplayName@
ownerDisplayNameSelector :: Selector
ownerDisplayNameSelector = mkSelector "ownerDisplayName"

