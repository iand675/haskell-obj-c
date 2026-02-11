{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PassKit.Internal.Classes (
    module ObjC.PassKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- PKPass ----------

-- | Phantom type for @PKPass@.
data PKPass

instance IsObjCObject (Id PKPass) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPass"

class IsPKObject a => IsPKPass a where
  toPKPass :: a -> Id PKPass

instance IsPKPass (Id PKPass) where
  toPKPass = unsafeCastId

instance IsPKObject (Id PKPass) where
  toPKObject = unsafeCastId

-- ---------- PKAddPassMetadataPreview ----------

-- | Phantom type for @PKAddPassMetadataPreview@.
data PKAddPassMetadataPreview

instance IsObjCObject (Id PKAddPassMetadataPreview) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddPassMetadataPreview"

class IsNSObject a => IsPKAddPassMetadataPreview a where
  toPKAddPassMetadataPreview :: a -> Id PKAddPassMetadataPreview

instance IsPKAddPassMetadataPreview (Id PKAddPassMetadataPreview) where
  toPKAddPassMetadataPreview = unsafeCastId

instance IsNSObject (Id PKAddPassMetadataPreview) where
  toNSObject = unsafeCastId

-- ---------- PKAddPaymentPassRequest ----------

-- | Phantom type for @PKAddPaymentPassRequest@.
data PKAddPaymentPassRequest

instance IsObjCObject (Id PKAddPaymentPassRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddPaymentPassRequest"

class IsNSObject a => IsPKAddPaymentPassRequest a where
  toPKAddPaymentPassRequest :: a -> Id PKAddPaymentPassRequest

instance IsPKAddPaymentPassRequest (Id PKAddPaymentPassRequest) where
  toPKAddPaymentPassRequest = unsafeCastId

instance IsNSObject (Id PKAddPaymentPassRequest) where
  toNSObject = unsafeCastId

-- ---------- PKAddPaymentPassRequestConfiguration ----------

-- | Phantom type for @PKAddPaymentPassRequestConfiguration@.
data PKAddPaymentPassRequestConfiguration

instance IsObjCObject (Id PKAddPaymentPassRequestConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddPaymentPassRequestConfiguration"

class IsNSObject a => IsPKAddPaymentPassRequestConfiguration a where
  toPKAddPaymentPassRequestConfiguration :: a -> Id PKAddPaymentPassRequestConfiguration

instance IsPKAddPaymentPassRequestConfiguration (Id PKAddPaymentPassRequestConfiguration) where
  toPKAddPaymentPassRequestConfiguration = unsafeCastId

instance IsNSObject (Id PKAddPaymentPassRequestConfiguration) where
  toNSObject = unsafeCastId

-- ---------- PKAddSecureElementPassConfiguration ----------

-- | Phantom type for @PKAddSecureElementPassConfiguration@.
data PKAddSecureElementPassConfiguration

instance IsObjCObject (Id PKAddSecureElementPassConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddSecureElementPassConfiguration"

class IsNSObject a => IsPKAddSecureElementPassConfiguration a where
  toPKAddSecureElementPassConfiguration :: a -> Id PKAddSecureElementPassConfiguration

instance IsPKAddSecureElementPassConfiguration (Id PKAddSecureElementPassConfiguration) where
  toPKAddSecureElementPassConfiguration = unsafeCastId

instance IsNSObject (Id PKAddSecureElementPassConfiguration) where
  toNSObject = unsafeCastId

-- ---------- PKAutomaticReloadPaymentRequest ----------

-- | Phantom type for @PKAutomaticReloadPaymentRequest@.
data PKAutomaticReloadPaymentRequest

instance IsObjCObject (Id PKAutomaticReloadPaymentRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAutomaticReloadPaymentRequest"

class IsNSObject a => IsPKAutomaticReloadPaymentRequest a where
  toPKAutomaticReloadPaymentRequest :: a -> Id PKAutomaticReloadPaymentRequest

instance IsPKAutomaticReloadPaymentRequest (Id PKAutomaticReloadPaymentRequest) where
  toPKAutomaticReloadPaymentRequest = unsafeCastId

instance IsNSObject (Id PKAutomaticReloadPaymentRequest) where
  toNSObject = unsafeCastId

-- ---------- PKBarcodeEventConfigurationRequest ----------

-- | Phantom type for @PKBarcodeEventConfigurationRequest@.
data PKBarcodeEventConfigurationRequest

instance IsObjCObject (Id PKBarcodeEventConfigurationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKBarcodeEventConfigurationRequest"

class IsNSObject a => IsPKBarcodeEventConfigurationRequest a where
  toPKBarcodeEventConfigurationRequest :: a -> Id PKBarcodeEventConfigurationRequest

instance IsPKBarcodeEventConfigurationRequest (Id PKBarcodeEventConfigurationRequest) where
  toPKBarcodeEventConfigurationRequest = unsafeCastId

instance IsNSObject (Id PKBarcodeEventConfigurationRequest) where
  toNSObject = unsafeCastId

-- ---------- PKBarcodeEventMetadataRequest ----------

-- | Phantom type for @PKBarcodeEventMetadataRequest@.
data PKBarcodeEventMetadataRequest

instance IsObjCObject (Id PKBarcodeEventMetadataRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKBarcodeEventMetadataRequest"

class IsNSObject a => IsPKBarcodeEventMetadataRequest a where
  toPKBarcodeEventMetadataRequest :: a -> Id PKBarcodeEventMetadataRequest

instance IsPKBarcodeEventMetadataRequest (Id PKBarcodeEventMetadataRequest) where
  toPKBarcodeEventMetadataRequest = unsafeCastId

instance IsNSObject (Id PKBarcodeEventMetadataRequest) where
  toNSObject = unsafeCastId

-- ---------- PKBarcodeEventMetadataResponse ----------

-- | Phantom type for @PKBarcodeEventMetadataResponse@.
data PKBarcodeEventMetadataResponse

instance IsObjCObject (Id PKBarcodeEventMetadataResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKBarcodeEventMetadataResponse"

class IsNSObject a => IsPKBarcodeEventMetadataResponse a where
  toPKBarcodeEventMetadataResponse :: a -> Id PKBarcodeEventMetadataResponse

instance IsPKBarcodeEventMetadataResponse (Id PKBarcodeEventMetadataResponse) where
  toPKBarcodeEventMetadataResponse = unsafeCastId

instance IsNSObject (Id PKBarcodeEventMetadataResponse) where
  toNSObject = unsafeCastId

-- ---------- PKBarcodeEventSignatureRequest ----------

-- | Phantom type for @PKBarcodeEventSignatureRequest@.
data PKBarcodeEventSignatureRequest

instance IsObjCObject (Id PKBarcodeEventSignatureRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKBarcodeEventSignatureRequest"

class IsNSObject a => IsPKBarcodeEventSignatureRequest a where
  toPKBarcodeEventSignatureRequest :: a -> Id PKBarcodeEventSignatureRequest

instance IsPKBarcodeEventSignatureRequest (Id PKBarcodeEventSignatureRequest) where
  toPKBarcodeEventSignatureRequest = unsafeCastId

instance IsNSObject (Id PKBarcodeEventSignatureRequest) where
  toNSObject = unsafeCastId

-- ---------- PKBarcodeEventSignatureResponse ----------

-- | Phantom type for @PKBarcodeEventSignatureResponse@.
data PKBarcodeEventSignatureResponse

instance IsObjCObject (Id PKBarcodeEventSignatureResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKBarcodeEventSignatureResponse"

class IsNSObject a => IsPKBarcodeEventSignatureResponse a where
  toPKBarcodeEventSignatureResponse :: a -> Id PKBarcodeEventSignatureResponse

instance IsPKBarcodeEventSignatureResponse (Id PKBarcodeEventSignatureResponse) where
  toPKBarcodeEventSignatureResponse = unsafeCastId

instance IsNSObject (Id PKBarcodeEventSignatureResponse) where
  toNSObject = unsafeCastId

-- ---------- PKContact ----------

-- | Phantom type for @PKContact@.
data PKContact

instance IsObjCObject (Id PKContact) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKContact"

class IsNSObject a => IsPKContact a where
  toPKContact :: a -> Id PKContact

instance IsPKContact (Id PKContact) where
  toPKContact = unsafeCastId

instance IsNSObject (Id PKContact) where
  toNSObject = unsafeCastId

-- ---------- PKDateComponentsRange ----------

-- | Phantom type for @PKDateComponentsRange@.
data PKDateComponentsRange

instance IsObjCObject (Id PKDateComponentsRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDateComponentsRange"

class IsNSObject a => IsPKDateComponentsRange a where
  toPKDateComponentsRange :: a -> Id PKDateComponentsRange

instance IsPKDateComponentsRange (Id PKDateComponentsRange) where
  toPKDateComponentsRange = unsafeCastId

instance IsNSObject (Id PKDateComponentsRange) where
  toNSObject = unsafeCastId

-- ---------- PKDeferredPaymentRequest ----------

-- | Phantom type for @PKDeferredPaymentRequest@.
data PKDeferredPaymentRequest

instance IsObjCObject (Id PKDeferredPaymentRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDeferredPaymentRequest"

class IsNSObject a => IsPKDeferredPaymentRequest a where
  toPKDeferredPaymentRequest :: a -> Id PKDeferredPaymentRequest

instance IsPKDeferredPaymentRequest (Id PKDeferredPaymentRequest) where
  toPKDeferredPaymentRequest = unsafeCastId

instance IsNSObject (Id PKDeferredPaymentRequest) where
  toNSObject = unsafeCastId

-- ---------- PKDisbursementRequest ----------

-- | Phantom type for @PKDisbursementRequest@.
data PKDisbursementRequest

instance IsObjCObject (Id PKDisbursementRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDisbursementRequest"

class IsNSObject a => IsPKDisbursementRequest a where
  toPKDisbursementRequest :: a -> Id PKDisbursementRequest

instance IsPKDisbursementRequest (Id PKDisbursementRequest) where
  toPKDisbursementRequest = unsafeCastId

instance IsNSObject (Id PKDisbursementRequest) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityAnyOfDescriptor ----------

-- | Used to request information from multiple identity documents.
-- 
-- Phantom type for @PKIdentityAnyOfDescriptor@.
data PKIdentityAnyOfDescriptor

instance IsObjCObject (Id PKIdentityAnyOfDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityAnyOfDescriptor"

class IsNSObject a => IsPKIdentityAnyOfDescriptor a where
  toPKIdentityAnyOfDescriptor :: a -> Id PKIdentityAnyOfDescriptor

instance IsPKIdentityAnyOfDescriptor (Id PKIdentityAnyOfDescriptor) where
  toPKIdentityAnyOfDescriptor = unsafeCastId

instance IsNSObject (Id PKIdentityAnyOfDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityAuthorizationController ----------

-- | Used to request information from an identity document stored as a Wallet pass.
-- 
-- Phantom type for @PKIdentityAuthorizationController@.
data PKIdentityAuthorizationController

instance IsObjCObject (Id PKIdentityAuthorizationController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityAuthorizationController"

class IsNSObject a => IsPKIdentityAuthorizationController a where
  toPKIdentityAuthorizationController :: a -> Id PKIdentityAuthorizationController

instance IsPKIdentityAuthorizationController (Id PKIdentityAuthorizationController) where
  toPKIdentityAuthorizationController = unsafeCastId

instance IsNSObject (Id PKIdentityAuthorizationController) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityDocument ----------

-- | Represents the response of a request for an identity document.
-- 
-- Phantom type for @PKIdentityDocument@.
data PKIdentityDocument

instance IsObjCObject (Id PKIdentityDocument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityDocument"

class IsNSObject a => IsPKIdentityDocument a where
  toPKIdentityDocument :: a -> Id PKIdentityDocument

instance IsPKIdentityDocument (Id PKIdentityDocument) where
  toPKIdentityDocument = unsafeCastId

instance IsNSObject (Id PKIdentityDocument) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityDocumentMetadata ----------

-- | Phantom type for @PKIdentityDocumentMetadata@.
data PKIdentityDocumentMetadata

instance IsObjCObject (Id PKIdentityDocumentMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityDocumentMetadata"

class IsNSObject a => IsPKIdentityDocumentMetadata a where
  toPKIdentityDocumentMetadata :: a -> Id PKIdentityDocumentMetadata

instance IsPKIdentityDocumentMetadata (Id PKIdentityDocumentMetadata) where
  toPKIdentityDocumentMetadata = unsafeCastId

instance IsNSObject (Id PKIdentityDocumentMetadata) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityDriversLicenseDescriptor ----------

-- | Used to request information from a user's driver's license (or equivalent document).
-- 
-- Phantom type for @PKIdentityDriversLicenseDescriptor@.
data PKIdentityDriversLicenseDescriptor

instance IsObjCObject (Id PKIdentityDriversLicenseDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityDriversLicenseDescriptor"

class IsNSObject a => IsPKIdentityDriversLicenseDescriptor a where
  toPKIdentityDriversLicenseDescriptor :: a -> Id PKIdentityDriversLicenseDescriptor

instance IsPKIdentityDriversLicenseDescriptor (Id PKIdentityDriversLicenseDescriptor) where
  toPKIdentityDriversLicenseDescriptor = unsafeCastId

instance IsNSObject (Id PKIdentityDriversLicenseDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityElement ----------

-- | Elements that can be requested from identity documents. Not all elements are supported by all document types. If an element is requested from a type that does not support it, the element is ignored.
-- 
-- Phantom type for @PKIdentityElement@.
data PKIdentityElement

instance IsObjCObject (Id PKIdentityElement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityElement"

class IsNSObject a => IsPKIdentityElement a where
  toPKIdentityElement :: a -> Id PKIdentityElement

instance IsPKIdentityElement (Id PKIdentityElement) where
  toPKIdentityElement = unsafeCastId

instance IsNSObject (Id PKIdentityElement) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityIntentToStore ----------

-- | Indicates your intention to store an identity element. This covers not only the element value, but also information derived from the element value such as signatures or digests.
-- 
-- Phantom type for @PKIdentityIntentToStore@.
data PKIdentityIntentToStore

instance IsObjCObject (Id PKIdentityIntentToStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityIntentToStore"

class IsNSObject a => IsPKIdentityIntentToStore a where
  toPKIdentityIntentToStore :: a -> Id PKIdentityIntentToStore

instance IsPKIdentityIntentToStore (Id PKIdentityIntentToStore) where
  toPKIdentityIntentToStore = unsafeCastId

instance IsNSObject (Id PKIdentityIntentToStore) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityNationalIDCardDescriptor ----------

-- | Used to request information from a user's national id card (or equivalent document).
-- 
-- Phantom type for @PKIdentityNationalIDCardDescriptor@.
data PKIdentityNationalIDCardDescriptor

instance IsObjCObject (Id PKIdentityNationalIDCardDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityNationalIDCardDescriptor"

class IsNSObject a => IsPKIdentityNationalIDCardDescriptor a where
  toPKIdentityNationalIDCardDescriptor :: a -> Id PKIdentityNationalIDCardDescriptor

instance IsPKIdentityNationalIDCardDescriptor (Id PKIdentityNationalIDCardDescriptor) where
  toPKIdentityNationalIDCardDescriptor = unsafeCastId

instance IsNSObject (Id PKIdentityNationalIDCardDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityPhotoIDDescriptor ----------

-- | Used to request information from a user's photo ID (or equivalent document).
-- 
-- Phantom type for @PKIdentityPhotoIDDescriptor@.
data PKIdentityPhotoIDDescriptor

instance IsObjCObject (Id PKIdentityPhotoIDDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityPhotoIDDescriptor"

class IsNSObject a => IsPKIdentityPhotoIDDescriptor a where
  toPKIdentityPhotoIDDescriptor :: a -> Id PKIdentityPhotoIDDescriptor

instance IsPKIdentityPhotoIDDescriptor (Id PKIdentityPhotoIDDescriptor) where
  toPKIdentityPhotoIDDescriptor = unsafeCastId

instance IsNSObject (Id PKIdentityPhotoIDDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PKIdentityRequest ----------

-- | Request for information from an identity document stored as a Wallet pass.
-- 
-- Phantom type for @PKIdentityRequest@.
data PKIdentityRequest

instance IsObjCObject (Id PKIdentityRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIdentityRequest"

class IsNSObject a => IsPKIdentityRequest a where
  toPKIdentityRequest :: a -> Id PKIdentityRequest

instance IsPKIdentityRequest (Id PKIdentityRequest) where
  toPKIdentityRequest = unsafeCastId

instance IsNSObject (Id PKIdentityRequest) where
  toNSObject = unsafeCastId

-- ---------- PKIssuerProvisioningExtensionHandler ----------

-- | Phantom type for @PKIssuerProvisioningExtensionHandler@.
data PKIssuerProvisioningExtensionHandler

instance IsObjCObject (Id PKIssuerProvisioningExtensionHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIssuerProvisioningExtensionHandler"

class IsNSObject a => IsPKIssuerProvisioningExtensionHandler a where
  toPKIssuerProvisioningExtensionHandler :: a -> Id PKIssuerProvisioningExtensionHandler

instance IsPKIssuerProvisioningExtensionHandler (Id PKIssuerProvisioningExtensionHandler) where
  toPKIssuerProvisioningExtensionHandler = unsafeCastId

instance IsNSObject (Id PKIssuerProvisioningExtensionHandler) where
  toNSObject = unsafeCastId

-- ---------- PKIssuerProvisioningExtensionPassEntry ----------

-- | Phantom type for @PKIssuerProvisioningExtensionPassEntry@.
data PKIssuerProvisioningExtensionPassEntry

instance IsObjCObject (Id PKIssuerProvisioningExtensionPassEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIssuerProvisioningExtensionPassEntry"

class IsNSObject a => IsPKIssuerProvisioningExtensionPassEntry a where
  toPKIssuerProvisioningExtensionPassEntry :: a -> Id PKIssuerProvisioningExtensionPassEntry

instance IsPKIssuerProvisioningExtensionPassEntry (Id PKIssuerProvisioningExtensionPassEntry) where
  toPKIssuerProvisioningExtensionPassEntry = unsafeCastId

instance IsNSObject (Id PKIssuerProvisioningExtensionPassEntry) where
  toNSObject = unsafeCastId

-- ---------- PKIssuerProvisioningExtensionStatus ----------

-- | Phantom type for @PKIssuerProvisioningExtensionStatus@.
data PKIssuerProvisioningExtensionStatus

instance IsObjCObject (Id PKIssuerProvisioningExtensionStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIssuerProvisioningExtensionStatus"

class IsNSObject a => IsPKIssuerProvisioningExtensionStatus a where
  toPKIssuerProvisioningExtensionStatus :: a -> Id PKIssuerProvisioningExtensionStatus

instance IsPKIssuerProvisioningExtensionStatus (Id PKIssuerProvisioningExtensionStatus) where
  toPKIssuerProvisioningExtensionStatus = unsafeCastId

instance IsNSObject (Id PKIssuerProvisioningExtensionStatus) where
  toNSObject = unsafeCastId

-- ---------- PKLabeledValue ----------

-- | Phantom type for @PKLabeledValue@.
data PKLabeledValue

instance IsObjCObject (Id PKLabeledValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKLabeledValue"

class IsNSObject a => IsPKLabeledValue a where
  toPKLabeledValue :: a -> Id PKLabeledValue

instance IsPKLabeledValue (Id PKLabeledValue) where
  toPKLabeledValue = unsafeCastId

instance IsNSObject (Id PKLabeledValue) where
  toNSObject = unsafeCastId

-- ---------- PKPassLibrary ----------

-- | Phantom type for @PKPassLibrary@.
data PKPassLibrary

instance IsObjCObject (Id PKPassLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPassLibrary"

class IsNSObject a => IsPKPassLibrary a where
  toPKPassLibrary :: a -> Id PKPassLibrary

instance IsPKPassLibrary (Id PKPassLibrary) where
  toPKPassLibrary = unsafeCastId

instance IsNSObject (Id PKPassLibrary) where
  toNSObject = unsafeCastId

-- ---------- PKPassRelevantDate ----------

-- | Phantom type for @PKPassRelevantDate@.
data PKPassRelevantDate

instance IsObjCObject (Id PKPassRelevantDate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPassRelevantDate"

class IsNSObject a => IsPKPassRelevantDate a where
  toPKPassRelevantDate :: a -> Id PKPassRelevantDate

instance IsPKPassRelevantDate (Id PKPassRelevantDate) where
  toPKPassRelevantDate = unsafeCastId

instance IsNSObject (Id PKPassRelevantDate) where
  toNSObject = unsafeCastId

-- ---------- PKPayment ----------

-- | Phantom type for @PKPayment@.
data PKPayment

instance IsObjCObject (Id PKPayment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPayment"

class IsNSObject a => IsPKPayment a where
  toPKPayment :: a -> Id PKPayment

instance IsPKPayment (Id PKPayment) where
  toPKPayment = unsafeCastId

instance IsNSObject (Id PKPayment) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentAuthorizationController ----------

-- | Phantom type for @PKPaymentAuthorizationController@.
data PKPaymentAuthorizationController

instance IsObjCObject (Id PKPaymentAuthorizationController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentAuthorizationController"

class IsNSObject a => IsPKPaymentAuthorizationController a where
  toPKPaymentAuthorizationController :: a -> Id PKPaymentAuthorizationController

instance IsPKPaymentAuthorizationController (Id PKPaymentAuthorizationController) where
  toPKPaymentAuthorizationController = unsafeCastId

instance IsNSObject (Id PKPaymentAuthorizationController) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentAuthorizationResult ----------

-- | Phantom type for @PKPaymentAuthorizationResult@.
data PKPaymentAuthorizationResult

instance IsObjCObject (Id PKPaymentAuthorizationResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentAuthorizationResult"

class IsNSObject a => IsPKPaymentAuthorizationResult a where
  toPKPaymentAuthorizationResult :: a -> Id PKPaymentAuthorizationResult

instance IsPKPaymentAuthorizationResult (Id PKPaymentAuthorizationResult) where
  toPKPaymentAuthorizationResult = unsafeCastId

instance IsNSObject (Id PKPaymentAuthorizationResult) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentInformationEventExtension ----------

-- | Phantom type for @PKPaymentInformationEventExtension@.
data PKPaymentInformationEventExtension

instance IsObjCObject (Id PKPaymentInformationEventExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentInformationEventExtension"

class IsNSObject a => IsPKPaymentInformationEventExtension a where
  toPKPaymentInformationEventExtension :: a -> Id PKPaymentInformationEventExtension

instance IsPKPaymentInformationEventExtension (Id PKPaymentInformationEventExtension) where
  toPKPaymentInformationEventExtension = unsafeCastId

instance IsNSObject (Id PKPaymentInformationEventExtension) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentMerchantSession ----------

-- | Phantom type for @PKPaymentMerchantSession@.
data PKPaymentMerchantSession

instance IsObjCObject (Id PKPaymentMerchantSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentMerchantSession"

class IsNSObject a => IsPKPaymentMerchantSession a where
  toPKPaymentMerchantSession :: a -> Id PKPaymentMerchantSession

instance IsPKPaymentMerchantSession (Id PKPaymentMerchantSession) where
  toPKPaymentMerchantSession = unsafeCastId

instance IsNSObject (Id PKPaymentMerchantSession) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentMethod ----------

-- | Phantom type for @PKPaymentMethod@.
data PKPaymentMethod

instance IsObjCObject (Id PKPaymentMethod) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentMethod"

class IsNSObject a => IsPKPaymentMethod a where
  toPKPaymentMethod :: a -> Id PKPaymentMethod

instance IsPKPaymentMethod (Id PKPaymentMethod) where
  toPKPaymentMethod = unsafeCastId

instance IsNSObject (Id PKPaymentMethod) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentOrderDetails ----------

-- | Phantom type for @PKPaymentOrderDetails@.
data PKPaymentOrderDetails

instance IsObjCObject (Id PKPaymentOrderDetails) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentOrderDetails"

class IsNSObject a => IsPKPaymentOrderDetails a where
  toPKPaymentOrderDetails :: a -> Id PKPaymentOrderDetails

instance IsPKPaymentOrderDetails (Id PKPaymentOrderDetails) where
  toPKPaymentOrderDetails = unsafeCastId

instance IsNSObject (Id PKPaymentOrderDetails) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentRequest ----------

-- | Phantom type for @PKPaymentRequest@.
data PKPaymentRequest

instance IsObjCObject (Id PKPaymentRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequest"

class IsNSObject a => IsPKPaymentRequest a where
  toPKPaymentRequest :: a -> Id PKPaymentRequest

instance IsPKPaymentRequest (Id PKPaymentRequest) where
  toPKPaymentRequest = unsafeCastId

instance IsNSObject (Id PKPaymentRequest) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentRequestMerchantSessionUpdate ----------

-- | Phantom type for @PKPaymentRequestMerchantSessionUpdate@.
data PKPaymentRequestMerchantSessionUpdate

instance IsObjCObject (Id PKPaymentRequestMerchantSessionUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestMerchantSessionUpdate"

class IsNSObject a => IsPKPaymentRequestMerchantSessionUpdate a where
  toPKPaymentRequestMerchantSessionUpdate :: a -> Id PKPaymentRequestMerchantSessionUpdate

instance IsPKPaymentRequestMerchantSessionUpdate (Id PKPaymentRequestMerchantSessionUpdate) where
  toPKPaymentRequestMerchantSessionUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestMerchantSessionUpdate) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentRequestUpdate ----------

-- | Phantom type for @PKPaymentRequestUpdate@.
data PKPaymentRequestUpdate

instance IsObjCObject (Id PKPaymentRequestUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestUpdate"

class IsNSObject a => IsPKPaymentRequestUpdate a where
  toPKPaymentRequestUpdate :: a -> Id PKPaymentRequestUpdate

instance IsPKPaymentRequestUpdate (Id PKPaymentRequestUpdate) where
  toPKPaymentRequestUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestUpdate) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentSummaryItem ----------

-- | Phantom type for @PKPaymentSummaryItem@.
data PKPaymentSummaryItem

instance IsObjCObject (Id PKPaymentSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentSummaryItem"

class IsNSObject a => IsPKPaymentSummaryItem a where
  toPKPaymentSummaryItem :: a -> Id PKPaymentSummaryItem

instance IsPKPaymentSummaryItem (Id PKPaymentSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

instance IsNSObject (Id PKPaymentSummaryItem) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentToken ----------

-- | Phantom type for @PKPaymentToken@.
data PKPaymentToken

instance IsObjCObject (Id PKPaymentToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentToken"

class IsNSObject a => IsPKPaymentToken a where
  toPKPaymentToken :: a -> Id PKPaymentToken

instance IsPKPaymentToken (Id PKPaymentToken) where
  toPKPaymentToken = unsafeCastId

instance IsNSObject (Id PKPaymentToken) where
  toNSObject = unsafeCastId

-- ---------- PKPaymentTokenContext ----------

-- | Phantom type for @PKPaymentTokenContext@.
data PKPaymentTokenContext

instance IsObjCObject (Id PKPaymentTokenContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentTokenContext"

class IsNSObject a => IsPKPaymentTokenContext a where
  toPKPaymentTokenContext :: a -> Id PKPaymentTokenContext

instance IsPKPaymentTokenContext (Id PKPaymentTokenContext) where
  toPKPaymentTokenContext = unsafeCastId

instance IsNSObject (Id PKPaymentTokenContext) where
  toNSObject = unsafeCastId

-- ---------- PKRecurringPaymentRequest ----------

-- | Phantom type for @PKRecurringPaymentRequest@.
data PKRecurringPaymentRequest

instance IsObjCObject (Id PKRecurringPaymentRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKRecurringPaymentRequest"

class IsNSObject a => IsPKRecurringPaymentRequest a where
  toPKRecurringPaymentRequest :: a -> Id PKRecurringPaymentRequest

instance IsPKRecurringPaymentRequest (Id PKRecurringPaymentRequest) where
  toPKRecurringPaymentRequest = unsafeCastId

instance IsNSObject (Id PKRecurringPaymentRequest) where
  toNSObject = unsafeCastId

-- ---------- PKShareablePassMetadata ----------

-- | Phantom type for @PKShareablePassMetadata@.
data PKShareablePassMetadata

instance IsObjCObject (Id PKShareablePassMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKShareablePassMetadata"

class IsNSObject a => IsPKShareablePassMetadata a where
  toPKShareablePassMetadata :: a -> Id PKShareablePassMetadata

instance IsPKShareablePassMetadata (Id PKShareablePassMetadata) where
  toPKShareablePassMetadata = unsafeCastId

instance IsNSObject (Id PKShareablePassMetadata) where
  toNSObject = unsafeCastId

-- ---------- PKStoredValuePassBalance ----------

-- | Phantom type for @PKStoredValuePassBalance@.
data PKStoredValuePassBalance

instance IsObjCObject (Id PKStoredValuePassBalance) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKStoredValuePassBalance"

class IsNSObject a => IsPKStoredValuePassBalance a where
  toPKStoredValuePassBalance :: a -> Id PKStoredValuePassBalance

instance IsPKStoredValuePassBalance (Id PKStoredValuePassBalance) where
  toPKStoredValuePassBalance = unsafeCastId

instance IsNSObject (Id PKStoredValuePassBalance) where
  toNSObject = unsafeCastId

-- ---------- PKStoredValuePassProperties ----------

-- | Phantom type for @PKStoredValuePassProperties@.
data PKStoredValuePassProperties

instance IsObjCObject (Id PKStoredValuePassProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKStoredValuePassProperties"

class IsNSObject a => IsPKStoredValuePassProperties a where
  toPKStoredValuePassProperties :: a -> Id PKStoredValuePassProperties

instance IsPKStoredValuePassProperties (Id PKStoredValuePassProperties) where
  toPKStoredValuePassProperties = unsafeCastId

instance IsNSObject (Id PKStoredValuePassProperties) where
  toNSObject = unsafeCastId

-- ---------- PKVehicleConnectionSession ----------

-- | Phantom type for @PKVehicleConnectionSession@.
data PKVehicleConnectionSession

instance IsObjCObject (Id PKVehicleConnectionSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKVehicleConnectionSession"

class IsNSObject a => IsPKVehicleConnectionSession a where
  toPKVehicleConnectionSession :: a -> Id PKVehicleConnectionSession

instance IsPKVehicleConnectionSession (Id PKVehicleConnectionSession) where
  toPKVehicleConnectionSession = unsafeCastId

instance IsNSObject (Id PKVehicleConnectionSession) where
  toNSObject = unsafeCastId

-- ---------- PKSecureElementPass ----------

-- | Phantom type for @PKSecureElementPass@.
data PKSecureElementPass

instance IsObjCObject (Id PKSecureElementPass) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKSecureElementPass"

class IsPKPass a => IsPKSecureElementPass a where
  toPKSecureElementPass :: a -> Id PKSecureElementPass

instance IsPKSecureElementPass (Id PKSecureElementPass) where
  toPKSecureElementPass = unsafeCastId

instance IsPKObject (Id PKSecureElementPass) where
  toPKObject = unsafeCastId

instance IsPKPass (Id PKSecureElementPass) where
  toPKPass = unsafeCastId

-- ---------- PKShareablePassMetadataPreview ----------

-- | Phantom type for @PKShareablePassMetadataPreview@.
data PKShareablePassMetadataPreview

instance IsObjCObject (Id PKShareablePassMetadataPreview) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKShareablePassMetadataPreview"

class IsPKAddPassMetadataPreview a => IsPKShareablePassMetadataPreview a where
  toPKShareablePassMetadataPreview :: a -> Id PKShareablePassMetadataPreview

instance IsPKShareablePassMetadataPreview (Id PKShareablePassMetadataPreview) where
  toPKShareablePassMetadataPreview = unsafeCastId

instance IsNSObject (Id PKShareablePassMetadataPreview) where
  toNSObject = unsafeCastId

instance IsPKAddPassMetadataPreview (Id PKShareablePassMetadataPreview) where
  toPKAddPassMetadataPreview = unsafeCastId

-- ---------- PKAddCarKeyPassConfiguration ----------

-- | Phantom type for @PKAddCarKeyPassConfiguration@.
data PKAddCarKeyPassConfiguration

instance IsObjCObject (Id PKAddCarKeyPassConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddCarKeyPassConfiguration"

class IsPKAddSecureElementPassConfiguration a => IsPKAddCarKeyPassConfiguration a where
  toPKAddCarKeyPassConfiguration :: a -> Id PKAddCarKeyPassConfiguration

instance IsPKAddCarKeyPassConfiguration (Id PKAddCarKeyPassConfiguration) where
  toPKAddCarKeyPassConfiguration = unsafeCastId

instance IsNSObject (Id PKAddCarKeyPassConfiguration) where
  toNSObject = unsafeCastId

instance IsPKAddSecureElementPassConfiguration (Id PKAddCarKeyPassConfiguration) where
  toPKAddSecureElementPassConfiguration = unsafeCastId

-- ---------- PKAddIdentityDocumentConfiguration ----------

-- | Phantom type for @PKAddIdentityDocumentConfiguration@.
data PKAddIdentityDocumentConfiguration

instance IsObjCObject (Id PKAddIdentityDocumentConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddIdentityDocumentConfiguration"

class IsPKAddSecureElementPassConfiguration a => IsPKAddIdentityDocumentConfiguration a where
  toPKAddIdentityDocumentConfiguration :: a -> Id PKAddIdentityDocumentConfiguration

instance IsPKAddIdentityDocumentConfiguration (Id PKAddIdentityDocumentConfiguration) where
  toPKAddIdentityDocumentConfiguration = unsafeCastId

instance IsNSObject (Id PKAddIdentityDocumentConfiguration) where
  toNSObject = unsafeCastId

instance IsPKAddSecureElementPassConfiguration (Id PKAddIdentityDocumentConfiguration) where
  toPKAddSecureElementPassConfiguration = unsafeCastId

-- ---------- PKAddShareablePassConfiguration ----------

-- | Phantom type for @PKAddShareablePassConfiguration@.
data PKAddShareablePassConfiguration

instance IsObjCObject (Id PKAddShareablePassConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddShareablePassConfiguration"

class IsPKAddSecureElementPassConfiguration a => IsPKAddShareablePassConfiguration a where
  toPKAddShareablePassConfiguration :: a -> Id PKAddShareablePassConfiguration

instance IsPKAddShareablePassConfiguration (Id PKAddShareablePassConfiguration) where
  toPKAddShareablePassConfiguration = unsafeCastId

instance IsNSObject (Id PKAddShareablePassConfiguration) where
  toNSObject = unsafeCastId

instance IsPKAddSecureElementPassConfiguration (Id PKAddShareablePassConfiguration) where
  toPKAddSecureElementPassConfiguration = unsafeCastId

-- ---------- PKAddIdentityDocumentMetadata ----------

-- | Phantom type for @PKAddIdentityDocumentMetadata@.
data PKAddIdentityDocumentMetadata

instance IsObjCObject (Id PKAddIdentityDocumentMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAddIdentityDocumentMetadata"

class IsPKIdentityDocumentMetadata a => IsPKAddIdentityDocumentMetadata a where
  toPKAddIdentityDocumentMetadata :: a -> Id PKAddIdentityDocumentMetadata

instance IsPKAddIdentityDocumentMetadata (Id PKAddIdentityDocumentMetadata) where
  toPKAddIdentityDocumentMetadata = unsafeCastId

instance IsNSObject (Id PKAddIdentityDocumentMetadata) where
  toNSObject = unsafeCastId

instance IsPKIdentityDocumentMetadata (Id PKAddIdentityDocumentMetadata) where
  toPKIdentityDocumentMetadata = unsafeCastId

-- ---------- PKJapanIndividualNumberCardMetadata ----------

-- | Phantom type for @PKJapanIndividualNumberCardMetadata@.
data PKJapanIndividualNumberCardMetadata

instance IsObjCObject (Id PKJapanIndividualNumberCardMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKJapanIndividualNumberCardMetadata"

class IsPKIdentityDocumentMetadata a => IsPKJapanIndividualNumberCardMetadata a where
  toPKJapanIndividualNumberCardMetadata :: a -> Id PKJapanIndividualNumberCardMetadata

instance IsPKJapanIndividualNumberCardMetadata (Id PKJapanIndividualNumberCardMetadata) where
  toPKJapanIndividualNumberCardMetadata = unsafeCastId

instance IsNSObject (Id PKJapanIndividualNumberCardMetadata) where
  toNSObject = unsafeCastId

instance IsPKIdentityDocumentMetadata (Id PKJapanIndividualNumberCardMetadata) where
  toPKIdentityDocumentMetadata = unsafeCastId

-- ---------- PKIssuerProvisioningExtensionPaymentPassEntry ----------

-- | Phantom type for @PKIssuerProvisioningExtensionPaymentPassEntry@.
data PKIssuerProvisioningExtensionPaymentPassEntry

instance IsObjCObject (Id PKIssuerProvisioningExtensionPaymentPassEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKIssuerProvisioningExtensionPaymentPassEntry"

class IsPKIssuerProvisioningExtensionPassEntry a => IsPKIssuerProvisioningExtensionPaymentPassEntry a where
  toPKIssuerProvisioningExtensionPaymentPassEntry :: a -> Id PKIssuerProvisioningExtensionPaymentPassEntry

instance IsPKIssuerProvisioningExtensionPaymentPassEntry (Id PKIssuerProvisioningExtensionPaymentPassEntry) where
  toPKIssuerProvisioningExtensionPaymentPassEntry = unsafeCastId

instance IsNSObject (Id PKIssuerProvisioningExtensionPaymentPassEntry) where
  toNSObject = unsafeCastId

instance IsPKIssuerProvisioningExtensionPassEntry (Id PKIssuerProvisioningExtensionPaymentPassEntry) where
  toPKIssuerProvisioningExtensionPassEntry = unsafeCastId

-- ---------- PKPaymentRequestCouponCodeUpdate ----------

-- | Phantom type for @PKPaymentRequestCouponCodeUpdate@.
data PKPaymentRequestCouponCodeUpdate

instance IsObjCObject (Id PKPaymentRequestCouponCodeUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestCouponCodeUpdate"

class IsPKPaymentRequestUpdate a => IsPKPaymentRequestCouponCodeUpdate a where
  toPKPaymentRequestCouponCodeUpdate :: a -> Id PKPaymentRequestCouponCodeUpdate

instance IsPKPaymentRequestCouponCodeUpdate (Id PKPaymentRequestCouponCodeUpdate) where
  toPKPaymentRequestCouponCodeUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestCouponCodeUpdate) where
  toNSObject = unsafeCastId

instance IsPKPaymentRequestUpdate (Id PKPaymentRequestCouponCodeUpdate) where
  toPKPaymentRequestUpdate = unsafeCastId

-- ---------- PKPaymentRequestPaymentMethodUpdate ----------

-- | Phantom type for @PKPaymentRequestPaymentMethodUpdate@.
data PKPaymentRequestPaymentMethodUpdate

instance IsObjCObject (Id PKPaymentRequestPaymentMethodUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestPaymentMethodUpdate"

class IsPKPaymentRequestUpdate a => IsPKPaymentRequestPaymentMethodUpdate a where
  toPKPaymentRequestPaymentMethodUpdate :: a -> Id PKPaymentRequestPaymentMethodUpdate

instance IsPKPaymentRequestPaymentMethodUpdate (Id PKPaymentRequestPaymentMethodUpdate) where
  toPKPaymentRequestPaymentMethodUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestPaymentMethodUpdate) where
  toNSObject = unsafeCastId

instance IsPKPaymentRequestUpdate (Id PKPaymentRequestPaymentMethodUpdate) where
  toPKPaymentRequestUpdate = unsafeCastId

-- ---------- PKPaymentRequestShippingContactUpdate ----------

-- | Phantom type for @PKPaymentRequestShippingContactUpdate@.
data PKPaymentRequestShippingContactUpdate

instance IsObjCObject (Id PKPaymentRequestShippingContactUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestShippingContactUpdate"

class IsPKPaymentRequestUpdate a => IsPKPaymentRequestShippingContactUpdate a where
  toPKPaymentRequestShippingContactUpdate :: a -> Id PKPaymentRequestShippingContactUpdate

instance IsPKPaymentRequestShippingContactUpdate (Id PKPaymentRequestShippingContactUpdate) where
  toPKPaymentRequestShippingContactUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestShippingContactUpdate) where
  toNSObject = unsafeCastId

instance IsPKPaymentRequestUpdate (Id PKPaymentRequestShippingContactUpdate) where
  toPKPaymentRequestUpdate = unsafeCastId

-- ---------- PKPaymentRequestShippingMethodUpdate ----------

-- | Phantom type for @PKPaymentRequestShippingMethodUpdate@.
data PKPaymentRequestShippingMethodUpdate

instance IsObjCObject (Id PKPaymentRequestShippingMethodUpdate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentRequestShippingMethodUpdate"

class IsPKPaymentRequestUpdate a => IsPKPaymentRequestShippingMethodUpdate a where
  toPKPaymentRequestShippingMethodUpdate :: a -> Id PKPaymentRequestShippingMethodUpdate

instance IsPKPaymentRequestShippingMethodUpdate (Id PKPaymentRequestShippingMethodUpdate) where
  toPKPaymentRequestShippingMethodUpdate = unsafeCastId

instance IsNSObject (Id PKPaymentRequestShippingMethodUpdate) where
  toNSObject = unsafeCastId

instance IsPKPaymentRequestUpdate (Id PKPaymentRequestShippingMethodUpdate) where
  toPKPaymentRequestUpdate = unsafeCastId

-- ---------- PKAutomaticReloadPaymentSummaryItem ----------

-- | Phantom type for @PKAutomaticReloadPaymentSummaryItem@.
data PKAutomaticReloadPaymentSummaryItem

instance IsObjCObject (Id PKAutomaticReloadPaymentSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKAutomaticReloadPaymentSummaryItem"

class IsPKPaymentSummaryItem a => IsPKAutomaticReloadPaymentSummaryItem a where
  toPKAutomaticReloadPaymentSummaryItem :: a -> Id PKAutomaticReloadPaymentSummaryItem

instance IsPKAutomaticReloadPaymentSummaryItem (Id PKAutomaticReloadPaymentSummaryItem) where
  toPKAutomaticReloadPaymentSummaryItem = unsafeCastId

instance IsNSObject (Id PKAutomaticReloadPaymentSummaryItem) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKAutomaticReloadPaymentSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKDeferredPaymentSummaryItem ----------

-- | Phantom type for @PKDeferredPaymentSummaryItem@.
data PKDeferredPaymentSummaryItem

instance IsObjCObject (Id PKDeferredPaymentSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDeferredPaymentSummaryItem"

class IsPKPaymentSummaryItem a => IsPKDeferredPaymentSummaryItem a where
  toPKDeferredPaymentSummaryItem :: a -> Id PKDeferredPaymentSummaryItem

instance IsPKDeferredPaymentSummaryItem (Id PKDeferredPaymentSummaryItem) where
  toPKDeferredPaymentSummaryItem = unsafeCastId

instance IsNSObject (Id PKDeferredPaymentSummaryItem) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKDeferredPaymentSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKDisbursementSummaryItem ----------

-- | Phantom type for @PKDisbursementSummaryItem@.
data PKDisbursementSummaryItem

instance IsObjCObject (Id PKDisbursementSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDisbursementSummaryItem"

class IsPKPaymentSummaryItem a => IsPKDisbursementSummaryItem a where
  toPKDisbursementSummaryItem :: a -> Id PKDisbursementSummaryItem

instance IsPKDisbursementSummaryItem (Id PKDisbursementSummaryItem) where
  toPKDisbursementSummaryItem = unsafeCastId

instance IsNSObject (Id PKDisbursementSummaryItem) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKDisbursementSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKInstantFundsOutFeeSummaryItem ----------

-- | Phantom type for @PKInstantFundsOutFeeSummaryItem@.
data PKInstantFundsOutFeeSummaryItem

instance IsObjCObject (Id PKInstantFundsOutFeeSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKInstantFundsOutFeeSummaryItem"

class IsPKPaymentSummaryItem a => IsPKInstantFundsOutFeeSummaryItem a where
  toPKInstantFundsOutFeeSummaryItem :: a -> Id PKInstantFundsOutFeeSummaryItem

instance IsPKInstantFundsOutFeeSummaryItem (Id PKInstantFundsOutFeeSummaryItem) where
  toPKInstantFundsOutFeeSummaryItem = unsafeCastId

instance IsNSObject (Id PKInstantFundsOutFeeSummaryItem) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKInstantFundsOutFeeSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKRecurringPaymentSummaryItem ----------

-- | Phantom type for @PKRecurringPaymentSummaryItem@.
data PKRecurringPaymentSummaryItem

instance IsObjCObject (Id PKRecurringPaymentSummaryItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKRecurringPaymentSummaryItem"

class IsPKPaymentSummaryItem a => IsPKRecurringPaymentSummaryItem a where
  toPKRecurringPaymentSummaryItem :: a -> Id PKRecurringPaymentSummaryItem

instance IsPKRecurringPaymentSummaryItem (Id PKRecurringPaymentSummaryItem) where
  toPKRecurringPaymentSummaryItem = unsafeCastId

instance IsNSObject (Id PKRecurringPaymentSummaryItem) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKRecurringPaymentSummaryItem) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKShippingMethod ----------

-- | Phantom type for @PKShippingMethod@.
data PKShippingMethod

instance IsObjCObject (Id PKShippingMethod) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKShippingMethod"

class IsPKPaymentSummaryItem a => IsPKShippingMethod a where
  toPKShippingMethod :: a -> Id PKShippingMethod

instance IsPKShippingMethod (Id PKShippingMethod) where
  toPKShippingMethod = unsafeCastId

instance IsNSObject (Id PKShippingMethod) where
  toNSObject = unsafeCastId

instance IsPKPaymentSummaryItem (Id PKShippingMethod) where
  toPKPaymentSummaryItem = unsafeCastId

-- ---------- PKTransitPassProperties ----------

-- | Phantom type for @PKTransitPassProperties@.
data PKTransitPassProperties

instance IsObjCObject (Id PKTransitPassProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKTransitPassProperties"

class IsPKStoredValuePassProperties a => IsPKTransitPassProperties a where
  toPKTransitPassProperties :: a -> Id PKTransitPassProperties

instance IsPKTransitPassProperties (Id PKTransitPassProperties) where
  toPKTransitPassProperties = unsafeCastId

instance IsNSObject (Id PKTransitPassProperties) where
  toNSObject = unsafeCastId

instance IsPKStoredValuePassProperties (Id PKTransitPassProperties) where
  toPKStoredValuePassProperties = unsafeCastId

-- ---------- PKPaymentPass ----------

-- | Phantom type for @PKPaymentPass@.
data PKPaymentPass

instance IsObjCObject (Id PKPaymentPass) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentPass"

class IsPKSecureElementPass a => IsPKPaymentPass a where
  toPKPaymentPass :: a -> Id PKPaymentPass

instance IsPKPaymentPass (Id PKPaymentPass) where
  toPKPaymentPass = unsafeCastId

instance IsPKObject (Id PKPaymentPass) where
  toPKObject = unsafeCastId

instance IsPKPass (Id PKPaymentPass) where
  toPKPass = unsafeCastId

instance IsPKSecureElementPass (Id PKPaymentPass) where
  toPKSecureElementPass = unsafeCastId

-- ---------- PKPaymentAuthorizationViewController ----------

-- | Phantom type for @PKPaymentAuthorizationViewController@.
data PKPaymentAuthorizationViewController

instance IsObjCObject (Id PKPaymentAuthorizationViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentAuthorizationViewController"

class IsNSViewController a => IsPKPaymentAuthorizationViewController a where
  toPKPaymentAuthorizationViewController :: a -> Id PKPaymentAuthorizationViewController

instance IsPKPaymentAuthorizationViewController (Id PKPaymentAuthorizationViewController) where
  toPKPaymentAuthorizationViewController = unsafeCastId

instance IsNSObject (Id PKPaymentAuthorizationViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PKPaymentAuthorizationViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id PKPaymentAuthorizationViewController) where
  toNSViewController = unsafeCastId

-- ---------- PKSuicaPassProperties ----------

-- | Phantom type for @PKSuicaPassProperties@.
data PKSuicaPassProperties

instance IsObjCObject (Id PKSuicaPassProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKSuicaPassProperties"

class IsPKTransitPassProperties a => IsPKSuicaPassProperties a where
  toPKSuicaPassProperties :: a -> Id PKSuicaPassProperties

instance IsPKSuicaPassProperties (Id PKSuicaPassProperties) where
  toPKSuicaPassProperties = unsafeCastId

instance IsNSObject (Id PKSuicaPassProperties) where
  toNSObject = unsafeCastId

instance IsPKStoredValuePassProperties (Id PKSuicaPassProperties) where
  toPKStoredValuePassProperties = unsafeCastId

instance IsPKTransitPassProperties (Id PKSuicaPassProperties) where
  toPKTransitPassProperties = unsafeCastId

-- ---------- PKPaymentButton ----------

-- | Phantom type for @PKPaymentButton@.
data PKPaymentButton

instance IsObjCObject (Id PKPaymentButton) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKPaymentButton"

class IsNSButton a => IsPKPaymentButton a where
  toPKPaymentButton :: a -> Id PKPaymentButton

instance IsPKPaymentButton (Id PKPaymentButton) where
  toPKPaymentButton = unsafeCastId

instance IsNSButton (Id PKPaymentButton) where
  toNSButton = unsafeCastId

instance IsNSControl (Id PKPaymentButton) where
  toNSControl = unsafeCastId

instance IsNSObject (Id PKPaymentButton) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id PKPaymentButton) where
  toNSResponder = unsafeCastId

instance IsNSView (Id PKPaymentButton) where
  toNSView = unsafeCastId
