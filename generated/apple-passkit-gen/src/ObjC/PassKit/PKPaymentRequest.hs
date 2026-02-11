{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentRequest@.
module ObjC.PassKit.PKPaymentRequest
  ( PKPaymentRequest
  , IsPKPaymentRequest(..)
  , availableNetworks
  , paymentContactInvalidErrorWithContactField_localizedDescription
  , paymentShippingAddressInvalidErrorWithKey_localizedDescription
  , paymentBillingAddressInvalidErrorWithKey_localizedDescription
  , paymentShippingAddressUnserviceableErrorWithLocalizedDescription
  , paymentCouponCodeInvalidErrorWithLocalizedDescription
  , paymentCouponCodeExpiredErrorWithLocalizedDescription
  , merchantIdentifier
  , setMerchantIdentifier
  , attributionIdentifier
  , setAttributionIdentifier
  , countryCode
  , setCountryCode
  , supportedNetworks
  , setSupportedNetworks
  , merchantCapabilities
  , setMerchantCapabilities
  , supportsCouponCode
  , setSupportsCouponCode
  , couponCode
  , setCouponCode
  , merchantCategoryCode
  , setMerchantCategoryCode
  , paymentSummaryItems
  , setPaymentSummaryItems
  , currencyCode
  , setCurrencyCode
  , requiredBillingContactFields
  , setRequiredBillingContactFields
  , requiredBillingAddressFields
  , setRequiredBillingAddressFields
  , billingContact
  , setBillingContact
  , requiredShippingContactFields
  , setRequiredShippingContactFields
  , requiredShippingAddressFields
  , setRequiredShippingAddressFields
  , shippingContact
  , setShippingContact
  , shippingMethods
  , setShippingMethods
  , shippingType
  , setShippingType
  , shippingContactEditingMode
  , setShippingContactEditingMode
  , applicationData
  , setApplicationData
  , supportedCountries
  , setSupportedCountries
  , multiTokenContexts
  , setMultiTokenContexts
  , recurringPaymentRequest
  , setRecurringPaymentRequest
  , automaticReloadPaymentRequest
  , setAutomaticReloadPaymentRequest
  , deferredPaymentRequest
  , setDeferredPaymentRequest
  , applePayLaterAvailability
  , setApplePayLaterAvailability
  , availableNetworksSelector
  , paymentContactInvalidErrorWithContactField_localizedDescriptionSelector
  , paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector
  , paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector
  , paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector
  , paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector
  , paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector
  , merchantIdentifierSelector
  , setMerchantIdentifierSelector
  , attributionIdentifierSelector
  , setAttributionIdentifierSelector
  , countryCodeSelector
  , setCountryCodeSelector
  , supportedNetworksSelector
  , setSupportedNetworksSelector
  , merchantCapabilitiesSelector
  , setMerchantCapabilitiesSelector
  , supportsCouponCodeSelector
  , setSupportsCouponCodeSelector
  , couponCodeSelector
  , setCouponCodeSelector
  , merchantCategoryCodeSelector
  , setMerchantCategoryCodeSelector
  , paymentSummaryItemsSelector
  , setPaymentSummaryItemsSelector
  , currencyCodeSelector
  , setCurrencyCodeSelector
  , requiredBillingContactFieldsSelector
  , setRequiredBillingContactFieldsSelector
  , requiredBillingAddressFieldsSelector
  , setRequiredBillingAddressFieldsSelector
  , billingContactSelector
  , setBillingContactSelector
  , requiredShippingContactFieldsSelector
  , setRequiredShippingContactFieldsSelector
  , requiredShippingAddressFieldsSelector
  , setRequiredShippingAddressFieldsSelector
  , shippingContactSelector
  , setShippingContactSelector
  , shippingMethodsSelector
  , setShippingMethodsSelector
  , shippingTypeSelector
  , setShippingTypeSelector
  , shippingContactEditingModeSelector
  , setShippingContactEditingModeSelector
  , applicationDataSelector
  , setApplicationDataSelector
  , supportedCountriesSelector
  , setSupportedCountriesSelector
  , multiTokenContextsSelector
  , setMultiTokenContextsSelector
  , recurringPaymentRequestSelector
  , setRecurringPaymentRequestSelector
  , automaticReloadPaymentRequestSelector
  , setAutomaticReloadPaymentRequestSelector
  , deferredPaymentRequestSelector
  , setDeferredPaymentRequestSelector
  , applePayLaterAvailabilitySelector
  , setApplePayLaterAvailabilitySelector

  -- * Enum types
  , PKAddressField(PKAddressField)
  , pattern PKAddressFieldNone
  , pattern PKAddressFieldPostalAddress
  , pattern PKAddressFieldPhone
  , pattern PKAddressFieldEmail
  , pattern PKAddressFieldName
  , pattern PKAddressFieldAll
  , PKApplePayLaterAvailability(PKApplePayLaterAvailability)
  , pattern PKApplePayLaterAvailable
  , pattern PKApplePayLaterUnavailableItemIneligible
  , pattern PKApplePayLaterUnavailableRecurringTransaction
  , PKMerchantCapability(PKMerchantCapability)
  , pattern PKMerchantCapability3DS
  , pattern PKMerchantCapabilityEMV
  , pattern PKMerchantCapabilityCredit
  , pattern PKMerchantCapabilityDebit
  , pattern PKMerchantCapabilityInstantFundsOut
  , PKShippingContactEditingMode(PKShippingContactEditingMode)
  , pattern PKShippingContactEditingModeAvailable
  , pattern PKShippingContactEditingModeStorePickup
  , pattern PKShippingContactEditingModeEnabled
  , PKShippingType(PKShippingType)
  , pattern PKShippingTypeShipping
  , pattern PKShippingTypeDelivery
  , pattern PKShippingTypeStorePickup
  , pattern PKShippingTypeServicePickup

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

-- | @+ availableNetworks@
availableNetworks :: IO (Id NSArray)
availableNetworks  =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMsg cls' (mkSelector "availableNetworks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ paymentContactInvalidErrorWithContactField:localizedDescription:@
paymentContactInvalidErrorWithContactField_localizedDescription :: (IsNSString field, IsNSString localizedDescription) => field -> localizedDescription -> IO (Id NSError)
paymentContactInvalidErrorWithContactField_localizedDescription field localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr field $ \raw_field ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        sendClassMsg cls' (mkSelector "paymentContactInvalidErrorWithContactField:localizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_field :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentShippingAddressInvalidErrorWithKey:localizedDescription:@
paymentShippingAddressInvalidErrorWithKey_localizedDescription :: (IsNSString postalAddressKey, IsNSString localizedDescription) => postalAddressKey -> localizedDescription -> IO (Id NSError)
paymentShippingAddressInvalidErrorWithKey_localizedDescription postalAddressKey localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr postalAddressKey $ \raw_postalAddressKey ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        sendClassMsg cls' (mkSelector "paymentShippingAddressInvalidErrorWithKey:localizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_postalAddressKey :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentBillingAddressInvalidErrorWithKey:localizedDescription:@
paymentBillingAddressInvalidErrorWithKey_localizedDescription :: (IsNSString postalAddressKey, IsNSString localizedDescription) => postalAddressKey -> localizedDescription -> IO (Id NSError)
paymentBillingAddressInvalidErrorWithKey_localizedDescription postalAddressKey localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr postalAddressKey $ \raw_postalAddressKey ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        sendClassMsg cls' (mkSelector "paymentBillingAddressInvalidErrorWithKey:localizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_postalAddressKey :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentShippingAddressUnserviceableErrorWithLocalizedDescription:@
paymentShippingAddressUnserviceableErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentShippingAddressUnserviceableErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "paymentShippingAddressUnserviceableErrorWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentCouponCodeInvalidErrorWithLocalizedDescription:@
paymentCouponCodeInvalidErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentCouponCodeInvalidErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "paymentCouponCodeInvalidErrorWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ paymentCouponCodeExpiredErrorWithLocalizedDescription:@
paymentCouponCodeExpiredErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentCouponCodeExpiredErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    withObjCPtr localizedDescription $ \raw_localizedDescription ->
      sendClassMsg cls' (mkSelector "paymentCouponCodeExpiredErrorWithLocalizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
merchantIdentifier pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "merchantIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setMerchantIdentifier pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setMerchantIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributionIdentifier@
attributionIdentifier :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
attributionIdentifier pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "attributionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributionIdentifier:@
setAttributionIdentifier :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setAttributionIdentifier pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setAttributionIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- countryCode@
countryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
countryCode pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCountryCode:@
setCountryCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCountryCode pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setCountryCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedNetworks@
supportedNetworks :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
supportedNetworks pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "supportedNetworks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedNetworks:@
setSupportedNetworks :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setSupportedNetworks pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setSupportedNetworks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- merchantCapabilities@
merchantCapabilities :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKMerchantCapability
merchantCapabilities pkPaymentRequest  =
    fmap (coerce :: CULong -> PKMerchantCapability) $ sendMsg pkPaymentRequest (mkSelector "merchantCapabilities") retCULong []

-- | @- setMerchantCapabilities:@
setMerchantCapabilities :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKMerchantCapability -> IO ()
setMerchantCapabilities pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setMerchantCapabilities:") retVoid [argCULong (coerce value)]

-- | @- supportsCouponCode@
supportsCouponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO Bool
supportsCouponCode pkPaymentRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPaymentRequest (mkSelector "supportsCouponCode") retCULong []

-- | @- setSupportsCouponCode:@
setSupportsCouponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> Bool -> IO ()
setSupportsCouponCode pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setSupportsCouponCode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- couponCode@
couponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
couponCode pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "couponCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCouponCode:@
setCouponCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCouponCode pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setCouponCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- merchantCategoryCode@
merchantCategoryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO CShort
merchantCategoryCode pkPaymentRequest  =
    fmap fromIntegral $ sendMsg pkPaymentRequest (mkSelector "merchantCategoryCode") retCInt []

-- | @- setMerchantCategoryCode:@
setMerchantCategoryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> CShort -> IO ()
setMerchantCategoryCode pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setMerchantCategoryCode:") retVoid [argCInt (fromIntegral value)]

-- | @- paymentSummaryItems@
paymentSummaryItems :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
paymentSummaryItems pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "paymentSummaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentSummaryItems:@
setPaymentSummaryItems :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setPaymentSummaryItems pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setPaymentSummaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currencyCode@
currencyCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
currencyCode pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCurrencyCode pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setCurrencyCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredBillingContactFields@
requiredBillingContactFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
requiredBillingContactFields pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "requiredBillingContactFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredBillingContactFields:@
setRequiredBillingContactFields :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setRequiredBillingContactFields pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setRequiredBillingContactFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredBillingAddressFields@
requiredBillingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKAddressField
requiredBillingAddressFields pkPaymentRequest  =
    fmap (coerce :: CULong -> PKAddressField) $ sendMsg pkPaymentRequest (mkSelector "requiredBillingAddressFields") retCULong []

-- | @- setRequiredBillingAddressFields:@
setRequiredBillingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKAddressField -> IO ()
setRequiredBillingAddressFields pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setRequiredBillingAddressFields:") retVoid [argCULong (coerce value)]

-- | @- billingContact@
billingContact :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKContact)
billingContact pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "billingContact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillingContact:@
setBillingContact :: (IsPKPaymentRequest pkPaymentRequest, IsPKContact value) => pkPaymentRequest -> value -> IO ()
setBillingContact pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setBillingContact:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredShippingContactFields@
requiredShippingContactFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
requiredShippingContactFields pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "requiredShippingContactFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredShippingContactFields:@
setRequiredShippingContactFields :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setRequiredShippingContactFields pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setRequiredShippingContactFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredShippingAddressFields@
requiredShippingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKAddressField
requiredShippingAddressFields pkPaymentRequest  =
    fmap (coerce :: CULong -> PKAddressField) $ sendMsg pkPaymentRequest (mkSelector "requiredShippingAddressFields") retCULong []

-- | @- setRequiredShippingAddressFields:@
setRequiredShippingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKAddressField -> IO ()
setRequiredShippingAddressFields pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setRequiredShippingAddressFields:") retVoid [argCULong (coerce value)]

-- | @- shippingContact@
shippingContact :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKContact)
shippingContact pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "shippingContact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShippingContact:@
setShippingContact :: (IsPKPaymentRequest pkPaymentRequest, IsPKContact value) => pkPaymentRequest -> value -> IO ()
setShippingContact pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setShippingContact:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
shippingMethods pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "shippingMethods") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setShippingMethods pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setShippingMethods:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shippingType@
shippingType :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKShippingType
shippingType pkPaymentRequest  =
    fmap (coerce :: CULong -> PKShippingType) $ sendMsg pkPaymentRequest (mkSelector "shippingType") retCULong []

-- | @- setShippingType:@
setShippingType :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKShippingType -> IO ()
setShippingType pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setShippingType:") retVoid [argCULong (coerce value)]

-- | @- shippingContactEditingMode@
shippingContactEditingMode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKShippingContactEditingMode
shippingContactEditingMode pkPaymentRequest  =
    fmap (coerce :: CULong -> PKShippingContactEditingMode) $ sendMsg pkPaymentRequest (mkSelector "shippingContactEditingMode") retCULong []

-- | @- setShippingContactEditingMode:@
setShippingContactEditingMode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKShippingContactEditingMode -> IO ()
setShippingContactEditingMode pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setShippingContactEditingMode:") retVoid [argCULong (coerce value)]

-- | @- applicationData@
applicationData :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSData)
applicationData pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "applicationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationData:@
setApplicationData :: (IsPKPaymentRequest pkPaymentRequest, IsNSData value) => pkPaymentRequest -> value -> IO ()
setApplicationData pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setApplicationData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedCountries@
supportedCountries :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
supportedCountries pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "supportedCountries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedCountries:@
setSupportedCountries :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setSupportedCountries pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setSupportedCountries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- multiTokenContexts@
multiTokenContexts :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
multiTokenContexts pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "multiTokenContexts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMultiTokenContexts:@
setMultiTokenContexts :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setMultiTokenContexts pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setMultiTokenContexts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recurringPaymentRequest@
recurringPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKRecurringPaymentRequest)
recurringPaymentRequest pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "recurringPaymentRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecurringPaymentRequest:@
setRecurringPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKRecurringPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setRecurringPaymentRequest pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setRecurringPaymentRequest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- automaticReloadPaymentRequest@
automaticReloadPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKAutomaticReloadPaymentRequest)
automaticReloadPaymentRequest pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "automaticReloadPaymentRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKAutomaticReloadPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setAutomaticReloadPaymentRequest pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setAutomaticReloadPaymentRequest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deferredPaymentRequest@
deferredPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKDeferredPaymentRequest)
deferredPaymentRequest pkPaymentRequest  =
    sendMsg pkPaymentRequest (mkSelector "deferredPaymentRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeferredPaymentRequest:@
setDeferredPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKDeferredPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setDeferredPaymentRequest pkPaymentRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentRequest (mkSelector "setDeferredPaymentRequest:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applePayLaterAvailability@
applePayLaterAvailability :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKApplePayLaterAvailability
applePayLaterAvailability pkPaymentRequest  =
    fmap (coerce :: CLong -> PKApplePayLaterAvailability) $ sendMsg pkPaymentRequest (mkSelector "applePayLaterAvailability") retCLong []

-- | @- setApplePayLaterAvailability:@
setApplePayLaterAvailability :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKApplePayLaterAvailability -> IO ()
setApplePayLaterAvailability pkPaymentRequest  value =
    sendMsg pkPaymentRequest (mkSelector "setApplePayLaterAvailability:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableNetworks@
availableNetworksSelector :: Selector
availableNetworksSelector = mkSelector "availableNetworks"

-- | @Selector@ for @paymentContactInvalidErrorWithContactField:localizedDescription:@
paymentContactInvalidErrorWithContactField_localizedDescriptionSelector :: Selector
paymentContactInvalidErrorWithContactField_localizedDescriptionSelector = mkSelector "paymentContactInvalidErrorWithContactField:localizedDescription:"

-- | @Selector@ for @paymentShippingAddressInvalidErrorWithKey:localizedDescription:@
paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector :: Selector
paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector = mkSelector "paymentShippingAddressInvalidErrorWithKey:localizedDescription:"

-- | @Selector@ for @paymentBillingAddressInvalidErrorWithKey:localizedDescription:@
paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector :: Selector
paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector = mkSelector "paymentBillingAddressInvalidErrorWithKey:localizedDescription:"

-- | @Selector@ for @paymentShippingAddressUnserviceableErrorWithLocalizedDescription:@
paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector :: Selector
paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector = mkSelector "paymentShippingAddressUnserviceableErrorWithLocalizedDescription:"

-- | @Selector@ for @paymentCouponCodeInvalidErrorWithLocalizedDescription:@
paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector :: Selector
paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector = mkSelector "paymentCouponCodeInvalidErrorWithLocalizedDescription:"

-- | @Selector@ for @paymentCouponCodeExpiredErrorWithLocalizedDescription:@
paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector :: Selector
paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector = mkSelector "paymentCouponCodeExpiredErrorWithLocalizedDescription:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @attributionIdentifier@
attributionIdentifierSelector :: Selector
attributionIdentifierSelector = mkSelector "attributionIdentifier"

-- | @Selector@ for @setAttributionIdentifier:@
setAttributionIdentifierSelector :: Selector
setAttributionIdentifierSelector = mkSelector "setAttributionIdentifier:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @setCountryCode:@
setCountryCodeSelector :: Selector
setCountryCodeSelector = mkSelector "setCountryCode:"

-- | @Selector@ for @supportedNetworks@
supportedNetworksSelector :: Selector
supportedNetworksSelector = mkSelector "supportedNetworks"

-- | @Selector@ for @setSupportedNetworks:@
setSupportedNetworksSelector :: Selector
setSupportedNetworksSelector = mkSelector "setSupportedNetworks:"

-- | @Selector@ for @merchantCapabilities@
merchantCapabilitiesSelector :: Selector
merchantCapabilitiesSelector = mkSelector "merchantCapabilities"

-- | @Selector@ for @setMerchantCapabilities:@
setMerchantCapabilitiesSelector :: Selector
setMerchantCapabilitiesSelector = mkSelector "setMerchantCapabilities:"

-- | @Selector@ for @supportsCouponCode@
supportsCouponCodeSelector :: Selector
supportsCouponCodeSelector = mkSelector "supportsCouponCode"

-- | @Selector@ for @setSupportsCouponCode:@
setSupportsCouponCodeSelector :: Selector
setSupportsCouponCodeSelector = mkSelector "setSupportsCouponCode:"

-- | @Selector@ for @couponCode@
couponCodeSelector :: Selector
couponCodeSelector = mkSelector "couponCode"

-- | @Selector@ for @setCouponCode:@
setCouponCodeSelector :: Selector
setCouponCodeSelector = mkSelector "setCouponCode:"

-- | @Selector@ for @merchantCategoryCode@
merchantCategoryCodeSelector :: Selector
merchantCategoryCodeSelector = mkSelector "merchantCategoryCode"

-- | @Selector@ for @setMerchantCategoryCode:@
setMerchantCategoryCodeSelector :: Selector
setMerchantCategoryCodeSelector = mkSelector "setMerchantCategoryCode:"

-- | @Selector@ for @paymentSummaryItems@
paymentSummaryItemsSelector :: Selector
paymentSummaryItemsSelector = mkSelector "paymentSummaryItems"

-- | @Selector@ for @setPaymentSummaryItems:@
setPaymentSummaryItemsSelector :: Selector
setPaymentSummaryItemsSelector = mkSelector "setPaymentSummaryItems:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @requiredBillingContactFields@
requiredBillingContactFieldsSelector :: Selector
requiredBillingContactFieldsSelector = mkSelector "requiredBillingContactFields"

-- | @Selector@ for @setRequiredBillingContactFields:@
setRequiredBillingContactFieldsSelector :: Selector
setRequiredBillingContactFieldsSelector = mkSelector "setRequiredBillingContactFields:"

-- | @Selector@ for @requiredBillingAddressFields@
requiredBillingAddressFieldsSelector :: Selector
requiredBillingAddressFieldsSelector = mkSelector "requiredBillingAddressFields"

-- | @Selector@ for @setRequiredBillingAddressFields:@
setRequiredBillingAddressFieldsSelector :: Selector
setRequiredBillingAddressFieldsSelector = mkSelector "setRequiredBillingAddressFields:"

-- | @Selector@ for @billingContact@
billingContactSelector :: Selector
billingContactSelector = mkSelector "billingContact"

-- | @Selector@ for @setBillingContact:@
setBillingContactSelector :: Selector
setBillingContactSelector = mkSelector "setBillingContact:"

-- | @Selector@ for @requiredShippingContactFields@
requiredShippingContactFieldsSelector :: Selector
requiredShippingContactFieldsSelector = mkSelector "requiredShippingContactFields"

-- | @Selector@ for @setRequiredShippingContactFields:@
setRequiredShippingContactFieldsSelector :: Selector
setRequiredShippingContactFieldsSelector = mkSelector "setRequiredShippingContactFields:"

-- | @Selector@ for @requiredShippingAddressFields@
requiredShippingAddressFieldsSelector :: Selector
requiredShippingAddressFieldsSelector = mkSelector "requiredShippingAddressFields"

-- | @Selector@ for @setRequiredShippingAddressFields:@
setRequiredShippingAddressFieldsSelector :: Selector
setRequiredShippingAddressFieldsSelector = mkSelector "setRequiredShippingAddressFields:"

-- | @Selector@ for @shippingContact@
shippingContactSelector :: Selector
shippingContactSelector = mkSelector "shippingContact"

-- | @Selector@ for @setShippingContact:@
setShippingContactSelector :: Selector
setShippingContactSelector = mkSelector "setShippingContact:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector
setShippingMethodsSelector = mkSelector "setShippingMethods:"

-- | @Selector@ for @shippingType@
shippingTypeSelector :: Selector
shippingTypeSelector = mkSelector "shippingType"

-- | @Selector@ for @setShippingType:@
setShippingTypeSelector :: Selector
setShippingTypeSelector = mkSelector "setShippingType:"

-- | @Selector@ for @shippingContactEditingMode@
shippingContactEditingModeSelector :: Selector
shippingContactEditingModeSelector = mkSelector "shippingContactEditingMode"

-- | @Selector@ for @setShippingContactEditingMode:@
setShippingContactEditingModeSelector :: Selector
setShippingContactEditingModeSelector = mkSelector "setShippingContactEditingMode:"

-- | @Selector@ for @applicationData@
applicationDataSelector :: Selector
applicationDataSelector = mkSelector "applicationData"

-- | @Selector@ for @setApplicationData:@
setApplicationDataSelector :: Selector
setApplicationDataSelector = mkSelector "setApplicationData:"

-- | @Selector@ for @supportedCountries@
supportedCountriesSelector :: Selector
supportedCountriesSelector = mkSelector "supportedCountries"

-- | @Selector@ for @setSupportedCountries:@
setSupportedCountriesSelector :: Selector
setSupportedCountriesSelector = mkSelector "setSupportedCountries:"

-- | @Selector@ for @multiTokenContexts@
multiTokenContextsSelector :: Selector
multiTokenContextsSelector = mkSelector "multiTokenContexts"

-- | @Selector@ for @setMultiTokenContexts:@
setMultiTokenContextsSelector :: Selector
setMultiTokenContextsSelector = mkSelector "setMultiTokenContexts:"

-- | @Selector@ for @recurringPaymentRequest@
recurringPaymentRequestSelector :: Selector
recurringPaymentRequestSelector = mkSelector "recurringPaymentRequest"

-- | @Selector@ for @setRecurringPaymentRequest:@
setRecurringPaymentRequestSelector :: Selector
setRecurringPaymentRequestSelector = mkSelector "setRecurringPaymentRequest:"

-- | @Selector@ for @automaticReloadPaymentRequest@
automaticReloadPaymentRequestSelector :: Selector
automaticReloadPaymentRequestSelector = mkSelector "automaticReloadPaymentRequest"

-- | @Selector@ for @setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequestSelector :: Selector
setAutomaticReloadPaymentRequestSelector = mkSelector "setAutomaticReloadPaymentRequest:"

-- | @Selector@ for @deferredPaymentRequest@
deferredPaymentRequestSelector :: Selector
deferredPaymentRequestSelector = mkSelector "deferredPaymentRequest"

-- | @Selector@ for @setDeferredPaymentRequest:@
setDeferredPaymentRequestSelector :: Selector
setDeferredPaymentRequestSelector = mkSelector "setDeferredPaymentRequest:"

-- | @Selector@ for @applePayLaterAvailability@
applePayLaterAvailabilitySelector :: Selector
applePayLaterAvailabilitySelector = mkSelector "applePayLaterAvailability"

-- | @Selector@ for @setApplePayLaterAvailability:@
setApplePayLaterAvailabilitySelector :: Selector
setApplePayLaterAvailabilitySelector = mkSelector "setApplePayLaterAvailability:"

