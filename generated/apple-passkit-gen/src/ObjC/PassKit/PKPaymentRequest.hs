{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , applePayLaterAvailabilitySelector
  , applicationDataSelector
  , attributionIdentifierSelector
  , automaticReloadPaymentRequestSelector
  , availableNetworksSelector
  , billingContactSelector
  , countryCodeSelector
  , couponCodeSelector
  , currencyCodeSelector
  , deferredPaymentRequestSelector
  , merchantCapabilitiesSelector
  , merchantCategoryCodeSelector
  , merchantIdentifierSelector
  , multiTokenContextsSelector
  , paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector
  , paymentContactInvalidErrorWithContactField_localizedDescriptionSelector
  , paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector
  , paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector
  , paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector
  , paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector
  , paymentSummaryItemsSelector
  , recurringPaymentRequestSelector
  , requiredBillingAddressFieldsSelector
  , requiredBillingContactFieldsSelector
  , requiredShippingAddressFieldsSelector
  , requiredShippingContactFieldsSelector
  , setApplePayLaterAvailabilitySelector
  , setApplicationDataSelector
  , setAttributionIdentifierSelector
  , setAutomaticReloadPaymentRequestSelector
  , setBillingContactSelector
  , setCountryCodeSelector
  , setCouponCodeSelector
  , setCurrencyCodeSelector
  , setDeferredPaymentRequestSelector
  , setMerchantCapabilitiesSelector
  , setMerchantCategoryCodeSelector
  , setMerchantIdentifierSelector
  , setMultiTokenContextsSelector
  , setPaymentSummaryItemsSelector
  , setRecurringPaymentRequestSelector
  , setRequiredBillingAddressFieldsSelector
  , setRequiredBillingContactFieldsSelector
  , setRequiredShippingAddressFieldsSelector
  , setRequiredShippingContactFieldsSelector
  , setShippingContactEditingModeSelector
  , setShippingContactSelector
  , setShippingMethodsSelector
  , setShippingTypeSelector
  , setSupportedCountriesSelector
  , setSupportedNetworksSelector
  , setSupportsCouponCodeSelector
  , shippingContactEditingModeSelector
  , shippingContactSelector
  , shippingMethodsSelector
  , shippingTypeSelector
  , supportedCountriesSelector
  , supportedNetworksSelector
  , supportsCouponCodeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' availableNetworksSelector

-- | @+ paymentContactInvalidErrorWithContactField:localizedDescription:@
paymentContactInvalidErrorWithContactField_localizedDescription :: (IsNSString field, IsNSString localizedDescription) => field -> localizedDescription -> IO (Id NSError)
paymentContactInvalidErrorWithContactField_localizedDescription field localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentContactInvalidErrorWithContactField_localizedDescriptionSelector (toNSString field) (toNSString localizedDescription)

-- | @+ paymentShippingAddressInvalidErrorWithKey:localizedDescription:@
paymentShippingAddressInvalidErrorWithKey_localizedDescription :: (IsNSString postalAddressKey, IsNSString localizedDescription) => postalAddressKey -> localizedDescription -> IO (Id NSError)
paymentShippingAddressInvalidErrorWithKey_localizedDescription postalAddressKey localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector (toNSString postalAddressKey) (toNSString localizedDescription)

-- | @+ paymentBillingAddressInvalidErrorWithKey:localizedDescription:@
paymentBillingAddressInvalidErrorWithKey_localizedDescription :: (IsNSString postalAddressKey, IsNSString localizedDescription) => postalAddressKey -> localizedDescription -> IO (Id NSError)
paymentBillingAddressInvalidErrorWithKey_localizedDescription postalAddressKey localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector (toNSString postalAddressKey) (toNSString localizedDescription)

-- | @+ paymentShippingAddressUnserviceableErrorWithLocalizedDescription:@
paymentShippingAddressUnserviceableErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentShippingAddressUnserviceableErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- | @+ paymentCouponCodeInvalidErrorWithLocalizedDescription:@
paymentCouponCodeInvalidErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentCouponCodeInvalidErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- | @+ paymentCouponCodeExpiredErrorWithLocalizedDescription:@
paymentCouponCodeExpiredErrorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id NSError)
paymentCouponCodeExpiredErrorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "PKPaymentRequest"
    sendClassMessage cls' paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
merchantIdentifier pkPaymentRequest =
  sendMessage pkPaymentRequest merchantIdentifierSelector

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setMerchantIdentifier pkPaymentRequest value =
  sendMessage pkPaymentRequest setMerchantIdentifierSelector (toNSString value)

-- | @- attributionIdentifier@
attributionIdentifier :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
attributionIdentifier pkPaymentRequest =
  sendMessage pkPaymentRequest attributionIdentifierSelector

-- | @- setAttributionIdentifier:@
setAttributionIdentifier :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setAttributionIdentifier pkPaymentRequest value =
  sendMessage pkPaymentRequest setAttributionIdentifierSelector (toNSString value)

-- | @- countryCode@
countryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
countryCode pkPaymentRequest =
  sendMessage pkPaymentRequest countryCodeSelector

-- | @- setCountryCode:@
setCountryCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCountryCode pkPaymentRequest value =
  sendMessage pkPaymentRequest setCountryCodeSelector (toNSString value)

-- | @- supportedNetworks@
supportedNetworks :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
supportedNetworks pkPaymentRequest =
  sendMessage pkPaymentRequest supportedNetworksSelector

-- | @- setSupportedNetworks:@
setSupportedNetworks :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setSupportedNetworks pkPaymentRequest value =
  sendMessage pkPaymentRequest setSupportedNetworksSelector (toNSArray value)

-- | @- merchantCapabilities@
merchantCapabilities :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKMerchantCapability
merchantCapabilities pkPaymentRequest =
  sendMessage pkPaymentRequest merchantCapabilitiesSelector

-- | @- setMerchantCapabilities:@
setMerchantCapabilities :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKMerchantCapability -> IO ()
setMerchantCapabilities pkPaymentRequest value =
  sendMessage pkPaymentRequest setMerchantCapabilitiesSelector value

-- | @- supportsCouponCode@
supportsCouponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO Bool
supportsCouponCode pkPaymentRequest =
  sendMessage pkPaymentRequest supportsCouponCodeSelector

-- | @- setSupportsCouponCode:@
setSupportsCouponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> Bool -> IO ()
setSupportsCouponCode pkPaymentRequest value =
  sendMessage pkPaymentRequest setSupportsCouponCodeSelector value

-- | @- couponCode@
couponCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
couponCode pkPaymentRequest =
  sendMessage pkPaymentRequest couponCodeSelector

-- | @- setCouponCode:@
setCouponCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCouponCode pkPaymentRequest value =
  sendMessage pkPaymentRequest setCouponCodeSelector (toNSString value)

-- | @- merchantCategoryCode@
merchantCategoryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO CShort
merchantCategoryCode pkPaymentRequest =
  sendMessage pkPaymentRequest merchantCategoryCodeSelector

-- | @- setMerchantCategoryCode:@
setMerchantCategoryCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> CShort -> IO ()
setMerchantCategoryCode pkPaymentRequest value =
  sendMessage pkPaymentRequest setMerchantCategoryCodeSelector value

-- | @- paymentSummaryItems@
paymentSummaryItems :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
paymentSummaryItems pkPaymentRequest =
  sendMessage pkPaymentRequest paymentSummaryItemsSelector

-- | @- setPaymentSummaryItems:@
setPaymentSummaryItems :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setPaymentSummaryItems pkPaymentRequest value =
  sendMessage pkPaymentRequest setPaymentSummaryItemsSelector (toNSArray value)

-- | @- currencyCode@
currencyCode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSString)
currencyCode pkPaymentRequest =
  sendMessage pkPaymentRequest currencyCodeSelector

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsPKPaymentRequest pkPaymentRequest, IsNSString value) => pkPaymentRequest -> value -> IO ()
setCurrencyCode pkPaymentRequest value =
  sendMessage pkPaymentRequest setCurrencyCodeSelector (toNSString value)

-- | @- requiredBillingContactFields@
requiredBillingContactFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
requiredBillingContactFields pkPaymentRequest =
  sendMessage pkPaymentRequest requiredBillingContactFieldsSelector

-- | @- setRequiredBillingContactFields:@
setRequiredBillingContactFields :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setRequiredBillingContactFields pkPaymentRequest value =
  sendMessage pkPaymentRequest setRequiredBillingContactFieldsSelector (toNSSet value)

-- | @- requiredBillingAddressFields@
requiredBillingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKAddressField
requiredBillingAddressFields pkPaymentRequest =
  sendMessage pkPaymentRequest requiredBillingAddressFieldsSelector

-- | @- setRequiredBillingAddressFields:@
setRequiredBillingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKAddressField -> IO ()
setRequiredBillingAddressFields pkPaymentRequest value =
  sendMessage pkPaymentRequest setRequiredBillingAddressFieldsSelector value

-- | @- billingContact@
billingContact :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKContact)
billingContact pkPaymentRequest =
  sendMessage pkPaymentRequest billingContactSelector

-- | @- setBillingContact:@
setBillingContact :: (IsPKPaymentRequest pkPaymentRequest, IsPKContact value) => pkPaymentRequest -> value -> IO ()
setBillingContact pkPaymentRequest value =
  sendMessage pkPaymentRequest setBillingContactSelector (toPKContact value)

-- | @- requiredShippingContactFields@
requiredShippingContactFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
requiredShippingContactFields pkPaymentRequest =
  sendMessage pkPaymentRequest requiredShippingContactFieldsSelector

-- | @- setRequiredShippingContactFields:@
setRequiredShippingContactFields :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setRequiredShippingContactFields pkPaymentRequest value =
  sendMessage pkPaymentRequest setRequiredShippingContactFieldsSelector (toNSSet value)

-- | @- requiredShippingAddressFields@
requiredShippingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKAddressField
requiredShippingAddressFields pkPaymentRequest =
  sendMessage pkPaymentRequest requiredShippingAddressFieldsSelector

-- | @- setRequiredShippingAddressFields:@
setRequiredShippingAddressFields :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKAddressField -> IO ()
setRequiredShippingAddressFields pkPaymentRequest value =
  sendMessage pkPaymentRequest setRequiredShippingAddressFieldsSelector value

-- | @- shippingContact@
shippingContact :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKContact)
shippingContact pkPaymentRequest =
  sendMessage pkPaymentRequest shippingContactSelector

-- | @- setShippingContact:@
setShippingContact :: (IsPKPaymentRequest pkPaymentRequest, IsPKContact value) => pkPaymentRequest -> value -> IO ()
setShippingContact pkPaymentRequest value =
  sendMessage pkPaymentRequest setShippingContactSelector (toPKContact value)

-- | @- shippingMethods@
shippingMethods :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
shippingMethods pkPaymentRequest =
  sendMessage pkPaymentRequest shippingMethodsSelector

-- | @- setShippingMethods:@
setShippingMethods :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setShippingMethods pkPaymentRequest value =
  sendMessage pkPaymentRequest setShippingMethodsSelector (toNSArray value)

-- | @- shippingType@
shippingType :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKShippingType
shippingType pkPaymentRequest =
  sendMessage pkPaymentRequest shippingTypeSelector

-- | @- setShippingType:@
setShippingType :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKShippingType -> IO ()
setShippingType pkPaymentRequest value =
  sendMessage pkPaymentRequest setShippingTypeSelector value

-- | @- shippingContactEditingMode@
shippingContactEditingMode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKShippingContactEditingMode
shippingContactEditingMode pkPaymentRequest =
  sendMessage pkPaymentRequest shippingContactEditingModeSelector

-- | @- setShippingContactEditingMode:@
setShippingContactEditingMode :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKShippingContactEditingMode -> IO ()
setShippingContactEditingMode pkPaymentRequest value =
  sendMessage pkPaymentRequest setShippingContactEditingModeSelector value

-- | @- applicationData@
applicationData :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSData)
applicationData pkPaymentRequest =
  sendMessage pkPaymentRequest applicationDataSelector

-- | @- setApplicationData:@
setApplicationData :: (IsPKPaymentRequest pkPaymentRequest, IsNSData value) => pkPaymentRequest -> value -> IO ()
setApplicationData pkPaymentRequest value =
  sendMessage pkPaymentRequest setApplicationDataSelector (toNSData value)

-- | @- supportedCountries@
supportedCountries :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSSet)
supportedCountries pkPaymentRequest =
  sendMessage pkPaymentRequest supportedCountriesSelector

-- | @- setSupportedCountries:@
setSupportedCountries :: (IsPKPaymentRequest pkPaymentRequest, IsNSSet value) => pkPaymentRequest -> value -> IO ()
setSupportedCountries pkPaymentRequest value =
  sendMessage pkPaymentRequest setSupportedCountriesSelector (toNSSet value)

-- | @- multiTokenContexts@
multiTokenContexts :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id NSArray)
multiTokenContexts pkPaymentRequest =
  sendMessage pkPaymentRequest multiTokenContextsSelector

-- | @- setMultiTokenContexts:@
setMultiTokenContexts :: (IsPKPaymentRequest pkPaymentRequest, IsNSArray value) => pkPaymentRequest -> value -> IO ()
setMultiTokenContexts pkPaymentRequest value =
  sendMessage pkPaymentRequest setMultiTokenContextsSelector (toNSArray value)

-- | @- recurringPaymentRequest@
recurringPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKRecurringPaymentRequest)
recurringPaymentRequest pkPaymentRequest =
  sendMessage pkPaymentRequest recurringPaymentRequestSelector

-- | @- setRecurringPaymentRequest:@
setRecurringPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKRecurringPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setRecurringPaymentRequest pkPaymentRequest value =
  sendMessage pkPaymentRequest setRecurringPaymentRequestSelector (toPKRecurringPaymentRequest value)

-- | @- automaticReloadPaymentRequest@
automaticReloadPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKAutomaticReloadPaymentRequest)
automaticReloadPaymentRequest pkPaymentRequest =
  sendMessage pkPaymentRequest automaticReloadPaymentRequestSelector

-- | @- setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKAutomaticReloadPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setAutomaticReloadPaymentRequest pkPaymentRequest value =
  sendMessage pkPaymentRequest setAutomaticReloadPaymentRequestSelector (toPKAutomaticReloadPaymentRequest value)

-- | @- deferredPaymentRequest@
deferredPaymentRequest :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO (Id PKDeferredPaymentRequest)
deferredPaymentRequest pkPaymentRequest =
  sendMessage pkPaymentRequest deferredPaymentRequestSelector

-- | @- setDeferredPaymentRequest:@
setDeferredPaymentRequest :: (IsPKPaymentRequest pkPaymentRequest, IsPKDeferredPaymentRequest value) => pkPaymentRequest -> value -> IO ()
setDeferredPaymentRequest pkPaymentRequest value =
  sendMessage pkPaymentRequest setDeferredPaymentRequestSelector (toPKDeferredPaymentRequest value)

-- | @- applePayLaterAvailability@
applePayLaterAvailability :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> IO PKApplePayLaterAvailability
applePayLaterAvailability pkPaymentRequest =
  sendMessage pkPaymentRequest applePayLaterAvailabilitySelector

-- | @- setApplePayLaterAvailability:@
setApplePayLaterAvailability :: IsPKPaymentRequest pkPaymentRequest => pkPaymentRequest -> PKApplePayLaterAvailability -> IO ()
setApplePayLaterAvailability pkPaymentRequest value =
  sendMessage pkPaymentRequest setApplePayLaterAvailabilitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableNetworks@
availableNetworksSelector :: Selector '[] (Id NSArray)
availableNetworksSelector = mkSelector "availableNetworks"

-- | @Selector@ for @paymentContactInvalidErrorWithContactField:localizedDescription:@
paymentContactInvalidErrorWithContactField_localizedDescriptionSelector :: Selector '[Id NSString, Id NSString] (Id NSError)
paymentContactInvalidErrorWithContactField_localizedDescriptionSelector = mkSelector "paymentContactInvalidErrorWithContactField:localizedDescription:"

-- | @Selector@ for @paymentShippingAddressInvalidErrorWithKey:localizedDescription:@
paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector :: Selector '[Id NSString, Id NSString] (Id NSError)
paymentShippingAddressInvalidErrorWithKey_localizedDescriptionSelector = mkSelector "paymentShippingAddressInvalidErrorWithKey:localizedDescription:"

-- | @Selector@ for @paymentBillingAddressInvalidErrorWithKey:localizedDescription:@
paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector :: Selector '[Id NSString, Id NSString] (Id NSError)
paymentBillingAddressInvalidErrorWithKey_localizedDescriptionSelector = mkSelector "paymentBillingAddressInvalidErrorWithKey:localizedDescription:"

-- | @Selector@ for @paymentShippingAddressUnserviceableErrorWithLocalizedDescription:@
paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id NSError)
paymentShippingAddressUnserviceableErrorWithLocalizedDescriptionSelector = mkSelector "paymentShippingAddressUnserviceableErrorWithLocalizedDescription:"

-- | @Selector@ for @paymentCouponCodeInvalidErrorWithLocalizedDescription:@
paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id NSError)
paymentCouponCodeInvalidErrorWithLocalizedDescriptionSelector = mkSelector "paymentCouponCodeInvalidErrorWithLocalizedDescription:"

-- | @Selector@ for @paymentCouponCodeExpiredErrorWithLocalizedDescription:@
paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id NSError)
paymentCouponCodeExpiredErrorWithLocalizedDescriptionSelector = mkSelector "paymentCouponCodeExpiredErrorWithLocalizedDescription:"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector '[] (Id NSString)
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector '[Id NSString] ()
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @attributionIdentifier@
attributionIdentifierSelector :: Selector '[] (Id NSString)
attributionIdentifierSelector = mkSelector "attributionIdentifier"

-- | @Selector@ for @setAttributionIdentifier:@
setAttributionIdentifierSelector :: Selector '[Id NSString] ()
setAttributionIdentifierSelector = mkSelector "setAttributionIdentifier:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @setCountryCode:@
setCountryCodeSelector :: Selector '[Id NSString] ()
setCountryCodeSelector = mkSelector "setCountryCode:"

-- | @Selector@ for @supportedNetworks@
supportedNetworksSelector :: Selector '[] (Id NSArray)
supportedNetworksSelector = mkSelector "supportedNetworks"

-- | @Selector@ for @setSupportedNetworks:@
setSupportedNetworksSelector :: Selector '[Id NSArray] ()
setSupportedNetworksSelector = mkSelector "setSupportedNetworks:"

-- | @Selector@ for @merchantCapabilities@
merchantCapabilitiesSelector :: Selector '[] PKMerchantCapability
merchantCapabilitiesSelector = mkSelector "merchantCapabilities"

-- | @Selector@ for @setMerchantCapabilities:@
setMerchantCapabilitiesSelector :: Selector '[PKMerchantCapability] ()
setMerchantCapabilitiesSelector = mkSelector "setMerchantCapabilities:"

-- | @Selector@ for @supportsCouponCode@
supportsCouponCodeSelector :: Selector '[] Bool
supportsCouponCodeSelector = mkSelector "supportsCouponCode"

-- | @Selector@ for @setSupportsCouponCode:@
setSupportsCouponCodeSelector :: Selector '[Bool] ()
setSupportsCouponCodeSelector = mkSelector "setSupportsCouponCode:"

-- | @Selector@ for @couponCode@
couponCodeSelector :: Selector '[] (Id NSString)
couponCodeSelector = mkSelector "couponCode"

-- | @Selector@ for @setCouponCode:@
setCouponCodeSelector :: Selector '[Id NSString] ()
setCouponCodeSelector = mkSelector "setCouponCode:"

-- | @Selector@ for @merchantCategoryCode@
merchantCategoryCodeSelector :: Selector '[] CShort
merchantCategoryCodeSelector = mkSelector "merchantCategoryCode"

-- | @Selector@ for @setMerchantCategoryCode:@
setMerchantCategoryCodeSelector :: Selector '[CShort] ()
setMerchantCategoryCodeSelector = mkSelector "setMerchantCategoryCode:"

-- | @Selector@ for @paymentSummaryItems@
paymentSummaryItemsSelector :: Selector '[] (Id NSArray)
paymentSummaryItemsSelector = mkSelector "paymentSummaryItems"

-- | @Selector@ for @setPaymentSummaryItems:@
setPaymentSummaryItemsSelector :: Selector '[Id NSArray] ()
setPaymentSummaryItemsSelector = mkSelector "setPaymentSummaryItems:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector '[Id NSString] ()
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @requiredBillingContactFields@
requiredBillingContactFieldsSelector :: Selector '[] (Id NSSet)
requiredBillingContactFieldsSelector = mkSelector "requiredBillingContactFields"

-- | @Selector@ for @setRequiredBillingContactFields:@
setRequiredBillingContactFieldsSelector :: Selector '[Id NSSet] ()
setRequiredBillingContactFieldsSelector = mkSelector "setRequiredBillingContactFields:"

-- | @Selector@ for @requiredBillingAddressFields@
requiredBillingAddressFieldsSelector :: Selector '[] PKAddressField
requiredBillingAddressFieldsSelector = mkSelector "requiredBillingAddressFields"

-- | @Selector@ for @setRequiredBillingAddressFields:@
setRequiredBillingAddressFieldsSelector :: Selector '[PKAddressField] ()
setRequiredBillingAddressFieldsSelector = mkSelector "setRequiredBillingAddressFields:"

-- | @Selector@ for @billingContact@
billingContactSelector :: Selector '[] (Id PKContact)
billingContactSelector = mkSelector "billingContact"

-- | @Selector@ for @setBillingContact:@
setBillingContactSelector :: Selector '[Id PKContact] ()
setBillingContactSelector = mkSelector "setBillingContact:"

-- | @Selector@ for @requiredShippingContactFields@
requiredShippingContactFieldsSelector :: Selector '[] (Id NSSet)
requiredShippingContactFieldsSelector = mkSelector "requiredShippingContactFields"

-- | @Selector@ for @setRequiredShippingContactFields:@
setRequiredShippingContactFieldsSelector :: Selector '[Id NSSet] ()
setRequiredShippingContactFieldsSelector = mkSelector "setRequiredShippingContactFields:"

-- | @Selector@ for @requiredShippingAddressFields@
requiredShippingAddressFieldsSelector :: Selector '[] PKAddressField
requiredShippingAddressFieldsSelector = mkSelector "requiredShippingAddressFields"

-- | @Selector@ for @setRequiredShippingAddressFields:@
setRequiredShippingAddressFieldsSelector :: Selector '[PKAddressField] ()
setRequiredShippingAddressFieldsSelector = mkSelector "setRequiredShippingAddressFields:"

-- | @Selector@ for @shippingContact@
shippingContactSelector :: Selector '[] (Id PKContact)
shippingContactSelector = mkSelector "shippingContact"

-- | @Selector@ for @setShippingContact:@
setShippingContactSelector :: Selector '[Id PKContact] ()
setShippingContactSelector = mkSelector "setShippingContact:"

-- | @Selector@ for @shippingMethods@
shippingMethodsSelector :: Selector '[] (Id NSArray)
shippingMethodsSelector = mkSelector "shippingMethods"

-- | @Selector@ for @setShippingMethods:@
setShippingMethodsSelector :: Selector '[Id NSArray] ()
setShippingMethodsSelector = mkSelector "setShippingMethods:"

-- | @Selector@ for @shippingType@
shippingTypeSelector :: Selector '[] PKShippingType
shippingTypeSelector = mkSelector "shippingType"

-- | @Selector@ for @setShippingType:@
setShippingTypeSelector :: Selector '[PKShippingType] ()
setShippingTypeSelector = mkSelector "setShippingType:"

-- | @Selector@ for @shippingContactEditingMode@
shippingContactEditingModeSelector :: Selector '[] PKShippingContactEditingMode
shippingContactEditingModeSelector = mkSelector "shippingContactEditingMode"

-- | @Selector@ for @setShippingContactEditingMode:@
setShippingContactEditingModeSelector :: Selector '[PKShippingContactEditingMode] ()
setShippingContactEditingModeSelector = mkSelector "setShippingContactEditingMode:"

-- | @Selector@ for @applicationData@
applicationDataSelector :: Selector '[] (Id NSData)
applicationDataSelector = mkSelector "applicationData"

-- | @Selector@ for @setApplicationData:@
setApplicationDataSelector :: Selector '[Id NSData] ()
setApplicationDataSelector = mkSelector "setApplicationData:"

-- | @Selector@ for @supportedCountries@
supportedCountriesSelector :: Selector '[] (Id NSSet)
supportedCountriesSelector = mkSelector "supportedCountries"

-- | @Selector@ for @setSupportedCountries:@
setSupportedCountriesSelector :: Selector '[Id NSSet] ()
setSupportedCountriesSelector = mkSelector "setSupportedCountries:"

-- | @Selector@ for @multiTokenContexts@
multiTokenContextsSelector :: Selector '[] (Id NSArray)
multiTokenContextsSelector = mkSelector "multiTokenContexts"

-- | @Selector@ for @setMultiTokenContexts:@
setMultiTokenContextsSelector :: Selector '[Id NSArray] ()
setMultiTokenContextsSelector = mkSelector "setMultiTokenContexts:"

-- | @Selector@ for @recurringPaymentRequest@
recurringPaymentRequestSelector :: Selector '[] (Id PKRecurringPaymentRequest)
recurringPaymentRequestSelector = mkSelector "recurringPaymentRequest"

-- | @Selector@ for @setRecurringPaymentRequest:@
setRecurringPaymentRequestSelector :: Selector '[Id PKRecurringPaymentRequest] ()
setRecurringPaymentRequestSelector = mkSelector "setRecurringPaymentRequest:"

-- | @Selector@ for @automaticReloadPaymentRequest@
automaticReloadPaymentRequestSelector :: Selector '[] (Id PKAutomaticReloadPaymentRequest)
automaticReloadPaymentRequestSelector = mkSelector "automaticReloadPaymentRequest"

-- | @Selector@ for @setAutomaticReloadPaymentRequest:@
setAutomaticReloadPaymentRequestSelector :: Selector '[Id PKAutomaticReloadPaymentRequest] ()
setAutomaticReloadPaymentRequestSelector = mkSelector "setAutomaticReloadPaymentRequest:"

-- | @Selector@ for @deferredPaymentRequest@
deferredPaymentRequestSelector :: Selector '[] (Id PKDeferredPaymentRequest)
deferredPaymentRequestSelector = mkSelector "deferredPaymentRequest"

-- | @Selector@ for @setDeferredPaymentRequest:@
setDeferredPaymentRequestSelector :: Selector '[Id PKDeferredPaymentRequest] ()
setDeferredPaymentRequestSelector = mkSelector "setDeferredPaymentRequest:"

-- | @Selector@ for @applePayLaterAvailability@
applePayLaterAvailabilitySelector :: Selector '[] PKApplePayLaterAvailability
applePayLaterAvailabilitySelector = mkSelector "applePayLaterAvailability"

-- | @Selector@ for @setApplePayLaterAvailability:@
setApplePayLaterAvailabilitySelector :: Selector '[PKApplePayLaterAvailability] ()
setApplePayLaterAvailabilitySelector = mkSelector "setApplePayLaterAvailability:"

