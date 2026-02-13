{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKDisbursementRequest@.
module ObjC.PassKit.PKDisbursementRequest
  ( PKDisbursementRequest
  , IsPKDisbursementRequest(..)
  , initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItems
  , disbursementContactInvalidErrorWithContactField_localizedDescription
  , disbursementCardUnsupportedError
  , merchantIdentifier
  , setMerchantIdentifier
  , regionCode
  , setRegionCode
  , supportedNetworks
  , setSupportedNetworks
  , merchantCapabilities
  , setMerchantCapabilities
  , summaryItems
  , setSummaryItems
  , currencyCode
  , setCurrencyCode
  , requiredRecipientContactFields
  , setRequiredRecipientContactFields
  , recipientContact
  , setRecipientContact
  , supportedRegions
  , setSupportedRegions
  , applicationData
  , setApplicationData
  , applicationDataSelector
  , currencyCodeSelector
  , disbursementCardUnsupportedErrorSelector
  , disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector
  , initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector
  , merchantCapabilitiesSelector
  , merchantIdentifierSelector
  , recipientContactSelector
  , regionCodeSelector
  , requiredRecipientContactFieldsSelector
  , setApplicationDataSelector
  , setCurrencyCodeSelector
  , setMerchantCapabilitiesSelector
  , setMerchantIdentifierSelector
  , setRecipientContactSelector
  , setRegionCodeSelector
  , setRequiredRecipientContactFieldsSelector
  , setSummaryItemsSelector
  , setSupportedNetworksSelector
  , setSupportedRegionsSelector
  , summaryItemsSelector
  , supportedNetworksSelector
  , supportedRegionsSelector

  -- * Enum types
  , PKMerchantCapability(PKMerchantCapability)
  , pattern PKMerchantCapability3DS
  , pattern PKMerchantCapabilityEMV
  , pattern PKMerchantCapabilityCredit
  , pattern PKMerchantCapabilityDebit
  , pattern PKMerchantCapabilityInstantFundsOut

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

-- | @- initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:@
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItems :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString merchantIdentifier, IsNSString currencyCode, IsNSString regionCode, IsNSArray supportedNetworks, IsNSArray summaryItems) => pkDisbursementRequest -> merchantIdentifier -> currencyCode -> regionCode -> supportedNetworks -> PKMerchantCapability -> summaryItems -> IO (Id PKDisbursementRequest)
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItems pkDisbursementRequest merchantIdentifier currencyCode regionCode supportedNetworks merchantCapabilities summaryItems =
  sendOwnedMessage pkDisbursementRequest initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector (toNSString merchantIdentifier) (toNSString currencyCode) (toNSString regionCode) (toNSArray supportedNetworks) merchantCapabilities (toNSArray summaryItems)

-- | @+ disbursementContactInvalidErrorWithContactField:localizedDescription:@
disbursementContactInvalidErrorWithContactField_localizedDescription :: (IsNSString field, IsNSString localizedDescription) => field -> localizedDescription -> IO (Id NSError)
disbursementContactInvalidErrorWithContactField_localizedDescription field localizedDescription =
  do
    cls' <- getRequiredClass "PKDisbursementRequest"
    sendClassMessage cls' disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector (toNSString field) (toNSString localizedDescription)

-- | @+ disbursementCardUnsupportedError@
disbursementCardUnsupportedError :: IO (Id NSError)
disbursementCardUnsupportedError  =
  do
    cls' <- getRequiredClass "PKDisbursementRequest"
    sendClassMessage cls' disbursementCardUnsupportedErrorSelector

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
merchantIdentifier pkDisbursementRequest =
  sendMessage pkDisbursementRequest merchantIdentifierSelector

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setMerchantIdentifier pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setMerchantIdentifierSelector (toNSString value)

-- | @- regionCode@
regionCode :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
regionCode pkDisbursementRequest =
  sendMessage pkDisbursementRequest regionCodeSelector

-- | @- setRegionCode:@
setRegionCode :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setRegionCode pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setRegionCodeSelector (toNSString value)

-- | @- supportedNetworks@
supportedNetworks :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
supportedNetworks pkDisbursementRequest =
  sendMessage pkDisbursementRequest supportedNetworksSelector

-- | @- setSupportedNetworks:@
setSupportedNetworks :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSupportedNetworks pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setSupportedNetworksSelector (toNSArray value)

-- | @- merchantCapabilities@
merchantCapabilities :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO PKMerchantCapability
merchantCapabilities pkDisbursementRequest =
  sendMessage pkDisbursementRequest merchantCapabilitiesSelector

-- | @- setMerchantCapabilities:@
setMerchantCapabilities :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> PKMerchantCapability -> IO ()
setMerchantCapabilities pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setMerchantCapabilitiesSelector value

-- | @- summaryItems@
summaryItems :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
summaryItems pkDisbursementRequest =
  sendMessage pkDisbursementRequest summaryItemsSelector

-- | @- setSummaryItems:@
setSummaryItems :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSummaryItems pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setSummaryItemsSelector (toNSArray value)

-- | @- currencyCode@
currencyCode :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
currencyCode pkDisbursementRequest =
  sendMessage pkDisbursementRequest currencyCodeSelector

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setCurrencyCode pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setCurrencyCodeSelector (toNSString value)

-- | @- requiredRecipientContactFields@
requiredRecipientContactFields :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
requiredRecipientContactFields pkDisbursementRequest =
  sendMessage pkDisbursementRequest requiredRecipientContactFieldsSelector

-- | @- setRequiredRecipientContactFields:@
setRequiredRecipientContactFields :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setRequiredRecipientContactFields pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setRequiredRecipientContactFieldsSelector (toNSArray value)

-- | @- recipientContact@
recipientContact :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id PKContact)
recipientContact pkDisbursementRequest =
  sendMessage pkDisbursementRequest recipientContactSelector

-- | @- setRecipientContact:@
setRecipientContact :: (IsPKDisbursementRequest pkDisbursementRequest, IsPKContact value) => pkDisbursementRequest -> value -> IO ()
setRecipientContact pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setRecipientContactSelector (toPKContact value)

-- | @- supportedRegions@
supportedRegions :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
supportedRegions pkDisbursementRequest =
  sendMessage pkDisbursementRequest supportedRegionsSelector

-- | @- setSupportedRegions:@
setSupportedRegions :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSupportedRegions pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setSupportedRegionsSelector (toNSArray value)

-- | @- applicationData@
applicationData :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSData)
applicationData pkDisbursementRequest =
  sendMessage pkDisbursementRequest applicationDataSelector

-- | @- setApplicationData:@
setApplicationData :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSData value) => pkDisbursementRequest -> value -> IO ()
setApplicationData pkDisbursementRequest value =
  sendMessage pkDisbursementRequest setApplicationDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:@
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, PKMerchantCapability, Id NSArray] (Id PKDisbursementRequest)
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector = mkSelector "initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:"

-- | @Selector@ for @disbursementContactInvalidErrorWithContactField:localizedDescription:@
disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector :: Selector '[Id NSString, Id NSString] (Id NSError)
disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector = mkSelector "disbursementContactInvalidErrorWithContactField:localizedDescription:"

-- | @Selector@ for @disbursementCardUnsupportedError@
disbursementCardUnsupportedErrorSelector :: Selector '[] (Id NSError)
disbursementCardUnsupportedErrorSelector = mkSelector "disbursementCardUnsupportedError"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector '[] (Id NSString)
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector '[Id NSString] ()
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector '[] (Id NSString)
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @setRegionCode:@
setRegionCodeSelector :: Selector '[Id NSString] ()
setRegionCodeSelector = mkSelector "setRegionCode:"

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

-- | @Selector@ for @summaryItems@
summaryItemsSelector :: Selector '[] (Id NSArray)
summaryItemsSelector = mkSelector "summaryItems"

-- | @Selector@ for @setSummaryItems:@
setSummaryItemsSelector :: Selector '[Id NSArray] ()
setSummaryItemsSelector = mkSelector "setSummaryItems:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector '[Id NSString] ()
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @requiredRecipientContactFields@
requiredRecipientContactFieldsSelector :: Selector '[] (Id NSArray)
requiredRecipientContactFieldsSelector = mkSelector "requiredRecipientContactFields"

-- | @Selector@ for @setRequiredRecipientContactFields:@
setRequiredRecipientContactFieldsSelector :: Selector '[Id NSArray] ()
setRequiredRecipientContactFieldsSelector = mkSelector "setRequiredRecipientContactFields:"

-- | @Selector@ for @recipientContact@
recipientContactSelector :: Selector '[] (Id PKContact)
recipientContactSelector = mkSelector "recipientContact"

-- | @Selector@ for @setRecipientContact:@
setRecipientContactSelector :: Selector '[Id PKContact] ()
setRecipientContactSelector = mkSelector "setRecipientContact:"

-- | @Selector@ for @supportedRegions@
supportedRegionsSelector :: Selector '[] (Id NSArray)
supportedRegionsSelector = mkSelector "supportedRegions"

-- | @Selector@ for @setSupportedRegions:@
setSupportedRegionsSelector :: Selector '[Id NSArray] ()
setSupportedRegionsSelector = mkSelector "setSupportedRegions:"

-- | @Selector@ for @applicationData@
applicationDataSelector :: Selector '[] (Id NSData)
applicationDataSelector = mkSelector "applicationData"

-- | @Selector@ for @setApplicationData:@
setApplicationDataSelector :: Selector '[Id NSData] ()
setApplicationDataSelector = mkSelector "setApplicationData:"

