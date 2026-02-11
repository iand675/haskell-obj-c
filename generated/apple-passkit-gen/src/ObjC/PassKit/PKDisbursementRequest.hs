{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector
  , disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector
  , disbursementCardUnsupportedErrorSelector
  , merchantIdentifierSelector
  , setMerchantIdentifierSelector
  , regionCodeSelector
  , setRegionCodeSelector
  , supportedNetworksSelector
  , setSupportedNetworksSelector
  , merchantCapabilitiesSelector
  , setMerchantCapabilitiesSelector
  , summaryItemsSelector
  , setSummaryItemsSelector
  , currencyCodeSelector
  , setCurrencyCodeSelector
  , requiredRecipientContactFieldsSelector
  , setRequiredRecipientContactFieldsSelector
  , recipientContactSelector
  , setRecipientContactSelector
  , supportedRegionsSelector
  , setSupportedRegionsSelector
  , applicationDataSelector
  , setApplicationDataSelector

  -- * Enum types
  , PKMerchantCapability(PKMerchantCapability)
  , pattern PKMerchantCapability3DS
  , pattern PKMerchantCapabilityEMV
  , pattern PKMerchantCapabilityCredit
  , pattern PKMerchantCapabilityDebit
  , pattern PKMerchantCapabilityInstantFundsOut

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

-- | @- initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:@
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItems :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString merchantIdentifier, IsNSString currencyCode, IsNSString regionCode, IsNSArray supportedNetworks, IsNSArray summaryItems) => pkDisbursementRequest -> merchantIdentifier -> currencyCode -> regionCode -> supportedNetworks -> PKMerchantCapability -> summaryItems -> IO (Id PKDisbursementRequest)
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItems pkDisbursementRequest  merchantIdentifier currencyCode regionCode supportedNetworks merchantCapabilities summaryItems =
  withObjCPtr merchantIdentifier $ \raw_merchantIdentifier ->
    withObjCPtr currencyCode $ \raw_currencyCode ->
      withObjCPtr regionCode $ \raw_regionCode ->
        withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
          withObjCPtr summaryItems $ \raw_summaryItems ->
              sendMsg pkDisbursementRequest (mkSelector "initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:") (retPtr retVoid) [argPtr (castPtr raw_merchantIdentifier :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ()), argPtr (castPtr raw_regionCode :: Ptr ()), argPtr (castPtr raw_supportedNetworks :: Ptr ()), argCULong (coerce merchantCapabilities), argPtr (castPtr raw_summaryItems :: Ptr ())] >>= ownedObject . castPtr

-- | @+ disbursementContactInvalidErrorWithContactField:localizedDescription:@
disbursementContactInvalidErrorWithContactField_localizedDescription :: (IsNSString field, IsNSString localizedDescription) => field -> localizedDescription -> IO (Id NSError)
disbursementContactInvalidErrorWithContactField_localizedDescription field localizedDescription =
  do
    cls' <- getRequiredClass "PKDisbursementRequest"
    withObjCPtr field $ \raw_field ->
      withObjCPtr localizedDescription $ \raw_localizedDescription ->
        sendClassMsg cls' (mkSelector "disbursementContactInvalidErrorWithContactField:localizedDescription:") (retPtr retVoid) [argPtr (castPtr raw_field :: Ptr ()), argPtr (castPtr raw_localizedDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @+ disbursementCardUnsupportedError@
disbursementCardUnsupportedError :: IO (Id NSError)
disbursementCardUnsupportedError  =
  do
    cls' <- getRequiredClass "PKDisbursementRequest"
    sendClassMsg cls' (mkSelector "disbursementCardUnsupportedError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- merchantIdentifier@
merchantIdentifier :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
merchantIdentifier pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "merchantIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMerchantIdentifier:@
setMerchantIdentifier :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setMerchantIdentifier pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setMerchantIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- regionCode@
regionCode :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
regionCode pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "regionCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRegionCode:@
setRegionCode :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setRegionCode pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setRegionCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedNetworks@
supportedNetworks :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
supportedNetworks pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "supportedNetworks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedNetworks:@
setSupportedNetworks :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSupportedNetworks pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setSupportedNetworks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- merchantCapabilities@
merchantCapabilities :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO PKMerchantCapability
merchantCapabilities pkDisbursementRequest  =
    fmap (coerce :: CULong -> PKMerchantCapability) $ sendMsg pkDisbursementRequest (mkSelector "merchantCapabilities") retCULong []

-- | @- setMerchantCapabilities:@
setMerchantCapabilities :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> PKMerchantCapability -> IO ()
setMerchantCapabilities pkDisbursementRequest  value =
    sendMsg pkDisbursementRequest (mkSelector "setMerchantCapabilities:") retVoid [argCULong (coerce value)]

-- | @- summaryItems@
summaryItems :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
summaryItems pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "summaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSummaryItems:@
setSummaryItems :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSummaryItems pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setSummaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currencyCode@
currencyCode :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSString)
currencyCode pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrencyCode:@
setCurrencyCode :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSString value) => pkDisbursementRequest -> value -> IO ()
setCurrencyCode pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setCurrencyCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiredRecipientContactFields@
requiredRecipientContactFields :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
requiredRecipientContactFields pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "requiredRecipientContactFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredRecipientContactFields:@
setRequiredRecipientContactFields :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setRequiredRecipientContactFields pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setRequiredRecipientContactFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recipientContact@
recipientContact :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id PKContact)
recipientContact pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "recipientContact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecipientContact:@
setRecipientContact :: (IsPKDisbursementRequest pkDisbursementRequest, IsPKContact value) => pkDisbursementRequest -> value -> IO ()
setRecipientContact pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setRecipientContact:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedRegions@
supportedRegions :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSArray)
supportedRegions pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "supportedRegions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedRegions:@
setSupportedRegions :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSArray value) => pkDisbursementRequest -> value -> IO ()
setSupportedRegions pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setSupportedRegions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- applicationData@
applicationData :: IsPKDisbursementRequest pkDisbursementRequest => pkDisbursementRequest -> IO (Id NSData)
applicationData pkDisbursementRequest  =
    sendMsg pkDisbursementRequest (mkSelector "applicationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplicationData:@
setApplicationData :: (IsPKDisbursementRequest pkDisbursementRequest, IsNSData value) => pkDisbursementRequest -> value -> IO ()
setApplicationData pkDisbursementRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkDisbursementRequest (mkSelector "setApplicationData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:@
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector :: Selector
initWithMerchantIdentifier_currencyCode_regionCode_supportedNetworks_merchantCapabilities_summaryItemsSelector = mkSelector "initWithMerchantIdentifier:currencyCode:regionCode:supportedNetworks:merchantCapabilities:summaryItems:"

-- | @Selector@ for @disbursementContactInvalidErrorWithContactField:localizedDescription:@
disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector :: Selector
disbursementContactInvalidErrorWithContactField_localizedDescriptionSelector = mkSelector "disbursementContactInvalidErrorWithContactField:localizedDescription:"

-- | @Selector@ for @disbursementCardUnsupportedError@
disbursementCardUnsupportedErrorSelector :: Selector
disbursementCardUnsupportedErrorSelector = mkSelector "disbursementCardUnsupportedError"

-- | @Selector@ for @merchantIdentifier@
merchantIdentifierSelector :: Selector
merchantIdentifierSelector = mkSelector "merchantIdentifier"

-- | @Selector@ for @setMerchantIdentifier:@
setMerchantIdentifierSelector :: Selector
setMerchantIdentifierSelector = mkSelector "setMerchantIdentifier:"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @setRegionCode:@
setRegionCodeSelector :: Selector
setRegionCodeSelector = mkSelector "setRegionCode:"

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

-- | @Selector@ for @summaryItems@
summaryItemsSelector :: Selector
summaryItemsSelector = mkSelector "summaryItems"

-- | @Selector@ for @setSummaryItems:@
setSummaryItemsSelector :: Selector
setSummaryItemsSelector = mkSelector "setSummaryItems:"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @setCurrencyCode:@
setCurrencyCodeSelector :: Selector
setCurrencyCodeSelector = mkSelector "setCurrencyCode:"

-- | @Selector@ for @requiredRecipientContactFields@
requiredRecipientContactFieldsSelector :: Selector
requiredRecipientContactFieldsSelector = mkSelector "requiredRecipientContactFields"

-- | @Selector@ for @setRequiredRecipientContactFields:@
setRequiredRecipientContactFieldsSelector :: Selector
setRequiredRecipientContactFieldsSelector = mkSelector "setRequiredRecipientContactFields:"

-- | @Selector@ for @recipientContact@
recipientContactSelector :: Selector
recipientContactSelector = mkSelector "recipientContact"

-- | @Selector@ for @setRecipientContact:@
setRecipientContactSelector :: Selector
setRecipientContactSelector = mkSelector "setRecipientContact:"

-- | @Selector@ for @supportedRegions@
supportedRegionsSelector :: Selector
supportedRegionsSelector = mkSelector "supportedRegions"

-- | @Selector@ for @setSupportedRegions:@
setSupportedRegionsSelector :: Selector
setSupportedRegionsSelector = mkSelector "setSupportedRegions:"

-- | @Selector@ for @applicationData@
applicationDataSelector :: Selector
applicationDataSelector = mkSelector "applicationData"

-- | @Selector@ for @setApplicationData:@
setApplicationDataSelector :: Selector
setApplicationDataSelector = mkSelector "setApplicationData:"

