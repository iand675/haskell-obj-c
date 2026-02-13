{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentAuthorizationViewController@.
module ObjC.PassKit.PKPaymentAuthorizationViewController
  ( PKPaymentAuthorizationViewController
  , IsPKPaymentAuthorizationViewController(..)
  , canMakePayments
  , canMakePaymentsUsingNetworks
  , canMakePaymentsUsingNetworks_capabilities
  , initWithPaymentRequest
  , supportsDisbursements
  , supportsDisbursementsUsingNetworks
  , supportsDisbursementsUsingNetworks_capabilities
  , initWithDisbursementRequest
  , delegate
  , setDelegate
  , canMakePaymentsSelector
  , canMakePaymentsUsingNetworksSelector
  , canMakePaymentsUsingNetworks_capabilitiesSelector
  , delegateSelector
  , initWithDisbursementRequestSelector
  , initWithPaymentRequestSelector
  , setDelegateSelector
  , supportsDisbursementsSelector
  , supportsDisbursementsUsingNetworksSelector
  , supportsDisbursementsUsingNetworks_capabilitiesSelector

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' canMakePaymentsSelector

-- | @+ canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
canMakePaymentsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' canMakePaymentsUsingNetworksSelector (toNSArray supportedNetworks)

-- | @+ canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
canMakePaymentsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' canMakePaymentsUsingNetworks_capabilitiesSelector (toNSArray supportedNetworks) capabilties

-- | @- initWithPaymentRequest:@
initWithPaymentRequest :: (IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController, IsPKPaymentRequest request) => pkPaymentAuthorizationViewController -> request -> IO (Id PKPaymentAuthorizationViewController)
initWithPaymentRequest pkPaymentAuthorizationViewController request =
  sendOwnedMessage pkPaymentAuthorizationViewController initWithPaymentRequestSelector (toPKPaymentRequest request)

-- | @+ supportsDisbursements@
supportsDisbursements :: IO Bool
supportsDisbursements  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' supportsDisbursementsSelector

-- | @+ supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
supportsDisbursementsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' supportsDisbursementsUsingNetworksSelector (toNSArray supportedNetworks)

-- | @+ supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
supportsDisbursementsUsingNetworks_capabilities supportedNetworks capabilities =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    sendClassMessage cls' supportsDisbursementsUsingNetworks_capabilitiesSelector (toNSArray supportedNetworks) capabilities

-- | @- initWithDisbursementRequest:@
initWithDisbursementRequest :: (IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController, IsPKDisbursementRequest request) => pkPaymentAuthorizationViewController -> request -> IO (Id PKPaymentAuthorizationViewController)
initWithDisbursementRequest pkPaymentAuthorizationViewController request =
  sendOwnedMessage pkPaymentAuthorizationViewController initWithDisbursementRequestSelector (toPKDisbursementRequest request)

-- | @- delegate@
delegate :: IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController => pkPaymentAuthorizationViewController -> IO RawId
delegate pkPaymentAuthorizationViewController =
  sendMessage pkPaymentAuthorizationViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController => pkPaymentAuthorizationViewController -> RawId -> IO ()
setDelegate pkPaymentAuthorizationViewController value =
  sendMessage pkPaymentAuthorizationViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector '[] Bool
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworksSelector :: Selector '[Id NSArray] Bool
canMakePaymentsUsingNetworksSelector = mkSelector "canMakePaymentsUsingNetworks:"

-- | @Selector@ for @canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilitiesSelector :: Selector '[Id NSArray, PKMerchantCapability] Bool
canMakePaymentsUsingNetworks_capabilitiesSelector = mkSelector "canMakePaymentsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithPaymentRequest:@
initWithPaymentRequestSelector :: Selector '[Id PKPaymentRequest] (Id PKPaymentAuthorizationViewController)
initWithPaymentRequestSelector = mkSelector "initWithPaymentRequest:"

-- | @Selector@ for @supportsDisbursements@
supportsDisbursementsSelector :: Selector '[] Bool
supportsDisbursementsSelector = mkSelector "supportsDisbursements"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworksSelector :: Selector '[Id NSArray] Bool
supportsDisbursementsUsingNetworksSelector = mkSelector "supportsDisbursementsUsingNetworks:"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilitiesSelector :: Selector '[Id NSArray, PKMerchantCapability] Bool
supportsDisbursementsUsingNetworks_capabilitiesSelector = mkSelector "supportsDisbursementsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithDisbursementRequest:@
initWithDisbursementRequestSelector :: Selector '[Id PKDisbursementRequest] (Id PKPaymentAuthorizationViewController)
initWithDisbursementRequestSelector = mkSelector "initWithDisbursementRequest:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

