{-# LANGUAGE PatternSynonyms #-}
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
  , initWithPaymentRequestSelector
  , supportsDisbursementsSelector
  , supportsDisbursementsUsingNetworksSelector
  , supportsDisbursementsUsingNetworks_capabilitiesSelector
  , initWithDisbursementRequestSelector
  , delegateSelector
  , setDelegateSelector

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePayments") retCULong []

-- | @+ canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
canMakePaymentsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePaymentsUsingNetworks:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ())]

-- | @+ canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
canMakePaymentsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePaymentsUsingNetworks:capabilities:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ()), argCULong (coerce capabilties)]

-- | @- initWithPaymentRequest:@
initWithPaymentRequest :: (IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController, IsPKPaymentRequest request) => pkPaymentAuthorizationViewController -> request -> IO (Id PKPaymentAuthorizationViewController)
initWithPaymentRequest pkPaymentAuthorizationViewController  request =
  withObjCPtr request $ \raw_request ->
      sendMsg pkPaymentAuthorizationViewController (mkSelector "initWithPaymentRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @+ supportsDisbursements@
supportsDisbursements :: IO Bool
supportsDisbursements  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursements") retCULong []

-- | @+ supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
supportsDisbursementsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursementsUsingNetworks:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ())]

-- | @+ supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
supportsDisbursementsUsingNetworks_capabilities supportedNetworks capabilities =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationViewController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursementsUsingNetworks:capabilities:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ()), argCULong (coerce capabilities)]

-- | @- initWithDisbursementRequest:@
initWithDisbursementRequest :: (IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController, IsPKDisbursementRequest request) => pkPaymentAuthorizationViewController -> request -> IO (Id PKPaymentAuthorizationViewController)
initWithDisbursementRequest pkPaymentAuthorizationViewController  request =
  withObjCPtr request $ \raw_request ->
      sendMsg pkPaymentAuthorizationViewController (mkSelector "initWithDisbursementRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @- delegate@
delegate :: IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController => pkPaymentAuthorizationViewController -> IO RawId
delegate pkPaymentAuthorizationViewController  =
    fmap (RawId . castPtr) $ sendMsg pkPaymentAuthorizationViewController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsPKPaymentAuthorizationViewController pkPaymentAuthorizationViewController => pkPaymentAuthorizationViewController -> RawId -> IO ()
setDelegate pkPaymentAuthorizationViewController  value =
    sendMsg pkPaymentAuthorizationViewController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworksSelector :: Selector
canMakePaymentsUsingNetworksSelector = mkSelector "canMakePaymentsUsingNetworks:"

-- | @Selector@ for @canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilitiesSelector :: Selector
canMakePaymentsUsingNetworks_capabilitiesSelector = mkSelector "canMakePaymentsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithPaymentRequest:@
initWithPaymentRequestSelector :: Selector
initWithPaymentRequestSelector = mkSelector "initWithPaymentRequest:"

-- | @Selector@ for @supportsDisbursements@
supportsDisbursementsSelector :: Selector
supportsDisbursementsSelector = mkSelector "supportsDisbursements"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworksSelector :: Selector
supportsDisbursementsUsingNetworksSelector = mkSelector "supportsDisbursementsUsingNetworks:"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilitiesSelector :: Selector
supportsDisbursementsUsingNetworks_capabilitiesSelector = mkSelector "supportsDisbursementsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithDisbursementRequest:@
initWithDisbursementRequestSelector :: Selector
initWithDisbursementRequestSelector = mkSelector "initWithDisbursementRequest:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

