{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VSUserAccount@.
module ObjC.VideoSubscriberAccount.VSUserAccount
  ( VSUserAccount
  , IsVSUserAccount(..)
  , init_
  , new
  , initWithAccountType_updateURL
  , updateURL
  , setUpdateURL
  , requiresSystemTrust
  , setRequiresSystemTrust
  , accountProviderIdentifier
  , setAccountProviderIdentifier
  , identifier
  , setIdentifier
  , accountType
  , setAccountType
  , signedOut
  , setSignedOut
  , subscriptionBillingCycleEndDate
  , setSubscriptionBillingCycleEndDate
  , tierIdentifiers
  , setTierIdentifiers
  , billingIdentifier
  , setBillingIdentifier
  , authenticationData
  , setAuthenticationData
  , fromCurrentDevice
  , deviceCategory
  , appleSubscription
  , setAppleSubscription
  , initSelector
  , newSelector
  , initWithAccountType_updateURLSelector
  , updateURLSelector
  , setUpdateURLSelector
  , requiresSystemTrustSelector
  , setRequiresSystemTrustSelector
  , accountProviderIdentifierSelector
  , setAccountProviderIdentifierSelector
  , identifierSelector
  , setIdentifierSelector
  , accountTypeSelector
  , setAccountTypeSelector
  , signedOutSelector
  , setSignedOutSelector
  , subscriptionBillingCycleEndDateSelector
  , setSubscriptionBillingCycleEndDateSelector
  , tierIdentifiersSelector
  , setTierIdentifiersSelector
  , billingIdentifierSelector
  , setBillingIdentifierSelector
  , authenticationDataSelector
  , setAuthenticationDataSelector
  , fromCurrentDeviceSelector
  , deviceCategorySelector
  , appleSubscriptionSelector
  , setAppleSubscriptionSelector

  -- * Enum types
  , VSOriginatingDeviceCategory(VSOriginatingDeviceCategory)
  , pattern VSOriginatingDeviceCategoryMobile
  , pattern VSOriginatingDeviceCategoryOther
  , VSUserAccountType(VSUserAccountType)
  , pattern VSUserAccountTypeFree
  , pattern VSUserAccountTypePaid

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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id VSUserAccount)
init_ vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VSUserAccount)
new  =
  do
    cls' <- getRequiredClass "VSUserAccount"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAccountType:updateURL:@
initWithAccountType_updateURL :: (IsVSUserAccount vsUserAccount, IsNSURL url) => vsUserAccount -> VSUserAccountType -> url -> IO (Id VSUserAccount)
initWithAccountType_updateURL vsUserAccount  accountType url =
withObjCPtr url $ \raw_url ->
    sendMsg vsUserAccount (mkSelector "initWithAccountType:updateURL:") (retPtr retVoid) [argCLong (coerce accountType), argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- updateURL@
updateURL :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSURL)
updateURL vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "updateURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateURL:@
setUpdateURL :: (IsVSUserAccount vsUserAccount, IsNSURL value) => vsUserAccount -> value -> IO ()
setUpdateURL vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setUpdateURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiresSystemTrust@
requiresSystemTrust :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
requiresSystemTrust vsUserAccount  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsUserAccount (mkSelector "requiresSystemTrust") retCULong []

-- | @- setRequiresSystemTrust:@
setRequiresSystemTrust :: IsVSUserAccount vsUserAccount => vsUserAccount -> Bool -> IO ()
setRequiresSystemTrust vsUserAccount  value =
  sendMsg vsUserAccount (mkSelector "setRequiresSystemTrust:") retVoid [argCULong (if value then 1 else 0)]

-- | @- accountProviderIdentifier@
accountProviderIdentifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
accountProviderIdentifier vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "accountProviderIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccountProviderIdentifier:@
setAccountProviderIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setAccountProviderIdentifier vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setAccountProviderIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
identifier vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setIdentifier vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accountType@
accountType :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO VSUserAccountType
accountType vsUserAccount  =
  fmap (coerce :: CLong -> VSUserAccountType) $ sendMsg vsUserAccount (mkSelector "accountType") retCLong []

-- | @- setAccountType:@
setAccountType :: IsVSUserAccount vsUserAccount => vsUserAccount -> VSUserAccountType -> IO ()
setAccountType vsUserAccount  value =
  sendMsg vsUserAccount (mkSelector "setAccountType:") retVoid [argCLong (coerce value)]

-- | @- signedOut@
signedOut :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
signedOut vsUserAccount  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsUserAccount (mkSelector "signedOut") retCULong []

-- | @- setSignedOut:@
setSignedOut :: IsVSUserAccount vsUserAccount => vsUserAccount -> Bool -> IO ()
setSignedOut vsUserAccount  value =
  sendMsg vsUserAccount (mkSelector "setSignedOut:") retVoid [argCULong (if value then 1 else 0)]

-- | @- subscriptionBillingCycleEndDate@
subscriptionBillingCycleEndDate :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSDate)
subscriptionBillingCycleEndDate vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "subscriptionBillingCycleEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionBillingCycleEndDate:@
setSubscriptionBillingCycleEndDate :: (IsVSUserAccount vsUserAccount, IsNSDate value) => vsUserAccount -> value -> IO ()
setSubscriptionBillingCycleEndDate vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setSubscriptionBillingCycleEndDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tierIdentifiers@
tierIdentifiers :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSArray)
tierIdentifiers vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "tierIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTierIdentifiers:@
setTierIdentifiers :: (IsVSUserAccount vsUserAccount, IsNSArray value) => vsUserAccount -> value -> IO ()
setTierIdentifiers vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setTierIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- billingIdentifier@
billingIdentifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
billingIdentifier vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "billingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBillingIdentifier:@
setBillingIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setBillingIdentifier vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setBillingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- authenticationData@
authenticationData :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
authenticationData vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "authenticationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAuthenticationData:@
setAuthenticationData :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setAuthenticationData vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setAuthenticationData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fromCurrentDevice@
fromCurrentDevice :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
fromCurrentDevice vsUserAccount  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsUserAccount (mkSelector "fromCurrentDevice") retCULong []

-- | @- deviceCategory@
deviceCategory :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO VSOriginatingDeviceCategory
deviceCategory vsUserAccount  =
  fmap (coerce :: CLong -> VSOriginatingDeviceCategory) $ sendMsg vsUserAccount (mkSelector "deviceCategory") retCLong []

-- | @- appleSubscription@
appleSubscription :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id VSAppleSubscription)
appleSubscription vsUserAccount  =
  sendMsg vsUserAccount (mkSelector "appleSubscription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAppleSubscription:@
setAppleSubscription :: (IsVSUserAccount vsUserAccount, IsVSAppleSubscription value) => vsUserAccount -> value -> IO ()
setAppleSubscription vsUserAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsUserAccount (mkSelector "setAppleSubscription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAccountType:updateURL:@
initWithAccountType_updateURLSelector :: Selector
initWithAccountType_updateURLSelector = mkSelector "initWithAccountType:updateURL:"

-- | @Selector@ for @updateURL@
updateURLSelector :: Selector
updateURLSelector = mkSelector "updateURL"

-- | @Selector@ for @setUpdateURL:@
setUpdateURLSelector :: Selector
setUpdateURLSelector = mkSelector "setUpdateURL:"

-- | @Selector@ for @requiresSystemTrust@
requiresSystemTrustSelector :: Selector
requiresSystemTrustSelector = mkSelector "requiresSystemTrust"

-- | @Selector@ for @setRequiresSystemTrust:@
setRequiresSystemTrustSelector :: Selector
setRequiresSystemTrustSelector = mkSelector "setRequiresSystemTrust:"

-- | @Selector@ for @accountProviderIdentifier@
accountProviderIdentifierSelector :: Selector
accountProviderIdentifierSelector = mkSelector "accountProviderIdentifier"

-- | @Selector@ for @setAccountProviderIdentifier:@
setAccountProviderIdentifierSelector :: Selector
setAccountProviderIdentifierSelector = mkSelector "setAccountProviderIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @setAccountType:@
setAccountTypeSelector :: Selector
setAccountTypeSelector = mkSelector "setAccountType:"

-- | @Selector@ for @signedOut@
signedOutSelector :: Selector
signedOutSelector = mkSelector "signedOut"

-- | @Selector@ for @setSignedOut:@
setSignedOutSelector :: Selector
setSignedOutSelector = mkSelector "setSignedOut:"

-- | @Selector@ for @subscriptionBillingCycleEndDate@
subscriptionBillingCycleEndDateSelector :: Selector
subscriptionBillingCycleEndDateSelector = mkSelector "subscriptionBillingCycleEndDate"

-- | @Selector@ for @setSubscriptionBillingCycleEndDate:@
setSubscriptionBillingCycleEndDateSelector :: Selector
setSubscriptionBillingCycleEndDateSelector = mkSelector "setSubscriptionBillingCycleEndDate:"

-- | @Selector@ for @tierIdentifiers@
tierIdentifiersSelector :: Selector
tierIdentifiersSelector = mkSelector "tierIdentifiers"

-- | @Selector@ for @setTierIdentifiers:@
setTierIdentifiersSelector :: Selector
setTierIdentifiersSelector = mkSelector "setTierIdentifiers:"

-- | @Selector@ for @billingIdentifier@
billingIdentifierSelector :: Selector
billingIdentifierSelector = mkSelector "billingIdentifier"

-- | @Selector@ for @setBillingIdentifier:@
setBillingIdentifierSelector :: Selector
setBillingIdentifierSelector = mkSelector "setBillingIdentifier:"

-- | @Selector@ for @authenticationData@
authenticationDataSelector :: Selector
authenticationDataSelector = mkSelector "authenticationData"

-- | @Selector@ for @setAuthenticationData:@
setAuthenticationDataSelector :: Selector
setAuthenticationDataSelector = mkSelector "setAuthenticationData:"

-- | @Selector@ for @fromCurrentDevice@
fromCurrentDeviceSelector :: Selector
fromCurrentDeviceSelector = mkSelector "fromCurrentDevice"

-- | @Selector@ for @deviceCategory@
deviceCategorySelector :: Selector
deviceCategorySelector = mkSelector "deviceCategory"

-- | @Selector@ for @appleSubscription@
appleSubscriptionSelector :: Selector
appleSubscriptionSelector = mkSelector "appleSubscription"

-- | @Selector@ for @setAppleSubscription:@
setAppleSubscriptionSelector :: Selector
setAppleSubscriptionSelector = mkSelector "setAppleSubscription:"

