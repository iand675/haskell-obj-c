{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKUserIdentityLookupInfo@.
module ObjC.CloudKit.CKUserIdentityLookupInfo
  ( CKUserIdentityLookupInfo
  , IsCKUserIdentityLookupInfo(..)
  , init_
  , new
  , initWithEmailAddress
  , initWithPhoneNumber
  , initWithUserRecordID
  , lookupInfosWithEmails
  , lookupInfosWithPhoneNumbers
  , lookupInfosWithRecordIDs
  , emailAddress
  , phoneNumber
  , userRecordID
  , emailAddressSelector
  , initSelector
  , initWithEmailAddressSelector
  , initWithPhoneNumberSelector
  , initWithUserRecordIDSelector
  , lookupInfosWithEmailsSelector
  , lookupInfosWithPhoneNumbersSelector
  , lookupInfosWithRecordIDsSelector
  , newSelector
  , phoneNumberSelector
  , userRecordIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id CKUserIdentityLookupInfo)
init_ ckUserIdentityLookupInfo =
  sendOwnedMessage ckUserIdentityLookupInfo initSelector

-- | @+ new@
new :: IO (Id CKUserIdentityLookupInfo)
new  =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithEmailAddress:@
initWithEmailAddress :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsNSString emailAddress) => ckUserIdentityLookupInfo -> emailAddress -> IO (Id CKUserIdentityLookupInfo)
initWithEmailAddress ckUserIdentityLookupInfo emailAddress =
  sendOwnedMessage ckUserIdentityLookupInfo initWithEmailAddressSelector (toNSString emailAddress)

-- | @- initWithPhoneNumber:@
initWithPhoneNumber :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsNSString phoneNumber) => ckUserIdentityLookupInfo -> phoneNumber -> IO (Id CKUserIdentityLookupInfo)
initWithPhoneNumber ckUserIdentityLookupInfo phoneNumber =
  sendOwnedMessage ckUserIdentityLookupInfo initWithPhoneNumberSelector (toNSString phoneNumber)

-- | @- initWithUserRecordID:@
initWithUserRecordID :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsCKRecordID userRecordID) => ckUserIdentityLookupInfo -> userRecordID -> IO (Id CKUserIdentityLookupInfo)
initWithUserRecordID ckUserIdentityLookupInfo userRecordID =
  sendOwnedMessage ckUserIdentityLookupInfo initWithUserRecordIDSelector (toCKRecordID userRecordID)

-- | @+ lookupInfosWithEmails:@
lookupInfosWithEmails :: IsNSArray emails => emails -> IO (Id NSArray)
lookupInfosWithEmails emails =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    sendClassMessage cls' lookupInfosWithEmailsSelector (toNSArray emails)

-- | @+ lookupInfosWithPhoneNumbers:@
lookupInfosWithPhoneNumbers :: IsNSArray phoneNumbers => phoneNumbers -> IO (Id NSArray)
lookupInfosWithPhoneNumbers phoneNumbers =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    sendClassMessage cls' lookupInfosWithPhoneNumbersSelector (toNSArray phoneNumbers)

-- | @+ lookupInfosWithRecordIDs:@
lookupInfosWithRecordIDs :: IsNSArray recordIDs => recordIDs -> IO (Id NSArray)
lookupInfosWithRecordIDs recordIDs =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    sendClassMessage cls' lookupInfosWithRecordIDsSelector (toNSArray recordIDs)

-- | @- emailAddress@
emailAddress :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id NSString)
emailAddress ckUserIdentityLookupInfo =
  sendMessage ckUserIdentityLookupInfo emailAddressSelector

-- | @- phoneNumber@
phoneNumber :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id NSString)
phoneNumber ckUserIdentityLookupInfo =
  sendMessage ckUserIdentityLookupInfo phoneNumberSelector

-- | @- userRecordID@
userRecordID :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id CKRecordID)
userRecordID ckUserIdentityLookupInfo =
  sendMessage ckUserIdentityLookupInfo userRecordIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKUserIdentityLookupInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKUserIdentityLookupInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEmailAddress:@
initWithEmailAddressSelector :: Selector '[Id NSString] (Id CKUserIdentityLookupInfo)
initWithEmailAddressSelector = mkSelector "initWithEmailAddress:"

-- | @Selector@ for @initWithPhoneNumber:@
initWithPhoneNumberSelector :: Selector '[Id NSString] (Id CKUserIdentityLookupInfo)
initWithPhoneNumberSelector = mkSelector "initWithPhoneNumber:"

-- | @Selector@ for @initWithUserRecordID:@
initWithUserRecordIDSelector :: Selector '[Id CKRecordID] (Id CKUserIdentityLookupInfo)
initWithUserRecordIDSelector = mkSelector "initWithUserRecordID:"

-- | @Selector@ for @lookupInfosWithEmails:@
lookupInfosWithEmailsSelector :: Selector '[Id NSArray] (Id NSArray)
lookupInfosWithEmailsSelector = mkSelector "lookupInfosWithEmails:"

-- | @Selector@ for @lookupInfosWithPhoneNumbers:@
lookupInfosWithPhoneNumbersSelector :: Selector '[Id NSArray] (Id NSArray)
lookupInfosWithPhoneNumbersSelector = mkSelector "lookupInfosWithPhoneNumbers:"

-- | @Selector@ for @lookupInfosWithRecordIDs:@
lookupInfosWithRecordIDsSelector :: Selector '[Id NSArray] (Id NSArray)
lookupInfosWithRecordIDsSelector = mkSelector "lookupInfosWithRecordIDs:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @userRecordID@
userRecordIDSelector :: Selector '[] (Id CKRecordID)
userRecordIDSelector = mkSelector "userRecordID"

