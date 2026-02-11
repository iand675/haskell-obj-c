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
  , initSelector
  , newSelector
  , initWithEmailAddressSelector
  , initWithPhoneNumberSelector
  , initWithUserRecordIDSelector
  , lookupInfosWithEmailsSelector
  , lookupInfosWithPhoneNumbersSelector
  , lookupInfosWithRecordIDsSelector
  , emailAddressSelector
  , phoneNumberSelector
  , userRecordIDSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id CKUserIdentityLookupInfo)
init_ ckUserIdentityLookupInfo  =
  sendMsg ckUserIdentityLookupInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKUserIdentityLookupInfo)
new  =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEmailAddress:@
initWithEmailAddress :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsNSString emailAddress) => ckUserIdentityLookupInfo -> emailAddress -> IO (Id CKUserIdentityLookupInfo)
initWithEmailAddress ckUserIdentityLookupInfo  emailAddress =
withObjCPtr emailAddress $ \raw_emailAddress ->
    sendMsg ckUserIdentityLookupInfo (mkSelector "initWithEmailAddress:") (retPtr retVoid) [argPtr (castPtr raw_emailAddress :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPhoneNumber:@
initWithPhoneNumber :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsNSString phoneNumber) => ckUserIdentityLookupInfo -> phoneNumber -> IO (Id CKUserIdentityLookupInfo)
initWithPhoneNumber ckUserIdentityLookupInfo  phoneNumber =
withObjCPtr phoneNumber $ \raw_phoneNumber ->
    sendMsg ckUserIdentityLookupInfo (mkSelector "initWithPhoneNumber:") (retPtr retVoid) [argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithUserRecordID:@
initWithUserRecordID :: (IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo, IsCKRecordID userRecordID) => ckUserIdentityLookupInfo -> userRecordID -> IO (Id CKUserIdentityLookupInfo)
initWithUserRecordID ckUserIdentityLookupInfo  userRecordID =
withObjCPtr userRecordID $ \raw_userRecordID ->
    sendMsg ckUserIdentityLookupInfo (mkSelector "initWithUserRecordID:") (retPtr retVoid) [argPtr (castPtr raw_userRecordID :: Ptr ())] >>= ownedObject . castPtr

-- | @+ lookupInfosWithEmails:@
lookupInfosWithEmails :: IsNSArray emails => emails -> IO (Id NSArray)
lookupInfosWithEmails emails =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    withObjCPtr emails $ \raw_emails ->
      sendClassMsg cls' (mkSelector "lookupInfosWithEmails:") (retPtr retVoid) [argPtr (castPtr raw_emails :: Ptr ())] >>= retainedObject . castPtr

-- | @+ lookupInfosWithPhoneNumbers:@
lookupInfosWithPhoneNumbers :: IsNSArray phoneNumbers => phoneNumbers -> IO (Id NSArray)
lookupInfosWithPhoneNumbers phoneNumbers =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    withObjCPtr phoneNumbers $ \raw_phoneNumbers ->
      sendClassMsg cls' (mkSelector "lookupInfosWithPhoneNumbers:") (retPtr retVoid) [argPtr (castPtr raw_phoneNumbers :: Ptr ())] >>= retainedObject . castPtr

-- | @+ lookupInfosWithRecordIDs:@
lookupInfosWithRecordIDs :: IsNSArray recordIDs => recordIDs -> IO (Id NSArray)
lookupInfosWithRecordIDs recordIDs =
  do
    cls' <- getRequiredClass "CKUserIdentityLookupInfo"
    withObjCPtr recordIDs $ \raw_recordIDs ->
      sendClassMsg cls' (mkSelector "lookupInfosWithRecordIDs:") (retPtr retVoid) [argPtr (castPtr raw_recordIDs :: Ptr ())] >>= retainedObject . castPtr

-- | @- emailAddress@
emailAddress :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id NSString)
emailAddress ckUserIdentityLookupInfo  =
  sendMsg ckUserIdentityLookupInfo (mkSelector "emailAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneNumber@
phoneNumber :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id NSString)
phoneNumber ckUserIdentityLookupInfo  =
  sendMsg ckUserIdentityLookupInfo (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userRecordID@
userRecordID :: IsCKUserIdentityLookupInfo ckUserIdentityLookupInfo => ckUserIdentityLookupInfo -> IO (Id CKRecordID)
userRecordID ckUserIdentityLookupInfo  =
  sendMsg ckUserIdentityLookupInfo (mkSelector "userRecordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithEmailAddress:@
initWithEmailAddressSelector :: Selector
initWithEmailAddressSelector = mkSelector "initWithEmailAddress:"

-- | @Selector@ for @initWithPhoneNumber:@
initWithPhoneNumberSelector :: Selector
initWithPhoneNumberSelector = mkSelector "initWithPhoneNumber:"

-- | @Selector@ for @initWithUserRecordID:@
initWithUserRecordIDSelector :: Selector
initWithUserRecordIDSelector = mkSelector "initWithUserRecordID:"

-- | @Selector@ for @lookupInfosWithEmails:@
lookupInfosWithEmailsSelector :: Selector
lookupInfosWithEmailsSelector = mkSelector "lookupInfosWithEmails:"

-- | @Selector@ for @lookupInfosWithPhoneNumbers:@
lookupInfosWithPhoneNumbersSelector :: Selector
lookupInfosWithPhoneNumbersSelector = mkSelector "lookupInfosWithPhoneNumbers:"

-- | @Selector@ for @lookupInfosWithRecordIDs:@
lookupInfosWithRecordIDsSelector :: Selector
lookupInfosWithRecordIDsSelector = mkSelector "lookupInfosWithRecordIDs:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @userRecordID@
userRecordIDSelector :: Selector
userRecordIDSelector = mkSelector "userRecordID"

