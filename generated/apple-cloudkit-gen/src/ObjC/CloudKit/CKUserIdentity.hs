{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKUserIdentity@.
module ObjC.CloudKit.CKUserIdentity
  ( CKUserIdentity
  , IsCKUserIdentity(..)
  , init_
  , new
  , userRecordID
  , lookupInfo
  , nameComponents
  , hasiCloudAccount
  , contactIdentifiers
  , contactIdentifiersSelector
  , hasiCloudAccountSelector
  , initSelector
  , lookupInfoSelector
  , nameComponentsSelector
  , newSelector
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

-- | Use @CKDiscoverUserIdentitiesOperation@ or @CKFetchShareParticipantsOperation@ to create a @CKUserIdentity@
--
-- ObjC selector: @- init@
init_ :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKUserIdentity)
init_ ckUserIdentity =
  sendOwnedMessage ckUserIdentity initSelector

-- | @+ new@
new :: IO (Id CKUserIdentity)
new  =
  do
    cls' <- getRequiredClass "CKUserIdentity"
    sendOwnedClassMessage cls' newSelector

-- | @- userRecordID@
userRecordID :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKRecordID)
userRecordID ckUserIdentity =
  sendMessage ckUserIdentity userRecordIDSelector

-- | This is the @lookupInfo@ you passed in to @CKDiscoverUserIdentitiesOperation@ or @CKFetchShareParticipantsOperation@
--
-- ObjC selector: @- lookupInfo@
lookupInfo :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKUserIdentityLookupInfo)
lookupInfo ckUserIdentity =
  sendMessage ckUserIdentity lookupInfoSelector

-- | @- nameComponents@
nameComponents :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id NSPersonNameComponents)
nameComponents ckUserIdentity =
  sendMessage ckUserIdentity nameComponentsSelector

-- | @- hasiCloudAccount@
hasiCloudAccount :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO Bool
hasiCloudAccount ckUserIdentity =
  sendMessage ckUserIdentity hasiCloudAccountSelector

-- | @- contactIdentifiers@
contactIdentifiers :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id NSArray)
contactIdentifiers ckUserIdentity =
  sendMessage ckUserIdentity contactIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKUserIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKUserIdentity)
newSelector = mkSelector "new"

-- | @Selector@ for @userRecordID@
userRecordIDSelector :: Selector '[] (Id CKRecordID)
userRecordIDSelector = mkSelector "userRecordID"

-- | @Selector@ for @lookupInfo@
lookupInfoSelector :: Selector '[] (Id CKUserIdentityLookupInfo)
lookupInfoSelector = mkSelector "lookupInfo"

-- | @Selector@ for @nameComponents@
nameComponentsSelector :: Selector '[] (Id NSPersonNameComponents)
nameComponentsSelector = mkSelector "nameComponents"

-- | @Selector@ for @hasiCloudAccount@
hasiCloudAccountSelector :: Selector '[] Bool
hasiCloudAccountSelector = mkSelector "hasiCloudAccount"

-- | @Selector@ for @contactIdentifiers@
contactIdentifiersSelector :: Selector '[] (Id NSArray)
contactIdentifiersSelector = mkSelector "contactIdentifiers"

