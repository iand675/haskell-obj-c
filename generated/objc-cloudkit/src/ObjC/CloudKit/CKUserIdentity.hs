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
  , initSelector
  , newSelector
  , userRecordIDSelector
  , lookupInfoSelector
  , nameComponentsSelector
  , hasiCloudAccountSelector
  , contactIdentifiersSelector


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

-- | Use @CKDiscoverUserIdentitiesOperation@ or @CKFetchShareParticipantsOperation@ to create a @CKUserIdentity@
--
-- ObjC selector: @- init@
init_ :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKUserIdentity)
init_ ckUserIdentity  =
  sendMsg ckUserIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKUserIdentity)
new  =
  do
    cls' <- getRequiredClass "CKUserIdentity"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- userRecordID@
userRecordID :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKRecordID)
userRecordID ckUserIdentity  =
  sendMsg ckUserIdentity (mkSelector "userRecordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This is the @lookupInfo@ you passed in to @CKDiscoverUserIdentitiesOperation@ or @CKFetchShareParticipantsOperation@
--
-- ObjC selector: @- lookupInfo@
lookupInfo :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id CKUserIdentityLookupInfo)
lookupInfo ckUserIdentity  =
  sendMsg ckUserIdentity (mkSelector "lookupInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nameComponents@
nameComponents :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id NSPersonNameComponents)
nameComponents ckUserIdentity  =
  sendMsg ckUserIdentity (mkSelector "nameComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasiCloudAccount@
hasiCloudAccount :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO Bool
hasiCloudAccount ckUserIdentity  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckUserIdentity (mkSelector "hasiCloudAccount") retCULong []

-- | @- contactIdentifiers@
contactIdentifiers :: IsCKUserIdentity ckUserIdentity => ckUserIdentity -> IO (Id NSArray)
contactIdentifiers ckUserIdentity  =
  sendMsg ckUserIdentity (mkSelector "contactIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @userRecordID@
userRecordIDSelector :: Selector
userRecordIDSelector = mkSelector "userRecordID"

-- | @Selector@ for @lookupInfo@
lookupInfoSelector :: Selector
lookupInfoSelector = mkSelector "lookupInfo"

-- | @Selector@ for @nameComponents@
nameComponentsSelector :: Selector
nameComponentsSelector = mkSelector "nameComponents"

-- | @Selector@ for @hasiCloudAccount@
hasiCloudAccountSelector :: Selector
hasiCloudAccountSelector = mkSelector "hasiCloudAccount"

-- | @Selector@ for @contactIdentifiers@
contactIdentifiersSelector :: Selector
contactIdentifiersSelector = mkSelector "contactIdentifiers"

