{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKDatabaseSubscription
--
-- A subscription fires whenever any change happens in the database that this subscription was saved in.
--
-- @CKDatabaseSubscription@ is only supported in the Private and Shared databases.
--
-- Generated bindings for @CKDatabaseSubscription@.
module ObjC.CloudKit.CKDatabaseSubscription
  ( CKDatabaseSubscription
  , IsCKDatabaseSubscription(..)
  , init_
  , new
  , initWithSubscriptionID
  , initWithCoder
  , recordType
  , setRecordType
  , initSelector
  , initWithCoderSelector
  , initWithSubscriptionIDSelector
  , newSelector
  , recordTypeSelector
  , setRecordTypeSelector


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
init_ :: IsCKDatabaseSubscription ckDatabaseSubscription => ckDatabaseSubscription -> IO (Id CKDatabaseSubscription)
init_ ckDatabaseSubscription =
  sendOwnedMessage ckDatabaseSubscription initSelector

-- | @+ new@
new :: IO (Id CKDatabaseSubscription)
new  =
  do
    cls' <- getRequiredClass "CKDatabaseSubscription"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithSubscriptionID:@
initWithSubscriptionID :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSString subscriptionID) => ckDatabaseSubscription -> subscriptionID -> IO (Id CKDatabaseSubscription)
initWithSubscriptionID ckDatabaseSubscription subscriptionID =
  sendOwnedMessage ckDatabaseSubscription initWithSubscriptionIDSelector (toNSString subscriptionID)

-- | @- initWithCoder:@
initWithCoder :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSCoder aDecoder) => ckDatabaseSubscription -> aDecoder -> IO (Id CKDatabaseSubscription)
initWithCoder ckDatabaseSubscription aDecoder =
  sendOwnedMessage ckDatabaseSubscription initWithCoderSelector (toNSCoder aDecoder)

-- | Optional property. If set, a database subscription is scoped to record changes for this record type
--
-- ObjC selector: @- recordType@
recordType :: IsCKDatabaseSubscription ckDatabaseSubscription => ckDatabaseSubscription -> IO (Id NSString)
recordType ckDatabaseSubscription =
  sendMessage ckDatabaseSubscription recordTypeSelector

-- | Optional property. If set, a database subscription is scoped to record changes for this record type
--
-- ObjC selector: @- setRecordType:@
setRecordType :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSString value) => ckDatabaseSubscription -> value -> IO ()
setRecordType ckDatabaseSubscription value =
  sendMessage ckDatabaseSubscription setRecordTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKDatabaseSubscription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKDatabaseSubscription)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSubscriptionID:@
initWithSubscriptionIDSelector :: Selector '[Id NSString] (Id CKDatabaseSubscription)
initWithSubscriptionIDSelector = mkSelector "initWithSubscriptionID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CKDatabaseSubscription)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @setRecordType:@
setRecordTypeSelector :: Selector '[Id NSString] ()
setRecordTypeSelector = mkSelector "setRecordType:"

