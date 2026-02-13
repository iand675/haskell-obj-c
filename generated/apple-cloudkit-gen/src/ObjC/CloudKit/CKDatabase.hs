{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKDatabase@.
module ObjC.CloudKit.CKDatabase
  ( CKDatabase
  , IsCKDatabase(..)
  , init_
  , new
  , addOperation
  , fetchRecordWithID_completionHandler
  , saveRecord_completionHandler
  , deleteRecordWithID_completionHandler
  , fetchRecordZoneWithID_completionHandler
  , saveRecordZone_completionHandler
  , deleteRecordZoneWithID_completionHandler
  , fetchSubscriptionWithID_completionHandler
  , saveSubscription_completionHandler
  , deleteSubscriptionWithID_completionHandler
  , databaseScope
  , addOperationSelector
  , databaseScopeSelector
  , deleteRecordWithID_completionHandlerSelector
  , deleteRecordZoneWithID_completionHandlerSelector
  , deleteSubscriptionWithID_completionHandlerSelector
  , fetchRecordWithID_completionHandlerSelector
  , fetchRecordZoneWithID_completionHandlerSelector
  , fetchSubscriptionWithID_completionHandlerSelector
  , initSelector
  , newSelector
  , saveRecordZone_completionHandlerSelector
  , saveRecord_completionHandlerSelector
  , saveSubscription_completionHandlerSelector

  -- * Enum types
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKDatabase ckDatabase => ckDatabase -> IO (Id CKDatabase)
init_ ckDatabase =
  sendOwnedMessage ckDatabase initSelector

-- | @+ new@
new :: IO (Id CKDatabase)
new  =
  do
    cls' <- getRequiredClass "CKDatabase"
    sendOwnedClassMessage cls' newSelector

-- | @- addOperation:@
addOperation :: (IsCKDatabase ckDatabase, IsCKDatabaseOperation operation) => ckDatabase -> operation -> IO ()
addOperation ckDatabase operation =
  sendMessage ckDatabase addOperationSelector (toCKDatabaseOperation operation)

-- | @CKFetchRecordsOperation@ and @CKModifyRecordsOperation@ are the more configurable, @CKOperation@ -based alternatives to these methods
--
-- ObjC selector: @- fetchRecordWithID:completionHandler:@
fetchRecordWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordID recordID) => ckDatabase -> recordID -> Ptr () -> IO ()
fetchRecordWithID_completionHandler ckDatabase recordID completionHandler =
  sendMessage ckDatabase fetchRecordWithID_completionHandlerSelector (toCKRecordID recordID) completionHandler

-- | @- saveRecord:completionHandler:@
saveRecord_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecord record) => ckDatabase -> record -> Ptr () -> IO ()
saveRecord_completionHandler ckDatabase record completionHandler =
  sendMessage ckDatabase saveRecord_completionHandlerSelector (toCKRecord record) completionHandler

-- | @- deleteRecordWithID:completionHandler:@
deleteRecordWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordID recordID) => ckDatabase -> recordID -> Ptr () -> IO ()
deleteRecordWithID_completionHandler ckDatabase recordID completionHandler =
  sendMessage ckDatabase deleteRecordWithID_completionHandlerSelector (toCKRecordID recordID) completionHandler

-- | @- fetchRecordZoneWithID:completionHandler:@
fetchRecordZoneWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZoneID zoneID) => ckDatabase -> zoneID -> Ptr () -> IO ()
fetchRecordZoneWithID_completionHandler ckDatabase zoneID completionHandler =
  sendMessage ckDatabase fetchRecordZoneWithID_completionHandlerSelector (toCKRecordZoneID zoneID) completionHandler

-- | @- saveRecordZone:completionHandler:@
saveRecordZone_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZone zone) => ckDatabase -> zone -> Ptr () -> IO ()
saveRecordZone_completionHandler ckDatabase zone completionHandler =
  sendMessage ckDatabase saveRecordZone_completionHandlerSelector (toCKRecordZone zone) completionHandler

-- | @- deleteRecordZoneWithID:completionHandler:@
deleteRecordZoneWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZoneID zoneID) => ckDatabase -> zoneID -> Ptr () -> IO ()
deleteRecordZoneWithID_completionHandler ckDatabase zoneID completionHandler =
  sendMessage ckDatabase deleteRecordZoneWithID_completionHandlerSelector (toCKRecordZoneID zoneID) completionHandler

-- | @CKFetchSubscriptionsOperation@ and @CKModifySubscriptionsOperation@ are the more configurable, @CKOperation@ -based alternative to these methods
--
-- ObjC selector: @- fetchSubscriptionWithID:completionHandler:@
fetchSubscriptionWithID_completionHandler :: (IsCKDatabase ckDatabase, IsNSString subscriptionID) => ckDatabase -> subscriptionID -> Ptr () -> IO ()
fetchSubscriptionWithID_completionHandler ckDatabase subscriptionID completionHandler =
  sendMessage ckDatabase fetchSubscriptionWithID_completionHandlerSelector (toNSString subscriptionID) completionHandler

-- | @- saveSubscription:completionHandler:@
saveSubscription_completionHandler :: (IsCKDatabase ckDatabase, IsCKSubscription subscription) => ckDatabase -> subscription -> Ptr () -> IO ()
saveSubscription_completionHandler ckDatabase subscription completionHandler =
  sendMessage ckDatabase saveSubscription_completionHandlerSelector (toCKSubscription subscription) completionHandler

-- | @- deleteSubscriptionWithID:completionHandler:@
deleteSubscriptionWithID_completionHandler :: (IsCKDatabase ckDatabase, IsNSString subscriptionID) => ckDatabase -> subscriptionID -> Ptr () -> IO ()
deleteSubscriptionWithID_completionHandler ckDatabase subscriptionID completionHandler =
  sendMessage ckDatabase deleteSubscriptionWithID_completionHandlerSelector (toNSString subscriptionID) completionHandler

-- | @- databaseScope@
databaseScope :: IsCKDatabase ckDatabase => ckDatabase -> IO CKDatabaseScope
databaseScope ckDatabase =
  sendMessage ckDatabase databaseScopeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKDatabase)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKDatabase)
newSelector = mkSelector "new"

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector '[Id CKDatabaseOperation] ()
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @fetchRecordWithID:completionHandler:@
fetchRecordWithID_completionHandlerSelector :: Selector '[Id CKRecordID, Ptr ()] ()
fetchRecordWithID_completionHandlerSelector = mkSelector "fetchRecordWithID:completionHandler:"

-- | @Selector@ for @saveRecord:completionHandler:@
saveRecord_completionHandlerSelector :: Selector '[Id CKRecord, Ptr ()] ()
saveRecord_completionHandlerSelector = mkSelector "saveRecord:completionHandler:"

-- | @Selector@ for @deleteRecordWithID:completionHandler:@
deleteRecordWithID_completionHandlerSelector :: Selector '[Id CKRecordID, Ptr ()] ()
deleteRecordWithID_completionHandlerSelector = mkSelector "deleteRecordWithID:completionHandler:"

-- | @Selector@ for @fetchRecordZoneWithID:completionHandler:@
fetchRecordZoneWithID_completionHandlerSelector :: Selector '[Id CKRecordZoneID, Ptr ()] ()
fetchRecordZoneWithID_completionHandlerSelector = mkSelector "fetchRecordZoneWithID:completionHandler:"

-- | @Selector@ for @saveRecordZone:completionHandler:@
saveRecordZone_completionHandlerSelector :: Selector '[Id CKRecordZone, Ptr ()] ()
saveRecordZone_completionHandlerSelector = mkSelector "saveRecordZone:completionHandler:"

-- | @Selector@ for @deleteRecordZoneWithID:completionHandler:@
deleteRecordZoneWithID_completionHandlerSelector :: Selector '[Id CKRecordZoneID, Ptr ()] ()
deleteRecordZoneWithID_completionHandlerSelector = mkSelector "deleteRecordZoneWithID:completionHandler:"

-- | @Selector@ for @fetchSubscriptionWithID:completionHandler:@
fetchSubscriptionWithID_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
fetchSubscriptionWithID_completionHandlerSelector = mkSelector "fetchSubscriptionWithID:completionHandler:"

-- | @Selector@ for @saveSubscription:completionHandler:@
saveSubscription_completionHandlerSelector :: Selector '[Id CKSubscription, Ptr ()] ()
saveSubscription_completionHandlerSelector = mkSelector "saveSubscription:completionHandler:"

-- | @Selector@ for @deleteSubscriptionWithID:completionHandler:@
deleteSubscriptionWithID_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
deleteSubscriptionWithID_completionHandlerSelector = mkSelector "deleteSubscriptionWithID:completionHandler:"

-- | @Selector@ for @databaseScope@
databaseScopeSelector :: Selector '[] CKDatabaseScope
databaseScopeSelector = mkSelector "databaseScope"

