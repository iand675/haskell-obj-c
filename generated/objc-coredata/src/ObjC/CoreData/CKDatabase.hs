{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKDatabase@.
module ObjC.CoreData.CKDatabase
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
  , initSelector
  , newSelector
  , addOperationSelector
  , fetchRecordWithID_completionHandlerSelector
  , saveRecord_completionHandlerSelector
  , deleteRecordWithID_completionHandlerSelector
  , fetchRecordZoneWithID_completionHandlerSelector
  , saveRecordZone_completionHandlerSelector
  , deleteRecordZoneWithID_completionHandlerSelector
  , fetchSubscriptionWithID_completionHandlerSelector
  , saveSubscription_completionHandlerSelector
  , deleteSubscriptionWithID_completionHandlerSelector
  , databaseScopeSelector

  -- * Enum types
  , CKDatabaseScope(CKDatabaseScope)
  , pattern CKDatabaseScopePublic
  , pattern CKDatabaseScopePrivate
  , pattern CKDatabaseScopeShared

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

import ObjC.CoreData.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKDatabase ckDatabase => ckDatabase -> IO (Id CKDatabase)
init_ ckDatabase  =
  sendMsg ckDatabase (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKDatabase)
new  =
  do
    cls' <- getRequiredClass "CKDatabase"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- addOperation:@
addOperation :: (IsCKDatabase ckDatabase, IsCKDatabaseOperation operation) => ckDatabase -> operation -> IO ()
addOperation ckDatabase  operation =
withObjCPtr operation $ \raw_operation ->
    sendMsg ckDatabase (mkSelector "addOperation:") retVoid [argPtr (castPtr raw_operation :: Ptr ())]

-- | @CKFetchRecordsOperation@ and @CKModifyRecordsOperation@ are the more configurable, @CKOperation@ -based alternatives to these methods
--
-- ObjC selector: @- fetchRecordWithID:completionHandler:@
fetchRecordWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordID recordID) => ckDatabase -> recordID -> Ptr () -> IO ()
fetchRecordWithID_completionHandler ckDatabase  recordID completionHandler =
withObjCPtr recordID $ \raw_recordID ->
    sendMsg ckDatabase (mkSelector "fetchRecordWithID:completionHandler:") retVoid [argPtr (castPtr raw_recordID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveRecord:completionHandler:@
saveRecord_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecord record) => ckDatabase -> record -> Ptr () -> IO ()
saveRecord_completionHandler ckDatabase  record completionHandler =
withObjCPtr record $ \raw_record ->
    sendMsg ckDatabase (mkSelector "saveRecord:completionHandler:") retVoid [argPtr (castPtr raw_record :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteRecordWithID:completionHandler:@
deleteRecordWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordID recordID) => ckDatabase -> recordID -> Ptr () -> IO ()
deleteRecordWithID_completionHandler ckDatabase  recordID completionHandler =
withObjCPtr recordID $ \raw_recordID ->
    sendMsg ckDatabase (mkSelector "deleteRecordWithID:completionHandler:") retVoid [argPtr (castPtr raw_recordID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchRecordZoneWithID:completionHandler:@
fetchRecordZoneWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZoneID zoneID) => ckDatabase -> zoneID -> Ptr () -> IO ()
fetchRecordZoneWithID_completionHandler ckDatabase  zoneID completionHandler =
withObjCPtr zoneID $ \raw_zoneID ->
    sendMsg ckDatabase (mkSelector "fetchRecordZoneWithID:completionHandler:") retVoid [argPtr (castPtr raw_zoneID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveRecordZone:completionHandler:@
saveRecordZone_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZone zone) => ckDatabase -> zone -> Ptr () -> IO ()
saveRecordZone_completionHandler ckDatabase  zone completionHandler =
withObjCPtr zone $ \raw_zone ->
    sendMsg ckDatabase (mkSelector "saveRecordZone:completionHandler:") retVoid [argPtr (castPtr raw_zone :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteRecordZoneWithID:completionHandler:@
deleteRecordZoneWithID_completionHandler :: (IsCKDatabase ckDatabase, IsCKRecordZoneID zoneID) => ckDatabase -> zoneID -> Ptr () -> IO ()
deleteRecordZoneWithID_completionHandler ckDatabase  zoneID completionHandler =
withObjCPtr zoneID $ \raw_zoneID ->
    sendMsg ckDatabase (mkSelector "deleteRecordZoneWithID:completionHandler:") retVoid [argPtr (castPtr raw_zoneID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @CKFetchSubscriptionsOperation@ and @CKModifySubscriptionsOperation@ are the more configurable, @CKOperation@ -based alternative to these methods
--
-- ObjC selector: @- fetchSubscriptionWithID:completionHandler:@
fetchSubscriptionWithID_completionHandler :: (IsCKDatabase ckDatabase, IsNSString subscriptionID) => ckDatabase -> subscriptionID -> Ptr () -> IO ()
fetchSubscriptionWithID_completionHandler ckDatabase  subscriptionID completionHandler =
withObjCPtr subscriptionID $ \raw_subscriptionID ->
    sendMsg ckDatabase (mkSelector "fetchSubscriptionWithID:completionHandler:") retVoid [argPtr (castPtr raw_subscriptionID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- saveSubscription:completionHandler:@
saveSubscription_completionHandler :: (IsCKDatabase ckDatabase, IsCKSubscription subscription) => ckDatabase -> subscription -> Ptr () -> IO ()
saveSubscription_completionHandler ckDatabase  subscription completionHandler =
withObjCPtr subscription $ \raw_subscription ->
    sendMsg ckDatabase (mkSelector "saveSubscription:completionHandler:") retVoid [argPtr (castPtr raw_subscription :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- deleteSubscriptionWithID:completionHandler:@
deleteSubscriptionWithID_completionHandler :: (IsCKDatabase ckDatabase, IsNSString subscriptionID) => ckDatabase -> subscriptionID -> Ptr () -> IO ()
deleteSubscriptionWithID_completionHandler ckDatabase  subscriptionID completionHandler =
withObjCPtr subscriptionID $ \raw_subscriptionID ->
    sendMsg ckDatabase (mkSelector "deleteSubscriptionWithID:completionHandler:") retVoid [argPtr (castPtr raw_subscriptionID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- databaseScope@
databaseScope :: IsCKDatabase ckDatabase => ckDatabase -> IO CKDatabaseScope
databaseScope ckDatabase  =
  fmap (coerce :: CLong -> CKDatabaseScope) $ sendMsg ckDatabase (mkSelector "databaseScope") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @addOperation:@
addOperationSelector :: Selector
addOperationSelector = mkSelector "addOperation:"

-- | @Selector@ for @fetchRecordWithID:completionHandler:@
fetchRecordWithID_completionHandlerSelector :: Selector
fetchRecordWithID_completionHandlerSelector = mkSelector "fetchRecordWithID:completionHandler:"

-- | @Selector@ for @saveRecord:completionHandler:@
saveRecord_completionHandlerSelector :: Selector
saveRecord_completionHandlerSelector = mkSelector "saveRecord:completionHandler:"

-- | @Selector@ for @deleteRecordWithID:completionHandler:@
deleteRecordWithID_completionHandlerSelector :: Selector
deleteRecordWithID_completionHandlerSelector = mkSelector "deleteRecordWithID:completionHandler:"

-- | @Selector@ for @fetchRecordZoneWithID:completionHandler:@
fetchRecordZoneWithID_completionHandlerSelector :: Selector
fetchRecordZoneWithID_completionHandlerSelector = mkSelector "fetchRecordZoneWithID:completionHandler:"

-- | @Selector@ for @saveRecordZone:completionHandler:@
saveRecordZone_completionHandlerSelector :: Selector
saveRecordZone_completionHandlerSelector = mkSelector "saveRecordZone:completionHandler:"

-- | @Selector@ for @deleteRecordZoneWithID:completionHandler:@
deleteRecordZoneWithID_completionHandlerSelector :: Selector
deleteRecordZoneWithID_completionHandlerSelector = mkSelector "deleteRecordZoneWithID:completionHandler:"

-- | @Selector@ for @fetchSubscriptionWithID:completionHandler:@
fetchSubscriptionWithID_completionHandlerSelector :: Selector
fetchSubscriptionWithID_completionHandlerSelector = mkSelector "fetchSubscriptionWithID:completionHandler:"

-- | @Selector@ for @saveSubscription:completionHandler:@
saveSubscription_completionHandlerSelector :: Selector
saveSubscription_completionHandlerSelector = mkSelector "saveSubscription:completionHandler:"

-- | @Selector@ for @deleteSubscriptionWithID:completionHandler:@
deleteSubscriptionWithID_completionHandlerSelector :: Selector
deleteSubscriptionWithID_completionHandlerSelector = mkSelector "deleteSubscriptionWithID:completionHandler:"

-- | @Selector@ for @databaseScope@
databaseScopeSelector :: Selector
databaseScopeSelector = mkSelector "databaseScope"

