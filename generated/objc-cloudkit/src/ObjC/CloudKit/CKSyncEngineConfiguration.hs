{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSyncEngineConfiguration@.
module ObjC.CloudKit.CKSyncEngineConfiguration
  ( CKSyncEngineConfiguration
  , IsCKSyncEngineConfiguration(..)
  , initWithDatabase_stateSerialization_delegate
  , init_
  , new
  , database
  , setDatabase
  , stateSerialization
  , setStateSerialization
  , automaticallySync
  , setAutomaticallySync
  , subscriptionID
  , setSubscriptionID
  , initWithDatabase_stateSerialization_delegateSelector
  , initSelector
  , newSelector
  , databaseSelector
  , setDatabaseSelector
  , stateSerializationSelector
  , setStateSerializationSelector
  , automaticallySyncSelector
  , setAutomaticallySyncSelector
  , subscriptionIDSelector
  , setSubscriptionIDSelector


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
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDatabase:stateSerialization:delegate:@
initWithDatabase_stateSerialization_delegate :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKDatabase database, IsCKSyncEngineStateSerialization stateSerialization) => ckSyncEngineConfiguration -> database -> stateSerialization -> RawId -> IO (Id CKSyncEngineConfiguration)
initWithDatabase_stateSerialization_delegate ckSyncEngineConfiguration  database stateSerialization delegate =
withObjCPtr database $ \raw_database ->
  withObjCPtr stateSerialization $ \raw_stateSerialization ->
      sendMsg ckSyncEngineConfiguration (mkSelector "initWithDatabase:stateSerialization:delegate:") (retPtr retVoid) [argPtr (castPtr raw_database :: Ptr ()), argPtr (castPtr raw_stateSerialization :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKSyncEngineConfiguration)
init_ ckSyncEngineConfiguration  =
  sendMsg ckSyncEngineConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineConfiguration)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The database for this sync engine to sync with.
--
-- You can have multiple instances of @CKSyncEngine@ in the same process, each targeting a different database. For example, you might have one for your private database and one for your shared database.
--
-- It's also technically possible to have multiple instances of @CKSyncEngine@ for the same ``CKDatabase``. This isn't recommended for production code, but it can be helpful for testing your @CKSyncEngine@ integration. For example, you might make multiple @CKSyncEngine@ instances to simulate multiple devices syncing back and forth.
--
-- ObjC selector: @- database@
database :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKDatabase)
database ckSyncEngineConfiguration  =
  sendMsg ckSyncEngineConfiguration (mkSelector "database") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The database for this sync engine to sync with.
--
-- You can have multiple instances of @CKSyncEngine@ in the same process, each targeting a different database. For example, you might have one for your private database and one for your shared database.
--
-- It's also technically possible to have multiple instances of @CKSyncEngine@ for the same ``CKDatabase``. This isn't recommended for production code, but it can be helpful for testing your @CKSyncEngine@ integration. For example, you might make multiple @CKSyncEngine@ instances to simulate multiple devices syncing back and forth.
--
-- ObjC selector: @- setDatabase:@
setDatabase :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKDatabase value) => ckSyncEngineConfiguration -> value -> IO ()
setDatabase ckSyncEngineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineConfiguration (mkSelector "setDatabase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The state serialization you last received in a ``CKSyncEngine/Event/StateUpdate``.
--
-- If this is the first time ever initializing your @CKSyncEngine@, you can provide @nil@.
--
-- ObjC selector: @- stateSerialization@
stateSerialization :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKSyncEngineStateSerialization)
stateSerialization ckSyncEngineConfiguration  =
  sendMsg ckSyncEngineConfiguration (mkSelector "stateSerialization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The state serialization you last received in a ``CKSyncEngine/Event/StateUpdate``.
--
-- If this is the first time ever initializing your @CKSyncEngine@, you can provide @nil@.
--
-- ObjC selector: @- setStateSerialization:@
setStateSerialization :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKSyncEngineStateSerialization value) => ckSyncEngineConfiguration -> value -> IO ()
setStateSerialization ckSyncEngineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineConfiguration (mkSelector "setStateSerialization:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether or not the sync engine should automatically sync on your behalf.
--
-- If true, then the sync engine will automatically sync using the system scheduler. This is the default value. When you add pending changes to the state, the sync engine will automatically schedule a sync task to send changes. When it receives a notification about new changes on the server, it will automatically schedule a sync task to fetch changes. It will also automatically re-schedule sync tasks for retryable errors such as network failures or server throttles.
--
-- If ``CKSyncEngineConfiguration/automaticallySync`` is off, then the sync engine will not perform any operations unless you tell it to do so via ``CKSyncEngine/fetchChanges(_:)`` or ``CKSyncEngine/sendChanges(_:)``.
--
-- Most applications likely want to enable automatic syncing during normal use. However, you might want to disable it if you have specific requirements for when you want to sync. For example, if you want to sync only once per day, you can turn off automatic sync and manually call ``CKSyncEngine/fetchChanges(_:)`` and ``CKSyncEngine/sendChanges(_:)`` once per day.
--
-- You might also disable automatic sync when writing automated tests for your integration with @CKSyncEngine@. This way, you can have fine grained control over exactly when the sync engine fetches or sends changes. This allows you to simulate edge cases and deterministically test your logic around scenarios like conflict resolution and error handling.
--
-- ObjC selector: @- automaticallySync@
automaticallySync :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO Bool
automaticallySync ckSyncEngineConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineConfiguration (mkSelector "automaticallySync") retCULong []

-- | Whether or not the sync engine should automatically sync on your behalf.
--
-- If true, then the sync engine will automatically sync using the system scheduler. This is the default value. When you add pending changes to the state, the sync engine will automatically schedule a sync task to send changes. When it receives a notification about new changes on the server, it will automatically schedule a sync task to fetch changes. It will also automatically re-schedule sync tasks for retryable errors such as network failures or server throttles.
--
-- If ``CKSyncEngineConfiguration/automaticallySync`` is off, then the sync engine will not perform any operations unless you tell it to do so via ``CKSyncEngine/fetchChanges(_:)`` or ``CKSyncEngine/sendChanges(_:)``.
--
-- Most applications likely want to enable automatic syncing during normal use. However, you might want to disable it if you have specific requirements for when you want to sync. For example, if you want to sync only once per day, you can turn off automatic sync and manually call ``CKSyncEngine/fetchChanges(_:)`` and ``CKSyncEngine/sendChanges(_:)`` once per day.
--
-- You might also disable automatic sync when writing automated tests for your integration with @CKSyncEngine@. This way, you can have fine grained control over exactly when the sync engine fetches or sends changes. This allows you to simulate edge cases and deterministically test your logic around scenarios like conflict resolution and error handling.
--
-- ObjC selector: @- setAutomaticallySync:@
setAutomaticallySync :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> Bool -> IO ()
setAutomaticallySync ckSyncEngineConfiguration  value =
  sendMsg ckSyncEngineConfiguration (mkSelector "setAutomaticallySync:") retVoid [argCULong (if value then 1 else 0)]

-- | An optional override for the sync engine's default database subscription ID. Use this for backward compatibility with a previous CloudKit sync implementation.
--
-- By default, @CKSyncEngine@ will create its own ``CKDatabaseSubscription`` with its own subscription ID. If you're migrating to @CKSyncEngine@ from a custom CloudKit sync implementation, you can specify your previous subscription ID here. This allows your @CKSyncEngine@ integration to be backward compatible with previous versions of your app.
--
-- >Note: @CKSyncEngine@ will automatically attempt to discover any previous database subscriptions, but you can be more explicit by giving the subscription ID through this configuration option.
--
-- ObjC selector: @- subscriptionID@
subscriptionID :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id NSString)
subscriptionID ckSyncEngineConfiguration  =
  sendMsg ckSyncEngineConfiguration (mkSelector "subscriptionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional override for the sync engine's default database subscription ID. Use this for backward compatibility with a previous CloudKit sync implementation.
--
-- By default, @CKSyncEngine@ will create its own ``CKDatabaseSubscription`` with its own subscription ID. If you're migrating to @CKSyncEngine@ from a custom CloudKit sync implementation, you can specify your previous subscription ID here. This allows your @CKSyncEngine@ integration to be backward compatible with previous versions of your app.
--
-- >Note: @CKSyncEngine@ will automatically attempt to discover any previous database subscriptions, but you can be more explicit by giving the subscription ID through this configuration option.
--
-- ObjC selector: @- setSubscriptionID:@
setSubscriptionID :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsNSString value) => ckSyncEngineConfiguration -> value -> IO ()
setSubscriptionID ckSyncEngineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineConfiguration (mkSelector "setSubscriptionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDatabase:stateSerialization:delegate:@
initWithDatabase_stateSerialization_delegateSelector :: Selector
initWithDatabase_stateSerialization_delegateSelector = mkSelector "initWithDatabase:stateSerialization:delegate:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @database@
databaseSelector :: Selector
databaseSelector = mkSelector "database"

-- | @Selector@ for @setDatabase:@
setDatabaseSelector :: Selector
setDatabaseSelector = mkSelector "setDatabase:"

-- | @Selector@ for @stateSerialization@
stateSerializationSelector :: Selector
stateSerializationSelector = mkSelector "stateSerialization"

-- | @Selector@ for @setStateSerialization:@
setStateSerializationSelector :: Selector
setStateSerializationSelector = mkSelector "setStateSerialization:"

-- | @Selector@ for @automaticallySync@
automaticallySyncSelector :: Selector
automaticallySyncSelector = mkSelector "automaticallySync"

-- | @Selector@ for @setAutomaticallySync:@
setAutomaticallySyncSelector :: Selector
setAutomaticallySyncSelector = mkSelector "setAutomaticallySync:"

-- | @Selector@ for @subscriptionID@
subscriptionIDSelector :: Selector
subscriptionIDSelector = mkSelector "subscriptionID"

-- | @Selector@ for @setSubscriptionID:@
setSubscriptionIDSelector :: Selector
setSubscriptionIDSelector = mkSelector "setSubscriptionID:"

