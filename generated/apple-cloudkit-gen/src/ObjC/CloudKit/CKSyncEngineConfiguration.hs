{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , automaticallySync
  , setAutomaticallySync
  , subscriptionID
  , setSubscriptionID
  , automaticallySyncSelector
  , databaseSelector
  , delegateSelector
  , initSelector
  , initWithDatabase_stateSerialization_delegateSelector
  , newSelector
  , setAutomaticallySyncSelector
  , setDatabaseSelector
  , setDelegateSelector
  , setStateSerializationSelector
  , setSubscriptionIDSelector
  , stateSerializationSelector
  , subscriptionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDatabase:stateSerialization:delegate:@
initWithDatabase_stateSerialization_delegate :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKDatabase database, IsCKSyncEngineStateSerialization stateSerialization) => ckSyncEngineConfiguration -> database -> stateSerialization -> RawId -> IO (Id CKSyncEngineConfiguration)
initWithDatabase_stateSerialization_delegate ckSyncEngineConfiguration database stateSerialization delegate =
  sendOwnedMessage ckSyncEngineConfiguration initWithDatabase_stateSerialization_delegateSelector (toCKDatabase database) (toCKSyncEngineStateSerialization stateSerialization) delegate

-- | @- init@
init_ :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKSyncEngineConfiguration)
init_ ckSyncEngineConfiguration =
  sendOwnedMessage ckSyncEngineConfiguration initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineConfiguration)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | The database for this sync engine to sync with.
--
-- You can have multiple instances of @CKSyncEngine@ in the same process, each targeting a different database. For example, you might have one for your private database and one for your shared database.
--
-- It's also technically possible to have multiple instances of @CKSyncEngine@ for the same ``CKDatabase``. This isn't recommended for production code, but it can be helpful for testing your @CKSyncEngine@ integration. For example, you might make multiple @CKSyncEngine@ instances to simulate multiple devices syncing back and forth.
--
-- ObjC selector: @- database@
database :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKDatabase)
database ckSyncEngineConfiguration =
  sendMessage ckSyncEngineConfiguration databaseSelector

-- | The database for this sync engine to sync with.
--
-- You can have multiple instances of @CKSyncEngine@ in the same process, each targeting a different database. For example, you might have one for your private database and one for your shared database.
--
-- It's also technically possible to have multiple instances of @CKSyncEngine@ for the same ``CKDatabase``. This isn't recommended for production code, but it can be helpful for testing your @CKSyncEngine@ integration. For example, you might make multiple @CKSyncEngine@ instances to simulate multiple devices syncing back and forth.
--
-- ObjC selector: @- setDatabase:@
setDatabase :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKDatabase value) => ckSyncEngineConfiguration -> value -> IO ()
setDatabase ckSyncEngineConfiguration value =
  sendMessage ckSyncEngineConfiguration setDatabaseSelector (toCKDatabase value)

-- | The state serialization you last received in a ``CKSyncEngine/Event/StateUpdate``.
--
-- If this is the first time ever initializing your @CKSyncEngine@, you can provide @nil@.
--
-- ObjC selector: @- stateSerialization@
stateSerialization :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id CKSyncEngineStateSerialization)
stateSerialization ckSyncEngineConfiguration =
  sendMessage ckSyncEngineConfiguration stateSerializationSelector

-- | The state serialization you last received in a ``CKSyncEngine/Event/StateUpdate``.
--
-- If this is the first time ever initializing your @CKSyncEngine@, you can provide @nil@.
--
-- ObjC selector: @- setStateSerialization:@
setStateSerialization :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsCKSyncEngineStateSerialization value) => ckSyncEngineConfiguration -> value -> IO ()
setStateSerialization ckSyncEngineConfiguration value =
  sendMessage ckSyncEngineConfiguration setStateSerializationSelector (toCKSyncEngineStateSerialization value)

-- | Your implementation of @CKSyncEngineDelegate@.
--
-- ObjC selector: @- delegate@
delegate :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO RawId
delegate ckSyncEngineConfiguration =
  sendMessage ckSyncEngineConfiguration delegateSelector

-- | Your implementation of @CKSyncEngineDelegate@.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> RawId -> IO ()
setDelegate ckSyncEngineConfiguration value =
  sendMessage ckSyncEngineConfiguration setDelegateSelector value

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
automaticallySync ckSyncEngineConfiguration =
  sendMessage ckSyncEngineConfiguration automaticallySyncSelector

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
setAutomaticallySync ckSyncEngineConfiguration value =
  sendMessage ckSyncEngineConfiguration setAutomaticallySyncSelector value

-- | An optional override for the sync engine's default database subscription ID. Use this for backward compatibility with a previous CloudKit sync implementation.
--
-- By default, @CKSyncEngine@ will create its own ``CKDatabaseSubscription`` with its own subscription ID. If you're migrating to @CKSyncEngine@ from a custom CloudKit sync implementation, you can specify your previous subscription ID here. This allows your @CKSyncEngine@ integration to be backward compatible with previous versions of your app.
--
-- >Note: @CKSyncEngine@ will automatically attempt to discover any previous database subscriptions, but you can be more explicit by giving the subscription ID through this configuration option.
--
-- ObjC selector: @- subscriptionID@
subscriptionID :: IsCKSyncEngineConfiguration ckSyncEngineConfiguration => ckSyncEngineConfiguration -> IO (Id NSString)
subscriptionID ckSyncEngineConfiguration =
  sendMessage ckSyncEngineConfiguration subscriptionIDSelector

-- | An optional override for the sync engine's default database subscription ID. Use this for backward compatibility with a previous CloudKit sync implementation.
--
-- By default, @CKSyncEngine@ will create its own ``CKDatabaseSubscription`` with its own subscription ID. If you're migrating to @CKSyncEngine@ from a custom CloudKit sync implementation, you can specify your previous subscription ID here. This allows your @CKSyncEngine@ integration to be backward compatible with previous versions of your app.
--
-- >Note: @CKSyncEngine@ will automatically attempt to discover any previous database subscriptions, but you can be more explicit by giving the subscription ID through this configuration option.
--
-- ObjC selector: @- setSubscriptionID:@
setSubscriptionID :: (IsCKSyncEngineConfiguration ckSyncEngineConfiguration, IsNSString value) => ckSyncEngineConfiguration -> value -> IO ()
setSubscriptionID ckSyncEngineConfiguration value =
  sendMessage ckSyncEngineConfiguration setSubscriptionIDSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDatabase:stateSerialization:delegate:@
initWithDatabase_stateSerialization_delegateSelector :: Selector '[Id CKDatabase, Id CKSyncEngineStateSerialization, RawId] (Id CKSyncEngineConfiguration)
initWithDatabase_stateSerialization_delegateSelector = mkSelector "initWithDatabase:stateSerialization:delegate:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @database@
databaseSelector :: Selector '[] (Id CKDatabase)
databaseSelector = mkSelector "database"

-- | @Selector@ for @setDatabase:@
setDatabaseSelector :: Selector '[Id CKDatabase] ()
setDatabaseSelector = mkSelector "setDatabase:"

-- | @Selector@ for @stateSerialization@
stateSerializationSelector :: Selector '[] (Id CKSyncEngineStateSerialization)
stateSerializationSelector = mkSelector "stateSerialization"

-- | @Selector@ for @setStateSerialization:@
setStateSerializationSelector :: Selector '[Id CKSyncEngineStateSerialization] ()
setStateSerializationSelector = mkSelector "setStateSerialization:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @automaticallySync@
automaticallySyncSelector :: Selector '[] Bool
automaticallySyncSelector = mkSelector "automaticallySync"

-- | @Selector@ for @setAutomaticallySync:@
setAutomaticallySyncSelector :: Selector '[Bool] ()
setAutomaticallySyncSelector = mkSelector "setAutomaticallySync:"

-- | @Selector@ for @subscriptionID@
subscriptionIDSelector :: Selector '[] (Id NSString)
subscriptionIDSelector = mkSelector "subscriptionID"

-- | @Selector@ for @setSubscriptionID:@
setSubscriptionIDSelector :: Selector '[Id NSString] ()
setSubscriptionIDSelector = mkSelector "setSubscriptionID:"

