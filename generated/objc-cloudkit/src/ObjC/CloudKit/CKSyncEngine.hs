{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @CKSyncEngine@ encapsulates the logic of syncing data with a CloudKit database.
--
-- Syncing with CloudKit involves many moving pieces. Apps need to schedule syncs, create and batch operations, subscribe to database changes, listen for push notifications, store sync state, handle a multitude of errors, and more. @CKSyncEngine@ is designed to encapsulate this logic in a higher-level API.
--
-- # Start Your Sync Engine
--
-- Generally, you should initialize your @CKSyncEngine@ soon after your process launches. The sync engine will perform work in the background on your behalf, and it needs to be initialized so that it can properly listen for push notifications and handle scheduled sync tasks.
--
-- When initializing your sync engine, you need to provide an object conforming to the @CKSyncEngineDelegate@ protocol. This protocol is the main method of communication between the sync engine and your app. You also need to provide your last known version of the ``CKSyncEngine/State/Serialization``. See ``CKSyncEngine/State`` and ``CKSyncEngine/Event/StateUpdate`` for more details on the sync engine state.
--
-- Note that before using @CKSyncEngine@ in your app, you need to add the CloudKit and remote notification capabilities.
--
-- # Sending Changes to the Server
--
-- In order to send changes to the server, you first need to tell the sync engine you have pending changes to send. You can do this by adding pending changes to the sync engine's ``CKSyncEngine/state`` property.
--
-- When you add pending changes to the state, the sync engine will schedule a task to sync. When the sync task runs, the sync engine will start sending changes to the server. The sync engine will automatically send database changes from ``CKSyncEngine/State/pendingDatabaseChanges``, but you need to provide the record zone changes yourself. In order to send record zone changes, you need to return them from @-[CKSyncEngineDelegate syncEngine:nextRecordZoneChangeBatchForContext:]@.
--
-- When the sync engine finishes sending a batch of changes to the server, your @CKSyncEngineDelegate@ will receive ``CKSyncEngine/Event/sentDatabaseChanges(_:)`` and ``CKSyncEngine/Event/sentRecordZoneChanges(_:)`` events. These events will notify you of the success or failure of the changes you tried to send.
--
-- At a high level, sending changes to the server happens with the following order of operations:
--
-- 1. You add pending changes to ``CKSyncEngine/state``. 2. You receive ``CKSyncEngine/Event/willSendChanges(_:)`` in @-[CKSyncEngineDelegate syncEngine:handleEvent:]@ 3. If there are pending database changes, the sync engine sends the next batch. 4. If any database changes were sent, your delegate receives``CKSyncEngine/Event/sentDatabaseChanges(_:)``. 5. Repeat from step 3 until all pending database changes are sent, then move on to record zone changes in step 6. 6. The sync engine asks for the next batch of record zone changes by calling @-[CKSyncEngineDelegate syncEngine:nextRecordZoneChangeBatchForContext:]@. 7. The sync engine sends the next record zone change batch to the server. 8. If any record zone changes were sent, your delegate receives ``CKSyncEngine/Event/sentRecordZoneChanges(_:)``. 9. If you added any pending database changes during steps 6-8, the sync engine repeats from step 3. Otherwise, it repeats from step 6. 10. When all pending changes are sent, your delegate receives ``CKSyncEngine/Event/didSendChanges(_:)``.
--
-- # Fetching Changes from the Server
--
-- The sync engine will automatically listen for remote notifications, and it will fetch changes from the server when necessary. Generally, you'll receive events in this order:
--
-- 1. Your delegate receives ``CKSyncEngine/Event/willFetchChanges(_:)``. 2. If there are new database changes to fetch, you receive batches of them in ``CKSyncEngine/Event/fetchedDatabaseChanges(_:)`` events. 3. If there are new record zone changes to fetch, you will receive ``CKSyncEngine/Event/willFetchRecordZoneChanges(_:)`` for each zone that has new changes. 4. The sync engine fetches record zone changes and gives you batches of them in ``CKSyncEngine/Event/fetchedRecordZoneChanges(_:)`` events. 5. Your delegate receives ``CKSyncEngine/Event/didFetchRecordZoneChanges(_:)`` for each zone that had changes to fetch. 6. Your delegate receives ``CKSyncEngine/Event/didFetchChanges(_:)``, indicating that sync engine has finished fetching changes.
--
-- # Sync Scheduling
--
-- ## Automatic sync
--
-- By default, the sync engine will automatically schedule sync tasks on your behalf. If the user is signed in, the device has a network connection, and the system is generally in a good state, these scheduled syncs will happen relatively quickly. However, if the device has no network, is low on power, or is otherwise under a heavy load, these automatic syncs might be delayed. Similarly, if the user isn't signed in to an account, the sync engine won't perform any sync tasks at all.
--
-- ## Manual sync
--
-- Generally, you should rely on this automatic sync behavior, but there may be some cases where you want to manually trigger a sync. For example, if you have a pull-to-refresh UI, you can call ``CKSyncEngine/fetchChanges(_:)`` to tell the sync engine to fetch immediately. Or if you want to provide some sort of "backup now" button, you can call ``CKSyncEngine/sendChanges(_:)`` to send to the server immediately.
--
-- ### Testing
--
-- These manual sync functions might also be useful during automated testing. When writing automated tests, you can turn off automatic sync via ``CKSyncEngine/Configuration/automaticallySync``. Then, you'll have complete control over the ordering of sync events. This allows you to interject behavior in the sync flow and simulate specific sequences of events.
--
-- # Error Handling
--
-- There are some transient errors that the sync engine will handle automatically behind the scenes. The sync engine will retry the operations for these transient errors automatically when it makes sense to do so. Specifically, the sync engine will handle the following errors on your behalf:
--
-- * ``CKError/notAuthenticated`` * ``CKError/accountTemporarilyUnavailable`` * ``CKError/networkFailure`` * ``CKError/networkUnavailable`` * ``CKError/requestRateLimited`` * ``CKError/serviceUnavailable`` * ``CKError/zoneBusy``
--
-- When the sync engine encounters one of these errors, it will wait for the system to be in a good state and try again. For example, if the server sends back a @.requestRateLimited@ error, the sync engine will respect this throttle and try again after the retry-after time.
--
-- @CKSyncEngine@ will _not_ handle errors that require application-specific logic. For example, if you try to save a record and get a ``CKError/serverRecordChanged``, you need to handle that error yourself. There are plenty of errors that the sync engine cannot handle on your behalf, see ``CKError`` for a list of all the possible errors.
--
-- # Accounts
--
-- @CKSyncEngine@ monitors for account status, and it will only sync if there's an account signed in. Because of this, you can initialize your @CKSyncEngine@ at any time, regardless of account status. If there is no account, or if the user disabled sync in settings, the sync engine will stay dormant in the background. Once an account is available, the sync engine will start syncing automatically.
--
-- It will also listen for when the user signs in or out of their account. When it notices an account change, it will send an ``CKSyncEngine/Event/accountChange(_:)`` to your delegate. It's your responsibility to react appropriately to this change and update your local persistence.
--
-- Generated bindings for @CKSyncEngine@.
module ObjC.CloudKit.CKSyncEngine
  ( CKSyncEngine
  , IsCKSyncEngine(..)
  , initWithConfiguration
  , init_
  , new
  , fetchChangesWithCompletionHandler
  , fetchChangesWithOptions_completionHandler
  , sendChangesWithCompletionHandler
  , sendChangesWithOptions_completionHandler
  , cancelOperationsWithCompletionHandler
  , database
  , state
  , initWithConfigurationSelector
  , initSelector
  , newSelector
  , fetchChangesWithCompletionHandlerSelector
  , fetchChangesWithOptions_completionHandlerSelector
  , sendChangesWithCompletionHandlerSelector
  , sendChangesWithOptions_completionHandlerSelector
  , cancelOperationsWithCompletionHandlerSelector
  , databaseSelector
  , stateSelector


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

-- | Initializes a @CKSyncEngine@ with the given configuration. See properties on ``CKSyncEngineConfiguration`` for more details on all the options.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsCKSyncEngine ckSyncEngine, IsCKSyncEngineConfiguration configuration) => ckSyncEngine -> configuration -> IO (Id CKSyncEngine)
initWithConfiguration ckSyncEngine  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg ckSyncEngine (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> IO (Id CKSyncEngine)
init_ ckSyncEngine  =
  sendMsg ckSyncEngine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngine)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngine"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Fetches changes from the server immediately, bypassing the system scheduler.
--
-- By default, the sync engine will automatically fetch changes from the server for you, and you should not have to call this function. However, you can call this if for some reason you need to ensure all changes have been fetched from the server before proceeding. For example, you might use this in your tests to simulate specific sync scenarios.
--
-- Fetching changes from the server might result in some events being posted to your delegate via @handleEvent@. For example, you might receive a @CKSyncEngineWillFetchChangesEvent@ or @CKSyncEngineDidFetchChangesEvent@. This will not complete until all the relevant events have been handled by your delegate.
--
-- ObjC selector: @- fetchChangesWithCompletionHandler:@
fetchChangesWithCompletionHandler :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> Ptr () -> IO ()
fetchChangesWithCompletionHandler ckSyncEngine  completionHandler =
  sendMsg ckSyncEngine (mkSelector "fetchChangesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetches changes from the server with the specified options. See ``fetchChangesWithCompletionHandler:`` for more information.
--
-- ObjC selector: @- fetchChangesWithOptions:completionHandler:@
fetchChangesWithOptions_completionHandler :: (IsCKSyncEngine ckSyncEngine, IsCKSyncEngineFetchChangesOptions options) => ckSyncEngine -> options -> Ptr () -> IO ()
fetchChangesWithOptions_completionHandler ckSyncEngine  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg ckSyncEngine (mkSelector "fetchChangesWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Sends any pending changes to the server immediately, bypassing the system scheduler.
--
-- By default, the sync engine will automatically send changes to the server for you, and you should not have to call this function. However, you can call this if for some reason you need to ensure all changes have been sent to the server before proceeding. For example, you might consider using this in your tests to simulate specific sync scenarios.
--
-- Sending changes to the server might result in some events being posted to your delegate via @handleEvent@. For example, you might receive a @CKSyncEngineWillSendChangesEvent@ or @CKSyncEngineDidSendChangesEvent@. This function will not return until all the relevant events have been handled by your delegate.
--
-- ObjC selector: @- sendChangesWithCompletionHandler:@
sendChangesWithCompletionHandler :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> Ptr () -> IO ()
sendChangesWithCompletionHandler ckSyncEngine  completionHandler =
  sendMsg ckSyncEngine (mkSelector "sendChangesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Sends pending changes to the server with the specified options. See discussion in ``sendChangesWithCompletionHandler:`` for more information.
--
-- ObjC selector: @- sendChangesWithOptions:completionHandler:@
sendChangesWithOptions_completionHandler :: (IsCKSyncEngine ckSyncEngine, IsCKSyncEngineSendChangesOptions options) => ckSyncEngine -> options -> Ptr () -> IO ()
sendChangesWithOptions_completionHandler ckSyncEngine  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg ckSyncEngine (mkSelector "sendChangesWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Cancels any currently executing or pending sync operations.
--
-- Note that cancellation does not happen synchronously, and it's possible some in-flight operations will succeed.
--
-- ObjC selector: @- cancelOperationsWithCompletionHandler:@
cancelOperationsWithCompletionHandler :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> Ptr () -> IO ()
cancelOperationsWithCompletionHandler ckSyncEngine  completionHandler =
  sendMsg ckSyncEngine (mkSelector "cancelOperationsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | The database this sync engine will sync with.
--
-- ObjC selector: @- database@
database :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> IO (Id CKDatabase)
database ckSyncEngine  =
  sendMsg ckSyncEngine (mkSelector "database") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A collection of state properties used to efficiently manage sync engine operation. See ``CKSyncEngineState`` for more details.
--
-- ObjC selector: @- state@
state :: IsCKSyncEngine ckSyncEngine => ckSyncEngine -> IO (Id CKSyncEngineState)
state ckSyncEngine  =
  sendMsg ckSyncEngine (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fetchChangesWithCompletionHandler:@
fetchChangesWithCompletionHandlerSelector :: Selector
fetchChangesWithCompletionHandlerSelector = mkSelector "fetchChangesWithCompletionHandler:"

-- | @Selector@ for @fetchChangesWithOptions:completionHandler:@
fetchChangesWithOptions_completionHandlerSelector :: Selector
fetchChangesWithOptions_completionHandlerSelector = mkSelector "fetchChangesWithOptions:completionHandler:"

-- | @Selector@ for @sendChangesWithCompletionHandler:@
sendChangesWithCompletionHandlerSelector :: Selector
sendChangesWithCompletionHandlerSelector = mkSelector "sendChangesWithCompletionHandler:"

-- | @Selector@ for @sendChangesWithOptions:completionHandler:@
sendChangesWithOptions_completionHandlerSelector :: Selector
sendChangesWithOptions_completionHandlerSelector = mkSelector "sendChangesWithOptions:completionHandler:"

-- | @Selector@ for @cancelOperationsWithCompletionHandler:@
cancelOperationsWithCompletionHandlerSelector :: Selector
cancelOperationsWithCompletionHandlerSelector = mkSelector "cancelOperationsWithCompletionHandler:"

-- | @Selector@ for @database@
databaseSelector :: Selector
databaseSelector = mkSelector "database"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

