{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that tracks some state required for proper and efficient operation of ``CKSyncEngine-5sie5``.
--
-- ``CKSyncEngine-5sie5`` needs to track several things in order to properly sync. For example, it needs to remember the last server change tokens for your database and zones. It also needs to keep track of things like the last known user record ID and other various pieces of state.
--
-- A lot of this state is hidden internally, but some of it you can control.
--
-- ## Pending changes
--
-- One of the main things you can control is the list of pending changes to send to the server. You can control these by calling functions like ``addPendingDatabaseChanges:`` and ``addPendingRecordZoneChanges:``. When you add new pending changes, the sync engine will automatically schedule a task to sync with the server.
--
-- ## State serialization
--
-- ``CKSyncEngine-5sie5`` will occasionally update its state in the background. When it updates its state, your delegate will receive a ``CKSyncEngineStateUpdateEvent``.
--
-- This event will contain a ``CKSyncEngineStateSerialization``, which you should persist locally. The next time your process launches, you initialize your sync engine with the last state serialization you received.
--
-- Generated bindings for @CKSyncEngineState@.
module ObjC.CloudKit.CKSyncEngineState
  ( CKSyncEngineState
  , IsCKSyncEngineState(..)
  , init_
  , new
  , addPendingRecordZoneChanges
  , removePendingRecordZoneChanges
  , addPendingDatabaseChanges
  , removePendingDatabaseChanges
  , pendingRecordZoneChanges
  , pendingDatabaseChanges
  , hasPendingUntrackedChanges
  , setHasPendingUntrackedChanges
  , zoneIDsWithUnfetchedServerChanges
  , initSelector
  , newSelector
  , addPendingRecordZoneChangesSelector
  , removePendingRecordZoneChangesSelector
  , addPendingDatabaseChangesSelector
  , removePendingDatabaseChangesSelector
  , pendingRecordZoneChangesSelector
  , pendingDatabaseChangesSelector
  , hasPendingUntrackedChangesSelector
  , setHasPendingUntrackedChangesSelector
  , zoneIDsWithUnfetchedServerChangesSelector


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
init_ :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> IO (Id CKSyncEngineState)
init_ ckSyncEngineState  =
  sendMsg ckSyncEngineState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineState)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Adds to the list of pending record zone changes.
--
-- When you add a new pending change, the sync engine will automatically schedule a sync task.
--
-- The sync engine will ensure consistency and deduplicate these changes under the hood.
--
-- ObjC selector: @- addPendingRecordZoneChanges:@
addPendingRecordZoneChanges :: (IsCKSyncEngineState ckSyncEngineState, IsNSArray changes) => ckSyncEngineState -> changes -> IO ()
addPendingRecordZoneChanges ckSyncEngineState  changes =
withObjCPtr changes $ \raw_changes ->
    sendMsg ckSyncEngineState (mkSelector "addPendingRecordZoneChanges:") retVoid [argPtr (castPtr raw_changes :: Ptr ())]

-- | Removes from the list of pending record zone changes.
--
-- ObjC selector: @- removePendingRecordZoneChanges:@
removePendingRecordZoneChanges :: (IsCKSyncEngineState ckSyncEngineState, IsNSArray changes) => ckSyncEngineState -> changes -> IO ()
removePendingRecordZoneChanges ckSyncEngineState  changes =
withObjCPtr changes $ \raw_changes ->
    sendMsg ckSyncEngineState (mkSelector "removePendingRecordZoneChanges:") retVoid [argPtr (castPtr raw_changes :: Ptr ())]

-- | Adds to the list of pending database changes.
--
-- When you add a new pending change, the sync engine will automatically schedule a sync task.
--
-- The sync engine will ensure consistency and deduplicate these changes under the hood.
--
-- ObjC selector: @- addPendingDatabaseChanges:@
addPendingDatabaseChanges :: (IsCKSyncEngineState ckSyncEngineState, IsNSArray changes) => ckSyncEngineState -> changes -> IO ()
addPendingDatabaseChanges ckSyncEngineState  changes =
withObjCPtr changes $ \raw_changes ->
    sendMsg ckSyncEngineState (mkSelector "addPendingDatabaseChanges:") retVoid [argPtr (castPtr raw_changes :: Ptr ())]

-- | Removes from the list of pending database changes.
--
-- ObjC selector: @- removePendingDatabaseChanges:@
removePendingDatabaseChanges :: (IsCKSyncEngineState ckSyncEngineState, IsNSArray changes) => ckSyncEngineState -> changes -> IO ()
removePendingDatabaseChanges ckSyncEngineState  changes =
withObjCPtr changes $ \raw_changes ->
    sendMsg ckSyncEngineState (mkSelector "removePendingDatabaseChanges:") retVoid [argPtr (castPtr raw_changes :: Ptr ())]

-- | A list of record changes that need to be sent to the server.
--
-- ``CKSyncEngine-5sie5`` provides the convenience of tracking your pending record zone changes. When the user makes some changes that need to be sent to the server, you can track them in this list. Then, you can use this list when creating your next ``CKSyncEngineRecordZoneChangeBatch`` in your ``CKSyncEngineDelegate-1q7g8``.
--
-- The sync engine will ensure consistency and deduplicate these pending changes under the hood. For example, if you add a pending save for record A, then record B, then record A again, this will result in a list of @[saveRecordA, saveRecordB]@. Similarly, if you add a pending save for record A, then add a pending delete for the same record A, this will result in a single pending change of @[deleteRecordA]@.
--
-- The sync engine will manage this list while it sends changes to the server. For example, when it successfully saves a record, it will remove that change from this list. If it fails to send a change due to some retryable error (e.g. a network failure), it will keep that change in this list.
--
-- If you'd prefer to track pending changes yourself, you can use ``CKSyncEngine/State/hasPendingUntrackedChanges`` instead.
--
-- ObjC selector: @- pendingRecordZoneChanges@
pendingRecordZoneChanges :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> IO (Id NSArray)
pendingRecordZoneChanges ckSyncEngineState  =
  sendMsg ckSyncEngineState (mkSelector "pendingRecordZoneChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of database changes that need to be sent to the server, similar to @pendingRecordZoneChanges@.
--
-- ObjC selector: @- pendingDatabaseChanges@
pendingDatabaseChanges :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> IO (Id NSArray)
pendingDatabaseChanges ckSyncEngineState  =
  sendMsg ckSyncEngineState (mkSelector "pendingDatabaseChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This represents whether or not you have pending changes to send to the server that aren't tracked in ``CKSyncEngine/State/pendingRecordZoneChanges``. This is useful if you want to track pending changes in your own local database instead of the sync engine state.
--
-- When this property is set, the sync engine will automatically schedule a sync. When the sync task runs, it will ask your delegate for pending changes in ``CKSyncEngineDelegate/nextRecordZoneChangeBatch(_:syncEngine:)``.
--
-- ObjC selector: @- hasPendingUntrackedChanges@
hasPendingUntrackedChanges :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> IO Bool
hasPendingUntrackedChanges ckSyncEngineState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineState (mkSelector "hasPendingUntrackedChanges") retCULong []

-- | This represents whether or not you have pending changes to send to the server that aren't tracked in ``CKSyncEngine/State/pendingRecordZoneChanges``. This is useful if you want to track pending changes in your own local database instead of the sync engine state.
--
-- When this property is set, the sync engine will automatically schedule a sync. When the sync task runs, it will ask your delegate for pending changes in ``CKSyncEngineDelegate/nextRecordZoneChangeBatch(_:syncEngine:)``.
--
-- ObjC selector: @- setHasPendingUntrackedChanges:@
setHasPendingUntrackedChanges :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> Bool -> IO ()
setHasPendingUntrackedChanges ckSyncEngineState  value =
  sendMsg ckSyncEngineState (mkSelector "setHasPendingUntrackedChanges:") retVoid [argCULong (if value then 1 else 0)]

-- | The list of zone IDs that have new changes to fetch from the server. ``CKSyncEngine-5sie5`` keeps track of these zones and will update this list as it receives new information.
--
-- ObjC selector: @- zoneIDsWithUnfetchedServerChanges@
zoneIDsWithUnfetchedServerChanges :: IsCKSyncEngineState ckSyncEngineState => ckSyncEngineState -> IO (Id NSArray)
zoneIDsWithUnfetchedServerChanges ckSyncEngineState  =
  sendMsg ckSyncEngineState (mkSelector "zoneIDsWithUnfetchedServerChanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @addPendingRecordZoneChanges:@
addPendingRecordZoneChangesSelector :: Selector
addPendingRecordZoneChangesSelector = mkSelector "addPendingRecordZoneChanges:"

-- | @Selector@ for @removePendingRecordZoneChanges:@
removePendingRecordZoneChangesSelector :: Selector
removePendingRecordZoneChangesSelector = mkSelector "removePendingRecordZoneChanges:"

-- | @Selector@ for @addPendingDatabaseChanges:@
addPendingDatabaseChangesSelector :: Selector
addPendingDatabaseChangesSelector = mkSelector "addPendingDatabaseChanges:"

-- | @Selector@ for @removePendingDatabaseChanges:@
removePendingDatabaseChangesSelector :: Selector
removePendingDatabaseChangesSelector = mkSelector "removePendingDatabaseChanges:"

-- | @Selector@ for @pendingRecordZoneChanges@
pendingRecordZoneChangesSelector :: Selector
pendingRecordZoneChangesSelector = mkSelector "pendingRecordZoneChanges"

-- | @Selector@ for @pendingDatabaseChanges@
pendingDatabaseChangesSelector :: Selector
pendingDatabaseChangesSelector = mkSelector "pendingDatabaseChanges"

-- | @Selector@ for @hasPendingUntrackedChanges@
hasPendingUntrackedChangesSelector :: Selector
hasPendingUntrackedChangesSelector = mkSelector "hasPendingUntrackedChanges"

-- | @Selector@ for @setHasPendingUntrackedChanges:@
setHasPendingUntrackedChangesSelector :: Selector
setHasPendingUntrackedChangesSelector = mkSelector "setHasPendingUntrackedChanges:"

-- | @Selector@ for @zoneIDsWithUnfetchedServerChanges@
zoneIDsWithUnfetchedServerChangesSelector :: Selector
zoneIDsWithUnfetchedServerChangesSelector = mkSelector "zoneIDsWithUnfetchedServerChanges"

