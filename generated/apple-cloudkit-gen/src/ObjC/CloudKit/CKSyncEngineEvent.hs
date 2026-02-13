{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An event that occurs during the operation of a @CKSyncEngine@.
--
-- While syncing, @CKSyncEngine@ posts several different types of events. Each event has an associated struct value with details describing the nature of the event.
--
-- At a high level, the sync engine events can be grouped into a few different categories:
--
-- ## Local state changes
--
-- - ``CKSyncEngineStateUpdateEvent`` - ``CKSyncEngineAccountChangeEvent``
--
-- ## Fetched changes
--
-- - ``CKSyncEngineFetchedDatabaseChangesEvent`` - ``CKSyncEngineFetchedRecordZoneChangesEvent``
--
-- ## Sent changes
--
-- - ``CKSyncEngineSentDatabaseChangesEvent`` - ``CKSyncEngineSentRecordZoneChangesEvent``
--
-- ## Fetch changes lifecycle
--
-- - ``CKSyncEngineWillFetchChangesEvent`` - ``CKSyncEngineWillFetchRecordZoneChangesEvent`` - ``CKSyncEngineDidFetchRecordZoneChangesEvent`` - ``CKSyncEngineDidFetchChangesEvent``
--
-- ## Send changes lifecycle
--
-- - ``CKSyncEngineWillSendChangesEvent`` - ``CKSyncEngineDidSendChangesEvent``
--
-- See the documentation for each event struct for more details about when and why an event might be posted.
--
-- Generated bindings for @CKSyncEngineEvent@.
module ObjC.CloudKit.CKSyncEngineEvent
  ( CKSyncEngineEvent
  , IsCKSyncEngineEvent(..)
  , init_
  , new
  , type_
  , stateUpdateEvent
  , accountChangeEvent
  , willFetchChangesEvent
  , fetchedDatabaseChangesEvent
  , didFetchChangesEvent
  , willFetchRecordZoneChangesEvent
  , fetchedRecordZoneChangesEvent
  , didFetchRecordZoneChangesEvent
  , willSendChangesEvent
  , sentDatabaseChangesEvent
  , sentRecordZoneChangesEvent
  , didSendChangesEvent
  , accountChangeEventSelector
  , didFetchChangesEventSelector
  , didFetchRecordZoneChangesEventSelector
  , didSendChangesEventSelector
  , fetchedDatabaseChangesEventSelector
  , fetchedRecordZoneChangesEventSelector
  , initSelector
  , newSelector
  , sentDatabaseChangesEventSelector
  , sentRecordZoneChangesEventSelector
  , stateUpdateEventSelector
  , typeSelector
  , willFetchChangesEventSelector
  , willFetchRecordZoneChangesEventSelector
  , willSendChangesEventSelector

  -- * Enum types
  , CKSyncEngineEventType(CKSyncEngineEventType)
  , pattern CKSyncEngineEventTypeStateUpdate
  , pattern CKSyncEngineEventTypeAccountChange
  , pattern CKSyncEngineEventTypeFetchedDatabaseChanges
  , pattern CKSyncEngineEventTypeFetchedRecordZoneChanges
  , pattern CKSyncEngineEventTypeSentDatabaseChanges
  , pattern CKSyncEngineEventTypeSentRecordZoneChanges
  , pattern CKSyncEngineEventTypeWillFetchChanges
  , pattern CKSyncEngineEventTypeWillFetchRecordZoneChanges
  , pattern CKSyncEngineEventTypeDidFetchRecordZoneChanges
  , pattern CKSyncEngineEventTypeDidFetchChanges
  , pattern CKSyncEngineEventTypeWillSendChanges
  , pattern CKSyncEngineEventTypeDidSendChanges

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
init_ :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineEvent)
init_ ckSyncEngineEvent =
  sendOwnedMessage ckSyncEngineEvent initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineEvent)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- type@
type_ :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO CKSyncEngineEventType
type_ ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent typeSelector

-- | @- stateUpdateEvent@
stateUpdateEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineStateUpdateEvent)
stateUpdateEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent stateUpdateEventSelector

-- | @- accountChangeEvent@
accountChangeEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineAccountChangeEvent)
accountChangeEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent accountChangeEventSelector

-- | @- willFetchChangesEvent@
willFetchChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillFetchChangesEvent)
willFetchChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent willFetchChangesEventSelector

-- | @- fetchedDatabaseChangesEvent@
fetchedDatabaseChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineFetchedDatabaseChangesEvent)
fetchedDatabaseChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent fetchedDatabaseChangesEventSelector

-- | @- didFetchChangesEvent@
didFetchChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidFetchChangesEvent)
didFetchChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent didFetchChangesEventSelector

-- | @- willFetchRecordZoneChangesEvent@
willFetchRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillFetchRecordZoneChangesEvent)
willFetchRecordZoneChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent willFetchRecordZoneChangesEventSelector

-- | @- fetchedRecordZoneChangesEvent@
fetchedRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineFetchedRecordZoneChangesEvent)
fetchedRecordZoneChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent fetchedRecordZoneChangesEventSelector

-- | @- didFetchRecordZoneChangesEvent@
didFetchRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidFetchRecordZoneChangesEvent)
didFetchRecordZoneChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent didFetchRecordZoneChangesEventSelector

-- | @- willSendChangesEvent@
willSendChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillSendChangesEvent)
willSendChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent willSendChangesEventSelector

-- | @- sentDatabaseChangesEvent@
sentDatabaseChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineSentDatabaseChangesEvent)
sentDatabaseChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent sentDatabaseChangesEventSelector

-- | @- sentRecordZoneChangesEvent@
sentRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineSentRecordZoneChangesEvent)
sentRecordZoneChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent sentRecordZoneChangesEventSelector

-- | @- didSendChangesEvent@
didSendChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidSendChangesEvent)
didSendChangesEvent ckSyncEngineEvent =
  sendMessage ckSyncEngineEvent didSendChangesEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CKSyncEngineEventType
typeSelector = mkSelector "type"

-- | @Selector@ for @stateUpdateEvent@
stateUpdateEventSelector :: Selector '[] (Id CKSyncEngineStateUpdateEvent)
stateUpdateEventSelector = mkSelector "stateUpdateEvent"

-- | @Selector@ for @accountChangeEvent@
accountChangeEventSelector :: Selector '[] (Id CKSyncEngineAccountChangeEvent)
accountChangeEventSelector = mkSelector "accountChangeEvent"

-- | @Selector@ for @willFetchChangesEvent@
willFetchChangesEventSelector :: Selector '[] (Id CKSyncEngineWillFetchChangesEvent)
willFetchChangesEventSelector = mkSelector "willFetchChangesEvent"

-- | @Selector@ for @fetchedDatabaseChangesEvent@
fetchedDatabaseChangesEventSelector :: Selector '[] (Id CKSyncEngineFetchedDatabaseChangesEvent)
fetchedDatabaseChangesEventSelector = mkSelector "fetchedDatabaseChangesEvent"

-- | @Selector@ for @didFetchChangesEvent@
didFetchChangesEventSelector :: Selector '[] (Id CKSyncEngineDidFetchChangesEvent)
didFetchChangesEventSelector = mkSelector "didFetchChangesEvent"

-- | @Selector@ for @willFetchRecordZoneChangesEvent@
willFetchRecordZoneChangesEventSelector :: Selector '[] (Id CKSyncEngineWillFetchRecordZoneChangesEvent)
willFetchRecordZoneChangesEventSelector = mkSelector "willFetchRecordZoneChangesEvent"

-- | @Selector@ for @fetchedRecordZoneChangesEvent@
fetchedRecordZoneChangesEventSelector :: Selector '[] (Id CKSyncEngineFetchedRecordZoneChangesEvent)
fetchedRecordZoneChangesEventSelector = mkSelector "fetchedRecordZoneChangesEvent"

-- | @Selector@ for @didFetchRecordZoneChangesEvent@
didFetchRecordZoneChangesEventSelector :: Selector '[] (Id CKSyncEngineDidFetchRecordZoneChangesEvent)
didFetchRecordZoneChangesEventSelector = mkSelector "didFetchRecordZoneChangesEvent"

-- | @Selector@ for @willSendChangesEvent@
willSendChangesEventSelector :: Selector '[] (Id CKSyncEngineWillSendChangesEvent)
willSendChangesEventSelector = mkSelector "willSendChangesEvent"

-- | @Selector@ for @sentDatabaseChangesEvent@
sentDatabaseChangesEventSelector :: Selector '[] (Id CKSyncEngineSentDatabaseChangesEvent)
sentDatabaseChangesEventSelector = mkSelector "sentDatabaseChangesEvent"

-- | @Selector@ for @sentRecordZoneChangesEvent@
sentRecordZoneChangesEventSelector :: Selector '[] (Id CKSyncEngineSentRecordZoneChangesEvent)
sentRecordZoneChangesEventSelector = mkSelector "sentRecordZoneChangesEvent"

-- | @Selector@ for @didSendChangesEvent@
didSendChangesEventSelector :: Selector '[] (Id CKSyncEngineDidSendChangesEvent)
didSendChangesEventSelector = mkSelector "didSendChangesEvent"

