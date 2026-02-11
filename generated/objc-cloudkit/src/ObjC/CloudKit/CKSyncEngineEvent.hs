{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , typeSelector
  , stateUpdateEventSelector
  , accountChangeEventSelector
  , willFetchChangesEventSelector
  , fetchedDatabaseChangesEventSelector
  , didFetchChangesEventSelector
  , willFetchRecordZoneChangesEventSelector
  , fetchedRecordZoneChangesEventSelector
  , didFetchRecordZoneChangesEventSelector
  , willSendChangesEventSelector
  , sentDatabaseChangesEventSelector
  , sentRecordZoneChangesEventSelector
  , didSendChangesEventSelector

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
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineEvent)
init_ ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineEvent)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- type@
type_ :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO CKSyncEngineEventType
type_ ckSyncEngineEvent  =
  fmap (coerce :: CLong -> CKSyncEngineEventType) $ sendMsg ckSyncEngineEvent (mkSelector "type") retCLong []

-- | @- stateUpdateEvent@
stateUpdateEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineStateUpdateEvent)
stateUpdateEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "stateUpdateEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountChangeEvent@
accountChangeEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineAccountChangeEvent)
accountChangeEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "accountChangeEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- willFetchChangesEvent@
willFetchChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillFetchChangesEvent)
willFetchChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "willFetchChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchedDatabaseChangesEvent@
fetchedDatabaseChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineFetchedDatabaseChangesEvent)
fetchedDatabaseChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "fetchedDatabaseChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- didFetchChangesEvent@
didFetchChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidFetchChangesEvent)
didFetchChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "didFetchChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- willFetchRecordZoneChangesEvent@
willFetchRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillFetchRecordZoneChangesEvent)
willFetchRecordZoneChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "willFetchRecordZoneChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchedRecordZoneChangesEvent@
fetchedRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineFetchedRecordZoneChangesEvent)
fetchedRecordZoneChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "fetchedRecordZoneChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- didFetchRecordZoneChangesEvent@
didFetchRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidFetchRecordZoneChangesEvent)
didFetchRecordZoneChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "didFetchRecordZoneChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- willSendChangesEvent@
willSendChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineWillSendChangesEvent)
willSendChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "willSendChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sentDatabaseChangesEvent@
sentDatabaseChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineSentDatabaseChangesEvent)
sentDatabaseChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "sentDatabaseChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sentRecordZoneChangesEvent@
sentRecordZoneChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineSentRecordZoneChangesEvent)
sentRecordZoneChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "sentRecordZoneChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- didSendChangesEvent@
didSendChangesEvent :: IsCKSyncEngineEvent ckSyncEngineEvent => ckSyncEngineEvent -> IO (Id CKSyncEngineDidSendChangesEvent)
didSendChangesEvent ckSyncEngineEvent  =
  sendMsg ckSyncEngineEvent (mkSelector "didSendChangesEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @stateUpdateEvent@
stateUpdateEventSelector :: Selector
stateUpdateEventSelector = mkSelector "stateUpdateEvent"

-- | @Selector@ for @accountChangeEvent@
accountChangeEventSelector :: Selector
accountChangeEventSelector = mkSelector "accountChangeEvent"

-- | @Selector@ for @willFetchChangesEvent@
willFetchChangesEventSelector :: Selector
willFetchChangesEventSelector = mkSelector "willFetchChangesEvent"

-- | @Selector@ for @fetchedDatabaseChangesEvent@
fetchedDatabaseChangesEventSelector :: Selector
fetchedDatabaseChangesEventSelector = mkSelector "fetchedDatabaseChangesEvent"

-- | @Selector@ for @didFetchChangesEvent@
didFetchChangesEventSelector :: Selector
didFetchChangesEventSelector = mkSelector "didFetchChangesEvent"

-- | @Selector@ for @willFetchRecordZoneChangesEvent@
willFetchRecordZoneChangesEventSelector :: Selector
willFetchRecordZoneChangesEventSelector = mkSelector "willFetchRecordZoneChangesEvent"

-- | @Selector@ for @fetchedRecordZoneChangesEvent@
fetchedRecordZoneChangesEventSelector :: Selector
fetchedRecordZoneChangesEventSelector = mkSelector "fetchedRecordZoneChangesEvent"

-- | @Selector@ for @didFetchRecordZoneChangesEvent@
didFetchRecordZoneChangesEventSelector :: Selector
didFetchRecordZoneChangesEventSelector = mkSelector "didFetchRecordZoneChangesEvent"

-- | @Selector@ for @willSendChangesEvent@
willSendChangesEventSelector :: Selector
willSendChangesEventSelector = mkSelector "willSendChangesEvent"

-- | @Selector@ for @sentDatabaseChangesEvent@
sentDatabaseChangesEventSelector :: Selector
sentDatabaseChangesEventSelector = mkSelector "sentDatabaseChangesEvent"

-- | @Selector@ for @sentRecordZoneChangesEvent@
sentRecordZoneChangesEventSelector :: Selector
sentRecordZoneChangesEventSelector = mkSelector "sentRecordZoneChangesEvent"

-- | @Selector@ for @didSendChangesEvent@
didSendChangesEventSelector :: Selector
didSendChangesEventSelector = mkSelector "didSendChangesEvent"

