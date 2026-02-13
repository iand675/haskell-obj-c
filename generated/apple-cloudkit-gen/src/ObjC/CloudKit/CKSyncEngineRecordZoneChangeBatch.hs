{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A batch of record zone changes that @CKSyncEngine@ will send to the server in a single request.
--
-- Generated bindings for @CKSyncEngineRecordZoneChangeBatch@.
module ObjC.CloudKit.CKSyncEngineRecordZoneChangeBatch
  ( CKSyncEngineRecordZoneChangeBatch
  , IsCKSyncEngineRecordZoneChangeBatch(..)
  , initWithPendingChanges_recordProvider
  , initWithRecordsToSave_recordIDsToDelete_atomicByZone
  , init_
  , new
  , recordsToSave
  , recordIDsToDelete
  , atomicByZone
  , setAtomicByZone
  , atomicByZoneSelector
  , initSelector
  , initWithPendingChanges_recordProviderSelector
  , initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector
  , newSelector
  , recordIDsToDeleteSelector
  , recordsToSaveSelector
  , setAtomicByZoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a batch of record zone changes according to a list of pending changes.
--
-- This will iterate over the pending changes in order and add them to the batch until it reaches the max batch size.
--
-- When it sees a pending save, it will ask the record provider for the actual ``CKRecord`` to send to the server. If you return @nil@ from the record provider, this will skip to the next pending change.
--
-- This will return @nil@ if there are no pending changes to send.
--
-- ObjC selector: @- initWithPendingChanges:recordProvider:@
initWithPendingChanges_recordProvider :: (IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch, IsNSArray pendingChanges) => ckSyncEngineRecordZoneChangeBatch -> pendingChanges -> Ptr () -> IO (Id CKSyncEngineRecordZoneChangeBatch)
initWithPendingChanges_recordProvider ckSyncEngineRecordZoneChangeBatch pendingChanges recordProvider =
  sendOwnedMessage ckSyncEngineRecordZoneChangeBatch initWithPendingChanges_recordProviderSelector (toNSArray pendingChanges) recordProvider

-- | Creates a batch of record zone changes to send to the server with a specific set of changes.
--
-- If you'd like to construct your own custom batches of changes to send to the server, you can do so with this initializer.
--
-- ## Batch size limitations
--
-- When creating your own batches, you need to consider batch size limitations. There is a maximum count and size of records that can be sent to the server in a single batch. If you supply too many changes, or if the total size of the records is too large, then you might get a ``CKError/limitExceeded``.
--
-- > Tip: These batch size limitations are handled automatically by the ``initWithPendingChanges:recordProvider:`` initializer.
--
-- ObjC selector: @- initWithRecordsToSave:recordIDsToDelete:atomicByZone:@
initWithRecordsToSave_recordIDsToDelete_atomicByZone :: (IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch, IsNSArray recordsToSave, IsNSArray recordIDsToDelete) => ckSyncEngineRecordZoneChangeBatch -> recordsToSave -> recordIDsToDelete -> Bool -> IO (Id CKSyncEngineRecordZoneChangeBatch)
initWithRecordsToSave_recordIDsToDelete_atomicByZone ckSyncEngineRecordZoneChangeBatch recordsToSave recordIDsToDelete atomicByZone =
  sendOwnedMessage ckSyncEngineRecordZoneChangeBatch initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector (toNSArray recordsToSave) (toNSArray recordIDsToDelete) atomicByZone

-- | @- init@
init_ :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id CKSyncEngineRecordZoneChangeBatch)
init_ ckSyncEngineRecordZoneChangeBatch =
  sendOwnedMessage ckSyncEngineRecordZoneChangeBatch initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineRecordZoneChangeBatch)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineRecordZoneChangeBatch"
    sendOwnedClassMessage cls' newSelector

-- | The records to save to the server.
--
-- ObjC selector: @- recordsToSave@
recordsToSave :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id NSArray)
recordsToSave ckSyncEngineRecordZoneChangeBatch =
  sendMessage ckSyncEngineRecordZoneChangeBatch recordsToSaveSelector

-- | The IDs of the records to delete from the server.
--
-- ObjC selector: @- recordIDsToDelete@
recordIDsToDelete :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id NSArray)
recordIDsToDelete ckSyncEngineRecordZoneChangeBatch =
  sendMessage ckSyncEngineRecordZoneChangeBatch recordIDsToDeleteSelector

-- | If set to true, the sync engine will modify these records atomically by zone.
--
-- If this is true, and if any record change fails, then any other changes from that zone in this batch will also fail with ``CKError/batchRequestFailed``.
--
-- Records that exist in different zones will not be modified together atomically.
--
-- ObjC selector: @- atomicByZone@
atomicByZone :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO Bool
atomicByZone ckSyncEngineRecordZoneChangeBatch =
  sendMessage ckSyncEngineRecordZoneChangeBatch atomicByZoneSelector

-- | If set to true, the sync engine will modify these records atomically by zone.
--
-- If this is true, and if any record change fails, then any other changes from that zone in this batch will also fail with ``CKError/batchRequestFailed``.
--
-- Records that exist in different zones will not be modified together atomically.
--
-- ObjC selector: @- setAtomicByZone:@
setAtomicByZone :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> Bool -> IO ()
setAtomicByZone ckSyncEngineRecordZoneChangeBatch value =
  sendMessage ckSyncEngineRecordZoneChangeBatch setAtomicByZoneSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPendingChanges:recordProvider:@
initWithPendingChanges_recordProviderSelector :: Selector '[Id NSArray, Ptr ()] (Id CKSyncEngineRecordZoneChangeBatch)
initWithPendingChanges_recordProviderSelector = mkSelector "initWithPendingChanges:recordProvider:"

-- | @Selector@ for @initWithRecordsToSave:recordIDsToDelete:atomicByZone:@
initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector :: Selector '[Id NSArray, Id NSArray, Bool] (Id CKSyncEngineRecordZoneChangeBatch)
initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector = mkSelector "initWithRecordsToSave:recordIDsToDelete:atomicByZone:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineRecordZoneChangeBatch)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineRecordZoneChangeBatch)
newSelector = mkSelector "new"

-- | @Selector@ for @recordsToSave@
recordsToSaveSelector :: Selector '[] (Id NSArray)
recordsToSaveSelector = mkSelector "recordsToSave"

-- | @Selector@ for @recordIDsToDelete@
recordIDsToDeleteSelector :: Selector '[] (Id NSArray)
recordIDsToDeleteSelector = mkSelector "recordIDsToDelete"

-- | @Selector@ for @atomicByZone@
atomicByZoneSelector :: Selector '[] Bool
atomicByZoneSelector = mkSelector "atomicByZone"

-- | @Selector@ for @setAtomicByZone:@
setAtomicByZoneSelector :: Selector '[Bool] ()
setAtomicByZoneSelector = mkSelector "setAtomicByZone:"

