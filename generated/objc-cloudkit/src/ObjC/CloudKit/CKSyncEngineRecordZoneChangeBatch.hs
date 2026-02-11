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
  , initWithPendingChanges_recordProviderSelector
  , initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector
  , initSelector
  , newSelector
  , recordsToSaveSelector
  , recordIDsToDeleteSelector
  , atomicByZoneSelector
  , setAtomicByZoneSelector


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
initWithPendingChanges_recordProvider ckSyncEngineRecordZoneChangeBatch  pendingChanges recordProvider =
withObjCPtr pendingChanges $ \raw_pendingChanges ->
    sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "initWithPendingChanges:recordProvider:") (retPtr retVoid) [argPtr (castPtr raw_pendingChanges :: Ptr ()), argPtr (castPtr recordProvider :: Ptr ())] >>= ownedObject . castPtr

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
initWithRecordsToSave_recordIDsToDelete_atomicByZone ckSyncEngineRecordZoneChangeBatch  recordsToSave recordIDsToDelete atomicByZone =
withObjCPtr recordsToSave $ \raw_recordsToSave ->
  withObjCPtr recordIDsToDelete $ \raw_recordIDsToDelete ->
      sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "initWithRecordsToSave:recordIDsToDelete:atomicByZone:") (retPtr retVoid) [argPtr (castPtr raw_recordsToSave :: Ptr ()), argPtr (castPtr raw_recordIDsToDelete :: Ptr ()), argCULong (if atomicByZone then 1 else 0)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id CKSyncEngineRecordZoneChangeBatch)
init_ ckSyncEngineRecordZoneChangeBatch  =
  sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineRecordZoneChangeBatch)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineRecordZoneChangeBatch"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The records to save to the server.
--
-- ObjC selector: @- recordsToSave@
recordsToSave :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id NSArray)
recordsToSave ckSyncEngineRecordZoneChangeBatch  =
  sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "recordsToSave") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The IDs of the records to delete from the server.
--
-- ObjC selector: @- recordIDsToDelete@
recordIDsToDelete :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO (Id NSArray)
recordIDsToDelete ckSyncEngineRecordZoneChangeBatch  =
  sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "recordIDsToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If set to true, the sync engine will modify these records atomically by zone.
--
-- If this is true, and if any record change fails, then any other changes from that zone in this batch will also fail with ``CKError/batchRequestFailed``.
--
-- Records that exist in different zones will not be modified together atomically.
--
-- ObjC selector: @- atomicByZone@
atomicByZone :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> IO Bool
atomicByZone ckSyncEngineRecordZoneChangeBatch  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "atomicByZone") retCULong []

-- | If set to true, the sync engine will modify these records atomically by zone.
--
-- If this is true, and if any record change fails, then any other changes from that zone in this batch will also fail with ``CKError/batchRequestFailed``.
--
-- Records that exist in different zones will not be modified together atomically.
--
-- ObjC selector: @- setAtomicByZone:@
setAtomicByZone :: IsCKSyncEngineRecordZoneChangeBatch ckSyncEngineRecordZoneChangeBatch => ckSyncEngineRecordZoneChangeBatch -> Bool -> IO ()
setAtomicByZone ckSyncEngineRecordZoneChangeBatch  value =
  sendMsg ckSyncEngineRecordZoneChangeBatch (mkSelector "setAtomicByZone:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPendingChanges:recordProvider:@
initWithPendingChanges_recordProviderSelector :: Selector
initWithPendingChanges_recordProviderSelector = mkSelector "initWithPendingChanges:recordProvider:"

-- | @Selector@ for @initWithRecordsToSave:recordIDsToDelete:atomicByZone:@
initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector :: Selector
initWithRecordsToSave_recordIDsToDelete_atomicByZoneSelector = mkSelector "initWithRecordsToSave:recordIDsToDelete:atomicByZone:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordsToSave@
recordsToSaveSelector :: Selector
recordsToSaveSelector = mkSelector "recordsToSave"

-- | @Selector@ for @recordIDsToDelete@
recordIDsToDeleteSelector :: Selector
recordIDsToDeleteSelector = mkSelector "recordIDsToDelete"

-- | @Selector@ for @atomicByZone@
atomicByZoneSelector :: Selector
atomicByZoneSelector = mkSelector "atomicByZone"

-- | @Selector@ for @setAtomicByZone:@
setAtomicByZoneSelector :: Selector
setAtomicByZoneSelector = mkSelector "setAtomicByZone:"

